library(mlbench); library(ROCR); library(neuralnet); library(DMwR);
data("PimaIndiansDiabetes2"); library(ggplot2);
n.layers <- 1;

downsample <- function(train.dat){
  
  ## separate into positive and negative cases
  pos.cases <- train.dat[train.dat$diabetes == 0,];
  neg.cases <- train.dat[train.dat$diabetes == 1,];
  
  ## find which has less cases
  adj.len <- min(length(pos.cases$diabetes), length(neg.cases$diabetes));
  
  ## downsample cases to prevent imbalance
  pos.cases <- pos.cases[1:adj.len,];
  neg.cases <- neg.cases[1:adj.len,];
  
  ## recombine downsampled data
  train.dat <- rbind(pos.cases, neg.cases);
  
  return(train.dat);
}

validate <- function(fit, val.dat){
  
  ## remove result from the validation data & save separately
  labels <- val.dat[,9];
  v.dat <- val.dat[,-9];
  
  ## get probibiltiies of positive case of diabetes from training data
  prob <- compute(fit, v.dat);
  prob.pos <- prob$net.result[,1];
  
  ## create prediction and find area under the curve. use ROCR:: to avoid intersection with neuralnet package
  pred <- ROCR::prediction(prob.pos, labels);
  perf <- performance(pred,"auc")
  
  ## save statistic to quantify performance: area under curve
  auc <- as.numeric(perf@y.values);
  
  return(auc);
}

shuffle <- function(train.dat, j){
  shuf.dat <- train.dat;
  s <- sample(nrow(train.dat));
  shuf.dat[j] <- train.dat[s, j];
  return(shuf.dat);
}

## import data and separate into caseså
dat <- PimaIndiansDiabetes2;
dat$diabetes <- as.numeric(dat$diabetes == "pos");

## use knnImputation from DMwR to imputate based on existing data for missing entries
dat <- knnImputation(dat)

## fix log-normal data
dat$insulin <- log(dat$insulin);
dat$pedigree <- log(dat$pedigree);
dat$glucose <- log(dat$glucose);

## number of k-folds 
k <- 5;

## shuffle data
dat<-dat[sample(nrow(dat)),];

## assign each row item with a 
outer.flds <- cut(seq(1, nrow(dat)), breaks=k, labels=FALSE);

## remove final test data 
inds <- which(outer.flds==k,arr.ind=TRUE)

## set up AUC array to store metric for each fold. set up fs.auc for each feature selection AUC.
auc <- array(dim=c(k, k-1));
fs.auc <- array(dim=c(k-1, k, ncol(dat)-1))

## for each k-fold, do another k-fold and use fifth as validation.
## the fifth k-fold in the outside loop is saved for final validation
for(i in 1:(k-1)){
  
  ## get data in fold i
  fld.i <- which(outer.flds==i,arr.ind=TRUE)
  inner.dat <- dat[fld.i,];
  
  ## cut fold i into k inner folds
  inner.flds <- cut(seq(1, nrow(inner.dat)), breaks=k, labels=FALSE);
  
  ## do training and validation on fold k
  for (ii in 1:k){
    
    ## select a fold within fold i, use each as validation in typical k-fold fashion
    inds <- which(inner.flds==ii,arr.ind=TRUE)
    
    ## training on all but ii, validation on ii
    val.dat <- inner.dat[inds,]; train.dat <- inner.dat[-inds,];
    
    ## downsample training data
    train.dat <- downsample(train.dat);
    
    ## get mean and sd of each column
    mean.arr <- apply(train.dat, 2, mean) 
    sd.arr <- apply(train.dat, 2, sd)
    
    ## scale each column
    train.dat <- as.data.frame(scale(train.dat, center = mean.arr, scale = sd.arr))
    
    ## train neural net
    fit <- neuralnet(diabetes~., data=train.dat, hidden=n.layers);

    ## scale each column in validation and perform validation
    val.dat <- as.data.frame(scale(val.dat, center = mean.arr, scale = sd.arr))
    auc[ii,i] <- validate(fit, val.dat);
    
    ## feature selection 
    for (j in 1:(ncol(dat) - 1)){
      
      ## shuffle the normal data (already scaled, etc.) for column j
      fs.dat <- shuffle(train.dat, j);
      
      ## create neural network for the fs data
      fs.fit <- neuralnet(diabetes~., data=fs.dat, hidden=n.layers);
      
      ## validate shuffled data vs. un-shuffled data
      fs.auc[i,ii, j] <- validate(fs.fit, val.dat);
    }
  }
}

## convert AUCs for cross-validation into a list for easier t-testing for feature selection
auc.pop <- as.vector(auc)

## convert FS AUCs into a table with ncol-1 columns
l <- length(as.vector(fs.auc[, ,1]))
fs.pop <- array(dim=c(l, ncol(dat)-1))

for (jj in 1:ncol(dat)-1){
  fs.pop[,jj] <- as.vector(fs.auc[, ,jj])
}

## save p-value
p <- numeric(ncol(dat)-1);

## run paired t-tests
for (m in 1:(ncol(dat)-1)){
  t <- t.test(auc.pop, fs.pop[,m], paired=TRUE, alternative="greater");
  p[m] <- t$p.value;
}

## sort and discard factors that are not helpful
df <- data.frame(p=p, names = names(dat)[1:ncol(dat)-1]);
df <- df[order(-p),];
min.layers <- 3;

r <- 1;
while(TRUE){
  
  if ((nrow(df) <= min.layers) | (r == nrow(df))){
    break;
  }
  
  if (df$p[1] > 0.2){
    df <- df[-r,];
  } else{
    r = r + 1;
  }
}

df2 <- data.frame(p=0.05, names="diabetes");
df <- rbind(df, df2);

## ::::: FINAL MODEL :::::

## select columns to be used
dat <- dat[,names(dat) %in% df$names]

## use first four k, didth is for validation
inds <- which(outer.flds==k,arr.ind=TRUE)
val.dat <- dat[inds,];
train.dat <- dat[-inds,];

## downsample
train.dat <- downsample(train.dat);

## get mean and sd of each column
mean.arr <- apply(train.dat, 2, mean) 
sd.arr <- apply(train.dat, 2, sd)

## scale each column
train.dat <- as.data.frame(scale(train.dat, center = mean.arr, scale = sd.arr))
val.dat <- as.data.frame(scale(val.dat, center=mean.arr, scale = sd.arr));

## train neural net
fit <- neuralnet(diabetes~., data=train.dat, hidden=n.layers);

## scale each column in validation and perform validation
## remove result from the validation data & save separately
labels <- val.dat[,ncol(val.dat)];
v.dat <- val.dat[,-ncol(val.dat)];

## get probibiltiies of positive case of diabetes from training data
prob <- compute(fit, v.dat);
prob.pos <- prob$net.result[,1];

## create prediction and find area under the curve. use ROCR:: to avoid intersection with neuralnet package
pred <- ROCR::prediction(prob.pos, labels);

## get ROC and PR curves
ROC <- performance(pred,"tpr", "fpr");
PR <- performance(pred, "prec", "tpr");

## plot performance
plot(ROC, ylim=c(0,1), colorize=T, lwd=2);
plot(PR, ylim=c(0,1), colorize=T, lwd=2);

## plot final neural network using all data (training and test) 
final.fit <- neuralnet(diabetes~., data=dat, hidden=n.layers);
plot(final.fit);
