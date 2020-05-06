## this is an R script written done for ChE 2475 at the University of Pittsburgh by Spencer Smith
## creates four MLCs created based on...
## i. decision tree with unmodified data
## ii. decision tree with downsampled data
## iii. decision tree with SMOTE
## iv. neural network with unmodified data

library(mlbench); library(rpart); library(ROCR); library(DMwR); 
library(neuralnet); data("BreastCancer");

## reconfigure dataset
BreastCancer <- BreastCancer[complete.cases(BreastCancer),]
BreastCancer$Class <- as.factor(BreastCancer$Class)
BreastCancer[,1:10] <- as.numeric(unlist(BreastCancer[,1:10]))

## get benign and malignant cases
benign <- BreastCancer[BreastCancer$Class == "benign",]
malignant <- BreastCancer[BreastCancer$Class == "malignant",]

## split each class in half for training and validation
benign_train <- benign[1:(nrow(benign)/2),]; benign_val <- benign[(nrow(benign)/2 + 1):nrow(benign),]
malignant_train <- malignant[1:(nrow(malignant)/2),]; malignant_val <- malignant[(nrow(malignant)/2 + 1):nrow(malignant),]

## downsampling
benign_train2 <- benign_train[1:nrow(malignant_train),]
benign_val2 <- benign_val[1:nrow(malignant_val),]

## group data
train_dat <- rbind(benign_train, malignant_train)
val_dat <- rbind(benign_val, malignant_val)

## downsampled grouped data
train_dat2 <- rbind(benign_train2, malignant_train)
val_dat2 <- rbind(benign_val2, malignant_val)

## build decision tree based on un-modified data
fit <- rpart(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size +
              Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data=train_dat, minsplit = 10)

## build decision tree based on downsampled data
fit2 <- rpart(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size +
               Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data=train_dat2, minsplit = 10)

## SMOTE
smote_dat <- rbind(benign_train, malignant_train)
balanced.data <- SMOTE(Class ~ ., data=smote_dat, perc.over = 800, k = 5, perc.under = 113)
bal_dat <- as.data.frame(balanced.data)

## split the balanced data into validation and training groups
bal_benign <- bal_dat[bal_dat$Class == "benign",]; bal_malignant <- bal_dat[bal_dat$Class == "malignant",]

## split upsampled data into validation and training
bal_benign_train <- bal_benign[1:(nrow(bal_benign)/2),]
bal_benign_val <- bal_benign[(nrow(bal_benign)/2 + 1):nrow(bal_benign),]

bal_malignant_train <- bal_malignant[1:(nrow(bal_malignant)/2),]
bal_malignant_val <- bal_malignant[(nrow(bal_malignant)/2 + 1):nrow(bal_malignant),]

smote_train <- rbind(bal_benign_train, bal_malignant_train)
smote_val <- rbind(bal_benign_val, bal_malignant_val)

## build decision tree based on SMOTE data
fit3 <- rpart(Class ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size +
                Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data=smote_train, minsplit = 10)

## build neural network 
fit4 <- neuralnet(Class == 'malignant' ~ Cl.thickness + Cell.size + Cell.shape + Marg.adhesion + Epith.c.size +
      Bare.nuclei + Bl.cromatin + Normal.nucleoli + Mitoses, data=train_dat, hidden=3)

plot(fit4);
text(fit3, use.n=TRUE, all=TRUE, cex=1)
par(mar = rep(0.2, 4))

plot(fit3, uniform = TRUE, branch = 0.2, compress = TRUE, margin = 0.1)
text(fit3, all = TRUE, use.n = TRUE, fancy = TRUE, cex= 0.9)

## get correct answers for each fit
labels <- val_dat[,11]; val_dat <- val_dat[,-11]
labels2 <- val_dat2[,11]; val_dat2 <- val_dat2[,-11]
labels3 <- smote_val[,11]; val_dat3 <- smote_val[,-11]

## get results via the tree
prob <- predict(fit, val_dat, type="prob")
prob2 <- predict(fit2,val_dat2, type="prob")
prob3 <- predict(fit3, val_dat3, type="prob")
prob4 <- compute(fit4, val_dat)

## probabilites for validation
prob.malignant <- prob[,2];
prob2.malignant <- prob2[,2];
prob3.malignant <- prob3[,2];
prob4.malignant <- prob4$net.result

pred <- ROCR::prediction(prob.malignant, labels=="malignant");
pred2 <- ROCR::prediction(prob2.malignant, labels2=="malignant");
pred3 <- ROCR::prediction(prob3.malignant, labels3 == "malignant");
pred4 <- ROCR::prediction(prob4.malignant, labels == "malignant");

x11(width=10, height = 6);
par(mfrow=c(2,4));
p1 <- plot(performance(pred, "tpr", "fpr"), lwd=6, main="ROC Curve - Raw Data")
p2 <- plot(performance(pred2, "tpr", "fpr"),lwd=6, main = "ROC Curve - Downsampling")
p3 <- plot(performance(pred3, "tpr", "fpr"), lwd=6, main="ROC Curve - SMOTE")
p4 <- plot(performance(pred4, "tpr", "fpr"), lwd=6, main ="ROC Curve - Neural Net")

p5 <- plot(performance(pred,"prec","tpr"), lwd=6, ylim=c(0,1), main = "PR Curve - Raw Data")
p6 <- plot(performance(pred2,"prec","tpr"), lwd=6, ylim=c(0,1), main = "PR Curve - Downsampling")
p7 <- plot(performance(pred3,"prec","tpr"), lwd=6, ylim=c(0,1), main = "PR Curve - SMOTE")
p8 <- plot(performance(pred4,"prec","tpr"), lwd=6, ylim=c(0,1), main = "PR Curve - Neural Net")
