## exercise showing difference in behavior for ROC and PR curves
## as a response to moderately imbalanced and extremely imbalanced data

library(ROCR); library(ggplot2); library(gridExtra);

hw8 <- function(dat) {
  
  dat$labels <- as.factor(dat$labels)
  
  # Iteratively calculate TRP and FPR. Non optimal.
  TPR <- NULL;
  FPR <- NULL;
  
  ## make random guesses
  random.guess <- numeric(length(dat$labels));
  for (i in 1:length(dat$labels)){
    random.guess[i] <- runif(1, min=0, max=1);
  }
  
  dat$random <- random.guess;
  
  pred <- prediction(dat$predictions, dat$labels);
  rand.pred <- prediction(dat$random, dat$labels);
  
  # ROC performance based on MLC + random guessing
  perf <- performance(pred, "tpr", "fpr");
  rand.perf <- performance(rand.pred, "tpr", "fpr");
  
  # PR performance based on MLC + random guessing
  pr <- performance(pred, "prec", "tpr");
  rand.pr <- performance(rand.pred, "prec", "tpr");
  
  ## put into dataframe
  dat <- data.frame(xp=perf@x.values[[1]], yp=perf@y.values[[1]], ap=perf@alpha.values[[1]],
                   xr=rand.perf@x.values[[1]], yr=rand.perf@y.values[[1]], ar=rand.perf@alpha.values[[1]],
                   xpr=pr@x.values[[1]], ypr=pr@y.values[[1]], apr=pr@alpha.values[[1]],
                   xprr=rand.pr@x.values[[1]], yprr=rand.pr@y.values[[1]], aprr=rand.pr@alpha.values[[1]])
  
  p <- ggplot(dat) + geom_line(aes(x=xp, y=yp, color=ap), size=2) + geom_line(aes(x=xr, y=yr, color=ar),size=2)  + 
    ylab("True positive rate") + xlab("False positive rate") + ggtitle("MLC vs. Random ROC curves") + labs(color="Confidence") +
    scale_colour_gradient(low = "blue", high = "red") + ylim(0,1);
  
  p2 <- ggplot(dat) + geom_line(aes(x=xpr, y=ypr, color=apr), size=2) + geom_line(aes(x=xprr, y=yprr, color=aprr),size=2)  + 
    ylab("Precision") + xlab("True positive rate") + labs(color="Confidence") +
    scale_colour_gradient(low = "blue", high = "red") + ylim(0,1);
  
  return_list <- list("p1"=p,"p2"=p2);
  return(return_list);
}

data(ROCR.simple);

## dataframe with all data
df1 <- data.frame(labels=ROCR.simple$labels, predictions=ROCR.simple$predictions);

## dataframe with equal positive and negative (50% p/ 50% n)
df2 <- df1[c(which(df1$labels == 1),
            which(df1$labels == 0)[1:length(which(df1$labels==1))]),];

## dataframe with fabricated imbalance (25% p/ 75% n)
df3 <- df1[c(which(df1$labels == 0)[1:105], which(df1$labels == 1)[1:35]),];

l1 <- hw8(df1);
p1 <- l1$p1 + ggtitle("MLC vs. Random ROC: Raw Data"); 
p2 <- l1$p2 + ggtitle("MLC vs. Random PR: Raw Data");

l2 <- hw8(df2);
p3 <- l2$p1 + ggtitle("MLC vs. Random ROC: Balanced Data"); 
p4 <- l2$p2 + ggtitle("MLC vs. Random PR: Balanced Data");

l3 <- hw8(df3);
p5 <- l3$p1 + ggtitle("MLC vs. Random ROC: Imbalanced Data"); 
p6 <- l3$p2 + ggtitle("MLC vs. Random PR: Imbalanced Data");

x11(width=10)
grid.arrange(p1, p2, p3, p4, p5, p6, ncol=2)
