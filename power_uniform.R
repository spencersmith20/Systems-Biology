#estimate the sample size needed for the following proposed experiment and its hypothesized conditions
#We want a significance (false positive rate) of 0.05 & a power of 0.9 (90% chance of 
#successfully identifying a difference). Use the t test and the U test
library("ggplot2");

norm_power <- function(n){
  
  N <- 10000
  s <- 1:N
  pt <- numeric(N); 
  pu <- numeric(N);
  
  for(i in s){
  
    #case 1 -- normal distributions w/ two means
    m1 <- 18;
    m2 <- 25;
    s <- 9;
    
    # create distributions
    norm1 <- rnorm(mean=m1, sd=s, n=n);
    norm2 <- rnorm(mean=m2, sd=s, n=n);
    
    mean1 <- mean(norm1); mean2 <- mean(norm2);
    sd1 <- sd(norm1); sd2 <- sd(norm2);
    denom <- sqrt((sd1^2)/n + (sd2^2)/n);
    
    #t-test
    t_norm <- (mean1-mean2)/denom;
    
    #p value
    pt[i] <- 2 * pt(-abs(t_norm), 2*n-2);
    
    #U-test
    pu[i] <- wilcox.test(x=norm1, y=norm2, alternative="two.sided")$p.value;
  }
  
  # calculate powers to return
  powt <- length(which(pt < 0.05))/N;
  powu <- length(which(pu < 0.05))/N;

  return_list <- list("powt"= powt, "powu"= powu);
  return(return_list);
}

unif_power <- function(n){
  
  N <- 1000;
  s <- 1:N;
  pt <- numeric(N); pu <- numeric(N);
  
  for(i in s){
    
    #case 2 -- two uniform distributions w/ ranges 9->27 and 16->34
    unif1 <- runif(n, min=9, max=27);
    unif2 <- runif(n, min=16, max=34);
    
    #t-test
    pt[i] <- t.test(unif1, unif2)$p.value;
    
    #U-test
    pu[i] <- wilcox.test(x=unif1, y=unif2, alternative="two.sided")$p.value;
    
  }
  
  # calculate powers to return
  powt <- length(which(pt < 0.05))/N;
  powu <- length(which(pu < 0.05))/N;

  return_list <- list("powt"= powt, "powu"= powu);
  return(return_list);
}

n1 <- 10:40; n2 <- 5:25;
t1 <- 1:length(n1); t2 <- 1:length(n2);

p1t <- numeric(length(n1)); p1u <- numeric(length(n1));
p2t <- numeric(length(n2)); p2u <- numeric(length(n2));

for (j in t1){
  
  #calculate power of given sample size
  p <- norm_power(n1[j]);
  
  p1t[j] <- p$powt; p1u[j] <- p$powu;
}

for (j in t2){
  p <- unif_power(n2[j])
  
  p2t[j] <- p$powt; p2u[j] <- p$powu;
}

#plot power vs. sample size for both U and t
df <- data.frame(n=c(n1,n1,n2,n2), 
                 type=c(rep('t.normal',length(n1)),c(rep('u.normal',
                 length(n1))), c(rep('t.uniform',length(n2))), 
                 c(rep('u.uniform',length(n2)))), power=c(p1t, p1u, p2t, p2u));
df$type <- factor(df$type)

plt <- ggplot(df, aes(x=n, y=power, color=type)) + 
  geom_line(aes(x=n, y=power, color=type)) +
  labs(title="Power vs. Sample Size", x = "Number of samples", y = "Power");
print(plt);

