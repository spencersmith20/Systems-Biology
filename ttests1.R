library(ggplot2)

n <- 100;
m <- 100;
m2 <- 103;
sd <- 20;

s <- 1:1000;
pvals <- numeric(1000);
pvals2 <- numeric(1000);

for (i in s){
  x <- rnorm(n, m, sd);
  y <- rnorm(n, m, sd);
  z <- rnorm(n, m2, sd);
  
  t <- t.test(x,y)
  t2 <- t.test(x,z)
  
  pvals[i] <- t$p.value;
  pvals2[i] <- t2$p.value;
}



df <- data.frame(p=pvals);
df2 <- data.frame(p=pvals2);

p1 <- ggplot(df, aes(x=p)) +
  geom_histogram(aes(x=p, y=..density..), binwidth=.1, fill="bisque3", color="black") +
  ggtitle("m1=m2=100");
p2<- ggplot(df2, aes(x=p)) +
  geom_histogram(aes(x=p, y=..density..), binwidth=.1, fill="cornflowerblue", color="black") +
  ggtitle("m1=100, m2=103");

grid.arrange(p1, p2, nrow = 1)
