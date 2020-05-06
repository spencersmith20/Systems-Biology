## assignment for determining power relationship with sample size for
## two different normal populations with three different sampling methods

library(reshape2); library(ggplot2);

calc_power <- function(n, m1, m2, s_pop, N) {

  p1 <- numeric(N); p2 <- numeric(N); p3 <- numeric(N);
  s <- 1:N;
  
  for (i in s){
    
    # sample
    x <- rnorm(n=n, mean=m1, sd=s_pop);
    y <- rnorm(n=n, mean=m2, sd=s_pop);
    
    # sample means
    xmean <- mean(x); 
    ymean <- mean(y);
    
    # sample standard deviation
    xsd <- sd(x); 
    ysd <- sd(y);
    
    # randomly sample both populations (many times), use the sample means and sample standard deviations to get p values
    t1 <- (xmean - ymean) * sqrt(n/2) / sqrt((xsd^2 + ysd^2)/2);
    p1[i] <- 2 * pt(-abs(t1), 2*n-2);
    
    # randomly sample one population many times. each instance, approximate other population by 
    # shifting the sample collected by the difference in pop means. Use sample means and sds to determine p values
    diff <- (m2 - m1); y_approx <- x + diff;
    
    # get new y stats
    ymean_approx <- mean(y_approx);
    ysd_approx <- sd(y_approx);
    
    t2 <- (xmean - ymean_approx) * sqrt(n/2) / sqrt((xsd^2 + ysd_approx^2)/2);
    p2[i] <- 2 * pt(-abs(t2), 2*n-2);
    
    # randomly sample both populations (many times), use the sample means
    # standard deviations are known and equal to the population SD to determine your p values.
    t3 <- (xmean - ymean) * sqrt(n/2) / s_pop;
    p3[i] <- 2 * pt(-abs(t3), 2*n-2);
  }
  
  pow1 <- length(which(p1 < .05))/N;
  pow2 <- length(which(p2 < .05))/N;
  pow3 <- length(which(p3 < .05))/N;
  
  return_list <- list("pow1" = pow1, "pow2" = pow2, "pow3" = pow3);
  return(return_list);
}

# population info
m1 <- 10; m2 <- 14; s_pop <- 2;

# set up sizes
N <- 20000;

# set up arrays to test for power
n <- 3:8; v <- 1:length(n);
pow1 <- numeric(length(n)); pow2 <- numeric(length(n)); pow3 <- numeric(length(n));

for (j in v) {
  a <- calc_power(n[j], m1, m2, s_pop, N);
  pow1[j] <- a$pow1;
  pow2[j] <- a$pow2;
  pow3[j] <- a$pow3;
}


# calculate power with tests
ncalc_1 <- power.t.test(power = 0.8, delta=(m2-m1), sd=s_pop)$n;
ncalc_2 <- power.anova.test(groups=2, power = 0.8, between.var = var(c(m1,m2)), within.var = 2^2)$n

# find intersection point manually
a  <- c(0, 0.8);
b  <- c(6, 0.87935);
c  <- c(10, 0.8);
d  <- c(5, 0.78920)

# find y 
k.1 <- ((c[2]-a[2])/(c[1]-a[1]))
k.2 <- ((b[2]-d[2])/(b[1]-d[1]))  
y <- (((-k.1/k.2)*d[2]+k.1*d[1]-k.1*c[1]+d[2])/(1-k.1/k.2))

# find x intersection point
ncalc_3 <- ((y-d[2])+d[1]*k.2)/k.2

## three different methods of determining sample size required for power = 0.80
message("power.t.test: ", ncalc_1);
message("power.anova.test:", ncalc_2);
message("manual estimae: ", ncalc_3);

df <- data.frame(n=c(n,n,n), 
                 type=c(rep(1,length(pow1)), rep(2, length(pow2)), rep(3, length(pow3))),
                 power=c(pow1,pow2,pow3));
df$type <- factor(df$type)

pl <- ggplot(df, aes(x=n, y=power, color=type)) + 
  geom_line(aes(x=n, y=power, color=type)) + geom_point(aes(x=n, y=power, color=type)) +
  labs(title="Power vs. Sample Size", x = "Number of samples", y = "Power");
print(pl);