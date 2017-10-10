# Bootstrap-Homework-1

##############
# Question 3 #
##############

x<-c(1,2,3.5,4,7,7.3,8.6,12.4,13.8,18.1)
theta_trim <- mean(x,trim=0.20)
theta_trim
x<- x[3:8]
mean(x)
# ????????????????????????????????????????????
#to do the bootstrap of trimmed mean, do we need to subset original x or sample from the
# full vector and trim then? Underneath I've sampled from a subset of 6 middle values
# ????????????????????????????????????????????

# nonparametric bootstrap:

n <- length(x)
index <- c(1:n)
B <- c(25,50,75,100,200,500,750,1000,2000,5000,10000)
Size <- length(B)
SD.B <- c(1:Size)
for (b in 1:Size) { #this loops through the list of different boostrap lengths and passes them to
                    # the next loop
  cat(b)
  boot <- B[b]
  mean.b <- c(1:boot)
  for (i in 1:boot) { #this loop creates the bootstrap samples for each value of b and creates a
                      #vector of means
    cat(i)
    index.b <- sample(index,n,replace = T)
    x.b <- x[index.b]
    mean.b[i] <- mean(x.b)
  }  
  SD.B[b] <- sqrt(var(mean.b)) # calculates the SD for each number of bootstrap samples and saves
                                # in a vector
}
SD.B

# it looks like the SD becomes more or less stable when the number of bootstrap samples
# reaches 200 


# parametric bootstrap:

meanX <- mean(x)
sdX <- sqrt(var(x))

n <- length(x)
B <- c(25,50,75,100,200,500,750,1000,2000,5000,10000)
Size <- length(B)
SD.B <- c(1:Size)
for (b in 1:Size) {
  cat(b)
  boot <- B[b]
  mean.b <- c(1:boot)
  for (i in 1:boot) {
    cat(i)
    x.b <- rnorm(n,meanX,sdX)
    mean.b[i] <- mean(x.b,trim = 0.20)
  }  
  SD.B[b] <- sqrt(var(mean.b))
}
SD.B

# here, the SD seems stable around 75 bootstrap replicates

##############
# Question 4 #
##############

# (a)

gender<-as.factor(c(rep("Male",89),rep("Female",89)))
length(gender)
vote<-c(rep("Clinton",45),rep("Trump",44),rep("Clinton",54),rep("Trump",35))
length(vote)
table(vote,gender)
Results <- data.frame(cbind(vote,gender)) 
# vote: Trump-vote to Donald Trump, Clinton-vote to Hillary Clinton

Clinton <- Results[vote == "Clinton",2]
p_F <-  sum(Clinton == 1)/length(Clinton)
p_M <- sum(Clinton == 2)/length(Clinton)
p_diff <- p_F - p_M
var_p_diff <- (p_F*(1-p_F))/sum(Clinton == 1) + (p_M*(1-p_M))/sum(Clinton == 2)
p_diff_lo <- p_diff - 1.96*(sqrt(var_p_diff))
p_diff_hi <- p_diff + 1.96*(sqrt(var_p_diff))
p_diff_lo
p_diff_hi

# the 95% CI for the difference is [-0.1061,0.2879]

# (b)

# nonparametric bootstrap 1 - bootstrap t interval

Clinton <- Results[vote == "Clinton",2]
n <- length(Clinton)
index <- c(1:n)
B <- 10000
z.boot <- c(1:B)
for (i in 1:B){
  gender.b <- sample(Clinton,n,replace = T)
  p_F.b <- sum(gender.b == 1)/n
  p_M.b <- sum(gender.b == 2)/n
  p_diff.b <- p_F.b - p_M.b
  var_p_diff.b <- (p_F.b*(1-p_F.b))/sum(gender.b == 1) + (p_M.b*(1-p_M.b))/sum(gender.b == 2)
  z.boot[i] <- (p_diff.b - p_diff)/sqrt(var_p_diff.b)
}
lo <- quantile(z.boot,probs = 0.05)
hi <- quantile(z.boot,probs = 0.95)
lo
hi

# 5%: -1.608061 95%: 1.608061

p_diff_lo <- p_diff - 1.608061 * (sqrt(var_p_diff))
p_diff_hi <- p_diff + 1.608061 * (sqrt(var_p_diff))
p_diff_lo
p_diff_hi

# the 95% bootstrap t CI for the difference is [-0.0707,0.2525]

# nonparametric bootstrap interval 2 - percentile

Clinton <- Results[vote == "Clinton",2]
n <- length(Clinton)
index <- c(1:n)
B <- 10000
p_diff.boot <- c(1:B)
for (i in 1:B){
  gender.b <- sample(Clinton,n,replace = T)
  p_F.b <- sum(gender.b == 1)/n
  p_M.b <- sum(gender.b == 2)/n
  p_diff.boot[i] <- p_F.b - p_M.b
}
lo <- quantile(p_diff.boot,probs = 0.05)
hi <- quantile(p_diff.boot,probs = 0.95)
lo
hi

# the 95% percentile CI for the difference is [-0.0707,0.2525]

# nonparametric BCa interval

x <- Results[vote == "Clinton",2]
theta <- function(x){sum(x == 1)/length(x) - sum(x == 2)/length(x)}
results <- bcanon(x,10000,theta,alpha=c(0.025,0.975))   
results

# the 95% CI for the difference is [-0.1313,0.2727]

# (c)

# Bootstrap standard normal interval

Clinton <- Results[vote == "Clinton",2]
p_F <-  sum(Clinton == 1)/length(Clinton)
p_M <- sum(Clinton == 2)/length(Clinton)
p_diff <- p_F - p_M
var_p_diff <- (p_F*(1-p_F))/sum(Clinton == 1) + (p_M*(1-p_M))/sum(Clinton == 2)

B = 10000
theta.b <- c(1:B)
for (i in 1:B){
  theta.b[i] <- rnorm(1,p_diff,sqrt(var_p_diff))
}
hist(theta.b)
lo <- quantile(theta.b,probs = 0.05)
hi <- quantile(theta.b,probs = 0.95)
lo
hi

# the 95% CI for the difference is [-0.0776,0.2549]

# the bootstrap parametric percentile interval

n = length(Clinton)
B = 10000
theta.b <- c(1:B)
for (i in 1:B){
  females.b <- rbinom(n,1,0.54)
  theta.b[i] <- (sum(females.b == 1)/n) - ((n - sum(females.b == 1))/n)
}

hist(theta.b)
lo <- quantile(theta.b,probs = 0.05)
hi <- quantile(theta.b,probs = 0.95)
lo
hi

# the 95% CI for the difference is [-0.0909,0.2323]

# (d)

Clinton <- Results[vote == "Clinton",2]
p_F <-  sum(Clinton == 1)/length(Clinton)
p_M <-  sum(Clinton == 2)/length(Clinton)
theta <- p_F/p_M

# distribution of theta and its variance
n = length(Clinton)
B = 10000
theta.b <- c(1:B)
for (i in 1:B){
  females.b <- rbinom(n,1,0.54)
  theta.b[i] <- (sum(females.b == 1)/n) / ((n - sum(females.b == 1))/n)
}

hist(theta.b)
var.theta<- var(theta.b)
var.theta

# bootstrap t interval
n <- length(Clinton)
B=1000  # this might not be enough, since the rule of thumb for number of bootstrap replicates is
        # n*n , which require around 10000 for this case, but my computer keeps hanging each time
z.boot <- c(1:B)
S <- 1000  # same remark
theta.s <- c(1:S)
for (i in 1:B){
  Clinton.b <- sample(Clinton,n,replace = T)
  for (s in 1:S){
    Clinton.se <- sample(Clinton.b,n,replace = T)
    theta.s[s] <-  (sum(Clinton.se == 1)/n) / ((n - sum(Clinton.se == 1))/n)
  }
  se.boot <- sqrt(var(theta.s))
  theta.b <- (sum(Clinton.b == 1)/n) / ((n - sum(Clinton.b == 1))/n)
  z.boot[i] <- (theta.b - theta) /se.boot
}

lo <- theta + quantile(z.boot,probs = 0.05)*sqrt(var.theta)
hi <- theta + quantile(z.boot,probs = 0.95)*sqrt(var.theta)
lo
hi

# bootstrap t interval is [0.7533,1.5194]

# bootstrap percentile interval 
n = length(Clinton)
B = 10000
theta.b <- c(1:B)
for (i in 1:B){
  females.b <- rbinom(n,1,0.54)
  theta.b[i] <- (sum(females.b == 1)/n) / ((n - sum(females.b == 1))/n)
}
lo <- quantile(theta.b,probs = 0.05)
hi <- quantile(theta.b,probs = 0.95)
lo
hi

#bootstrap percentile interval is [0.8333,1.6053]
