
#Logistic regression
max_iter = 50
power_acu <- c()
pool <- seq(20, 60, 5)
for(sample_size in pool){
  num_fixed <- 2
  fix_coe <- c(-1.5, -1)
  fix_effect <- matrix(rbinom(sample_size*num_fixed,1,0.5),sample_size,num_fixed)
  colnames(fix_effect) <- paste("Feature", 1: length(fix_coe), sep="")
  power <- 0
  for(iter in 1:max_iter){
    eta <- fix_effect %*% fix_coe
    obs <- rbinom(sample_size, 1, exp(eta)/(1+exp(eta)))
    obs_data <- data.frame(fix_effect,obs) 
    m <- glm(obs ~ .- obs - 1, data = obs_data, family = binomial)
    (tab <- confint(m))
    if (sum(is.na(tab[1,])) == 0) {
    power <- power + ((tab[1,1] < fix_coe[1]) &  (tab[1,2] > fix_coe[1]))}
    print(tab[1,])
    print(iter)
  }
  power_acu <- c(power_acu,power/max_iter)
}

data = data.frame(Sample_Size=pool,Power=power_acu)
ggplot(data,aes(Sample_Size,Power)) + geom_point(shape=1) + 
  geom_smooth(method='lm',formula=y~x) 

#Generalized linear mixed effect model
#general setup
sigma1 <- 1
sigma2 <- 2
power_acu <- c()
pool <- seq(20, 500, 20)
for(sample_size in pool){
num_fixed <- 5
fix_coe <- c(-1.5, -1, 0, 1, 2)
fix_effect <- matrix(rbinom(sample_size*num_fixed,1,0.5),sample_size,num_fixed)
colnames(fix_effect) <- paste("Feature", 1: length(fix_coe), sep="")
group_ind <- rbinom(sample_size, 1, 0.5)
power <- 0
for(iter in 1:max_iter){
eta <- fix_effect %*% fix_coe + (group_ind == 0) * rnorm(sample_size, 0, sigma1) + (group_ind == 1) * rnorm(sample_size, 0, sigma2)
obs <- rbinom(sample_size, 1, exp(eta)/(1+exp(eta)))
obs_data <- data.frame(fix_effect,obs,group_ind) 
m <- glmer(obs ~ .+ (1 | group_ind) - obs - group_ind - 1, data = obs_data, family = binomial, control = glmerControl(optimizer = "bobyqa"),
           nAGQ = 10)
se <- sqrt(diag(vcov(m)))
# table of estimates with 95% CI
(tab <- cbind(Est = fixef(m), LL = fixef(m) - 1.96 * se, UL = fixef(m) + 1.96 * se))
power <- power + ((tab[1,2] < fix_coe[1]) &  (tab[1,3] > fix_coe[1]))
print(tab[1,])
print(iter)
}
power_acu <- c(power_acu, power/max_iter)
}


data = data.frame(Sample_Size=seq(20, 500, 20),Power=power_acu * 10)
ggplot(data,aes(Sample_Size,Power)) + geom_point(shape=1) + 
  geom_smooth(method='lm',formula=y~x) 

