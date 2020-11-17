x_observed = c(2,6)
print(x_observed)

dpois(x = 2, lambda = 4.5)
dpois(x = 6, lambda = 4.5)

dpois(x = 2, lambda = 4.5) * dpois(x = 6, lambda = 4.5)

wiwa_counts = c(2, 6)
dpois(x = wiwa_counts, lambda = 4.5)

prod(dpois(x = wiwa_counts, lambda = 4.5))

sum(log(dpois(x = wiwa_counts, lambda = 4.5)))

dat_bird = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv", TRUE)
dat_habitat = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv", TRUE)
dat_all = merge(dat_bird, dat_habitat)

summary(dat_all$WIWA)


hist(dat_all$WIWA, breaks = 6)
hist(dat_all$WIWA - 0.5, breaks = 6)
sum(log(dpois(x = dat_all$WIWA, lambda = 1.0)))


hist(dat_all$WIWA - 0.5, breaks = 6)
hist(dat_all$WIWR, breaks = 6)



# Question 1

wiwa_counts = c(2 , 6)
dpois(x = wiwa_counts, lambda =  4.5)
dpois(x = wiwa_counts, lambda =  4.0)


sum(log(dpois(x = wiwa_counts, lambda = 4.0))) #<-
sum(log(dpois(x = wiwa_counts, lambda = 3.9)))
sum(log(dpois(x = wiwa_counts, lambda = 4.1)))



#Question 2
summary(dat_all$WIWR)
sum(log(dpois(x = dat_all$WIWR, lambda = 1.0)))
sum(log(dpois(x = dat_all$WIWR, lambda = 1.456)))
sum(log(dpois(x = dat_all$WIWR, lambda = 1.4)))
sum(log(dpois(x = dat_all$WIWR, lambda = 1.5)))

hist(dat_all$WIWR, breaks = 6)


##Question 3 ## 

# 1046, 0.3
n  = 8
dbinom(x=dat_all$WIWR, 8, 0.3)
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.3))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.2))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.1))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.1))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.11))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.12))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.13)))
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.14))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.15))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.16))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.17)))
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.18))) 
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.19)))
sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.181)))

hist(sum(log(dbinom(x = dat_all$WIWR, 8, p = 0.181)))
)