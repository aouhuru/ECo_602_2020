install.packages("here")
require(here)

dat_delomys= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/delomys.csv")
head(dat_delomys)
ncol(dat_delomys)


hist(dat_delomys$body_mass) 
hist(dat_delomys$body_length)
plot(dat_delomys$body_mass, dat_delomys$body_length)


boxplot(body_length ~ sex, data = dat_delomys)



boxplot(body_length ~ sex * binomial, data = dat_delomys)
