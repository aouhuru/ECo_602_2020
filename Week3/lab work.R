#intro to lab work workthrough
install.packages("psych")
require(psych)
pairs.panels(iris)
#creates nicer pairplot
dat_all=dat_bird_hab
dat_all


#change of name from information after the merge of dat_bird and dat_hab
plot(ba.tot ~ elev, data = dat_all)
#checks to make sure information is corect
sample(dat_bird$CEWA, 100)
sample(dat_all[CEWA], 100)
sample(dat_all$CEWA, 100)
sample(dat_bird[CEWA], 100)
#this will not run properly, CEWA is in the metadata for dat_bird, i have tried it both this way and for the merged information 

#more walk through
my_vec = rep(1:3, 5)
my_vec == 3
my_vec > 1
as.numeric(my_vec > 1)

#I id not not get rhe boolean example,which is why this will not run 
plot(x= dat_all$elev, y = cewa_present_absent)

#attempt at lab - No success, file too big. 
pairs.panels(dat_bird)

pairs.panels(dat_all$elev, 0)
