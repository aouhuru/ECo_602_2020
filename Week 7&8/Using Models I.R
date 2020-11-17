#---- Catastrophe Rate Data ----
catrate=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/catrate.csv")
head(catrate)

#---- Numerical and Graphical exploration ----
summary(catrate)
hist(catrate$cat.rate)

#----Shapiro Test ----
shapiro.test(catrate$cat.rate)

install.packages(nortest)
#---- Parametric One-Sample Test: the T-test ----
t.test(catrate$cat.rate, alternative = "two.sided", mu = 0.28)

#---- One-sided Alternative Hypothesis ----
t.test(catrate$cat.rate, alternative = "greater", mu = 0.28)

#---- Non-Parametric One-Sample Test: The Wilcoxon Rank Sum Test ----
wilcox.test(catrate$cat.rate, mu = 2/7)


#---- Comparing two sample means ----
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))
summary(penguin_dat)
boxplot(flipper_length_mm ~ species, data = penguin_dat)

#---- Testing for normality ----
dat_adelie = subset(penguin_dat, species == "Adelie")
dat_chinstrap = subset(penguin_dat, species == "Chinstrap")

shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
#----Parametric and Nonparametric Tests ----
t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)

wilcox.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)

levels(penguin_dat$species)

#----Question 01----
summary(catrate)
png(filename = "Cathist.png")
hist(catrate$cat.rate, main= "Historgram of Salamander\nReproduction Catastrophe Rates", xlab = "Catastrophic Rates")
dev.off()
#----Question 02----
shapiro.test(catrate$cat.rate)

#----Question 03----
p-value is less than 0.05.  
#The null hypothesis would be that there is a normal distribution and that tthe cat.rate has no affect on the catrate data.
#from this data we reject the null hypothesis because the p-value is less than 0.05 and that means that there is a significant difference between the cat.rate data and catrate

#----Question 04----
t.test(catrate$cat.rate, alternative = "two.sided", mu = 0.28) 
#there is no difference in salamander reproduction rates

#----Question 05----
#p-value: 0.01054
#CI --> 0.3526250 and 0.7261295, no it didnt include 0
#We reject the null hypothesis because the p value is lower than 0.05.This evidence is atrong after looking at the results of the t-test

#----Question 06----
wilcox.test(catrate$cat.rate, mu = 2/7)

#----Question 07----
#p-value: 0.006275
#reject very low p-value

#----Question 08----
#should you use t.test or wilcoxon for this?
#Compare overall conclusions you draw from the results of the two tests

#---- Question 09----
shapiro.test(dat_adelie$flipper_length_mm)
shapiro.test(dat_chinstrap$flipper_length_mm)
#yes

#----Question 10----
dev.off()

png(filename = "flipperhist.png", width = 1400, height = 800, units = "px", res = 140)
par(mfrow = c(1, 2), oma=c(2,0,2,0))
hist(dat_adelie$flipper_length_mm, xlab = "Adelie", main = "")
hist(dat_chinstrap$flipper_length_mm, xlab = "Chinstrap", main= "")
mtext("Historgram of Adelie and Chinstrap flipper lengths ", side = 3, outer = T, line = 0, cex = 2)
mtext("Flipper Lengths (mm.)", side = 1, outer = T, line = 0, cex = 1.5)
dev.off()

#----Question 11----
t.test(penguin_dat$flipper_length_mm ~ penguin_dat$species)
#the alternative hypothesis would be that there is a difference in flipper length and species
