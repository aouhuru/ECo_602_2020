#---- Carastropic Rate ----
catrate=read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/catrate.csv")
head(catrate)

  #pond: ID number
  #success: # of years of reproduction
  #years: total # years pond observed
  #cat.rate: ratio of successes to total observation years

#---- Binomial Test for PRoportions ----
success = sum(catrate$success)
years = sum(catrate$years)
binom.test(success,years)

binom.test(success, years, p = 5/7)
#failures
binom.test(success, years, p = 5/7, alternative = "less")


#---- F-distribution Example: Vegetation Data ----
data= require(datasets)
veg= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/vegdata.csv")
head(veg)

boxplot(pine ~ treatment, data = veg)

#----Variance Test ----
var.test(
  pine ~ treatment,
  data = veg, 
  subset = treatment %in% c('control', 'clipped')
)

#---- F-Tests assumes Normality ----
shapiro.test(veg$pine[veg$treatment=="control"])
shapiro.test(veg$pine[veg$treatment=="clipped"])

#---- Non-Parametric Variance Test ----
fligner.test(
  pine ~ treatment, 
  data = veg,
  subset = treatment %in% c('control', 'clipped')
)
#comparing variance of two samples 

bartlett.test(pine ~ treatment, data=veg)
#like the fishers F test is highly sensitive to non-normality and the presence of outliers

fligner.test(pine ~ treatment, data = veg)
#tests n variances

#----T-Test ----
t.test(pine~treatment,data = veg, subset= treatment %in% c('control', 'clipped'), conf.int=TRUE)

#----Wilcox Test ----
wilcox.test(pine~treatment,data=veg,subset=treatment %in% c('control','clipped'), conf.int=TRUE)

#----Test for paired samples ----
control = veg$pine[veg$treatment=='control']
clipped = veg$pine[veg$treatment=='clipped']
t.test(control, clipped, paired= TRUE)

wilcox.test(control, clipped, paired=TRUE) 

#----Correlation: Marbled Salamander -----
disp = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/dispersal.csv")
disp
plot(disp$disp.rate.ftb, disp$disp.rate.eb)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs'
)

cor.test(
  disp$disp.rate.ftb,
  disp$disp.rate.eb,
  use='complete.obs',
  method='spearman')

#----Comparing Two Distributions----
plot(
  ecdf(disp$disp.rate.ftb),
  verticals = TRUE
)

plot(
  ecdf(disp$disp.rate.ftb),
  verticals=TRUE)
plot(
  ecdf(disp$disp.rate.eb),
  verticals=TRUE,
  lty=3,
  add=TRUE)

ks.test(disp$disp.rate.ftb,disp$disp.rate.eb)

#determine if they differ significantly in any aspect - maximum difference in value of cumulative distribution functions, aximum veritical difference in the curves for a given value of X  

#----Comparing two or more proportions----
prop.test(c(4,16), c(40,250))

owls = matrix(c(16,9,4,11), nrow = 2)
rownames(owls) = c("present", "absent")
colnames(owls) = c("old", "young")
owls

#----Contingency- Chi-square test ----
chisq.test(owls) 
fisher.test(owls)

birds = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sta.csv")
hab= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sta.csv")
birdhab=merge(birds,hab, by=c("basin", "sub", "sta"))
#create a contingency table for edge/interior and brown creeper presence/absence
table(birdhab$s.edge, birdhab$BRCR > 0)

# set the presence to be in the first column
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]


#----Question 1 ----
penguin=require(palmerpenguins)
head(penguins)

bartlett.test(penguins$body_mass_g ~ species, data = penguins)
#0.0501
#----Question 2 ----
bartlett.test(penguins$body_mass_g ~ sex, data = penguins )
#0.0319
#----Question 3 ----
require(palmerpenguins)
par(mar = c(8, 4, 2, 2))
boxplot(
  body_mass_g ~ sex * species,
  data = penguins,
  las = 2, 
  xlab = NULL,
  ylab = "body mass (g)")

dat_groups = aggregate(
  body_mass_g ~ island,
  data = penguins,
  FUN = c)
str(dat_groups)

dat_groups$body_mass_g

#questionsinfo

#I cant get bartlett.t test to run

sex_species = aggregate( 
  body_mass_g ~ sex * species, 
  data = penguins,FUN = c)
str(sex_species)

bartlett.test(sex_species$body_mass_g)


#high p value -- homogenous variance


#----Question 4 ----
#Which chisquare test should I use? 

table(birdhab$s.edge, birdhab$BRCR > 0)
br_creeper_table = table(birdhab$s.edge, birdhab$BRCR > 0)[, 2:1]

chisq.test(br_creeper_table)
#E -- Exterior
#I -- Interior

#null- brown  show no preference for edge or interior habitat

#we have evidence that it does prefer one or the other
