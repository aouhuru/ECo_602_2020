head(rope_dat)
nrow(rope_dat)
ncol(rope_dat)
dim(rope_dat)

rope_dat = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/rope.csv")
rope_dat$rope.type=factor(rope_dat$rope.type)


head(rope_dat)
nrow(rope_dat)
ncol(rope_dat)
dim(rope_dat)
  #Quick Data --> options nrow, ncol, dim 

#Factorial Experiment
#rope: 6 types
#Blade: 4 types


#-----ANOVA by Hand ----
levels(rope_dat$rope.type)
#----Number of Observation ----
n_obs= nrow(rope_dat)

n_groups= length(levels(rope_dat$rope.type))

#----Partitioning Variance: Total ----
#Regression: 
  #E(y- mean)^2= E(y1-mean)^2 + E(y-y1)^2


ss_tot= sum((rope_dat$p.cut - mean(rope_dat$p.cut))^2)


#----Partitioning Variance: Within-Group ----
aggregate(
  x = rope_dat$p.cut,
  by = list(rope_dat$rope.type), 
  FUN = mean)

aggregate(
  x = rope_dat$p.cut,
  by = list(rope_dat$rope.type),
  FUN = function(x) mean(x))

agg_sq_resids = aggregate(
  x = rope_dat$p.cut,
  by = list(rope_dat$rope.type),
  FUN = function(x) sum((x - mean(x))^2))

ss_within= str(agg_sq_resids)
ss_within= sum(agg_sq_resids$x)
#----Partitioning Variance: Among Groups ----
ss_among = ss_tot - ss_within
df_among = n_groups - 1
#----Normalizing ----
ms_among  =  ss_among / (n_groups - 1)
ms_within = ss_within / (n_obs - n_groups)
df_within= n_obs - n_groups
#---- The Test Statistic: F ----
f_ratio = ms_among/ms_within

f_pval = 1 - pf(f_ratio, df_among, df_within)
#---- ANOVA in R----
fit_1 = lm(p.cut ~ rope.type, data=rope_dat)
anova(fit_1)

anova_fit_1 = anova(fit_1)
str(anova_fit_1)

anova_fit_1$"Sum Sq"


# number comparison tolerance
digits_check = 5

# Build the reference model using R functions
fit_1 = lm(p.cut ~ rope.type, data=rope_dat)
anova(fit_1)
anova_fit_1 = anova(fit_1)

# Check degrees of freedom
anova_fit_1$Df == c(df_among, df_within)

# Check sums of squares
round(anova_fit_1$`Sum Sq`, digits = digits_check) == round(c(ss_among, ss_within), digits = digits_check)

# Check mean squares
round(anova_fit_1$`Mean Sq`, digits = digits_check) == round(c(ms_among, ms_within), digits = digits_check)

# Check the F-ratio
round(anova_fit_1$`F value`[1], digits = digits_check) == round(f_ratio, digits = digits_check)

# Check the F test statistic p-value
round(anova_fit_1$`Pr(>F)`[1], digits = digits_check) == round(f_pval, digits = digits_check)