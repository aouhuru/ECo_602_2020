# ---- Data Files ----
veg_data= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/vegdata.csv")
bird_dat = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sub.csv")
hab_dat = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sub.csv ")
require(boot)
require(veg_data)
require(bird_dat)
require(hab_dat)
# ---- Walkthrough ----
require(palmerpenguins)
penguin_dat = droplevels(subset(penguins, species != "Gentoo"))

# ----- Parametric Two- Sample Test ----
t.test(flipper_length_mm ~ species, data = penguin_dat, alternative = "less")
install.packages("simpleboot")
require(simpleboot)

# ---- Tree Data ----
boxplot(pine ~ treatment, dat = veg_data)
dat_tree = droplevels(subset(veg_data, treatment %in% c("control", "clipped")))
boxplot(pine ~ treatment, dat = dat_tree)

# ---- Bootstrap ----
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")
quantile(tree_boot$t, 0.025)

#---- Resampling: Linear regression ----
dat_all = merge(
  bird_dat, 
  hab_dat,
  by = c("basin", "sub"))

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")


#---- Simple Linear Regression -----
fit_1 = lm(b.sidi ~ s.sidi, data = dat_all)
coef(fit_1)

slope_observed = coef(fit_1)[2]

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_1)

#---- The Slope Coefficient ----
dat_1 = 
  subset(
    dat_all,
    select = c(b.sidi, s.sidi))

#---- Resampling the DATA ---- 
index_1 = sample(nrow(dat_1), replace = TRUE)
index_2 = sample(nrow(dat_1), replace = TRUE)

dat_resampled_i = 
  data.frame(
    b.sidi = dat_1$b.sidi[index_1],
    s.sidi = dat_1$s.sidi[index_2]
  )

fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
slope_resampled_i = coef(fit_resampled_i)[2]

print(slope_resampled_i)


plot(
  b.sidi ~ s.sidi, data = dat_resampled_i,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")
abline(fit_resampled_i)

# ---- Randomization loop ----
  #resample data
  #fit simple linear regression 
  #extract slope parameter 
m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  
  
  result[i] = coef(fit_resampled_i)[2]
} 


# ---- The Null Distribution ----
dev.off()
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 2, col = "red", lwd = 2)

# ---- Critical Slope Value ----
quantile(result, c(.05))

# ---- LAB QUESTIONS ----
# ---- Question 1 Histogram ----
pen_boot= 
  two.boot(
    subset(penguin_dat, species == "Adelie")$flipper_length_mm,
    subset(penguin_dat, species == "Chinstrap")$flipper_length_mm,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )
boot.ci(pen_boot)

png(filename = "FLipper_hist.png", width = 480, height = 480,units = "px")
hist(pen_boot$t, main = "Bootstrap of Flipper Length distribution\nbetween Adelie and Chinstrap penguins", col = "blue")

# ---- Question 2 CI ----
quantile(pen_boot$t, c(0.025, 0.975))
# ---- Question 3 Empirical dist.----

pen_ecdf=ecdf(pen_boot$t)
# ---- Question 4 hypotheses ----
# We fail to reject the null hypothesis, due to the fact that x and z are less than the p value

# ---- Question 5 empirical dist.----
pen_ecdf(-4.5)
1- 0.9134
#1- pen_ecdf(-4.5)
#0.0866

pen_ecdf()
# ---- Question 6 wilcoxon test ----
wilcox.test(dat_tree$pine)
#p-value: 0.002507
# ---- Question 7 CI ----
tree_boot = 
  two.boot(
    subset(dat_tree, treatment == "clipped")$pine,
    subset(dat_tree, treatment == "control")$pine,
    FUN = mean,
    R = 10000,
    na.rm = TRUE
  )

# sum(tree_boot$t >= 0)
# sum(tree_boot$t < 0)

boot.ci(tree_boot)

hist(tree_boot$t, main = "Bootstrap sampling distribution")
quantile(tree_boot$t, 0.025)

#2.5% and 4.125

# ---- Question 8 Resample Loop ----
dat_all = merge(
  bird_dat, 
  hab_dat,
  by = c("basin", "sub"))

plot(
  b.sidi ~ s.sidi, data = dat_all,
  main = "Simpson's diversity indices",
  xlab = "Vegetation cover diversity",
  ylab = "Bird diversity")

# ---- Question 9 null dist. ----
m = 10000 
result = numeric(m) 

for(i in 1:m)
{
  
  index_1 = sample(nrow(dat_1), replace = TRUE)
  index_2 = sample(nrow(dat_1), replace = TRUE)
  
  dat_resampled_i = 
    data.frame(
      b.sidi = dat_1$b.sidi[index_1],
      s.sidi = dat_1$s.sidi[index_2]
    )
  fit_resampled_i = lm(b.sidi ~ s.sidi, data = dat_resampled_i)
  slope_resampled_i = coef(fit_resampled_i)[2]
  
  
  result[i] = coef(fit_resampled_i)[2]
} 

dev.off()
png(filename = "SimpDiv.Nulldis.png")
hist(result, main = "Null Distribution of Regression Slope", xlab = "Slope Parameter")
abline(v = slope_observed, lty = 1, col = "blue", lwd = 2)
abline(v=quantile(result, c(0.5)), lty = 2, col = "red", lwd = 2)
dev.off()

# ---- Question 10 Critical Value ----

quantile(result, c(.05))
# 5% - -0.01322147
#More bird diversity results in decline of vegetation
#Less vegetation less bird diversity 