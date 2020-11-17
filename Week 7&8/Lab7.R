#apply works on 2D arrays --> Dataframes and matrices

# x = 2D data 
# MARGIN --> row or column (1 = row, 2 = column)
# FUN --> Applies to rows and columns


dat = matrix(1:49, nrow = 7, ncol = 7)
print(dat)

#ROWS 
apply(dat, MARGIN = 1, FUN = min)
apply(dat, MARGIN = 1, FUN = max)

#COLUMNS
apply(dat, MARGIN = 2, FUN = min)
apply(dat, MARGIN = 2, FUN = max)

#MEAN COLUMNS
apply(dat, MARGIN = 2, FUN = mean)
#MEAN ROW
apply(dat, MARGIN = 1, FUN = mean)


#-------DATA FILE-----------### 
# ---- Moth DataSet ---- 
moths = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/moths.csv")
head(moths)


#Moth Abundance#
hist(moths$anst, main = "Histogram of Moths", xlab = "anst",col = "light green")



#parametric Confidence Interval# 
alpha = 0.05
anst = moths$anst
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(anst) / sqrt(n)

sample_mean = mean(anst)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )

m = 10000
# numeric() creates an vector of length m with all values initiailized to zero
result = numeric(m)
head(result)

#Perform the bootstrap
for(i in 1:m)
{
  result[i] = mean(sample(anst, replace=TRUE))
}
#calculating quantiles#
mean(result)
quantile(result, c(0.025, 0.975))

#Bootstrap using boot 
install.packages("boot")
require(boot)
boot(data, statistic, R)

#Modified mean function
boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

#bootsrtap 
myboot = 
  boot(
    data = anst,
    statistic = boot_mean, 
    R = 10000)
print(myboot)

str(myboot)

mean(anst)
myboot$t0
# ---- Wrong Answers ----
mean(myboot$t) - myboot$t0.
sd(myboot$t)

# ---- Wrong Answer ----
#Extraction of bootstrap confidence
quantile(
  myboot$t, 
  c(0.025, 0.975)
)

moth_dat = moths[, -1]
head(moth_dat)

n = nrow(moth_dat) #number of rows or sample observations
m = 100 #number of bootstrap iterations
moth_result = matrix(
  nrow = m,
  ncol = n)

n = nrow(moth_dat) #number of rows or sample observations

m = 100 #number of bootstrap iterations

moth_result = matrix(
  nrow = m,
  ncol = n)


# The outer loop: runs once for each bootstrap iteration.  index variable is i
for(i in 1:m)
{
  # The inner loop: simulates increasing sampling intensity
  # Sampling intensity ranges from 1 site to the complete count of sites (24)
  # index variable is j
  for(j in 1:n)
  {
    # sample the input data row indices, with replacement
    rows_j = sample(n, size = j, replace=TRUE)
    
    # Creates a new data matrix
    t1 = moth_dat[rows_j, ]
    
    # Calculates the column sums
    t2 = apply(t1, 2, sum)
    
    # Counts the number of columns in which any moths were observed
    moth_result[i, j] = sum(t2 > 0)
  }
}

head(moth_result)



### ---------- FIRST DRAFT --------- ###
rarefaction_sampler = function(input_dat, n_iterations)
{
  n = nrow(moth_dat) #number of rows or sample observations
  m = 100 #number of bootstrap iterations
  
  moth_result = matrix(
    nrow = m,
    ncol = n)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:m)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of sites (24)
    # index variable is j
    for(j in 1:n)
    {
      
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = moth_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      moth_result[i, j] = sum(t2 > 0)
    }
  }
  
  return(moth_result)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

n = 24
# ---- Second Draft ----
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    # index variable is j
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

### FRESH ENVIRONMENT ###
# This clears the current R session's environment
rm(list = ls())

# Re-read my data:
moths = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/moths.csv")
moth_dat = moths[,-1]

rarefaction_sampler = function(input_dat, n_iterations)
{
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)


##Debugging template##
rm(list = ls())

# Re-read my data:
moths = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/moths.csv")
n = 24
rarefaction_sampler = function(input_dat, n_iterations)
{
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moths[,-1], 100)
head(rarefact)


###Building Rarefaction Curve
moths = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/moths.csv")
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))
         
dev.off()
### Plotting curve
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main='Rarefaction Curve')

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))



# ---- Question 1: Parametric CI: Gentoo Penguins# ----
install.packages("palmerpenguins")
require(palmerpenguins)


#Calculate a parametric 95% CI for mean bill length (in mm) for the Gentoo penguins in penguins dataset from package palmerpenguins.
#Use the student’s t-distribution. You’ll need to:
  #extract only the Gentoo penguin data from the main penguins data frame
dat_pen = subset(penguins, species == "Gentoo")
t.test(dat_pen$bill_length_mm ~ dat_pen$species)

alpha = 0.05
anst = dat_pen$bill_length_mm
n = sum(!is.na(anst))
t_crit = abs(qt(alpha / 2, df = n - 1))

sse = sd(anst, na.rm = TRUE) / sqrt(n)


anst
sample_mean = mean(anst, na.rm = TRUE)
ci_parametric = sse * t_crit

confidence_intervals = 
  data.frame(
    technique = c("parametric: t-dist"),
    mean = sample_mean,
    ci_radius = sse * t_crit,
    lower = sample_mean - ci_parametric,
    upper = sample_mean + ci_parametric
  )
confidence_intervals
#check for missing data

  #calculate n
#Paste the r code you used to perform the following:
  #extract the bill length data for Gentoo penguins
  #calculate the critical t-values
  #calculate the sample mean, standard deviation, and standard error
  #calculate the CI

# ---- Question 2 : Bootstrap Confidence Interval----
require(palmerpenguins)
require(boot)
dat_pen = subset(penguins, species == "Gentoo")
anst = dat_pen$bill_length_mm

boot_mean = function(x, i)
{
  return(mean(x[i], na.rm = TRUE))
}

#bootsrtap 
myboot = 
  boot(
    data = anst,
    statistic = boot_mean, 
    R = 10000)
print(myboot)

quantile(
  myboot$t, 
  c(0.025, 0.975)
)

# ---- Question 3: Rarefaction function ----
moths = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/moths.csv")
moth_dat = moths[,-1]
n = 24
rarefaction_sampler = function(input_dat, n_iterations)
{
  
  n_input_rows = nrow(input_dat)
  
  results_out = matrix(
    nrow = n_iterations,
    ncol = n_input_rows)
  
  # The outer loop: runs once for each bootstrap iteration.  index variable is i
  for(i in 1:n_iterations)
  {
    # The inner loop: simulates increasing sampling intensity
    # Sampling intensity ranges from 1 site to the complete count of 
    # sites in the input data (n)
    for(j in 1:n)
    {
      # sample the input data row indices, with replacement
      rows_j = sample(n, size = j, replace=TRUE)
      
      # Creates a new data matrix
      t1 = input_dat[rows_j, ]
      
      # Calculates the column sums
      t2 = apply(t1, 2, sum)
      
      # Counts the number of columns in which any moths were observed
      results_out[i, j] = sum(t2 > 0)
    }
  }
  return(results_out)
}

rarefact = rarefaction_sampler(moth_dat, 100)
head(rarefact)

dev.off()
# ---- Question 4: Rarefaction function ----
moths = read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/moths.csv")
rarefact = rarefaction_sampler(moths[,-1], 10000)

rare_mean = apply(rarefact, 2, mean)
rare_quant = apply(rarefact, 2, quantile, probs=c(0.025, 0.975))
rare = t(rbind(rare_mean, rare_quant))

png(filename = "Q4.png")
matplot(
  rare,
  type='l',
  xlab='Number of sampling plots',
  ylab='Species richness', 
  main="Rarefaction Curve of lower\nand Upper 95% Confidence Intervals")

legend(
  'bottomright',
  legend=c('mean','2.5%','97.5%'),
  lty=c(1,2,3),col=c(1,2,3), inset=c(.1,.1))
dev.off()