
#----Data---- 
bird.sub= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/bird.sub.csv")
hab.sub= read.csv("https://michaelfrancenelson.github.io/eco_602_634_2020/data/hab.sub.csv")

#Correct Read in
birdhab=merge(bird.sub, hab.sub)
dim(birdhab)
birdhab

# brown creeper abundance 
brownCreeper=birdhab$BRCR
# late successional forest 
lates=birdhab$ls

#----Simulating Static Envi. Process: Linear Regression
  # rnorm()  --> Normal
  # rpois() ---> Poisson

#----Graphical Exploration----
plot(brownCreeper ~ lates, col = "black", 
     pch = 16, xlab= "Late-successional forest", 
     ylab= "Brown Creeper abundance", 
     main="Graphical Exploration of Brown Creeper\nabunadance in late-successional forest")

#----Fit a model----
fit_1= lm(brownCreeper~lates)
fit_1


abline(fit_1)

#----Simulator Function----

#Deterministic Model; Linear Function
linear= function(x, y_int, slope)
{ 
  y_int + x * slope
  }
linear(x = 1, y_int = 1, slope = 1)
linear(x = 3:5, y_int = 1, slope = 1)
linear(x = 3:5, y_int = -1, slope = 1)
line=linear(x = 3:5, y_int = -1, slope = 0.01)

plot(3:5,line)
x = c(runif(100))
y = linear(x, y_int = 1, slope =1)

plot(x, y)
#Stochastic Model: Normal Distribution
  #rnorm() - appropriate arguments
st_dev = 5
rnorm(length(x) , sd= st_dev )

length(x)
length(y)
#Simulation Function
linear_simulator= function(x, y_int, slope, st_dev)
  {
    stocat=rnorm(length(x) , sd= st_dev)
    sim= linear(x, y_int, slope)
    sim + rnorm(length(x) , sd= st_dev)
    return(sim + stocat)
  }


#Test# 
n = 200

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, 
    linear_simulator(x, y_int = 1, slope = 4.5, st_dev = 0.1),
    main = "", xlab = "x", ylab = "y",
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}

dev.off()

#TEST 2
n = 400

par(mfrow = c(2, 2))
for (i in 1:4)
{
  x = runif(n = n)
  plot(
    x, linear_simulator(x, y_int = 10, slope = -6.5, st_dev = 1.1),
    main = "", xlab = "x", ylab = "y",
    ylim = c(2,12),
    pch = 16, col = rgb(0, 0.2, 0, 0.2))
}


#Build the Simulation: 
#Retrieving the model coefficients

fit_1_coefs = coefficients(fit_1)
str(fit_1_coefs)

fit_1_summary = summary(fit_1)
fit_1_summary$sigma

#Storing intercept, slope, st.dev parameters 
int_obs=fit_1_coefs[1]
slope_obs= fit_1_coefs[2]
sd_obs= fit_1_summary$sigma

int_obs
slope_obs
sd_obs

dev.off()

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")

plot(
  x = birdhab$ls, 
  y = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  ),
  main = "Simulated Data",
  xlab = "late-successional forest",
  ylab = "Brown Creeper Abundance")


#Single Simulation: 
  y_sim = linear_simulator(
    x = birdhab$ls,
    y_int = int_obs,
    slope = slope_obs,
    st_dev = sd_obs
  )
  
#----Repeated Simulations - i ----
  n_sims = 1000
  p_vals = numeric(n_sims)
  for(i in 1:n_sims)
  {
    y_sim = linear_simulator(
      x = birdhab$ls,
      y_int = int_obs,
      slope = slope_obs,
      st_dev = sd_obs
    )
    fit_sim = lm(y_sim ~ birdhab$ls)
    
    p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
  }
  sum(p_vals < 0.05) / n_sims
  
  ### Simple Quick Function
  linear_sim_fit = function(x, y_int, slope, st_dev)
  {
    y_sim = linear_simulator(
      x = x,
      y_int = y_int,
      slope = slope,
      st_dev = st_dev
    )
    return(lm(y_sim ~ x))
  }
  
#----simulating Effect Sizes - j & i ----
  alpha = 0.05
  n_sims = 10
  p_vals = numeric(n_sims)
  
  n_effect_sizes = 20
  effect_sizes_1 = seq(-.01, .01, length.out = n_effect_sizes)
  
  effect_size_powers = numeric(n_effect_sizes)
  
  for(j in 1:n_effect_sizes)
  {
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = birdhab$ls,
        y_int = int_obs,
        slope = effect_sizes_1[j],
        st_dev = sd_obs
      )
      
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    effect_size_powers[j] = sum(p_vals < alpha) / n_sims
  }
  
  sim_effect_size = 
    data.frame(
      power       = effect_size_powers,
      effect_size = effect_sizes_1)
  
  #Ploting 
  plot(
    power ~ effect_size, data = sim_effect_size,
    type = 'l', xlab = 'Effect size', ylab = 'Power')
  abline(v = coef(fit_1)[2], lty = 2, col = 'red')
  
  
#----Simulating Sample Sizes - j & i----
  alpha = 0.05
  n_sims = 1000
  p_vals = numeric(n_sims)
  
  sample_sizes = seq(5, 100)
  sample_size_powers = numeric(length(sample_sizes))
  
  for(j in 1:length(sample_sizes))
  {
    x_vals = seq(0, 100, length.out = sample_sizes[j])
    
    for(i in 1:n_sims)
    {
      fit_sim = linear_sim_fit(
        x = x_vals,
        y_int = int_obs,
        slope = slope_obs,
        st_dev = sd_obs
      )
      p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
    }
    sample_size_powers[j] = sum(p_vals < alpha) / n_sims
  }
  
  sim_sample_size = 
    data.frame(
      power       = sample_size_powers,
      sample_size = sample_sizes)
  
#plotting 
  plot(
    power ~ sample_size, data = sim_sample_size,
    type = 'l', xlab = 'Sample size', ylab = 'Power')
  abline(v = nrow(birdhab), lty = 2, col = 'red')

#Bivariate Power Analysis
  
#----Effect Size and Sample Size- k, j, i----
  alpha = 0.01
  n_sims = 50
  
  p_vals = numeric(n_sims)
  
  n_effect_sizes = 20
  effect_sizes = seq(-.01, .01, length.out = n_effect_sizes)
  sample_sizes = seq(10, 100)
  
  sim_output_2 = matrix(nrow = length(effect_sizes), ncol = length(sample_sizes))
  
  for(k in 1:length(effect_sizes))
  {
    effect_size = effect_sizes[k]
    for(j in 1:length(sample_sizes))
    {
      x_vals = seq(0, 100, length.out = sample_sizes[j])
      
      for(i in 1:n_sims)
      {
        fit_sim = linear_sim_fit(
          x = x_vals,
          y_int = int_obs,
          slope = effect_size,
          st_dev = sd_obs
        )
        p_vals[i] = summary(fit_sim)$coefficients[2, 'Pr(>|t|)']
      }
      sim_output_2[k, j] = sum(p_vals < alpha) / n_sims
    }
    print(paste0("computing effect size ", k," of ", length(effect_sizes)))
  }
  
  sim_n_effect_size = 
    list(
      power = sim_output_2,
      effect_size = effect_sizes,
      sample_size = sample_sizes
    )

  #----Image ----
  image(sim_n_effect_size$power)
  
dev.off()
  # Contour Plotting 
  x = sim_n_effect_size$effect_size
  y = sim_n_effect_size$sample_size
  z = sim_n_effect_size$power
  contour(x,y,z)
  contour(x,y,z, ylim = c(10, 100))

#---- 3D Plotting ----

#Static plot
persp(
  x = sim_n_effect_size$effect_size,
  y = sim_n_effect_size$sample_size,
  z = sim_n_effect_size$power,
  xlab = "beta", ylab = "n", zlab = "power",
  col = 'lightblue',
  theta = 30, phi = 30, expand = .75,
  ticktype = 'detailed')

#Interactive Plot
install.packages("rgl")
require(rgl)


install.packages("https://dl.bintray.com/xquartz/downloads/XQuartz-2.7.11.dmg")
persp3d(x = sim_n_effect_size$effect_size,
        y = sim_n_effect_size$sample_size,
        z = sim_n_effect_size$power,
        xlab = "beta", ylab = "n", zlab = "power",
        col = 'lightblue',
        theta = 30, phi = 30, expand = .75,
        ticktype = 'detailed')


#----Assignment----
#Population Dispersion Analysis
alpha = 0.05
n_sims = 100
p_vals = numeric(n_sims)

# What was the observed standard deviation?
sd_obs= fit_1_summary$sigma

# specify the number of different standard deviation values to simulate:
n_sds = 20
pop_sds = seq(from = 0.01, to = 1.5, length.out = n_sds)

pop_sd_power = numeric(n_sds)

for(j in 1:length(pop_sds))
{
  pop_sd_j = ...
  for(i in 1:n_sims)
  {
    fit_sim = linear_sim_fit(...)
    p_vals[i] = ...
  }
  pop_sd_power[j] = ...
}

sim_output_dispersion = data.frame(
  sd = ...,
  power = ...)

# You should save your simulation results so you don't have to run it every time.
save(
  sim_output_dispersion, 
  file = here::here("data", "lab_ll_dat_dispersion_sim.RData"))

# Line plot of standard deviation (x-axis) and statistical power (y-axis)
plot(...)

# Add a dotted vertical red line at the observed population standard deviation value.
abline(v = ...)