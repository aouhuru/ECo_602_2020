#----Recap of t-Tests----
t.test(subset(penguins, species == "Gentoo")$flipper_length_mm)

#test to see if equal to 218 mm: 
t.test(
  x = subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218
)

#smaller than 218mm: 
t.test(
  x= subset(penguins, species == "Gentoo")$flipper_length_mm,
  mu = 218, alternative = "less"
)

#----2-sample t-test ----
t.test(flipper_length_mm ~ species, 
       data = subset(penguins, species !="Chinstrap"))

#----Analysis of Variance (ANOVA) ----
#model 1: Body mass explained by species
  #response variable: body mass
  #continuous variable
  #ratio scale
  #Predictor variable: species
  #categorical variable
  #nominal scale

#----1-Way analysis of Variance: Anova----
  #perform graphical and numerical data exploration
  # Fit Linear model using lm()
  #Examine model coefficient table using summary()
  #Conduct the Analysis of Variance using anova()

#----Step 1:Data exploration of ANOVA-----
  #Graphical
    #historgram and density plot
par(mfrow = c(1,2))
hist(penguins$body_mass_g, 
     breaks = 80, 
     main = "histogram of body mass", 
     xlab = "body mass (g)")
plot(density(penguins$body_mass_g, na.rm = T),
     main = "density plot of body mass")

dev.off()
require(palmerpenguins)
boxplot(body_mass_g ~ species, data = penguins)

  #Numerical
    #Extract measurements of each species
    #calculate mean body mass for each species
    #conduct Shapiro tests on each species body mass

dat_chinstrap = subset(penguins, species == "Chinstrap")
mean(dat_chinstrap$body_mass_g, na.rm = T)

shapiro.test(dat_chinstrap$body_mass_g)

#shortcut for calculating mwan body masses uses aggregate()
aggregate(body_mass_g ~ species, data = penguins, FUN = mean)


#----Step 2:Fit Linear Model----
fit_species = lm(body_mass_g ~ species, data = penguins)

#----Step 3:Examine model coefficient table----
summary(fit_species)


#----Step 4: Conduct ANOVA----
anova(fit_species)


#----One-Way Anova Complete Walkthrough----
fit_species = lm(body_mass_g ~ species, data = penguins)
summary(fit_species)
anova(fit_species)


#----Two Tables: Model coefficients and ANOVA

#sum of squares - 
#mean squares - compare relative amount of information that each factor explains
#F-Value - how much adding a variable to the model will improve the model fit

#the Sum Sq and Mean Sq columns give us information about how 
  #much variability the predictor variable is able to explain.

#When you have more than one predictor variable, you can think
  #of the Mean Sq column as an estimate of the relative importance 
  #of each predictor (this will make more sense later).

#The Pr(>F) column gives us a rough idea of whether the predictor 
  #significantly improves the model’s prediction or not.

#A low p-value here (approximately) means that adding the predictor 
  #creates a significantly better model than leaving it out.

#----Two-way Factorial ANOVA
boxplot(body_mass_g ~ sex * species, data = penguins)

fit_both= lm(body_mass_g ~ sex * species, data = penguins)
summary(fit_both)
anova(fit_both)

#----Question 1----
#yes they are significantly heavier than  female penguins. The male boxplots are visually higher than those of female penguins in each species

#----Question 2----
Ask about this question 
#yes it does
#----Question 3----
fit_both= lm(body_mass_g ~ sex * species, data = penguins)

#----Question 4----
summary(fit_both)
#The intercept is the base case, which is the adelie female penguin. 

#----Question 5----
Ask question about this
#Intercept or the base case plus species chinstrap. 
  
#Question 6----
(3368.84  + 158.37)
#intercept + speciesChinstrap - sexmale:speciesChinstrap 
ask about this

