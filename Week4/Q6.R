#Choose one of your datasets from the previous question.
#Using the code in the lab walkthrough, visually fit a linear deterministic function through the data. Make sure you save your parameters to variables so you can use them in the next question.
#Create a plot of your simulated data and the line of your fitted model.
#Q1 (3 pts.) Paste the R code you used to fit your model.
#Q3 (1 pt.) Paste your figure file into the file entry box.n_pts = 13
dev.off()
x_min = 0
x_max = 15
set.seed(43)
x = runif(n = n_pts, min = x_min, max = x_max)

dat = data.frame(x = x, y_observed = rnorm(n_pts))
head(dat)
plot(y_observed ~ x, data = dat, pch = 17, main= "Plot 2 of 13 randomly distributed\nnumbers and visually fit linear line", xlab= "Values", ylab = "Points", col= "blue")

guess_x= 10
guess_y= 0
guess_slope= .2

plot(y_observed ~ x, data = dat, pch = 17, main= "Plot 2 of 13 randomly distributed\nnumbers and visually fit linear line", xlab= "Values", ylab = "Points", col= "blue")
curve(line_point_slope(x, guess_x, guess_y, guess_slope), add = T)

line_point_slope(dat$x, guess_x, guess_y, guess_slope)
names(line_point_slope)
dat$y_predicted= c(line_point_slope(dat$x, guess_x, guess_y, guess_slope))
y_predicted= c(line_point_slope(dat$x, guess_x, guess_y, guess_slope))

dat$resids= c(y_predicted - dat$y_observed)
dat
c
