#Q1 (2 pts.) Qualitatively describe what happens 
#to the curve as you vary parameter a
  #Parameter a describes the height of the line.
#Q2 (2 pts.) Qualitatively describe what happens 
#to the curve as you vary parameter b
  # When you vary b, it changes the depth of the slope. 
dev.off()

#Q3 -- Ricker Plot
png(filename = "Rickerplot_Q4.png")
ricker_fun = function(x, a, b) 
{
  return(a * x * exp(-b * x))
}

curve(
  ylim= c(0,100),xlim = c(0,75),
  ricker_fun(x, 25, 0.1), 
  from = 0, to = 75, add = FALSE, 
  main = "Ricker plot of 4 curves with\ndifferent parameter values",
  ylab = "f(x)", xlab = "x", col = "orange", lty= "solid")
curve(
  ricker_fun(x, 20, 0.2), 
  from = 0, to = 75, add = TRUE, 
  main = "Ricker plot of 4 curves with\ndifferent parameter values",
  ylab = "f(x)", xlab = "x", col = "purple", lty= "dashed")
curve(
  ricker_fun(x, 10, 0.2), 
  from = 0, to = 75, add = TRUE, 
  main = "Ricker plot of 4 curves with\ndifferent parameter values",
  ylab = "f(x)", xlab = "x", col = "black", lty= "dotted")
curve(
  ricker_fun(x, 75, 0.3), 
  from = 0, to = 75, add = TRUE, 
  main = "Ricker plot of 4 curves with\ndifferent parameter values",
  ylab = "f(x)", xlab = "x", col = "red", lty= "solid")
curve(
  ricker_fun(x, 50, 0.3), 
  from = 0, to = 75, add = TRUE, 
  main = "Ricker plot of 4 curves with\ndifferent parameter values",
  ylab = "f(x)", xlab = "x", col = "blue", lty= "dashed")
curve(
  ricker_fun(x, 40, 0.3), 
  from = 0, to = 75, add = TRUE, 
  main = "Ricker plot of 4 curves with\ndifferent parameter values",
  ylab = "f(x)", xlab = "x", col = "green", lty= "solid")
dev.off()


####  4       ######

