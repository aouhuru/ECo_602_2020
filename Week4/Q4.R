#Example
x = seq(-3, 3, length.out = 1000)
y = dnorm(x)

plot(x, y, main = "Normal PDF", type = "l")
abline(h = 0)


dev.off()
#Question4

png(
  filename = here("norm_1.png"),
  width = 700, height = 800 , res = 180 , units = "px", bg = "transparent")
x= seq(0, 25, length.out = 1000)
y=dnorm(x, mean=10.4, sd=2.4)
plot(x, y, main = "Normal PDF with a mean of 10.4\nand standard deviation of 2.4", type = "l")
abline(h = 0)
dev.off()
