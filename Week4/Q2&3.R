require(here)
png(
  filename = here("lab_04_hist_01.png"),
  width = 700, height = 1400 , res = 180 , units = "px", bg = "transparent")

par(mfrow=c(3,1))
hist(norm_17, main = "Histogram of 17 randomly normal distribution Numbers", xlab = "Values", col = "green"  )
hist(norm_30, main = "Histogram of 30 randomly normal distribution Numbers", xlab = "Values", col = "blue" )
hist(norm_300, main = "Histogram of 300 randomly normal distributed Numbers", xlab = "Values", col = "purple" )
dev.off()



#Q1 Qualitatively describe the differences in the patterns you observe.

#Q2 Explain why they histograms are different.