data <- read.csv("Evelynn_stats.csv", sep=",")
outliersKillRatio <- boxplot.stats(data$killRatio)$out #find outliers using IQR

pdf(file = "./KillRatioOutliers.pdf", width = 8, height = 8)

boxplot(data$killRatio,
  ylab = "Kill Ratio",
  main = "Kill Ratio",
  col = "red"
)
mtext(paste("Outliers: ", paste(outliersKillRatio, collapse = ", ")))

summary(data$killRatio)
cat("MAD = ", mad(data$killRatio), "\n")
cat("Var(x) = ", var(data$killRatio), "\n")
cat("IQR = ", IQR(data$killRatio), "\n")

if(shapiro.test(data$killRatio)$p.value >= 0.05) {
    print("Kill ratio is close to a normal distribution")
} else {
    print("Kill ratio is not close to a normal distribution")
}

outliersDeathRatio <- boxplot.stats(data$deathRatio)$out #find outliers using IQR

pdf(file = "./DeathRatioOutliers.pdf", width = 8, height = 8)

boxplot(data$deathRatio,
  ylab = "Death Ratio",
  main = "Death Ratio",
  col = "blue"
)
mtext(paste("Outliers: ", paste(outliersDeathRatio, collapse = ", ")))

summary(data$deathRatio)
cat("MAD = ", mad(data$deathRatio), "\n")
cat("Var(x) = ", var(data$deathRatio), "\n")
cat("IQR = ", IQR(data$deathRatio), "\n")

if(shapiro.test(data$deathRatio)$p.value >= 0.05) {
    print("Death ratio is close to a normal distribution")
} else {
    print("Death ratio is not close to a normal distribution")
}