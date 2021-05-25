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
mad(data$killRatio)
var(data$killRatio)
IQR(data$killRatio)

# shapiro.test(data$killRatio)

outliersDeathRatio <- boxplot.stats(data$deathRatio)$out #find outliers using IQR

pdf(file = "./DeathRatioOutliers.pdf", width = 8, height = 8)

boxplot(data$deathRatio,
  ylab = "Death Ratio",
  main = "Death Ratio",
  col = "blue"
)
mtext(paste("Outliers: ", paste(outliersDeathRatio, collapse = ", ")))

summary(data$deathRatio)
mad(data$deathRatio)
var(data$deathRatio)
IQR(data$deathRatio)
