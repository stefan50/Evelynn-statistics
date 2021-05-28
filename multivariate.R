data <- read.csv("Evelynn_stats.csv", sep=",")

# Kill rate and death rate do not resemble normal distributions.
# Therefore, I will be using non-parametrized tests.

# Connection between tier and blue team

tierBlueTeam <- table(data$tier, data$blue_team)

# Blue team selection based on the different tiers (in percentages)
prop.table(tierBlueTeam, margin = 1)

# Tiers based on blue team selection (in percentages)
prop.table(tierBlueTeam, margin = 2)

# Connection between tier and win rate

tierWinRate <- table(data$tier, data$win)

# Win rate based on tier
prop.table(tierWinRate, margin = 1)

# Tier based on win rate
prop.table(tierWinRate, margin = 2)

# Connection between blue team and win rate

blueTeamWinRate <- table(data$blue_team, data$win)

# Win rate based on blue team
prop.table(blueTeamWinRate, margin = 1)

# Blue team based on win rate
prop.table(blueTeamWinRate, margin = 2)

# Correlation analysis between kill ratio and death ratio
# Data is not normal. Therefore, using Spearman's correlation index
correl_index <- abs(cor(data$killRatio, data$deathRatio, method = "spearman")) 

if(correl_index >= 0.9 && correl_index <= 1.0) {
    print("Kill ratio and death ratio are extremely correlated.")
} else if(correl_index >= 0.75 && correl_index < 0.9) {
    print("Kill ratio and death ratio are quite correlated.")
} else if(correl_index >= 0.5 && correl_index < 0.75) {
    print("Kill ratio and death ratio are correlated.")
} else {
    print("Kill ratio and death ratio are vaguely correlated or not correlated at all.")
}

test <- lm(data$killRatio ~ data$deathRatio)
test2 <- lm(data$deathRatio ~ data$killRatio)

# Source : https://stackoverflow.com/questions/5587676/pull-out-p-values-and-r-squared-from-a-linear-regression

areParametersRelevant <- function(regr) {
    fStatLinearReg <- summary(regr)$fstatistic
    fDist <- pf(fStatLinearReg[1],fStatLinearReg[2],fStatLinearReg[3],lower.tail=F)
    attributes(fDist) <- NULL

    if(fDist > 0.05) {
        print("All parameters are statistically irrelevant")
    }
}

areParametersRelevant(test)
areParametersRelevant(test2)

tests <- function(vector, name) {
    if(wilcox.test(vector ~ data$win)$p.value < 0.05) {
        cat("Whether we win or not influences", name, "\n")
    } else {
        cat("Whether we win or not does not influence", name, "\n")
    }

    if(wilcox.test(vector ~ data$blue_team)$p.value < 0.05) {
        cat("Whether we are in blue team or not influences", name, "\n")
    } else {
        cat("Whether we are in blue team or not does not influence", name, "\n")
    }

    if(kruskal.test(vector ~ data$tier)$p.value < 0.05) {
        cat("Tier influences", name, "\n")
    } else {
        cat("Tier does not influence", name, "\n")
    }
}

tests2 <- function(vector, name) {
    if(kruskal.test(vector ~ data$killRatio)$p.value < 0.05) {
        cat("Kill ratio influences", name, "\n")
    } else {
        cat("Kill ratio does not influence", name, "\n")
    }

    if(kruskal.test(vector ~ data$deathRatio)$p.value < 0.05) {
        cat("Death ratio influences", name, "\n")
    } else {
        cat("Death ratio does not influence", name, "\n")
    }
}

tests(data$killRatio, "kill ratio")
tests(data$deathRatio, "death ratio")
tests2(data$win, "win rate")
tests2(data$blue_team, "being in blue team")
tests2(data$tier, "tier")