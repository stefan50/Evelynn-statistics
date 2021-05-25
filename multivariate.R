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