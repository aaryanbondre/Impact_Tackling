install.packages("ggplot2")
install.packages("gganimate")
install.packages("gapminder")
install.packages("gifski")
install.packages("transformr")
install.packages("lubridate")
install.packages("proxy")
library(dplyr)
library(ggplot2)
library(gganimate)
library(gapminder)
library(gifski)
library(transformr)
library(lubridate)
library(proxy)

games <- #data
players <- #data
plays <- #data
tackles <- #data
week1 <- #data
week2 <- #data
week3 <- #data
week4 <- #data
week5 <- #data
week6 <- #data
week7 <- #data
week8 <- #data
week9 <- #data

options(jupyter.rich_display = F)

## Data Filtering

playerTackles <- playerTackles[,-14]
playerTackles <- playerTackles[,-11]
playerTackles <- playerTackles[,-10]
playerTackles <- playerTackles[,-9]
playerTackles <- playerTackles[,-8]

soloTackles <- subset(playerTackles, assist == 0)
soloTackles <- soloTackles[,-6]
soloTackles <- soloTackles[,-5]


week1data <- week1
week1data <- week1data %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))
week2data <- week2 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))
week3data <- week3 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))

week4data <- week4 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))
week5data <- week5 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))

week6data <- week6 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))
week7data <- week7 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))

week8data <- week8 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))

week9data <- week9 %>%
  group_by(gameId, playId, displayName) %>%
  reframe(
    startX = first(x),
    startY = first(y),
    endX = last(x),
    endY = last(y),
    speedAtTackle = unique(s[event == "tackle"]),
    accelerationAtTackle = unique(a[event == "tackle"]),
    angleAtTackle = unique(dir[event == "tackle"]),
    distance_traveled = sum(dis)
  ) %>% 
  filter(!is.na(speedAtTackle))

weekdata <- rbind(week1data, week2data, week3data, week4data, week5data, week6data, week7data, week8data, week9data)
 
playsFinal <- soloTackles
playsFinal <- merge(playsFinal, plays, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"), all.x = TRUE, suffixes = c("", "_tackles"))
playsFinal <- subset(playsFinal, playNullifiedByPenalty == "N")
playsFinal <- subset(playsFinal, select = c("gameId", "playId", "nflId", "tackle", "pff_missedTackle", "position", "displayName", "ballCarrierId", "ballCarrierDisplayName", "quarter", "down", "yardsToGo", "gameClock", "playResult"))

playsFinal <- playsFinal %>%
  mutate(gameClock = as.integer(lubridate::hms(paste0("00:", gameClock))))

playsFinalAll <- playerTackles
playsFinalAll <- merge(playsFinalAll, plays, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"), all.x = TRUE, suffixes = c("", "_tackles"))
playsFinalAll <- subset(playsFinalAll, playNullifiedByPenalty == "N")
playsFinalAll  <- subset(playsFinalAll, select = c("gameId", "playId", "nflId", "tackle", "pff_missedTackle", "position", "displayName", "ballCarrierId", "ballCarrierDisplayName", "quarter", "down", "yardsToGo", "gameClock", "playResult"))
playsFinalAll <- subset(playsFinalAll, quarter != 4 & !(quarter == 2 & gameClock <= 120))

playsFinal <- subset(playsFinal, quarter == 4 | (quarter == 2 & gameClock <= 120))
colnames(playsFinal)[c(7, 9)] <- c("tacklerDisplayName", "ballCarrierDisplayName")
colnames(playsFinalAll)[c(7, 9)] <- c("tacklerDisplayName", "ballCarrierDisplayName")

playsFinal <- playsFinal %>%
  left_join(weekdata %>% select(gameId, playId, displayName, startX, endX, startY, endY, speedAtTackle, accelerationAtTackle, distance_traveled, angleAtTackle), 
            by = c("gameId" = "gameId", "playId" = "playId", "ballCarrierDisplayName" = "displayName")) %>%
  #mutate(ballCarrierStartX = startX, ballCarrierStartY = startY, ballCarrierEndX = endX, ballCarrierEndY = endY) %>% 
  mutate(ballCarrierSpeedAtTackle = speedAtTackle, ballCarrierAccAtTackle = accelerationAtTackle, ballCarrierDistanceTrav = distance_traveled, ballCarrierAngle = angleAtTackle) %>% 
  select(-startX, -startY, -endX, -endY, -speedAtTackle, -accelerationAtTackle, -distance_traveled, -angleAtTackle) %>% 
  filter(!is.na(ballCarrierSpeedAtTackle)) %>% #automatically filters out any play that doesn't end in a tackle
  left_join(weekdata %>% select(gameId, playId, displayName, startX, endX, startY, endY, speedAtTackle, accelerationAtTackle, distance_traveled, angleAtTackle), 
            by = c("gameId" = "gameId", "playId" = "playId", "tacklerDisplayName" = "displayName")) %>%
  #mutate(tacklerStartX = startX, tacklerStartY = startY, tacklerEndX = endX, tacklerEndY = endY) %>% 
  mutate(tacklerSpeedAtTackle = speedAtTackle, tacklerAccAtTackle = accelerationAtTackle, tacklerDistanceTrav = distance_traveled, tacklerAngle = angleAtTackle) %>% 
  select(-startX, -startY, -endX, -endY, -speedAtTackle, -accelerationAtTackle, -distance_traveled, -angleAtTackle)

playsFinalAll <- playsFinalAll %>%
  left_join(weekdata %>% select(gameId, playId, displayName, startX, endX, startY, endY, speedAtTackle, accelerationAtTackle, distance_traveled, angleAtTackle), 
            by = c("gameId" = "gameId", "playId" = "playId", "ballCarrierDisplayName" = "displayName")) %>%
  mutate(ballCarrierSpeedAtTackle = speedAtTackle, ballCarrierAccAtTackle = accelerationAtTackle, ballCarrierDistanceTrav = distance_traveled, ballCarrierAngle = angleAtTackle) %>% 
  select(-startX, -startY, -endX, -endY, -speedAtTackle, -accelerationAtTackle, -distance_traveled, -angleAtTackle) %>% 
  filter(!is.na(ballCarrierSpeedAtTackle)) %>% #automatically filters out any play that doesn't end in a tackle
  left_join(weekdata %>% select(gameId, playId, displayName, startX, endX, startY, endY, speedAtTackle, accelerationAtTackle, distance_traveled, angleAtTackle), 
            by = c("gameId" = "gameId", "playId" = "playId", "tacklerDisplayName" = "displayName")) %>%
  mutate(tacklerSpeedAtTackle = speedAtTackle, tacklerAccAtTackle = accelerationAtTackle, tacklerDistanceTrav = distance_traveled, tacklerAngle = angleAtTackle) %>% 
  select(-startX, -startY, -endX, -endY, -speedAtTackle, -accelerationAtTackle, -distance_traveled, -angleAtTackle)


#BALLCARRIERforce = BALLCARRIER MASS (found in players.csv) * ballcarrier acceleration at Tackle * angle
#Tacklerforce = tackler MASS (found in players.csv) * tackler acceleration at Tackle

#take net force wasted (abs(ballcarrier force - tackler force)) and multiply by net speed at the tackle (abs(ball carrier speed - tackler speed))
#subtract from 100

#multiply score by 0.01*(110-yards gained))
#multiply score by net distance traveled


force <- function(id, acceleration, angle) {
  length_players_nflId <- length(players$nflId)
  length_id <- length(id)
  weight <- players$weight[players$nflId %in% id]
  radians <- angle * (pi / 180)
  force_value <- abs(weight * acceleration * cos(radians))^(1/2)
  return(force_value)
}

## Score Calculations

playsFinal <- playsFinal %>% 
  mutate(tacklerImpactScore = (tackle*(0.01*(110-playResult)) * (0-(1.5*(force(ballCarrierId, ballCarrierSpeedAtTackle, ballCarrierAngle) - force(nflId, tacklerSpeedAtTackle, tacklerAngle))))))

playsFinalAll <- playsFinalAll %>% 
  mutate(tacklerImpactScore = (tackle*(0.01*(110-playResult)) * (0-(1.5*(force(ballCarrierId, ballCarrierSpeedAtTackle, ballCarrierAngle) - force(nflId, tacklerSpeedAtTackle, tacklerAngle))))))


final_impact_scores <- playsFinal %>% 
  group_by(nflId, tacklerDisplayName) %>%
  summarize(position = players$position[players$nflId == nflId], overallTacklerImpactScore = mean(tacklerImpactScore))

final_scores <- playsFinalAll %>% 
  group_by(nflId, tacklerDisplayName) %>%
  summarize(position = players$position[players$nflId == nflId], overallTacklerScore = mean(tacklerImpactScore))

pro_bowlers <- data.frame(
  tacklerDisplayName = c("Myles Garrett", "Maxx Crosby", "Quinnen Williams", "Chris Jones", "T.J. Watt",
                         "Roquan Smith", "Ahmad Gardner", "Patrick Surtain", "Xavien Howard", "Minkah Fitzpatrick", "Derwin James",
                         "C.J. Mosley", "Matt Milano", "Bradley Chubb", "Trey Hendrickson", "Trey Hendrickson", 
                         "Marlon Humphrey", "Jordan Poyer", "Nick Bosa", "Brian Burns", "DeMarcus Lawrence", 
                         "Cameron Jordan", "Aaron Donald", "Jonathan Allen", "Dexter Lawrence", 
                         "Daron Payne", "Micah Parsons", "Za'Darius Smith", "Haason Reddick", "Danielle Hunter", 
                         "Fred Warner", "Demario Davis", "Darius Slay", "Trevon Diggs", "Tariq Woolen", 
                         "Jaire Alexander", "Jalen Ramsey", "Quandre Diggs", "Budda Baker", "Talanoa Hufanga")
)
tackling_leaders <- data.frame(
  tacklerDisplayName = c("Foyesade Oluokun", "Nick Bolton", "Roquan Smith", "Alex Singleton", "Zaire Franklin")
)

id_counts <- playsFinal %>%
  group_by(nflId) %>%
  summarize(tackles = n())
final_impact_scores <- left_join(final_impact_scores, id_counts, by = "nflId")


id_counts_all <- playsFinalAll %>%
  group_by(nflId) %>%
  summarize(tackles = n())
final_scores <- left_join(final_scores, id_counts_all, by = "nflId")


## Position Scores

final_pro_bowler_scores <- final_impact_scores %>% 
  filter(tacklerDisplayName %in% pro_bowlers$tacklerDisplayName)

dt_scores <- final_impact_scores %>% 
  filter(position == "DT" | position == "NT") %>% 
  arrange(-overallTacklerImpactScore)
edge_scores <- final_impact_scores %>% 
  filter(position == "DE") %>% 
  arrange(-overallTacklerImpactScore)
lb_scores <- final_impact_scores %>% 
  filter(position == "OLB" | position == "ILB" | position == "MLB") %>% 
  arrange(-overallTacklerImpactScore)
db_scores <- final_impact_scores %>% 
  filter(position == "CB" | position == "SS" | position == "FS") %>% 
  arrange(-overallTacklerImpactScore)


## Net Difference DF

player_scores <- merge(final_impact_scores, final_scores, by=c("nflId", "tacklerDisplayName", "position"), all = TRUE)
player_scores[is.na(player_scores)] <- 0
player_scores <- mutate(player_scores, net_diff = overallTacklerImpactScore - overallTacklerScore, totalTackles = tackles.x + tackles.y, impact_tackles = tackles.x)
player_scores <- select(player_scores, nflId, tacklerDisplayName, position, overallTacklerScore, overallTacklerImpactScore, net_diff, totalTackles, impact_tackles)
player_scores <- player_scores %>%
  filter(totalTackles >= 20)

## Net Difference Plot

plot_scores <- ggplot(player_scores, aes(x = totalTackles, y = net_diff)) +
  geom_point(alpha = 0.7, color = "grey")
plot_scores + #geom_point(data = player_scores[player_scores$tacklerDisplayName %in% tackling_leaders$tacklerDisplayName, ], color = "blue", size = 2) +
  #geom_text(data = player_scores[player_scores$tacklerDisplayName %in% tackling_leaders$tacklerDisplayName, ], aes(label = tacklerDisplayName), vjust = -1, size = 2) +
  #geom_point(data = player_scores[player_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "black", size = 2) + 
  #geom_text(data = player_scores[player_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], aes(label = tacklerDisplayName), vjust = -1, size = 2) +
  geom_point(data = player_scores[player_scores$tacklerDisplayName %in% dt_scores$tacklerDisplayName, ], color = "blue", size = 2) +
  geom_hline(yintercept = 0, linetype = "dotted", color = "black")

player_scores <- player_scores[order(player_scores$net_diff, decreasing = TRUE), ]
player_scores <- player_scores[1:20, ]
par(mar = c(7, 5, 2, 2) + 0.1)
barplot(player_scores$net_diff, names.arg=player_scores$tacklerDisplayName, ylab = "Net Difference", col = "skyblue", cex.names = 0.7, las = 2)

final_impact_scores <- filter(final_impact_scores, tackles > 9)
final_scores <- filter(final_scores, tackles > 9)

impact_column_mean <- mean(final_impact_scores$overallTacklerImpactScore, na.rm = TRUE)
column_mean <- mean(final_scores$overallTacklerScore, na.rm = TRUE)

## Impact Scores Plot

plot <- ggplot(final_impact_scores, aes(x = tackles, y = overallTacklerImpactScore)) +
  geom_point(alpha = 0.7, color = "grey")
plot + #geom_point(data = db_scores[db_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "red", size = 2) + 
  #geom_point(data = dt_scores[dt_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "green", size = 2) + 
  #geom_point(data = edge_scores[edge_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "orange", size = 2) +
  #geom_point(data = lb_scores[lb_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "blue", size = 2) +
  #geom_text(data = lb_scores[lb_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], aes(label = tacklerDisplayName), vjust = -0.5, size = 2) +
  geom_point(data = final_impact_scores[final_impact_scores$tacklerDisplayName %in% tackling_leaders$tacklerDisplayName, ], color = "blue", size = 2) +
  geom_text(data = final_impact_scores[final_impact_scores$tacklerDisplayName %in% tackling_leaders$tacklerDisplayName, ], aes(label = tacklerDisplayName), vjust = -1, size = 2) +
  geom_hline(yintercept = impact_column_mean, linetype = "dashed", color = "black")

## Normal Scores Plot

final_scores <- filter(final_scores, tackles > 9)
plot_2 <- ggplot(final_scores, aes(x = tackles, y = overallTacklerScore)) +
  geom_point(alpha = 0.7, color = "grey")
plot_2 + #geom_point(data = db_scores[db_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "red", size = 2) + 
  #geom_point(data = dt_scores[dt_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "green", size = 2) + 
  #geom_point(data = edge_scores[edge_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "orange", size = 2) +
  #geom_point(data = lb_scores[lb_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], color = "blue", size = 2) +
  #geom_text(data = lb_scores[lb_scores$tacklerDisplayName %in% pro_bowlers$tacklerDisplayName, ], aes(label = tacklerDisplayName), vjust = -0.5, size = 2) +
  geom_point(data = final_scores[final_scores$tacklerDisplayName %in% tackling_leaders$tacklerDisplayName, ], color = "blue", size = 2) +
  geom_text(data = final_scores[final_scores$tacklerDisplayName %in% tackling_leaders$tacklerDisplayName, ], aes(label = tacklerDisplayName), vjust = -0.5, size = 2) +
  geom_hline(yintercept = column_mean, linetype = "dashed", color = "black")







