install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
library(readr)
library(tidyr)
library(dplyr)

data <- read_csv("C:/Data_Analytics/NFL_2025_Big_Data/nfl-big-data-bowl-2025/games.csv")

head(data)
data
summary(data)
head(data[c("homeTeamAbbr", "visitorTeamAbbr", "homeFinalScore", "visitorFinalScore")])



# Assuming your dataset is called 'data'
new_dataset <- data %>%
  group_by(homeTeamAbbr) %>%  # Group by homeTeamAbbr
  summarise(HomeFinalscr = sum(homeFinalScore, na.rm = TRUE))  # Sum homeFinalScore

# View the result
print(new_dataset)

Homescore <- new_dataset

Awayscore <- data %>%
  group_by(visitorTeamAbbr) %>%
  summarise(visitorFinalscr = sum(visitorFinalScore, na.rm = TRUE))

print(Awayscore)
print(Homescore)






# Combine the datasets
Teamscores <- Homescore %>%
  mutate(Awaypoints = Awayscore$visitorFinalscr)

head(Teamscores)

Teamscores_sorted <- Teamscores %>%
  mutate(TotalPoints = HomeFinalscr + Awaypoints)

head(Teamscores_sorted)


Teamscores_sorted <- Teamscores_sorted %>%
  rename(
    team = homeTeamAbbr,
    homepoints = HomeFinalscr,
    awaypoints = Awaypoints,
    totalpoints = TotalPoints
  )

head(Teamscores_sorted)

season_summary <- Teamscores_sorted %>%
  mutate(diff = homepoints - awaypoints)

head(season_summary)

stadium <- read_csv("C:/Data_Analytics/NFL_2025_Big_Data/nfl-big-data-bowl-2025/stadium_style.csv")

head(stadium)


install.packages("readxl")
install.packages("writexl")  # Optional, for writing back to Excel

library(readxl)
library(writexl)

stadium <- read_excel("C:/Data_Analytics/NFL_2025_Big_Data/nfl-big-data-bowl-2025/stadium_style.xlsx")
head(stadium)
write.csv(stadium, "C:/Data_Analytics/NFL_2025_Big_Data/nfl-big-data-bowl-2025/stadium_style.csv", row.names = FALSE)
stadium <- stadium %>%
  mutate(covered_stadium = covered_stadium == "TRUE")

head(stadium)
head(season_summary)


season2022 <- season_summary %>%
  left_join(stadium, by = "team")


head(season2022)



season2022 <- season2022 %>%
  mutate(covered_stadium_numeric = as.numeric(covered_stadium))

head(season2022)



cor_test_result <- cor.test(season2022$diff, season2022$covered_stadium_numeric)
print(cor_test_result)

stadium_lin_model <- lm(diff ~ covered_stadium_numeric, data = season2022)
summary(stadium_lin_model)

library(ggplot2)

ggplot(season2022, aes(x = covered_stadium_numeric, y = diff)) +
  geom_point(aes(color = as.factor(covered_stadium_numeric)), size = 3) +  # Scatter plot
  geom_smooth(method = "lm", se = FALSE, color = "blue") +  # Add regression line
  labs(
    title = "Diff vs Covered Stadium",
    x = "Covered Stadium (Numeric)",
    y = "Difference (Diff)",
    color = "Covered Stadium"
  ) +
  theme_minimal()


allgames <- data %>%
  rename(
    hometeam = homeTeamAbbr,
    awayteam = visitorTeamAbbr,
    hometeam_score = homeFinalScore,
    awayteam_score = visitorFinalScore
  )

stadium1 <- stadium %>%
  rename(hometeam = team)

allgames_homestadium <- allgames %>%
  left_join(stadium1, by = "hometeam")


stadium2 <- stadium %>%
  rename(awayteam = team)

allgames_2022 <- allgames_homestadium %>%
  left_join(stadium2, by = "awayteam")


tail(allgames_2022)

summary(allgames_2022)

allgames_2022 <- allgames_2022 %>%
  rename(hometeam_cover = covered_stadium.x,
         awayteam_cover = covered_stadium.y)

summary(allgames_2022)

season2022 <- season2022 %>%
  mutate(Home_stadium_covered = covered_stadium)

season2022_clean <- allgames_2022 %>%
  select(-"gameId", -"season", -"week", -"gameDate", -"gameTimeEastern")


season2022_clean

awayteams <- season2022_clean %>%
  select(-"hometeam")

print(awayteams)

awayteams <- awayteams %>%
  rename(teamname = awayteam,
         points_received = hometeam_score,
         points_made = awayteam_score,
         game_cover = hometeam_cover,
         own_stadium_cover = awayteam_cover)
print(awayteams)

awayteams <- awayteams %>%
  select(teamname, points_made, points_received, game_cover, own_stadium_cover)

print(awayteams)

hometeams <- season2022_clean %>%
  select(-"awayteam", -"awayteam_cover")

print(hometeams)
hometeams <- hometeams %>%
  rename(teamname = hometeam,
         points_made = hometeam_score,
         points_received = awayteam_score,
         game_cover = hometeam_cover)

print(hometeams)

hometeams <- hometeams %>%
  mutate(own_stadium_cover = game_cover)

print(hometeams)
print(awayteams)

allteams_dataset <- bind_rows(hometeams, awayteams)

summary(allteams_dataset)
print(allteams_dataset)

library(dplyr)

# Dataset with rows where own_stadium_cover is TRUE
teams_with_covered_stadiums <- allteams_dataset %>%
  filter(own_stadium_cover == TRUE)

# Dataset with rows where own_stadium_cover is FALSE
teams_without_covered_stadiums <- allteams_dataset %>%
  filter(own_stadium_cover == FALSE)

teams_without_covered_stadiums <- teams_without_covered_stadiums %>%
  mutate(diff = points_made - points_received)

print(teams_without_covered_stadiums)

teams_with_covered_stadiums <- teams_with_covered_stadiums %>%
  mutate(diff = points_made - points_received)

print(teams_with_covered_stadiums, n = 76)


summary_with_cover <- teams_with_covered_stadiums %>%
  group_by(teamname, game_cover) %>%
  summarize(total_diff = sum(diff), .groups = "drop")

print(summary_with_cover)

summary_without_cover <- teams_without_covered_stadiums %>%
  group_by(teamname, game_cover) %>%
  summarize(total_diff = sum(diff), .groups = "drop")

print(summary_without_cover)




#scatterplot for teams with covered home stadiums

# Plot with regression line (note: game_cover as numeric internally for lm)
ggplot(summary_with_cover, aes(x = as.numeric(game_cover), y = total_diff)) +
  geom_point(size = 3, aes(color = as.factor(game_cover))) +  # Points with color by group
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # Add regression line
  labs(
    title = "Scatterplot of Total Diff by Game Cover",
    x = "Game Cover (1 = TRUE, 0 = FALSE)",
    y = "Total Diff",
    color = "Game Cover"
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("FALSE", "TRUE")) + # Custom x-axis labels
  theme_minimal()


# Plot with regression line (note: game_cover as numeric internally for lm)
ggplot(summary_without_cover, aes(x = as.numeric(game_cover), y = total_diff)) +
  geom_point(size = 3, aes(color = as.factor(game_cover))) +  # Points with color by group
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # Add regression line
  labs(
    title = "Scatterplot of Total Diff by Game Cover",
    x = "Game Cover (1 = TRUE, 0 = FALSE)",
    y = "Total Diff",
    color = "Game Cover"
  ) +
  scale_x_continuous(breaks = c(0, 1), labels = c("FALSE", "TRUE")) + # Custom x-axis labels
  theme_minimal()

# Add a new column to distinguish subsets
summary_with_cover$subset <- "Teams with covered home stadiums"
summary_without_cover$subset <- "Teams without covered home stadiums"

# Combine the two datasets
combined_summary <- rbind(summary_with_cover, summary_without_cover)

# Plot with facets for comparison
ggplot(combined_summary, aes(x = as.numeric(game_cover), y = total_diff)) +
  geom_point(size = 3, aes(color = as.factor(game_cover))) +  # Points with color by group
  geom_smooth(method = "lm", se = TRUE, color = "black") +   # Add regression line
  labs(
    title = "Do NFL teams significantly play worse in a stadium not built as their home stadium?",
    subtitle = "Analysis of Total Diff by Game Cover",
    x = "Game Type",
    y = "Total Point Difference",
    color = "Game Type",
    caption = "Data Source: NFL-Big-Data-Bowl 2025
    Visuals: Jarkko Schaad"  # Optional for attribution
  ) +
  scale_x_continuous(
    breaks = c(0, 1),
    labels = c("Not covered games", "Covered games")  # Custom x-axis labels
  ) +
  scale_color_manual(
    values = c("red", "blue"),  # Adjust point colors for clarity
    labels = c("Not covered games", "Covered games")  # Match x-axis labels
  ) +
  theme_minimal(base_size = 14) +  # Increase base font size
  facet_wrap(~subset)  # Create separate panels for each subsets


# Fit a linear regression model
model_with <- lm(diff ~ game_cover, data = teams_with_covered_stadiums)

# Display the model summary
summary(model_with)

model_without <- lm(diff ~ game_cover, data = teams_without_covered_stadiums)
summary(model_without)


# Summarize diff by teams with covered home stadiums and game_cover
summarized_data_with <- teams_with_covered_stadiums %>%
  group_by(teamname, game_cover) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE), .groups = "drop")

# Print the new dataset
print(summarized_data_with)

# Create a bar plot with teamname on x-axis, total_diff on y-axis, and colored by game_cover
ggplot(summarized_data_with, aes(x = teamname, y = total_diff, fill = as.factor(game_cover))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Create side-by-side bars
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +  # Red for FALSE, blue for TRUE
  labs(
    title = "Points difference by teams with covered home stadiums",
    x = "Team",
    y = "Total Diff Points made vs. Points received",
    fill = "In covered stadiums",
    caption = "Data: NFL-Big-Data-Bowl-2025\nVisuals: Jarkko Schaad"  # Adding the caption
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    plot.caption = element_text(hjust = 1, vjust = 1)  # Position the caption at the bottom right
  )

# Summarize diff by teams without covered homestadium and game_cover
summarized_data_without <- teams_without_covered_stadiums %>%
  group_by(teamname, game_cover) %>%
  summarise(total_diff = sum(diff, na.rm = TRUE), .groups = "drop")

# Print the new dataset
print(summarized_data_without)

# Create a bar plot with teamname on x-axis, total_diff on y-axis, and colored by game_cover
ggplot(summarized_data_without, aes(x = teamname, y = total_diff, fill = as.factor(game_cover))) +
  geom_bar(stat = "identity", position = "dodge", width = 0.7) +  # Create side-by-side bars
  scale_fill_manual(values = c("FALSE" = "red", "TRUE" = "blue")) +  # Red for FALSE, blue for TRUE
  labs(
    title = "Points difference by teams without covered home stadiums",
    x = "Team",
    y = "Total Diff Points made vs. Points received",
    fill = "In covered stadiums",
    caption = "Data: NFL-Big-Data-Bowl-2025\nVisuals: Jarkko Schaad"  # Adding the caption
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels for better visibility
    panel.grid.major = element_blank(),  # Remove major grid lines
    panel.grid.minor = element_blank(),   # Remove minor grid lines
    plot.caption = element_text(hjust = 1, vjust = 1)  # Position the caption at the bottom right
  )

