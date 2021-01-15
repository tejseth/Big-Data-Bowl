library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)
library(grid)
library(gridExtra)
library(rpart)
library(rpart.plot)
library(caret) 
library(e1071)
library(randomForest)
library(gt)
library(ggridges)

options(warn=-1)
options(repr.plot.width=15, repr.plot.height = 10)

#Reading in all the data
df_games <- read.csv("~/Desktop/Football/Big Data Bowl/nfl-big-data-bowl-2021/games.csv")
df_players <- read.csv("~/Desktop/Football/Big Data Bowl/nfl-big-data-bowl-2021/players.csv")
df_plays <- read.csv("~/Desktop/Football/Big Data Bowl/nfl-big-data-bowl-2021/plays.csv")

weeks <- seq(1, 17)
df_tracking <- data.frame()
for(w in weeks){
  df_tracking_temp <- read_csv(paste0("~/Desktop/Football/Big Data Bowl/nfl-big-data-bowl-2021/week",w,".csv"),
                               col_types = cols())
  df_tracking <- bind_rows(df_tracking_temp, df_tracking)}


#Standardizing tracking data so its always in direction of offense vs raw on-field coordinates.
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

###############################################################################
df_tracking <- df_tracking %>%
  mutate(x = ifelse(playDirection == "left", 120-x, x),
         y = ifelse(playDirection == "left", 160/3 - y, y))

df_merged <- inner_join(df_games,
                        df_plays,
                        by = c("gameId" = "gameId"))

df_merged <- inner_join(df_merged,
                        df_tracking,
                        by = c("gameId" = "gameId",
                               "playId" = "playId"))

passArivalEvents <- c('pass_outcome_caught',
                      'pass_arrived',
                      'pass_outcome_incomplete',
                      'pass_outcome_interception',
                      'pass_outcome_touchdown')


df_distanceToFootball <- df_merged %>%
  
  #determining side of ball
  mutate(sideOfBall = ifelse(#if tracked player is home and home has ball
    ((team == "home") &
       (possessionTeam == homeTeamAbbr)) |
      
      #if tracked player is away and away has ball
      ((team == "away") &
         (possessionTeam == visitorTeamAbbr)),
    
    
    #if either condition is true, offense
    "offense",
    
    #if neither condition is true, defense
    "defense"),
    
    #defining defensive team
    defensiveTeam = ifelse(possessionTeam == homeTeamAbbr,
                           visitorTeamAbbr,
                           homeTeamAbbr)) %>%
  
  
  #using DET on defense only
  filter(defensiveTeam == "DET", sideOfBall == "defense") %>%
  
  #grouping by game, play and frame
  group_by(gameId, playId, frameId) %>%
  
  #checking if football reading is in frame
  mutate(footballInPlay = sum(displayName == "Football") > 0) %>%
  
  #using only frames with football marked; some plays its missing
  filter(footballInPlay) %>%
  
  #adding x and y location of football as columns
  mutate(xFootball = x[displayName == "Football"],
         yFootball = y[displayName == "Football"]) %>%
  
  #ungrouping
  ungroup() %>%
  
  #grouping by game and play
  group_by(gameId, playId) %>%
  
  #selecting frames that contain pass arrival events
  filter(event %in% passArivalEvents) %>%
  
  #selecting first frame with in case there are multiple
  #filter(frameId == min(frameId)) %>%
  
  #calculating distance to football
  mutate(
    
    distToFootballAtBallArrival = sqrt((x - xFootball) ^ 2 +
                                         (y - yFootball) ^ 2)
    
  )

#Getting the line of scrimmage and distance
line_of_scrim <- df_tracking %>%
  filter(displayName == "Football", frameId == 1) %>%
  select(gameId, playId, x)
names(line_of_scrim)[names(line_of_scrim) == "x"] <- "los"
df_tracking <- df_tracking %>%
  left_join(line_of_scrim, by = c("gameId", "playId"))

df_tracking <- df_tracking %>%
  mutate(dist_from_los = x - los,
         dist_from_mid = y - 26.65)

line_of_scrim_1 <- week1 %>%
  filter(displayName == "Football", frameId == 1) %>%
  select(gameId, playId, x)
names(line_of_scrim_1)[names(line_of_scrim_1) == "x"] <- "los"
week1 <- week1 %>%
  left_join(line_of_scrim, by = c("gameId", "playId"))

week1 <- week1 %>%
  mutate(dist_from_los = x - los,
         dist_from_mid = y - 26.65)

#Getting the personnel groupings
df_plays <- df_plays %>%
  mutate(num_dl = substring(df_plays$personnelD, 1, 1),
         num_lb = substring(df_plays$personnelD, 7, 7),
         num_cb = substring(df_plays$personnelD, 13, 13))

df_plays$num_dl <- as.numeric(df_plays$num_dl)
df_plays$num_lb <- as.numeric(df_plays$num_lb)
df_plays$num_cb <- as.numeric(df_plays$num_cb)

df_plays_imp_stuff <- df_plays %>%
  select(gameId, playId, down, yardsToGo, defendersInTheBox, numberOfPassRushers, 
         num_dl, num_lb, num_cb, epa)

################################################################################

df_coverages <- read.csv("~/Desktop/Football/Big Data Bowl/nfl-big-data-bowl-2021/coverages_week1.csv")
week1 <- read.csv("~/Desktop/Football/Big Data Bowl/nfl-big-data-bowl-2021/week1.csv")

week1 <- week1 %>%
  left_join(df_coverages, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"))

week1 <- week1 %>%
  left_join(df_plays_imp_stuff, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"))

playStartEvent <- 'ball_snap'
passArivalEvents <- c('pass_arrived')

week1_throw_events <- week1 %>%
  filter(event %in% passArivalEvents)

week1_football <- week1_throw_events %>%
  filter(team == "football") %>%
  mutate(ball_x = x, ball_y = y) %>%
  select(gameId, playId, frameId, ball_x, ball_y)

week1_throw_events <- week1_throw_events %>%
  left_join(week1_football, by = c("gameId", "playId", "frameId"))

week1_throw_events <- week1_throw_events %>% 
  mutate(dist_from_ball = sqrt((x-ball_x)^2 + (y-ball_y)^2))
###############################################################################
week1_throw_events_d <- week1_throw_events %>%
  filter(position == "SS" | position == "FS" | position == "CB" | position == "LB" |
           position == "OLB" | position == "ILB" | position == "DB")

wide_week1_d <- week1_throw_events_d %>%
  pivot_wider(names_from = position, values_from = c(s, a, dis, 
                                                     o, dir, dist_from_los, dist_from_mid, dist_from_ball), values_fill = 0)

wide_week1_d$coverage <- as.factor(wide_week1_d$coverage)

#Data Partition
smp_size <- floor(0.75 * nrow(wide_week1_d))

set.seed(123)
week1_ind_d <- sample(seq_len(nrow(wide_week1_d)), size = smp_size)

week1_train_d <- wide_week1_d[week1_ind_d, ]
week1_test_d <- wide_week1_d[-week1_ind_d, ]

#Random Forest
set.seed(222)
rf_d <- randomForest(coverage ~ los + down + yardsToGo + defendersInTheBox + numberOfPassRushers + num_dl + num_lb + num_cb + epa + ball_x + ball_y + s_SS + s_FS + s_CB + s_LB + s_OLB + s_ILB + s_DB + a_SS + a_FS + a_CB + a_LB + a_OLB + a_ILB + a_DB + dis_SS + dis_FS + dis_CB + dis_LB + dis_OLB + dis_ILB + dis_DB + o_SS + o_FS + o_CB + o_LB + o_OLB + o_ILB + o_DB + dir_SS + dir_FS + dir_CB + dir_LB + dir_OLB + dir_ILB + dir_DB + dist_from_los_SS + dist_from_los_FS + dist_from_los_CB + dist_from_los_LB + dist_from_los_OLB + dist_from_los_ILB + dist_from_los_DB + dist_from_mid_SS + dist_from_mid_FS + dist_from_mid_CB + dist_from_mid_LB + dist_from_mid_OLB + dist_from_mid_ILB + dist_from_mid_DB + dist_from_ball_SS + dist_from_ball_FS + dist_from_ball_CB + dist_from_ball_LB + dist_from_ball_OLB + dist_from_ball_ILB + dist_from_ball_DB, 
                     data = week1_train_d, na.action = na.omit)

rf_d

#Prediction & Confusion Matrix - train data
p1_d <- predict(rf_d, week1_train_d)
confusionMatrix(p1_d, week1_train_d$coverage)

#Prediction & Confusion Matrix - test data
p2 <- predict(rf_d, week1_test_d)
confusionMatrix(p2, week1_test_d$coverage)

#Error rate of Random Forest
plot(rf)

#Number of nodes for the tree
hist(treesize(rf), main = "Number of Nodes For the Trees", col = "dark green")

#Variable Importance
varImpPlot(rf_d,
           sort = T,
           main = "Variable Importance")

varUsed(rf)

#Extract a single tree
tree <- getTree(rf, 1, labelVar = TRUE)
tree

line_of_scrim <- df_tracking %>%
  filter(displayName == "Football", frameId == 1) %>%
  select(gameId, playId, x)
names(line_of_scrim)[names(line_of_scrim) == "x"] <- "los"
df_tracking <- df_tracking %>%
  left_join(line_of_scrim, by = c("gameId", "playId"))

df_tracking <- df_tracking %>%
  mutate(dist_from_los = x - los,
         dist_from_mid = y - 26.65)

df_plays <- df_plays %>%
  mutate(num_dl = substring(df_plays$personnelD, 1, 1),
         num_lb = substring(df_plays$personnelD, 7, 7),
         num_cb = substring(df_plays$personnelD, 13, 13))

df_plays$num_dl <- as.numeric(df_plays$num_dl)
df_plays$num_lb <- as.numeric(df_plays$num_lb)
df_plays$num_cb <- as.numeric(df_plays$num_cb)

df_plays_imp_stuff <- df_plays %>%
  select(gameId, playId, down, yardsToGo, defendersInTheBox, numberOfPassRushers, 
         num_dl, num_lb, num_cb, epa)

df_tracking <- df_tracking %>%
  left_join(df_plays_imp_stuff, by.x = c("gameId", "playId"), by.y = c("gameId", "playId"))

playStartEvent <- 'ball_snap'
passArivalEvents <- c('pass_arrived')

df_throw_events <- df_tracking %>%
  filter(event %in% passArivalEvents)

df_football <- df_throw_events %>%
  filter(team == "football") %>%
  mutate(ball_x = x, ball_y = y) %>%
  select(gameId, playId, frameId, ball_x, ball_y)

df_throw_events <- df_throw_events %>%
  left_join(df_football, by = c("gameId", "playId", "frameId"))

df_throw_events <- df_throw_events %>% 
  mutate(dist_from_ball = sqrt((x-ball_x)^2 + (y-ball_y)^2))

df_throw_events_d <- df_throw_events %>%
  filter(position == "SS" | position == "FS" | position == "CB" | position == "LB" |
           position == "OLB" | position == "ILB" | position == "DB")

df_throw_events_d <- df_throw_events_d %>%
  filter(!is.na(ball_x)) %>%
  filter(!is.na(ball_y))

df_throw_events_d <- df_throw_events_d %>%
  select(c(-los.x, los.y))

wide_df_d <- df_throw_events_d %>%
  group_by(gameId, playId, frameId) %>%
  mutate(row = row_number()) %>%
  tidyr::pivot_wider(names_from = position, 
                     values_from = c(s, a, dis, o, dir, dist_from_los, dist_from_mid, dist_from_ball), 
                     values_fill = 0) %>%
  select(-row)

p3 <- predict(rf_d, wide_df_d)
p3 <- as_tibble(p3)
p3_df <- data.frame(matrix(unlist(p3), nrow=length(p3), byrow=T))

wide_df_d <- wide_df_d %>%
  ungroup() %>%
  mutate(row_num = row_number())
p3 <- p3 %>%
  mutate(row_num = row_number())

merged_coverages <- merge(wide_df_d, p3, by = "row_num")

names(merged_coverages)[names(merged_coverages) == "value"] <- "coverage"

merged_coverages2 <- merged_coverages %>%
  select(displayName, frameId, gameId, playId, epa, los, coverage)

merged_coverages3 <- merged_coverages2 %>%
  group_by(gameId, playId, frameId) %>%
  summarize(most_freq_cov = tail(names(sort(table(coverage))), 1))

df_plays_cov <- merge(df_plays, merged_coverages3, by = c("gameId", "playId"))

cov_stats <- df_plays_cov %>%
  group_by(most_freq_cov) %>%
  summarize(plays = n(),
            def_epa = mean(epa, na.rm = T)) %>%
  arrange(desc(plays))

cov_stats <- cov_stats %>%
  mutate(freq = plays / 14575)

#Creating a table
cov_table <- cov_stats %>% 
  gt() %>%
  tab_header(
    title = "2018 Coverage Statistics", 
    subtitle = "Coverages were assigned based on random forest classification" 
  ) %>%
  cols_label(most_freq_cov = "Coverage", plays = "Number of Plays", def_epa = "EPA/Play Allowed", freq = "Frequency") %>%
  data_color( 
    columns = vars(def_epa), 
    colors = scales::col_numeric(
      palette = c(
        "green", "yellow", "red"), 
      domain = c(0.1,0.8))) %>%
  data_color( 
    columns = vars(plays), 
    colors = scales::col_numeric(
      palette = c(
        "cadetblue1", "cadetblue2", "cadetblue3"), 
      domain = c(40,8000))) %>%
  cols_align(
    align = "center") %>%
  tab_source_note(md("By Tej Seth | @mfbanalytics | Data from the Big Data Bowl"))
cov_table
gtsave(cov_table, "cov_table.png")


ridges <- df_plays_cov %>%
  filter(epa > -2) %>%
  filter(epa < 2)
ggplot(ridges, aes(x = epa, y = most_freq_cov, fill = most_freq_cov)) +
  geom_density_ridges() +
  theme_ridges() +
  labs(x = "EPA Allowed",
       y = "Coverage",
       title = "EPA Ridge for Each Coverage",
       caption = "By Tej Seth | @mfbanalytics | Data from the Big Data Bowl") +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    legend.position = "None"                         
  ) +
  geom_vline(xintercept =  0, color = "black", linetype = "solid", alpha=1.0) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))
ggsave('big_data_bowl_ridges.png', dpi = 300)
  
  

