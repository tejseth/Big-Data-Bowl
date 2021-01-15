library(tidyverse)
library(gganimate)
library(cowplot)
library(repr)

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

# General field boundaries
xmin <- 0
xmax <- 160/3
hash.right <- 38.35
hash.left <- 12
hash.width <- 3.3

#picking a random play
set.seed(123)

example_play <- df_plays %>%
  select(gameId, playId, playDescription) %>% 
  sample_n(1)


#merging games data to play
example_play <- inner_join(example_play,
                           df_games,
                           by = c("gameId" = "gameId"))

#merging tracking data to play
example_play <- inner_join(example_play,
                           df_tracking,
                           by = c("gameId" = "gameId",
                                  "playId" = "playId"))

#colors used for plot - using colors of team
#DEN vs OAK
cols_fill <- c("#FB4F14", "#000000", "#A5ACAF")
cols_col <- c("#E31837", "#FFB81C", "#000000")

plot_title <- str_trim(gsub("\\s*\\([^\\)]+\\)","",as.character(example_play$playDescription[1])))

# Specific boundaries for a given play
ymin <- max(round(min(example_play$x, na.rm = TRUE) - 10, -1), 0)
ymax <- min(round(max(example_play$x, na.rm = TRUE) + 10, -1), 120)

#hash marks
df.hash <- expand.grid(x = c(0, 23.36667, 29.96667, xmax), y = (10:110))
df.hash <- df.hash %>% filter(!(floor(y %% 5) == 0))
df.hash <- df.hash %>% filter(y < ymax, y > ymin)

#plotting
ggplot() +
  #setting size and color parameters
  scale_size_manual(values = c(6, 4, 6), guide = FALSE) + 
  scale_shape_manual(values = c(21, 16, 21), guide = FALSE) +
  scale_fill_manual(values = cols_fill, guide = FALSE) + 
  scale_colour_manual(values = cols_col, guide = FALSE) + 
  #adding hash marks
  annotate("text", x = df.hash$x[df.hash$x < 55/2], 
           y = df.hash$y[df.hash$x < 55/2], label = "_", hjust = 0, vjust = -0.2) + 
  annotate("text", x = df.hash$x[df.hash$x > 55/2], 
           y = df.hash$y[df.hash$x > 55/2], label = "_", hjust = 1, vjust = -0.2) +
  #adding yardlines
  annotate("segment", x = xmin, 
           y = seq(max(10, ymin), min(ymax, 110), by = 5), 
           xend =  xmax, 
           yend = seq(max(10, ymin), min(ymax, 110), by = 5)) + 
  #adding field yardline text
  annotate("text", x = rep(hash.left, 11), y = seq(10, 110, by = 10), 
           label = c("G   ", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "   G"), 
           angle = 270, size = 4) + 
  annotate("text", x = rep((xmax - hash.left), 11), y = seq(10, 110, by = 10), 
           label = c("   G", seq(10, 50, by = 10), rev(seq(10, 40, by = 10)), "G   "), 
           angle = 90, size = 4) + 
  #adding field exterior
  annotate("segment", x = c(xmin, xmin, xmax, xmax), 
           y = c(ymin, ymax, ymax, ymin), 
           xend = c(xmin, xmax, xmax, xmin), 
           yend = c(ymax, ymax, ymin, ymin), colour = "black") + 
  
  #adding players
  geom_point(data = example_play, aes(x = (xmax-y),
                                      y = x, 
                                      shape = team,
                                      fill = team,
                                      group = nflId,
                                      size = team,
                                      colour = team), alpha = 0.7) +  
  #adding jersey numbers
  geom_text(data = example_play, aes(x = (xmax-y), y = x, label = jerseyNumber), colour = "white", 
            vjust = 0.36, size = 3.5) + 
  #applying plot limits
  ylim(ymin, ymax) + 
  coord_fixed() +
  #applying theme
  theme_nothing() + 
  theme(plot.title = element_text()) +
  #titling plot with play description
  labs(title = plot_title) +
  #setting animation parameters
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

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
  
         


