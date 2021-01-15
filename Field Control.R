library(tidyverse)
library(gganimate)
library(mvtnorm)

team_ <- "DET"
qb_ <- "M.Stafford"
min_offense_play_result <- 25

stafford_play <- df_plays %>% 
  dplyr::filter(possessionTeam == team_,
                stringr::str_detect(playDescription, qb_),
                offensePlayResult > min_offense_play_result) %>%
  dplyr::arrange(-offensePlayResult) %>%
  dplyr::select(gameId, playId, possessionTeam, playDescription, absoluteYardlineNumber, yardsToGo) %>%
  dplyr::slice(7)

stafford_game <- df_games %>%
  dplyr::filter(gameId == stafford_play$gameId)

stafford_track <- df_tracking %>%
  dplyr::filter(gameId == stafford_game$gameId, playId == stafford_play$playId)

stafford_direction <- stafford_track %>% head(1) %>% dplyr::pull(playDirection)

stafford_track <- stafford_track %>%
  dplyr::select(x, y, s, dir, event, displayName, jerseyNumber, frameId, team)

stafford_game <- as_factor(stafford_game)
typeof(stafford_game)

stafford_track <- stafford_track %>%
  dplyr::mutate(
    dir_rad = dir * pi / 180,
    v_x = sin(dir_rad) * s,
    v_y = cos(dir_rad) * s,
    v_theta = atan(v_y / v_x),
    v_theta = ifelse(is.nan(v_theta), 0, v_theta),
    team_name = case_when(
      team == "home" ~ "DET",
      team == "away" ~ "GB",
      TRUE ~ team,
    )
  ) %>%
  dplyr::select(frameId, event, team = team_name, jerseyNumber, displayName, x, y, s, v_theta, v_x, v_y)

plot_field <- function(field_color="#ffffff", line_color = "#212529", number_color = "#adb5bd") {
  field_height <- 160/3
  field_width <- 120
  
  field <- ggplot() +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 13, hjust = 0.5),
      plot.subtitle = element_text(hjust = 1),
      legend.position = "bottom",
      legend.title.align = 1,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank(),
      axis.text = element_blank(),
      axis.line = element_blank(),
      panel.background = element_rect(fill = field_color, color = "white"),
      panel.border = element_blank(),
      aspect.ratio = field_height/field_width
    ) +
    # major lines
    annotate(
      "segment",
      x = c(0, 0, 0,field_width, seq(10, 110, by=5)),
      xend = c(field_width,field_width, 0, field_width, seq(10, 110, by=5)),
      y = c(0, field_height, 0, 0, rep(0, 21)),
      yend = c(0, field_height, field_height, field_height, rep(field_height, 21)),
      colour = line_color
    ) +
    # hashmarks
    annotate(
      "segment",
      x = rep(seq(10, 110, by=1), 4),
      xend = rep(seq(10, 110, by=1), 4),
      y = c(rep(0, 101), rep(field_height-1, 101), rep(160/6 + 18.5/6, 101), rep(160/6 - 18.5/6, 101)),
      yend = c(rep(1, 101), rep(field_height, 101), rep(160/6 + 18.5/6 + 1, 101), rep(160/6 - 18.5/6 - 1, 101)),
      colour = line_color
    ) +
    # yard numbers
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      size = 10,
      colour = number_color,
    ) +
    # yard numbers upside down
    annotate(
      "text",
      x = seq(20, 100, by = 10),
      y = rep(field_height-12, 9),
      label = c(seq(10, 50, by = 10), rev(seq(10, 40, by = 10))),
      angle = 180,
      size = 10,
      colour = number_color, 
    )
  
  return(field)
}

fetch_team_colors <- function(team_colors_=NULL, h_team_, a_team_, diverge_=FALSE) {
  colors_url <- "https://raw.githubusercontent.com/asonty/ngs_highlights/master/utils/data/nfl_team_colors.tsv"
  
  if (is.null(team_colors_)) {
    team_colors_ <- suppressMessages(readr::read_tsv(colors_url))
  }
  
  h_team_color1 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color1)
  h_team_color2 <- team_colors_ %>% filter(teams == h_team_) %>% pull(color2)
  a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color1)
  a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% pull(color2)
  
  if (diverge_ == TRUE) {
    h_team_color1_family <- team_colors_ %>% filter(teams == h_team_) %>% select(color1_family) %>% pull()
    a_team_color1_family <- team_colors_ %>% filter(teams == a_team_) %>% select(color1_family) %>% pull()
    
    if (h_team_color1_family == a_team_color1_family) {
      a_team_color1 <- team_colors_ %>% filter(teams == a_team_) %>% select(color2) %>% pull()
      a_team_color2 <- team_colors_ %>% filter(teams == a_team_) %>% select(color1) %>% pull()
    }
  }
  
  df_colors <- tibble(
    home_1 = h_team_color1, home_2 = h_team_color2, away_1 = a_team_color1, away_2 = a_team_color2
  )
  
  
  return(df_colors)
}

if (stafford_direction == "left") {
  line_of_scrimmage = stafford_play$absoluteYardlineNumber
  to_go_line = line_of_scrimmage - stafford_play$yardsToGo
} else {
  line_of_scrimmage = 100 - stafford_play$absoluteYardlineNumber
  to_go_line = line_of_scrimmage + stafford_play$yardsToGo
}

df_colors <- fetch_team_colors(h_team_ = "DET", a_team_ = "GB", diverge_ = T)

play_frames <- plot_field() + 
  # line of scrimmage
  annotate(
    "segment",
    x = line_of_scrimmage, xend = line_of_scrimmage, y = 0, yend = 160/3,
    colour = "#0d41e1", size = 1.5
  ) +
  # 1st down marker
  annotate(
    "segment",
    x = to_go_line, xend = to_go_line, y = 0, yend = 160/3,
    colour = "#f9c80e", size = 1.5
  ) +
  # away team velocities
  geom_segment(
    data = stafford_track %>% dplyr::filter(team == "GB"),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = df_colors$away_1, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) + 
  # home team velocities
  geom_segment(
    data = stafford_track %>% dplyr::filter(team == "DET"),
    mapping = aes(x = x, y = y, xend = x + v_x, yend = y + v_y),
    colour = df_colors$home_2, size = 1, arrow = arrow(length = unit(0.01, "npc"))
  ) +
  # away team locs and jersey numbers
  geom_point(
    data = stafford_track %>% dplyr::filter(team == "GB"),
    mapping = aes(x = x, y = y),
    fill = "#f8f9fa", colour = df_colors$away_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = stafford_track %>% dplyr::filter(team == "GB"),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$away_1, size = 4.5
  ) +
  # home team locs and jersey numbers
  geom_point(
    data = stafford_track %>% dplyr::filter(team == "DET"),
    mapping = aes(x = x, y = y),
    fill = df_colors$home_1, colour = df_colors$home_2,
    shape = 21, alpha = 1, size = 8, stroke = 1.5
  ) +
  geom_text(
    data = stafford_track %>% dplyr::filter(team == "DET"),
    mapping = aes(x = x, y = y, label = jerseyNumber),
    colour = df_colors$home_2, size = 4.5, 
  ) +
  # ball
  geom_point(
    data = stafford_track %>% dplyr::filter(team == "football"),
    mapping = aes(x = x, y = y),
    fill = "#935e38", colour = "#d9d9d9",
    shape = 21, alpha = 1, size = 4, stroke = 1
  ) +
  # title 
  labs(title = stafford_play$playDescription) +
  # animation stuff
  transition_time(frameId)  +
  ease_aes('linear') + 
  NULL

play_length <- length(unique(stafford_track$frameId))
play_anim <- animate(
  play_frames,
  fps = 10, 
  nframe = play_length,
  width = 800,
  height = 400,
  end_pause = 0
)

play_anim

############## implementing a field control model ############## 

# 1. compute player's distance from ball
compute_distance_from_ball <- function(tracking_data) {
  tracking_data <- tracking_data %>%
    dplyr::inner_join(
      tracking_data %>%
        dplyr::filter(team == "football") %>%
        dplyr::select(frameId, ball_x = x, ball_y = y),
      by = "frameId"
    ) %>%
    dplyr::mutate(
      distance_from_ball = sqrt((x-ball_x)^2 + (y-ball_y)^2)
    ) %>% 
    dplyr::select(-ball_x, -ball_y)
  return(tracking_data)
}

stafford_track <- stafford_track %>% compute_distance_from_ball()

# 2. compute each player's speed ratio
#    here we're using a max speed of 13 yds/s, 
#    which about lines up with the max speeds seen in 
#    the Next Gen Stats Fastest Ballcarrier tables
compute_speed_ratio <- function(tracking_data, s_max = 13.00) {
  tracking_data <- tracking_data %>%
    dplyr::mutate(
      s_ratio = s / s_max
    )
  return(tracking_data)
}

stafford_track <- stafford_track %>% compute_speed_ratio() 

# 3. compute each player's next location
compute_next_loc <- function(tracking_data, delta_t = 0.50) {
  tracking_data <- tracking_data %>%
    dplyr::mutate(
      x_next = x + v_x * delta_t,
      y_next = y + v_y * delta_t
    )
  return(tracking_data)
}

stafford_track <- stafford_track %>% compute_next_loc()

# 4. compute each player's radius of influence for a given frame
#    here we're using a model that approximates the plot shown in
#    the appendix of Wide Open Spaces. this original function was
#    found by Will Thomson. the modification that I'll make is that
#    I'll add a few parameters to the equation, so we can alter the
#    min/max radius of influence a player can have, as well as the
#    rate at which that radius changes (based on their proximity 
#    to the ball)
compute_radius_of_influence <- function(tracking_data,
                                        min_radius = 4.00,
                                        max_radius = 10.00,
                                        max_distance_from_ball = 20.00) {
  tracking_data <- tracking_data %>%
    dplyr::mutate(
      radius_of_influence = min_radius + distance_from_ball^3 * (max_radius-min_radius) / max_distance_from_ball,
      radius_of_influence = dplyr::case_when(
        radius_of_influence > max_radius ~ max_radius,
        TRUE ~ radius_of_influence
      )
    )
  return(tracking_data)
}

stafford_track <- stafford_track %>% compute_radius_of_influence()

compute_rotation_matrix <- function(v_theta) {
  R <- matrix(
    c(cos(v_theta), -sin(v_theta),
      sin(v_theta),  cos(v_theta)),
    nrow = 2,
    byrow = TRUE
  )
  return(R)
}

compute_scaling_matrix <- function(radius_of_influence, s_ratio) {
  S <- matrix(
    c(radius_of_influence * (1 + s_ratio), 0,
      0, radius_of_influence * (1 - s_ratio)),
    nrow = 2,
    byrow = TRUE
  )
  return(S)
}

compute_covariance_matrix <- function(v_theta, radius_of_influence, s_ratio) {
  R <- compute_rotation_matrix(v_theta)
  S <- compute_scaling_matrix(radius_of_influence, s_ratio)
  Sigma <- R %*% S %*% S %*% solve(R)
  return(Sigma)
}

# note that this is meant operate on just 1 row of the tracking dataset
compute_player_zoi <- function(player_frame_tracking_data, field_grid = NULL) {
  if(is.null(field_grid)) {
    field_grid <- expand_grid(
      x = seq(0, 120, length.out = 120),
      y = seq(0, 160/3, length.out = 160/3)
    )
  }
  
  frameId_ <- player_frame_tracking_data %>% pull(frameId)
  displayName_ <- player_frame_tracking_data %>% pull(displayName) 
  jerseyNumber_ <- player_frame_tracking_data %>% pull(jerseyNumber) 
  team_ <- player_frame_tracking_data %>% pull(team) 
  
  zoi_center_x_ <- player_frame_tracking_data %>% pull(x_next)
  zoi_center_y_ <- player_frame_tracking_data %>% pull(y_next)
  v_theta_ <- player_frame_tracking_data %>% pull(v_theta)
  radius_of_influence_ <- player_frame_tracking_data %>% pull(radius_of_influence)
  s_ratio_ <- player_frame_tracking_data %>% pull(s_ratio)
  
  mu <- c(zoi_center_x_, zoi_center_y_)
  Sigma <- compute_covariance_matrix(v_theta_, radius_of_influence_, s_ratio_)
  
  player_zoi <- field_grid %>%
    dplyr::mutate(
      influence = mvtnorm::dmvnorm(x = field_grid, mean = mu, sigma = Sigma),
      influence = influence / max(influence),
      frameId = frameId_,
      displayName = displayName_,
      jerseyNumber = jerseyNumber_,
      team = team_
    )
  
  return(player_zoi)
}

compute_team_frame_control <- function(frame_tracking_data, home_team) {
  team_frame_control <- frame_tracking_data %>%
    dplyr::filter(team != "football") %>%
    dplyr::group_split(displayName) %>%
    purrr::map_dfr(., compute_player_zoi) %>%
    dplyr::mutate(
      influence = dplyr::case_when(
        team == home_team ~ -1 * influence,
        TRUE ~ influence
      )
    ) %>%
    dplyr::group_by(frameId, x, y) %>%
    dplyr::summarise(control = sum(influence), .groups = "keep") %>%
    dplyr::mutate(control = 1 / (1 + exp(control)))
  
  return(team_frame_control)
}

df_control <- stafford_track %>%
  dplyr::filter(team != "football") %>%
  dplyr::group_split(frameId) %>%
  purrr::map_dfr(., compute_team_frame_control, "DET")




