library(readr)
library(dplyr)
library(ggplot2)
library(gganimate)

duke <- read_csv("C:/Users/Pat/Desktop/website2/pipeline/ncaa/basketball/data/team_stats/team_Duke_2026.csv",
                  show_col_types = FALSE)

duke <- duke |>
  mutate(game_date = as.Date(game_date, "%m/%d/%Y")) |>
  arrange(game_date) |>
  mutate(game_num = row_number())

# Fit once, on the full season, outside the animation
fit <- lm(team_score ~ field_goal_pct, data = duke)
line_df <- data.frame(
  field_goal_pct = range(duke$field_goal_pct)
) |>
  mutate(team_score = predict(fit, newdata = pick(everything())))

p <- ggplot(duke, aes(x = field_goal_pct, y = team_score)) +
  geom_point(color = "#00539b", size = 2.5, alpha = 0.7) +
  geom_line(data = line_df, aes(x = field_goal_pct, y = team_score),
            color = "#00274c", linewidth = 1, inherit.aes = FALSE) +
  labs(
    title    = "Duke — Field Goal % vs. Points Scored Through Game {frame_along}",
    subtitle = "2025-26 season, per game",
    x        = "Field Goal %",
    y        = "Points Scored"
  ) +
  theme_minimal(base_size = 13) +
  transition_reveal(game_num)

anim <- animate(
  p,
  nframes  = min(max(duke$game_num), 60),
  fps      = 6,
  width    = 900,
  height   = 650,
  renderer = gifski_renderer()
)

anim_save("ncaa/basketball/duke-blue-devils/duke_fg_pct_vs_score.gif", animation = anim)
cat("Saved animation.\n")
