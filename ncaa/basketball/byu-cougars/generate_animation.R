library(readxl)
library(dplyr)
library(ggplot2)
library(gganimate)

team_BYU_2026 <- read_excel("C:/Users/Pat/Desktop/website2/pipeline/ncaa/basketball/data/team_stats/team_BYU_2026.xlsx")

team_BYU_2026 <- team_BYU_2026 |>
  mutate(game_date = as.Date(game_date)) |>
  arrange(game_date) |>
  mutate(game_num = row_number())

fit <- lm(team_score ~ field_goal_pct, data = team_BYU_2026)
line_df <- data.frame(
  field_goal_pct = range(team_BYU_2026$field_goal_pct)
) |>
  mutate(team_score = predict(fit, newdata = pick(everything())))

p <- ggplot(team_BYU_2026, aes(x = field_goal_pct, y = team_score)) +
  geom_point(color = "#0047ba", size = 2.5, alpha = 0.7) +
  geom_line(data = line_df, aes(x = field_goal_pct, y = team_score),
            color = "#002e5d", linewidth = 1, inherit.aes = FALSE) +
  labs(
    title    = "BYU — Field Goal % vs. Points Scored Through Game {frame_along}",
    subtitle = "2025-26 season, per game",
    x        = "Field Goal %",
    y        = "Points Scored"
  ) +
  theme_minimal(base_size = 13) +
  transition_reveal(game_num)

anim <- animate(
  p,
  nframes  = min(max(team_BYU_2026$game_num), 60),
  fps      = 6,
  width    = 900,
  height   = 650,
  renderer = gifski_renderer()
)

anim_save("ncaa/basketball/byu-cougars/byu_fg_pct_vs_score.gif", animation = anim)
cat("Saved animation.\n")
