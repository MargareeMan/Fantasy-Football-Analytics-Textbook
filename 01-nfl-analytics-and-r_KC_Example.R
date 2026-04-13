################################################################################
#  Introduction to NFL Analytics with R
#  https://bradcongelio.com/nfl-analytics-with-r-book/
#
#  1  An Introduction to NFL Analytics and the R Programming Language
#  https://bradcongelio.com/nfl-analytics-with-r-book/01-nfl-analytics-and-r.html
#
#  1.3 Investigating the Rams’ 2022 Offensive Line
#  https://bradcongelio.com/nfl-analytics-with-r-book/01-nfl-analytics-and-r.html#investigating-the-rams-2022-offensive-line
################################################################################
setwd("D:/R/Introduction to NFL Analytics with R")
source("book-functions.R")

library(showtext)
library(tidyverse)
library(nflreadr)
library(nflplotR)
library(extrafont)
library(ggtext)
library(vroom)
library(cowplot)
library(showtext)

font_add(family = "Roboto", regular = "C:/Windows/Fonts/RobotoCondensed-Regular.ttf")
font_import()  # scans and imports all system fonts — takes a minute or two
loadfonts(device = "win")
fonts()  # lists all fonts extrafont knows about


# load_participation(
#   seasons = most_recent_season(TRUE),
#   include_pbp = TRUE,
#   file_type = getOption("nflreadr.prefer", default = "CSV")
# )

setwd("D:/R/Introduction to NFL Analytics with R")
participation <- nflreadr::load_participation(2025, include_pbp = TRUE)
# write_csv(participation,"participation_2022.csv")
rosters <- nflreadr::load_rosters(2025) %>%
  select(full_name, gsis_id, depth_chart_position)
# write_csv(rosters,"rosters_2025.csv")

oline_participation <- participation %>%
  filter(play_type %in% c("pass", "run")) %>%
  group_by(nflverse_game_id, possession_team, fixed_drive) %>%
  filter(fixed_drive == 1 | fixed_drive == 2) %>%
  filter(row_number() == 1) %>%
  select(nflverse_game_id, play_id, possession_team, 
         offense_personnel, offense_players) %>%
  dplyr::mutate(gsis_id = stringr::str_split(offense_players, ";")) %>%
  tidyr::unnest(c(gsis_id)) %>%
  left_join(rosters, by = c("gsis_id" = "gsis_id"))

head(oline_participation, n= 10)

oline_participation <- oline_participation %>%
  filter(depth_chart_position %in% c("T", "G", "C")) %>%
  group_by(nflverse_game_id, possession_team) %>%
  mutate(starting_line = toString(full_name)) %>%
  select(nflverse_game_id, possession_team, 
         offense_personnel, starting_line) %>%
  distinct()

oline_participation %>%
  group_by(offense_personnel) %>%
  summarize(total = n())

oline_participation

################################################################################
##### snap count data

oline_snap_counts <- nflreadr::load_snap_counts(seasons = 2022)
# write_csv(oline_snap_counts,"oline_snap_counts.csv")

oline_snap_counts <- oline_snap_counts %>%
  select(game_id, week, player, position, team, offense_pct) %>%
  filter(position %in% c("T", "G", "C")) %>%
  group_by(game_id, team) %>%
  arrange(-offense_pct) %>%
  dplyr::slice(1:5) %>%    # selecting just the five offensive linemen with the most snap counts in that singular game.
  ungroup()



oline_snap_counts <- oline_snap_counts %>%
  group_by(game_id, team) %>%
  arrange(player, .by_group = TRUE)

oline_final_data <- oline_snap_counts %>%
  group_by(game_id, week, team) %>%
  mutate(starting_line = toString(player)) %>%
  select(game_id, week, team, starting_line) %>%
  distinct(game_id, .keep_all = TRUE)

total_combos <- oline_final_data %>%
  group_by(team) %>%
  summarize(combos = n_distinct(starting_line)) %>%
  arrange(-combos)

total_combos

# install.packages("remotes")
#remotes::install_github("jthomasmock/espnscrapeR")
library(espnscrapeR)
records <- espnscrapeR::get_nfl_standings(season = 2022) %>%
  select(team_abb, win_pct)
# write_csv(records,"nfl_standings_season_2022.csv")

records$team_abb <- nflreadr::clean_team_abbrs(records$team_abb)
# write_csv(records,"nfl_standings_season_2022.csv")

total_combos <- total_combos %>%
  left_join(records, by = c("team" = "team_abb"))
# write_csv(total_combos,"Ch 1.3.2 Unique Offensive Line Combinations vs Win Percentage.csv")

total_combos

nfl_analytics_theme <- function(..., base_size = 12) {
  theme(
    text = element_text(family = "Roboto",
                        size = base_size,
                        color = "black"),
    axis.ticks = element_blank(),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    plot.title.position = "plot",
    plot.title = element_markdown(size = 16,
                                  vjust = .02,
                                  hjust = 0.5),
    plot.subtitle = element_markdown(hjust = 0.5),
    plot.caption = element_markdown(size = 8),
    panel.grid.minor = element_blank(),
    panel.grid.major =  element_line(color = "#d0d0d0"),
    panel.background = element_rect(fill = "#f7f7f7"),
    plot.background = element_rect(fill = "#f7f7f7"),
    panel.border = element_blank(),
    legend.background = element_rect(color = "#F7F7F7"),
    legend.key = element_rect(color = "#F7F7F7"),
    legend.title = element_text(face = "bold"),
#    legend.title = element_text(0.5),
    strip.text = element_text(face = "bold"))
}

ggplot(data = total_combos, aes(x = combos, y = win_pct)) +
  geom_line(stat = "smooth", method = "lm",
            linewidth = .7, color = "blue",
            alpha = 0.25) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = win_pct),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  nfl_analytics_theme() +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 6),
                     labels = scales::label_number(accuracy = 0.001)) +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Win Percentage") +
  labs(title = "**Unique Offensive Line Combinations vs. Win Percentage**",
       subtitle = "*Through Week #15*",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**")

################################################################################
# 1.3.3 Unique Offensive Line Combinations vs. Pressure Rate
################################################################################
pressure_rate <- vroom("http://nfl-book.bradcongelio.com/pressure-rate")

teams <- nflreadr::load_teams() %>%
  select(team_abbr, team_nick)

pressure_rate <- pressure_rate %>%
  left_join(teams, by = c("team" = "team_nick"))

total_combos <- total_combos %>%
  left_join(pressure_rate, by = c("team" = "team_abbr"))
write_csv(total_combos,"1.3.3 Unique Offensive Line Combinations vs Pressure Rate.csv")

ggplot(data = total_combos, aes(x = combos, y = pressure_percent)) +
  geom_line(stat = "smooth", method = "lm",
            size = .7, color = "blue",
            alpha = 0.25) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = pressure_percent),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks(n = 12)) +
  scale_y_reverse(breaks = scales::pretty_breaks(n = 6),
                  labels = scales::percent_format(scale = 1,
                                                  accuracy = 0.1)) +
  nfl_analytics_theme() +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Pressure Rate (per Dropback)") +
  labs(title = "**Unique Offensive Line Combinations vs. Pressure Rate**",
       subtitle = "*2022 Season*",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**")


################################################################################
# 1.3.4 Unique Offensive Line Combinations vs. QB Hits Allowed
################################################################################

pbp <- nflreadr::load_pbp(2022)

qb_hits <- pbp %>%
  filter(!is.na(posteam)) %>%
  group_by(posteam) %>%
  summarize(total_qb_hits = sum(qb_hit == 1, na.rm = TRUE))

qb_hits_combined <- left_join(
  total_combos, qb_hits, by = c("team" = "posteam"))

ggplot(data = qb_hits_combined, aes(x = combos, y = total_qb_hits)) +
  geom_line(stat = "smooth", method = "lm",
            size = .7, color = "blue",
            alpha = 0.25) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = total_qb_hits),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_reverse(breaks = scales::pretty_breaks()) +
  nfl_analytics_theme() +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Total QB Hits Allowed") +
  labs(title = "Unique Offensive Line Combinations vs. QB Hits Allowed",
       subtitle = "2022 Season",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**")

################################################################################
# 1.3.5 Unique Offensive Line Combinations vs. Adjusted Sack Rate
################################################################################

adjusted_sack_rate <- vroom("http://nfl-book.bradcongelio.com/adj-sack-rate")
# write_csv(adjusted_sack_rate,"adjusted_sack_rate.csv")

adjusted_sack_rate %>%
  filter(season == 2022) %>%
  ggplot(aes(x = combos, y = adj_sack_rate)) +
  geom_line(stat = "smooth", method = "lm",
            size = .7,
            color = "blue",
            alpha = 0.25) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = adj_sack_rate),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_reverse(breaks = scales::pretty_breaks(),
                  labels = scales::percent_format(scale = 1)) +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Adjusted Sack Rate") +
  labs(title = "**Unique Offensive Line Combinations vs.
       Adjusted Sack Rate**",
       subtitle = "*2022 Season*",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**") +
  nfl_analytics_theme()

adjusted_sack_rate %>%
  ggplot(aes(x = combos, y = adj_sack_rate)) +
  geom_line(stat = "smooth", method = "lm",
            size = .7,
            color = "blue",
            alpha = 0.25) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = adj_sack_rate),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_reverse(breaks = scales::pretty_breaks(),
                  labels = scales::percent_format(scale = 1)) +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Adjusted Sack Rate") +
  labs(title = "**Unique Offensive Line Combinations vs.
       Adjusted Sack Rate**",
       subtitle = "*2020 - 2022 Seasons*",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**") +
  nfl_analytics_theme() +
  facet_wrap(~season, nrow = 2)


################################################################################
# 1.3.6a Unique Offensive Line Combinations vs. Adjusted Line Yards
################################################################################

fb_outsiders_oline <- vroom("http://nfl-book.bradcongelio.com/fbo_oline")
fb_outsiders_oline
# write_csv(fb_outsiders_oline,"fb_outsiders_oline.csv")

power_rank_plot <- ggplot(fb_outsiders_oline,
                          aes(x = combos, y = power_rank)) +
  nflplotR::geom_mean_lines(aes(x0 = combos,
                                y0 = power_rank),
                            color = "black",
                            size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_reverse(breaks = seq(1, 32, 2)) +
  labs(title = "**Unique Offensive Line Combinations vs.
       Power Ranking**",
       subtitle = "*2022 Season  |  FootballOutsiders.com*") +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Power Ranking (1 = best, 32 = worst)") +
  nfl_analytics_theme()

stuffed_rank_plot <- ggplot(fb_outsiders_oline,
                            aes(x = combos, y = stuffed_rank)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.8, alpha = .08) +
  nflplotR::geom_mean_lines(aes(x0 = combos,
                                y0 = stuffed_rank),
                            color = "black",
                            size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_reverse(breaks = seq(1, 32, 2)) +
  labs(title = "**Unique Offensive Line Combinations vs.
       Stuffed Rank**",
       subtitle = "*2022 Season  |  FootballOutsiders.com*")+
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Stuffed Ranking (1 = best, 32 = worst)") +
  nfl_analytics_theme()

plot_grid(power_rank_plot, stuffed_rank_plot, ncol = 1)

ggplot(fb_outsiders_oline, aes(x = combos, y = adj_line_yards)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.8, alpha = .08) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = adj_line_yards),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title = "**Unique Offensive Line Combinations vs.
       Adjusted Line Yards**",
       subtitle = "*2022 Season  |  FootballOutsiders.com*",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**") +
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Adjusted Line Yards") +
  nfl_analytics_theme()

################################################################################
# 1.3.6b Elusive Rating
################################################################################

oline_combo_elusive <- vroom("http://nfl-book.bradcongelio.com/ol-elusive")
write_excel_csv(oline_combo_elusive, "oline_combo_elusive.csv")

oline_combo_elusive <- oline_combo_elusive %>%
  mutate(adjusted_elu = scale(avg_elu),
         weighted_aly = adj_line_yards - adjusted_elu)

online_combo_test <- oline_combo_elusive %>%
  mutate(weighted_aly = adj_line_yards - adjusted_elu)

ggplot(oline_combo_elusive, aes(x = combos, y = weighted_aly)) +
  geom_smooth(method = "lm", se = FALSE, size = 0.8, alpha = .08) +
  nflplotR::geom_mean_lines(aes(x0 = combos, y0 = weighted_aly),
                            color = "black", size = .8) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.065) +
  scale_x_reverse(breaks = scales::pretty_breaks()) +
  scale_y_continuous(breaks = scales::pretty_breaks()) +
  labs(title = "**Unique Offensive Line Combinations vs.
       Weighted Adjusted Line Yards**",
       subtitle = "*2022 Season  |  Adjusted with PFF's 'Elusive'*",
       caption = "*An Introduction to NFL Analytics with R*<br>
       **Brad J. Congelio**")+
  xlab("# of Unique Offensive Line Combinations") +
  ylab("Weighted Adjusted Line Yards") +
  nfl_analytics_theme()

################################################################################
# 1.4 Exploring Explosive Plays per Touchdown Drive
################################################################################

pbp <- nflreadr::load_pbp(2022)

explosive <- pbp %>%
  filter(!is.na(posteam) &
           !is.na(yards_gained)
         & fixed_drive_result == "Touchdown") %>%
  filter(special == 0 & fumble == 0 & interception == 0) %>%
  group_by(posteam, game_id, drive) %>%
  summarize(max_yards = max(yards_gained)) %>%
  mutate(explosive_play = if_else(max_yards >= 20, 1, 0)) %>%
  ungroup() %>%
  group_by(posteam) %>%
  summarize(tds_no_explosive = sum(explosive_play == 0),
            tds_explosive = sum(explosive_play == 1),
            total_drives = sum(tds_no_explosive + tds_explosive),
            percent_no_exp = tds_no_explosive / total_drives,
            percent_w_exp = tds_explosive / total_drives) %>%
  select(posteam, percent_w_exp, percent_no_exp)
































