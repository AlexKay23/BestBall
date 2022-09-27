library(tidyverse)
library(nflverse)

passing_data <- load_pfr_advstats(stat_type = 'pass', summary_level = 'season')

stats_2022 <- load_player_stats(seasons = 2022)


t10_pass <- passing_data %>% 
  arrange(desc(pass_attempts)) %>% 
  head(10)


test_stats <- stats_2022 %>%   #load_player_stats(2022)
  mutate(player = player_display_name)

test_join <- t10_pass %>%
  inner_join(test_stats, by = "player") %>% ## will need to edit.. we only want to add player ID to t10_pas
  select(1:29,34) %>% 
  distinct()



# view(test_join %>%
#        select(29,30,1))


ggplot(test_join, aes(x = reorder(player_id, -pass_attempts), y= pass_attempts))+
  geom_col(aes(color = team, fill = team))+
  scale_color_nfl(type = "secondary")+
  scale_fill_nfl(alpha = 0.4)+
  labs(title = "2022 Pass Attempt leaders",
       y= "Number of Pass Attempts")+
  theme_minimal()+
  theme(plot.title = ggplot2::element_text(face = "bold"),
        plot.title.position = "plot",
        axis.title.x = element_blank(),
        axis.text.x = element_nfl_headshot(size = 2)
  )


bad_pass_leaders <- test_join %>% 
  arrange(desc(bad_throw_pct)) %>% 
  filter(pass_attempts >=11) %>% 
  head(10)

ggplot(bad_pass_leaders, aes(x = reorder(player_id, -bad_throw_pct), y= bad_throw_pct))+
  geom_col(aes(color = team, fill = team))+
  scale_color_nfl(type = "secondary")+
  scale_fill_nfl(alpha = 0.7)+
  labs(title = "2022 Worst Passers",
       y= "Bad Throw %")+
  theme_minimal()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_nfl_headshot(size = 2)
  )