library(shiny)
library(dplyr)
library(ggplot2)

# read in ATP and WTA data
raw_atp_data <- read.csv("../data/atp_matches_2017.csv")
raw_wta_data <- read.csv("../data/wta_matches_2016.csv")

# random plot
ggplot(raw_atp_data) +
  geom_point(aes(x = winner_age, y = loser_age))

# Objective 1: Find the number of wins (and losses) each person had
# obtain list of names along with their wins
winners <- raw_atp_data %>%
  select(winner_name) %>%
  group_by(winner_name) %>%
  summarize(num_wins = n()) %>%
  arrange(desc(num_wins))
colnames(winners) <- c("Name", "Number of Wins")

# obtain list of names along with their losses
losers <- raw_atp_data %>%
  select(loser_name) %>%
  group_by(loser_name) %>%
  summarize(num_losses = n())
colnames(losers) <- c("Name", "Number of Losses")

# Note: has NA values
winners_losers <- full_join(winners, losers, by = "Name") %>%
  mutate

View(raw_atp_data)


# graph each unique player's age (winner age) against num of wins they have overall?
wins_age <- raw_atp_data %>%
  select(winner_name, winner_age) %>%
  group_by(winner_name, winner_age) %>% #note: has duplicate names due to same player with diff ages at diff times
  summarize(num_wins = n()) %>%
  arrange(winner_age)

View(wins_age)
plot(wins_age$winner_age, wins_age$num_wins)


unique(raw_atp_data$winner_name)

## other ideas
test first serve accuracy against wins
test stroke speeds against wins
test net time against wins_
etc


### test for impact of 1st serves in for match outcome for each unique match
first_serve_in <- raw_atp_data %>%
  select(winner_name, w_1stIn, loser_name, l_1stIn)

nrow(first_serve_in)


#scatterplot directly comparing number of 1st serves in between winners and losers
plot(first_serve_in$w_1stIn, first_serve_in$l_1stIn)

# barplot
first_serve_in_ratio_unsorted <- first_serve_in$w_1stIn / first_serve_in$l_1stIn
first_serve_in_ratio <- sort(w_to_l_first_serve_in_unsorted)
                              
match_num_fsi <- c(1:length(w_to_l_first_serve_in))

first_serve_in_proc_data <- data.frame(match_num, w_to_l_first_serve_in)

# keep this graph!!
ggplot(first_serve_in_proc_data) + 
  geom_point(aes(x = match_num, y = w_to_l_first_serve_in)) + 
  geom_line(aes(x = match_num, y = 1), col = "purple")

first_serve_in_ratio
first_serve_in_ratio_high <- first_serve_in_ratio[first_serve_in_ratio > 1]
first_serve_in_ratio_high
first_serve_in_ratio_low <- first_serve_in_ratio[first_serve_in_ratio < 1]
first_serve_in_ratio_low

first_serve_in_impact <- length(first_serve_in_ratio_high) / length(first_serve_in_ratio_low)
# surprising that this is around 0.7, meaning higher first serves in ratio is correlated with losing

# further notes
# * do we want to perform a t-test here to show significance??
# * winner and loser could have different number of serves as well,
#   should average out to be the same-ish but still...



### same thing as above but with aces ratio...
aces <- raw_atp_data %>%
  select(winner_name, w_ace, loser_name, l_ace)


#scatterplot directly comparing number of aces between winners and losers
ggplot(aces) +
  geom_point(aes(x = w_ace, y = l_ace))

# barplot
aces_ratio_unsorted <- aces$w_ace / aces$l_ace
aces_ratio <- sort(aces_ratio_unsorted)

match_num_aces <- c(1:length(aces_ratio))

aces_proc_data <- data.frame(match_num_aces, aces_ratio)

# keep this graph!!
ggplot(aces_proc_data) + 
  geom_point(aes(x = match_num_aces, y = aces_ratio)) + 
  geom_line(aes(x = match_num_aces, y = 1), col = "purple")

##### Additional note, maybe for the analyses above, take the number of matches w/ >1 ratio
## and divide by that w/ <1 ratio to get some sort of metric for "impact on match"
aces_ratio_high <- aces_ratio[aces_ratio > 1]
aces_ratio_high
aces_ratio_low <- aces_ratio[aces_ratio < 1]
aces_ratio_low

# this number is essentially the ratio between the number of matches where the winner had more aces than the loser 
# and the number of matches where the loser had more aces than the loser ("impact of ace on match")
aces_impact <- length(aces_ratio_high) / length(aces_ratio_low)


###### Next steps
## create function to streamline processes like the two above (first serve in and aces)
## create lots of graphs
## create lots of insights
## put on shiny!!


## function --> ratio
##     function --> graph
##     function --> impact

##### Function to observe any desired tennis statistic
make_ratios <- function(stat) {
  wstat <- paste0("w_", stat)
  lstat <- paste0("l_", stat)
  stat_frame <- raw_atp_data %>%
   select(winner_name, contains(wstat), loser_name, contains(lstat))
  stat_ratio_unsorted <- stat_frame[,2] / stat_frame[,4]
  stat_ratio <- sort(stat_ratio_unsorted)
  return(stat_ratio)
}

make_plot <- function(stat_ratios) {
  num_matches <- c(1:length(stat_ratios))
  stat_proc_data <- data.frame(match_num, stat_ratios)
  return(
    ggplot(stat_proc_data) + 
      geom_point(aes(x = num_matches, y = stat_ratios)) + 
      geom_line(aes(x = num_matches, y = 1), col ="purple")
  )
}

make_impact <- function(stat_ratios) {
  stat_ratios_high <- stat_ratios[stat_ratios > 1]
  stat_ratios_low <- stat_ratios[stat_ratios < 1]
  stat_impact <- length(stat_ratios_high) / length(stat_ratios_low)
  return(stat_impact)
}

ace_ratios <- make_ratios("ace")
make_plot(ace_ratios)
make_impact(ace_ratios)

first_serve_in_ratios <- make_ratios("1stIn")
make_plot(first_serve_in_ratios)
make_impact(first_serve_in_ratios)


match_num <- c(1:length(stat_ratio))
stat_proc_data <- data.frame(match_num, stat_ratio)
return(
  ggplot(stat_proc_data) +
    geom_point(aes(x = match_num, y = stat_ratio)) +
    geom_line(aes(x = match_num, y = 1), col = "purple")
)

plot_maker("1stIn")
plot_maker("ace") + labs(title = "food!")
## and divide by that w/ <1 ratio to get some sort of metric for "impact on match"
aces_ratio_high <- aces_ratio[aces_ratio > 1]
aces_ratio_high
aces_ratio_low <- aces_ratio[aces_ratio < 1]
aces_ratio_low

# this number is essentially the ratio between the number of matches where the winner had more aces than the loser 
# and the number of matches where the loser had more aces than the loser ("impact of ace on match")
aces_impact <- length(aces_ratio_high) / length(aces_ratio_low)



ggplot(first_serve_in, aes(first_serve_in)) +
  geom_bar()
?geom_bar

ggplot(first_serve_in) + 
  geom_point(aes(x = match_num, y = w_to_l_first_serve_in))

%>%
  group_by(winner_name) %>%
  summarize(total winners hit