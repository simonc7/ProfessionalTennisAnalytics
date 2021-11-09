##########################
###### SERVER SCRIPT #####
##########################

library(shiny)
library(dplyr)
library(ggplot2)
library(kableExtra)

# read in ATP and WTA data
raw_atp_data <- read.csv("atp_matches_2016.csv")
raw_wta_data <- read.csv("wta_matches_2016.csv")

# multivariable logistic regression here!!!
win_atp_data <- raw_atp_data %>%
  select(winner_name, winner_age, w_ace, w_df, w_svpt, w_1stIn, w_bpFaced, w_bpSaved)
colnames(win_atp_data) <- c("outcome", "age", "ace", "df", "svpt", "firstIn", "bpFaced", "bpSaved")
win_atp_data$outcome <- 1

lose_atp_data <- raw_atp_data %>%
  select(loser_name, loser_age, l_ace, l_df, l_svpt, l_1stIn, l_bpFaced, l_bpSaved)
colnames(lose_atp_data) <- c("outcome", "age", "ace", "df", "svpt", "firstIn", "bpFaced", "bpSaved")
lose_atp_data$outcome <- 0
         
outcome_data <- rbind(win_atp_data, lose_atp_data)

set.seed(1)
sample <- sample(c(TRUE, FALSE), nrow(outcome_data), replace=TRUE, prob=c(0.7,0.3))
train <- outcome_data[sample, ]
test <- outcome_data[!sample, ]

# ### Multivariable logistic regression on various types of shots in determining outcome
#model <- glm(outcome ~ age + ace + df + svpt + firstIn + bpFaced + bpSaved, family="binomial", data=train)
model <- glm(outcome ~ ace + df + svpt + firstIn, family="binomial", data=train)

options(scipen=999)

summary <- summary(model)
summary$coefficients
log_reg_table <- as.data.frame(as.matrix(summary$coefficients))

row.names(log_reg_table) <- NULL
shot_types <- c("(Intercept)", "ace", "double faults", "serving points", "first serve in (%)")
log_reg_table2 <- cbind(shot_types, log_reg_table)
# %>%
#   knitr::kable("html") %>%
#   kable_styling("striped", full_width = F)
View(summary(model))

## Player's general statistics throughout the year
winners_list <- raw_atp_data %>%
  select(winner_name) %>%
  group_by(winner_name) %>%
  summarize(num_wins = n())
colnames(winners_list) <- c("player", "wins")

losers_list <- raw_atp_data %>%
  select(loser_name) %>%
  group_by(loser_name) %>%
  summarize(num_losses = n())
colnames(losers_list) <- c("player", "losses")

players_wins_losses <- full_join(winners_list, losers_list) %>%
  na.omit()
win_pct_table <- players_wins_losses %>%
  mutate(win_percentage = paste0(round(100 * wins / (wins + losses), 2), "%")) %>%
  arrange(desc(win_percentage)) %>%
  knitr::kable("html") %>%
  kable_styling("striped", full_width = F)


## next steps, use kable and put on page... (done)
## court conditions
raw_atp_data %>%
  select(surface, winner_name)

winners_surface <- raw_atp_data %>%
  select(winner_name, surface) %>%
  group_by(winner_name, surface) %>%
  summarize(num_wins = n())
colnames(winners_surface) <- c("player", "surface", "wins")

losers_surface <- raw_atp_data %>%
  select(loser_name, surface) %>%
  group_by(loser_name, surface) %>%
  summarize(num_wins = n())
colnames(losers_surface) <- c("player", "surface", "losses")

surface_wins_losses <- full_join(winners_surface, losers_surface) %>%
  na.omit()



## Function: observe any desired tennis statistic and output a ratio
## representing the winner's stat / loser's stat
make_ratios <- function(stat) {
  wstat <- paste0("w_", stat)
  lstat <- paste0("l_", stat)
  stat_frame <- raw_atp_data %>%
    select(winner_name, contains(wstat), loser_name, contains(lstat))
  stat_ratio_unsorted <- stat_frame[,2] / stat_frame[,4]
  stat_ratio <- sort(stat_ratio_unsorted)
  return(stat_ratio)
}

## Function: create a plot with the ratios from above with regards to every
## match
make_plot <- function(stat_ratios) {
  num_matches <- c(1:length(stat_ratios))
  stat_proc_data <- data.frame(num_matches, stat_ratios)
  return(
    ggplot(stat_proc_data) + 
      geom_point(aes(x = num_matches, y = stat_ratios)) + 
      geom_line(aes(x = num_matches, y = 1), col ="purple")
  )
}

## Function: output impact value of the specified statistic ratio
## (impact is a custom metric representing the shot type's "strength" in
##  predicting the match outcome)
make_impact <- function(stat_ratios) {
  stat_ratios_high <- stat_ratios[stat_ratios > 1]
  stat_ratios_low <- stat_ratios[stat_ratios < 1]
  stat_impact <- length(stat_ratios_high) / length(stat_ratios_low)
  return(stat_impact)
}

# make plot and compute impact of aces
ace_ratios <- make_ratios("ace")
ace_plot <- make_plot(ace_ratios)
ace_impact <- make_impact(ace_ratios)

# make plot and compute impact of first serve in percentages
first_serve_in_ratios <- make_ratios("1stIn")
fsi_plot <- make_plot(first_serve_in_ratios)
fsi_impact <- make_impact(first_serve_in_ratios)

# make plot and compute impact of number of serving points won
sp_ratio <- make_ratios("svpt")
sp_plot <- make_plot(sp_ratio)
sp_impact <- make_impact(sp_ratio)


names <- c("Ace Impact", "First Serve In Impact", "Serving Points")
impacts <- c(ace_impact, fsi_impact, sp_impact)
impact_table <- data.frame(names, impacts)

########

# Contains the servers for the three visualization pages and conclusion
server <- function(input, output) {
  
    output$winpcttable <- function() {
      win_pct_table
    }
    
    
    
    output$surfacetable <- function() {
      surface_win_pct_table <- surface_wins_losses %>%
        mutate(win_percentage = paste0(round(100 * wins / (wins + losses), 2), "%")) %>%
        arrange(surface, desc(win_percentage))
      
      surface_table <- surface_win_pct_table %>%
        filter(surface == input$surface_type) %>%
        knitr::kable("html") %>%
        kable_styling("striped", full_width = F)
      
      surface_table
    }
  
    output$impactplots <- renderPlot({
      make_plot(make_ratios(input$shot_type))
    })
    
    output$impacttable <- renderTable({
      impact_table
    })
    
    output$log_reg_summary <- renderTable({
      log_reg_table2
    })


    # output$aceplot <- renderPlot({
    #   ace_plot
    # })
    # output$fsiplot <- renderPlot({
    #   fsi_plot
    # })
}
