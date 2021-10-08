# Potential Questions to Answer:
# 1. Create snacks that the customers can buy and randomize who buys which snack
# 2. Pretend you own multiple theaters and run two simulations to represent each theater and plot the results
# 3. Create conditional statements for movies that may be PG-13 and children are not allowed to watch
#  * Required libraries *
library(ggplot2)
library(RColorBrewer)
# Theater 1
# Cost for adults and children
t1_ticket_cost <- 6 
t1_ticket_cost_child <- 4 
t1_movies <- c('Shrek', 'Avengers', 'Scary Movie', 'Titanic', 'After Earth')  # List 5 of your favorite movies
t1_PG_13 <- c(0, 0, 1, 0, 0)
t1_adult_assis <- rep(0, 7)
t1_child_assis <- rep(0, 7)
t1_popcorn_price <- 5
t1_drink_price <- 3
t1_screens <-  5
t1_seats <-  80
t1_rent_cost_day <- 300
t1_insurance_cost_day <- 100
t1_emp_man_cost <- 500
t1_week_days <- rep(0, 7)  # Store totals for each day

# Theater 2
# Cost for adults and children
t2_ticket_cost <- 6 
t2_ticket_cost_child <- 4 
t2_movies <- c('The Nun', 'Transformers', 'Harry Potter', 'Interstellar', 'Spider-Man')  # List 5 of your favorite movies
t2_PG_13 <- c(1, 0, 0, 1, 0)
t2_adult_assis <- rep(0, 7)
t2_child_assis <- rep(0, 7)
t2_popcorn_price <- 4
t2_drink_price <- 2
t2_screens <-  5
t2_seats <-  80
t1_rent_cost_day <- 300
t1_insurance_cost_day <- 100
t1_emp_man_cost <- 400
t2_week_days <- rep(0, 7)  # Store totals for each day


calculate_revenue <- function(adult_cost, child_cost, v_movies, pg_13, popcorn_price,
                              drink_price, n_screens, n_seats){
    # iterate through the week
    week_temp <- rep(0, 7)
    adult_assis <- rep(0, 7)
    child_assis <- rep(0, 7)
    for (day in seq_along(week_temp)) {
      # Keep track of total revenue for the day
      day_revenue <- 0
      # iterate through the amount of screens on a particular day
      for (movie in seq_along(v_movies)) {
        # Calculate the revenue for adults and children
        if(pg_13[movie] == 0){
          # Calculate  how many adults and children are watching the movie
        visitors_adults <- sample(1:(n_seats), 1)
        visitors_children <- sample(1:(n_seats-visitors_adults),1)
         # Calculate how many people buy Snacks
        popcorns <- sample(0:(visitors_adults), 1)
        drinks <- sample(0:(visitors_adults), 1)
        adults_revenue <- adult_cost*visitors_adults
        children_revenue <- child_cost*visitors_children
        popcorns_revenue <- popcorn_price*popcorns
        drinks_revenue <- drink_price*drinks
        adult_assis[day] <- adult_assis[day]+visitors_adults
        child_assis[day] <- child_assis[day]+visitors_children
        # Calculate revenue, and add to running total for the day
        day_revenue <- day_revenue+adults_revenue+children_revenue+popcorns_revenue+drinks_revenue
        }else if(pg_13[movie] == 1){
          # Calculate  how many adults are watching the movie
          visitors_adults <- sample(1:(n_seats), 1)
          adults_revenue <- adult_cost*visitors_adults
          # Calculate how many people buy Snacks
          popcorns <- sample(0:(visitors_adults), 1)
          drinks <- sample(0:(visitors_adults), 1)
          popcorns_revenue <- popcorn_price*popcorns
          drinks_revenue <- drink_price*drinks
          adult_assis[day] <- adult_assis[day]+visitors_adults
          # Calculate revenue, and add to running total for the day
          day_revenue <- day_revenue+adults_revenue+popcorns_revenue+drinks_revenue
        }
      }
      # Save total to the corresponding day
      week_temp[day] <- day_revenue
    }
    return(list(week_temp, adult_assis, child_assis))
}

    # Make a barchart showing total revenue per day
day_labels_long <- c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday', 'Sunday')
day_labels_short <- c('Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun')

chart_generator_1_theater <- function(t1_vector){    
  # Generate a pallette for the plot
  pallette <- brewer.pal(7, "Set2")
  
  par(mfrow = c(1, 1),
      mar = c(6,5,3,3))
  barplot(height = t1_vector,
          names = day_labels_long,
          main = "Movie Theater Revenues",
          col = pallette,
          ylab = 'Revenue',
          las=2,
          cex.axis = 0.8
  )
  # Make any other chart
  
  # Which day had the highest revenue? 
  barplot_df <- data.frame(day_labels_long, t1_vector)
  highest_revenue <- max(barplot_df$t1_vector)
  highest_day <- barplot_df$day_labels[barplot_df$t1_vector == highest_revenue]
  sprintf('%s is the day with the highest revenue: %d', highest_day, highest_revenue)      
}

    
chart_generator_2_theater <- function(t1_vector, t2_vector){    
  # Generate a pallette for the plot
  pallette1 <- brewer.pal(7, "Set2")
  pallette2 <- brewer.pal(7, "Set3")
  par(mfrow = c(2, 1),
      mar = c(2.5,3,3,3))
  barplot(height = t1_vector,
          names = day_labels_short,
          main = "Movie Theater 1 Revenues",
          col = pallette1,
          ylab = 'Revenue',
          cex.axis = 0.8,
          cex.main = 0.8
  )
  barplot(height = t2_vector,
          names = day_labels_short,
          main = "Movie Theater 2 Revenues",
          col = pallette2,
          ylab = 'Revenue',
          cex.axis = 0.8,
          cex.main = 0.8,
          
  )
  t1_dataframe <- data.frame(day_labels_long, t1_vector)
  t2_dataframe <- data.frame(day_labels_long, t2_vector)
  # Which day had the highest revenue? 
  # Theater 1
  t1_highest_revenue <- max(t1_dataframe$t1_vector)
  t1_total_revenue <- sum(t1_dataframe$t1_vector)
  t1_highest_day <- t1_dataframe$day_labels_long[t1_dataframe$t1_vector == t1_highest_revenue]
  print(paste(t1_highest_day,' is the day with the highest revenue: ',t1_highest_revenue, ', for theater 1. Total Revenue of week: $', t1_total_revenue, sep = ''))
  # Theater 2
  t2_highest_revenue <- max(t2_dataframe$t2_vector)
  t2_total_revenue <- sum(t2_dataframe$t2_vector)
  t2_highest_day <- t2_dataframe$day_labels_long[t2_dataframe$t2_vector == t2_highest_revenue]
  sprintf('%s is the day with the highest revenue: %d, for theater 2. Total Revenue of week: $%d', t2_highest_day, t2_highest_revenue, t2_total_revenue)
}    
    
###################
# Test Functions  #
###################

### One Theater ###
t1_raw_data <- calculate_revenue(t1_ticket_cost,
                                  t1_ticket_cost_child, t1_movies, t1_PG_13,
                                  t1_popcorn_price, t1_drink_price,
                                  t1_screens, t1_seats)

t1_week_days <- t1_raw_data[[1]]
t1_dataframe <- data.frame(day_labels_short,
                           t1_raw_data[[2]],
                           t1_raw_data[[3]]
                           )
names(t1_dataframe) <- c('Days', 'adults_assis', 'child_assis')
#Generate one theater chart
chart_generator_1_theater(t1_week_days)
# Make any other chart
ggplot(data = t1_dataframe) +
  geom_bar(mapping = aes(x = Days, y = adults_assis, ), stat = 'identity') +
  geom_bar(mapping = aes(x = Days, y = child_assis, fill = 'red'), stat = 'identity', position = 'stack')+
  labs(fill = 'Childs',
       y = 'Assistance')+
  ggtitle('Adults vs Child Assitance')


### Two Theaters ###
t1_week_days <- calculate_revenue(t1_ticket_cost,
                                  t1_ticket_cost_child, t1_movies, t1_PG_13,
                                  t1_popcorn_price, t1_drink_price, 
                                  t1_screens, t1_seats)

t2_week_days <- calculate_revenue(t2_ticket_cost,
                                  t2_ticket_cost_child, t2_movies, t2_PG_13,
                                  t2_popcorn_price, t2_drink_price, 
                                  t2_screens, t2_seats)
# Generate one theater chart
chart_generator_2_theater(t1_week_days, t2_week_days)

ggplot(t1_dataframe,
       mapping = aes(x = Days,
                     y = adults_assis,
                     fill = Days))+
  geom_bar(stat = 'identity')


