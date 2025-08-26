# Step 1: Load our toolkits
library(tidyverse)
library(lubridate)

# Step 2: Define the file path
file_path <- "Day-ahead_prices_Hour.csv"

# Step 3: Load the data using base R's read.csv()
price_data_raw <- read.csv(file_path, sep = ";")

# Step 4: Clean the data
german_prices <- as_tibble(price_data_raw) %>%
  select(
    timestamp_raw = Start.date,
    # --- THIS IS THE CORRECTED COLUMN NAME ---
    price_eur_per_mwh = Germany.Luxembourg....MWh..Original.resolutions
  ) %>%
  mutate(
    timestamp_utc = mdy_hm(timestamp_raw, tz = "UTC"),
    # The price column might be loaded as a character, so we ensure it's numeric
    price_eur_per_mwh = as.numeric(price_eur_per_mwh)
  ) %>%
  drop_na() %>%
  select(timestamp_utc, price_eur_per_mwh)

# Step 5: Preview our clean data
print("Data for 2024 successfully loaded and cleaned!")
glimpse(german_prices)

# Step 6: Visualize the data
ggplot(german_prices, aes(x = timestamp_utc, y = price_eur_per_mwh)) +
  geom_line(color = "darkred", alpha = 0.8) +
  labs(
    title = "German Day-Ahead Electricity Prices for 2024",
    subtitle = "Data from ENTSO-E Transparency Platform",
    x = "Date",
    y = "Price (€ / MWh)"
  ) +
  theme_light()
-----------------------------------------
  # --- PHASE 1: SIMULATION ENGINE ---
  
  ## Step 2a: Create a function to model the EV battery
  
  # This function will be our "battery calculator." It takes the current
  # battery level and an action (like "charge"), and it returns the new
  # battery level, accounting for real-world energy losses.
  update_battery <- function(current_kwh,
                             capacity_kwh,
                             action = "hold", # Can be "charge", "discharge", or "drive"
                             kwh_amount = 0,
                             charge_efficiency = 0.9, # 90% efficient
                             discharge_efficiency = 0.9) {
    
    if (action == "charge") {
      energy_added <- kwh_amount * charge_efficiency
      new_kwh <- current_kwh + energy_added
      # The battery cannot be charged beyond its maximum capacity
      return(min(new_kwh, capacity_kwh))
    }
    else if (action == "discharge" || action == "drive") {
      energy_removed <- kwh_amount / discharge_efficiency
      new_kwh <- current_kwh - energy_removed
      # The battery cannot be drained below zero
      return(max(new_kwh, 0))
    }
    else { # "hold"
      return(current_kwh)
    }
  }

## Step 2b: Define the Driving Profile

# We'll create a simple schedule for a typical "Daily Commuter."
# This tells our simulation when the car is being driven and is unavailable.
daily_commuter_profile <- tibble(
  day_of_week = c("Mon", "Tue", "Wed", "Thu", "Fri"),
  morning_commute_hour = 8,  # Drives between 8 AM and 9 AM
  evening_commute_hour = 17, # Drives between 5 PM and 6 PM
  kwh_consumed_per_trip = 7.5 # Energy used for each one-way trip
)

# Print the profile to see what we've created
print("Created the rules for our simulation world:")
print(daily_commuter_profile)
-------------------------------------------------
  ## Step 3: Build the Main Simulation Loop
  
  # First, let's define the parameters for this specific simulation run
ev_battery_capacity_kwh <- 75  # A 75 kWh battery (like a Tesla Model 3 Long Range)
max_charge_rate_kw <- 11       # A standard 11 kW home charger
charge_cost_per_mwh <- 15      # A hypothetical grid fee/tax of €15 per MWh

# --- Initialize the Simulation ---
# We'll create a new table to store our results.
# We start by copying the price data and adding new columns to track our EV.
simulation_results <- german_prices %>%
  mutate(
    battery_kwh = 0.0,      # The battery's state of charge in kWh
    action_taken = "hold",  # What our agent decided to do
    cost_revenue_eur = 0.0  # The financial result of that action
  )

# Let's start the simulation with the battery 50% full
simulation_results$battery_kwh[1] <- ev_battery_capacity_kwh * 0.5

# --- Define the Agent's Strategy ---
# We'll use a simple but effective strategy:
# - Charge when the price is in the bottom 20% of all prices for the year.
# - Sell power when the price is in the top 20%.
lower_price_threshold <- quantile(simulation_results$price_eur_per_mwh, 0.20)
upper_price_threshold <- quantile(simulation_results$price_eur_per_mwh, 0.80)

# --- The Main Loop ---
# This loop will go from the 2nd hour to the very last hour of the year.
for (i in 2:nrow(simulation_results)) {
  
  # 1. Get the battery level from the PREVIOUS hour
  previous_kwh <- simulation_results$battery_kwh[i - 1]
  
  # 2. Get the information for the CURRENT hour
  current_timestamp <- simulation_results$timestamp_utc[i]
  current_price <- simulation_results$price_eur_per_mwh[i]
  current_hour <- hour(current_timestamp)
  current_day <- wday(current_timestamp, label = TRUE, abbr = TRUE)
  
  # 3. Check if the car is being driven right now
  is_driving <- FALSE
  kwh_consumed <- 0
  
  if (current_day %in% daily_commuter_profile$day_of_week) {
    if (current_hour == daily_commuter_profile$morning_commute_hour[1] ||
        current_hour == daily_commuter_profile$evening_commute_hour[1]) {
      is_driving <- TRUE
      kwh_consumed <- daily_commuter_profile$kwh_consumed_per_trip[1]
    }
  }
  
  # 4. The "Decision Agent" makes a choice
  if (is_driving) {
    action <- "drive"
    kwh_change <- kwh_consumed
    cost_rev <- 0 # No direct profit/loss from driving
    
  } else if (current_price <= lower_price_threshold) {
    # If the price is cheap, charge the car
    action <- "charge"
    kwh_change <- max_charge_rate_kw # Charge for one hour at the max rate
    # Cost = (price per MWh + grid fee) / 1000 * kWh charged
    cost_rev <- -((current_price + charge_cost_per_mwh) / 1000) * kwh_change
    
  } else if (current_price >= upper_price_threshold) {
    # If the price is expensive, sell power
    action <- "discharge"
    kwh_change <- max_charge_rate_kw # Sell for one hour at the max rate
    # Revenue = price per MWh / 1000 * kWh sold
    cost_rev <- (current_price / 1000) * kwh_change
    
  } else {
    # If the price is average, just wait
    action <- "hold"
    kwh_change <- 0
    cost_rev <- 0
  }
  
  # 5. Update the battery and store the results for this hour
  simulation_results$battery_kwh[i] <- update_battery(
    current_kwh = previous_kwh,
    capacity_kwh = ev_battery_capacity_kwh,
    action = action,
    kwh_amount = kwh_change
  )
  simulation_results$action_taken[i] <- action
  simulation_results$cost_revenue_eur[i] <- cost_rev
}

# --- Simulation Complete! ---
# Let's see what happened.
print("Simulation complete. Here is a summary of the results:")
glimpse(simulation_results)

# Calculate the total profit for the entire year
total_revenue <- sum(simulation_results$cost_revenue_eur)
print(paste("Total V2G Revenue for the Year:", round(total_revenue, 2), "EUR"))
--------------------------------------------------------------------------------
  # --- PHASE 2: GENERATING THE TRAINING DATASET ---
  
  ## Step 4: Create a reusable function for the V2G simulation
  
  # This function takes the EV specs as inputs and returns the final revenue
run_v2g_simulation <- function(
    ev_battery_capacity_kwh = 75,
    max_charge_rate_kw = 11,
    driving_profile_df = daily_commuter_profile,
    price_data_df = german_prices
  ) {
    
    # --- This is the same logic from Step 3, now inside a function ---
    
    charge_cost_per_mwh <- 15 
    
    simulation_results <- price_data_df %>%
      mutate(
        battery_kwh = 0.0,
        action_taken = "hold",
        cost_revenue_eur = 0.0
      )
    
    simulation_results$battery_kwh[1] <- ev_battery_capacity_kwh * 0.5
    
    lower_price_threshold <- quantile(simulation_results$price_eur_per_mwh, 0.20)
    upper_price_threshold <- quantile(simulation_results$price_eur_per_mwh, 0.80)
    
    for (i in 2:nrow(simulation_results)) {
      previous_kwh <- simulation_results$battery_kwh[i - 1]
      current_timestamp <- simulation_results$timestamp_utc[i]
      current_price <- simulation_results$price_eur_per_mwh[i]
      current_hour <- hour(current_timestamp)
      current_day <- wday(current_timestamp, label = TRUE, abbr = TRUE)
      
      is_driving <- FALSE
      kwh_consumed <- 0
      
      if (current_day %in% driving_profile_df$day_of_week) {
        if (current_hour == driving_profile_df$morning_commute_hour[1] ||
            current_hour == driving_profile_df$evening_commute_hour[1]) {
          is_driving <- TRUE
          kwh_consumed <- driving_profile_df$kwh_consumed_per_trip[1]
        }
      }
      
      if (is_driving) {
        action <- "drive"
        kwh_change <- kwh_consumed
        cost_rev <- 0
      } else if (current_price <= lower_price_threshold) {
        action <- "charge"
        kwh_change <- max_charge_rate_kw
        cost_rev <- -((current_price + charge_cost_per_mwh) / 1000) * kwh_change
      } else if (current_price >= upper_price_threshold) {
        action <- "discharge"
        kwh_change <- max_charge_rate_kw
        cost_rev <- (current_price / 1000) * kwh_change
      } else {
        action <- "hold"
        kwh_change <- 0
        cost_rev <- 0
      }
      
      simulation_results$battery_kwh[i] <- update_battery(
        current_kwh = previous_kwh,
        capacity_kwh = ev_battery_capacity_kwh,
        action = action,
        kwh_amount = kwh_change
      )
      simulation_results$action_taken[i] <- action
      simulation_results$cost_revenue_eur[i] <- cost_rev
    }
    
    # The function now RETURNS the final calculated revenue
    total_revenue <- sum(simulation_results$cost_revenue_eur)
    return(total_revenue)
  }

# Let's test our new function with the same parameters as before
# We expect to get the same result: 1872.64
test_revenue <- run_v2g_simulation(
  ev_battery_capacity_kwh = 75,
  max_charge_rate_kw = 11
)

print(paste("Test run revenue:", round(test_revenue, 2), "EUR"))
-------------------------------------------------------------------
  # --- PHASE 2: GENERATING THE TRAINING DATASET ---
  
  ## Step 5: Generate many simulation scenarios to create our ML dataset
  
  # First, create a "grid" of all the parameter combinations we want to test.
  # We'll test a range of battery sizes and charger speeds.
simulation_parameters <- expand_grid(
    ev_battery_capacity_kwh = seq(from = 40, to = 100, by = 5), # From 40 to 100 kWh in 5 kWh steps
    max_charge_rate_kw = seq(from = 7, to = 22, by = 1)      # From 7 to 22 kW in 1 kW steps
  )

print("Created parameter grid for simulation:")
print(head(simulation_parameters))
print(paste("Total number of simulations to run:", nrow(simulation_parameters)))


# Now, run the simulation for each row of our parameter grid.
# We use purrr::map2_dbl which is a tidy way to apply a function to two changing inputs.
print("Running simulations... please wait.")

training_dataset <- simulation_parameters %>%
  mutate(
    annual_v2g_revenue_eur = map2_dbl(
      .x = ev_battery_capacity_kwh,
      .y = max_charge_rate_kw,
      .f = ~ run_v2g_simulation(
        ev_battery_capacity_kwh = .x,
        max_charge_rate_kw = .y
      )
    )
  )

print("All simulations complete!")
print("Here is a preview of your final training dataset:")
print(head(training_dataset))
