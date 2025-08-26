# -Predicting-Vehicle-to-Grid-V2G-Revenue-Potential-in-Germany

##  Project Overview

This project analyzes the financial viability of **Vehicle-to-Grid (V2G)** technology in the German energy market. It features a complete data science pipeline built in **R** that goes from raw, real-world data to a fully interactive predictive web application.

The core goal is to answer the question: **"How much money could an electric vehicle owner in Germany make per year by selling power back to the grid?"**

The final output is an interactive **R Shiny** application that allows users to input their EV's battery size and home charger speed to get an instant, data-driven prediction of their potential annual revenue.



##  Methodology

The project is broken down into two main phases: **Simulation** and **Prediction**.

### 1. Simulation Engine
First, a detailed simulation engine was built to model an EV's interaction with the power grid for a full year.
* **Data:** The simulation is driven by real **hourly day-ahead electricity price data** for Germany in 2024, sourced from the ENTSO-E transparency platform.
* **Logic:** A rule-based "Decision Agent" was created to make intelligent financial decisions every hour. It charges the EV battery when prices are in the bottom 20% (cheap) and discharges (sells) power back to the grid when prices are in the top 20% (expensive), all while respecting the daily driving needs of a typical commuter.

### 2. Predictive Model
The simulation is accurate but slow. To create an instant prediction tool, a machine learning model was trained on the simulation's results.
* **Data Generation:** The simulation engine was run **208 times** with a wide range of EV battery capacities (40-100 kWh) and charger speeds (7-22 kW) to generate a unique training dataset.
* **Machine Learning:** An **XGBoost regression model** was trained on this dataset using the `tidymodels` framework. The model learned the complex relationship between an EV's technical specifications and its annual revenue potential.
* **Performance:** The final model is extremely accurate, achieving an **R-squared of 1.00** on the test set, meaning it can perfectly replicate the results of the detailed simulation.

---

##  Key Technologies Used

* **Programming Language:** R
* **Core Packages:** `tidyverse`, `lubridate` for data manipulation.
* **Machine Learning:** `tidymodels`, `xgboost` for model training and evaluation.
* **Interactive Dashboard:** `shiny` for building the web application.

---

## How to Use

To run this project:
1.  Ensure you have R and RStudio installed.
2.  Place the `Day-ahead_prices_2024...csv` file in the project directory.
3.  Install the required packages.
4.  Run the main R script to perform the simulation and train the model.
5.  Run the `app.R` file to launch the interactive Shiny dashboard.
