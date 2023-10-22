# Rshiny-Vehicle-Colliisions-Dasboard
Shiny Dashboard for Accident Data Analysis
Shiny Logo

Overview
This Shiny Dashboard is a powerful tool for data analysis and visualization, specifically designed for analyzing accident data. It offers a user-friendly interface for exploring accident-related information, making data-driven decisions, and gaining insights from the dataset. The dashboard is equipped with interactive components, allowing users to customize their analysis based on specific criteria.

Features
Data Upload: Users can upload their own accident data or use the default dataset provided (in this case, "new.csv").

Filter by Number of Injured Persons: A slider enables users to filter accidents based on the number of injured persons, helping to focus on severe accidents.

Select Hour of the Day: Users can analyze accidents occurring at a specific hour of the day using a slider.

Death Victim Type: A dropdown menu allows users to select the victim type for analyzing fatalities among motorists, pedestrians, and cyclists.

Injured Victim Type: Similar to death victim type, this dropdown helps users analyze the injuries sustained by motorists, pedestrians, or cyclists.

Street Map: Displays accident locations on an interactive map with customizable markers. Users can explore accident details by clicking on the markers.

Histogram: Visualizes the data using interactive histograms.

Top Risky Streets: A table displaying streets with the highest number of injuries or deaths, depending on the selected victim type.

Top Fatal Streets: Provides insights into streets with the highest number of fatalities based on the chosen victim type.

Vehicle Type in Accidents: Shows the distribution of vehicle types involved in accidents.

Causes of Accidents: A bar chart representing the top contributing factors for vehicle accidents.

Data Overview: A table presenting the raw data, allowing users to explore the dataset.

Hourly Map: Offers an interactive map view specifically tailored to the selected hour.

Installation and Deployment
Clone this repository to your local machine or download the files.

Ensure you have R and RStudio installed on your system.

Open the R project in RStudio.

Install the necessary R packages if you haven't already:

R
Copy code
install.packages(c("shiny", "leaflet", "dygraphs", "dplyr", "lubridate", "DT", "plotly", "ggplot2"))
Launch the Shiny app by running the following code in the R console:

R
Copy code
library(shiny)
shiny::runApp("path/to/your/app")
Replace "path/to/your/app" with the full path to the directory containing the Shiny app files.

The Shiny dashboard should open in your default web browser.


Maintained by:

Adrian Kasito



I hope this Shiny Dashboard simplifies your accident data analysis and provides valuable insights into accident-related information. Thank you


Happy Analyzing! 🚗📊👨‍💼
