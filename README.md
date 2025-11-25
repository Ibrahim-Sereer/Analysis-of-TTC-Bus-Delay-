# Analysis of TTC Bus Delay Data (2022)

## Table of Contents
- [Project Overview](#project-overview)
- [Tools](#tools)
- [Data Preparation](#data-preparation)
- [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
- [Analysis Script](#analysis-script)
- [Results](#results)
- [Recommendations](#recommendations)
- [Limitations](#limitations)
- [References](#references)

## Project Overview
This project analyzes Toronto Transit Commission (TTC) bus delay records for 2022. The dataset lists the date, time, route, location, and incident that triggered each delay. The goal is to surface patterns—such as the busiest hours, top locations, and most common causes—and to demonstrate skills in data cleaning, manipulation, visualization, and descriptive analysis.

![Hourly delays](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/a92c0232-b1fe-49bb-874c-fb17ca7b4d88)
![Daily delays](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/3a94ceb1-47d9-4627-9260-9098520caa8b)
![Top causes](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/9001119f-3ae5-4c75-b572-e56f50f46c29)
![Top locations](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/c96ba39b-334b-4116-841d-c863be680154)

## Tools
- R for data wrangling and visualization ([download](https://www.r-project.org/))
- `tidyverse` and `lubridate` for manipulation and date-time handling
- `ggplot2` for charts

## Data Preparation
**Exploration**
1. Load the CSV file and verify it is not empty.
2. Inspect the first rows, structure, and summary statistics.

**Cleaning and transformation**
1. Convert `Date` to `Date` format and `Time` to a parsed datetime.
2. Derive an `Hour` column and drop rows where the hour cannot be parsed.
3. Aggregate delays by hour, by location, and by incident type.

## Exploratory Data Analysis (EDA)
Key guiding questions:
- How many delays are recorded overall?
- How are delays distributed by hour of day?
- Which locations and incidents account for the most delays?

## Analysis Script
The snippet below assumes the CSV file resides alongside this README as `ttc-bus-delay-data-2022.csv`.

```r
library(tidyverse)
library(lubridate)

# Load data
file_path <- "ttc-bus-delay-data-2022.csv"
stopifnot(file.exists(file_path))
data <- read.csv(file_path, stringsAsFactors = FALSE)
stopifnot(nrow(data) > 0)

# Parse and enrich
cleaned <- data %>%
  mutate(
    Date = as.Date(Date, format = "%d-%b-%y"),
    Time = parse_date_time(Time, orders = "H:M"),
    Hour = hour(Time)
  ) %>%
  drop_na(Hour)

# Aggregations
hourly_delays <- cleaned %>%
  count(Hour, name = "Total_Delays")

cause_delays <- cleaned %>%
  count(Incident, name = "Frequency") %>%
  arrange(desc(Frequency))

location_delays <- cleaned %>%
  count(Location, name = "Total_Delays") %>%
  arrange(desc(Total_Delays)) %>%
  slice_head(n = 20)

# Example visualization
hourly_delays %>%
  ggplot(aes(Hour, Total_Delays)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  labs(
    title = "Total Bus Delays by Hour of the Day",
    x = "Hour of Day",
    y = "Total Delays"
  ) +
  theme_minimal()
```

## Results
- **Volume:** 27,351 delay records were logged in 2022.
- **Hourly patterns:** Delays peak at 14:00 (2 PM) with additional spikes in the early morning and evening.
- **Locations:** Kipling Station, Kennedy Station, and Pioneer Village Station experience the most delays; the top 20 locations are primarily major transit hubs and intersections.
- **Causes:** "Operations - Operator" is the leading cause (9,554 instances), followed by mechanical issues (7,979) and TTC collisions (1,823).

## Recommendations
- **Mitigate peak-hour delays:** Focus staffing and dispatch adjustments around 14:00 and other high-volume periods.
- **Target key locations:** Prioritize operational improvements at high-delay hubs such as Kipling and Kennedy Stations.
- **Address frequent causes:** Invest in operator training and preventative maintenance to reduce operator- and mechanical-related delays.
- **Improve real-time response:** Enhance monitoring and incident response to resolve delays quickly as they arise.
- **Deepen diagnostics:** Explore external drivers (weather, traffic, events) for a more holistic understanding of delay patterns.

## Limitations
- The analysis is limited to the 2022 dataset and may not reflect longer-term trends.
- Results depend on the completeness and consistency of reported delay data.
- External factors (e.g., weather or special events) are not modeled here.
- Findings may not generalize to other transit systems with different operating conditions.

## References
- Toronto Transit Commission bus delay data (2022) on Kaggle: [dataset link](https://www.kaggle.com/datasets/reihanenamdari/toronto-bus-delay-2022)
- R Core Team (2023). *R: A language and environment for statistical computing*. R Foundation for Statistical Computing. [https://www.R-project.org/](https://www.R-project.org/)
