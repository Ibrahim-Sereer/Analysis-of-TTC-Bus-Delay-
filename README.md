# Analysis of TTC Bus Delay Data for 2022

## Table of Contents
- [Project Overview](#project-overview)
- [Tools](#tools)
- [Data Preparation](#data-preparation)
  - [Data Exploration](#data-exploration)
  - [Data Cleaning and Feature Creation](#data-cleaning-and-feature-creation)
- [Exploratory Data Analysis (EDA)](#exploratory-data-analysis-eda)
- [Data Analysis Code](#data-analysis-code)
- [Results and Findings](#results-and-findings)
- [Recommendations](#recommendations)
- [Limitations](#limitations)
- [External Repository Access](#external-repository-access)
- [References](#references)

### Project Overview
This project analyzes TTC bus delay data for the year 2022. The dataset includes details about the date, time, route, and incident causing the delay. The aim is to identify patterns in the delays—such as which times of day or days of the week have the most delays and the most common causes. The analysis demonstrates skills in data cleaning, manipulation, visualization, and statistical analysis.

![Rplot05](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/a92c0232-b1fe-49bb-874c-fb17ca7b4d88)
![Rplot01](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/3a94ceb1-47d9-4627-9260-9098520caa8b)
![Rplot](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/9001119f-3ae5-4c75-b572-e56f50f46c29)
![Rplot02](https://github.com/NeoSphereAnalytics/Analysis-of-TTC-Bus-Delay-/assets/174109528/c96ba39b-334b-4116-841d-c863be680154)

### Tools
- R – Data analysis and visualization ([download here](https://www.r-project.org/))

### Data Preparation

#### Data Exploration
1. Loaded the dataset from the specified file path.
2. Checked if the dataset is empty and inspected the first few rows.
3. Summarized the dataset to understand the distribution and basic statistics of each variable.
4. Checked the structure and types of variables in the dataset.

#### Data Cleaning and Feature Creation
1. Converted the `Date` column to `Date` format.
2. Converted the `Time` column to `POSIXct` format and extracted the hour from the time.
3. Removed rows with `NA` values in the `Hour` column.
4. Grouped the data by hour to count the frequency of delays.
5. Grouped the data by location to count the frequency of delays at different locations.

### Exploratory Data Analysis (EDA)

**Total Aggregates**
- What is the total number of bus delays recorded in the dataset?
- What is the distribution of delays by the hour of the day?
- What are the top 20 locations with the most bus delays?

**Hourly Distribution**
- What is the total number of delays for each hour of the day?
- What is the hour with the most delays?
- How does the number of delays vary across different hours of the day?

**Location-Specific Insights**
- Which locations have the highest number of bus delays?
- What are the top 20 locations with the most delays?
- How does the frequency of delays vary across different locations?

**Cause of Delays Exploration**
- What are the most common causes of bus delays?
- How does the frequency of delays vary by different causes?
- Which causes of delays are most frequent?

### Data Analysis Code

```R
# Load necessary libraries
library(tidyverse)
library(lubridate)
library(ggplot2)

# Define file path
data_path <- "C:/Users/Owner/Downloads/archive/ttc-bus-delay-data-2022.csv"

# Check if the file exists
if (file.exists(data_path)) {
  # Load the dataset
  data <- read.csv(data_path)
  
  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("The dataset is empty.")
  }
  
  # Inspect the dataset to ensure it's loaded correctly
  print("First few rows of the dataset:")
  print(head(data))
  print("Structure of the dataset:")
  print(str(data))
  print("Summary of the dataset:")
  print(summary(data))
  
  # Data Cleaning and Preparation
  # Convert date column to Date format
  data$Date <- as.Date(data$Date, format = "%d-%b-%y")
  
  # Convert time column to hms format
  data$Time <- parse_date_time(data$Time, orders = "H:M")
  
  # Check if the Date and Time conversions are successful
  print("Summary of Date column after conversion:")
  print(summary(data$Date))
  print("Summary of Time column after conversion:")
  print(summary(data$Time))
  
  # Extract hour from time, remove rows with NA values in Hour
  data <- data %>%
    mutate(Hour = hour(Time)) %>%
    filter(!is.na(Hour))
  
  # Check the Hour column
  print("First few values in the Hour column:")
  print(head(data$Hour))
  print("Summary of the Hour column:")
  print(summary(data$Hour))
  
  # Group by Hour to count the frequency of delays
  hourly_delays <- data %>%
    group_by(Hour) %>%
    summarise(Total_Delays = n()) %>%
    arrange(Hour)
  
  # Visualize the delays by hour
  ggplot(hourly_delays, aes(x = Hour, y = Total_Delays)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(title = "Total Bus Delays by Hour of the Day",
         x = "Hour of the Day",
         y = "Total Delays") +
    theme_minimal()
  
  # Display the results
  print("Total Bus Delays by Hour of the Day:")
  print(hourly_delays)
  
  # Additional Analysis: Causes of delays
  # Group by Incident to count the frequency of each cause of delay
  cause_delays <- data %>%
    group_by(Incident) %>%
    summarise(Frequency = n()) %>%
    arrange(desc(Frequency))
  
  # Visualize the causes of delays
  ggplot(cause_delays, aes(x = reorder(Incident, Frequency), y = Frequency)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Frequency of Causes of Bus Delays",
         x = "Cause of Delay",
         y = "Frequency") +
    theme_minimal()
  
  # Display the results
  print("Causes of Bus Delays from Highest Frequency to Lowest:")
  print(cause_delays)
  
  # Ensure the Weekday column is present and correctly formatted
  if ("Day" %in% colnames(data)) {
    # Rename the 'Day' column to 'Weekday'
    data <- data %>%
      rename(Weekday = Day)
    
    # Additional Analysis: Delays by weekdays
    # Group by Weekday to count the frequency of delays
    weekday_delays <- data %>%
      group_by(Weekday) %>%
      summarise(Total_Delays = n()) %>%
      arrange(factor(Weekday, levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")))
    
    # Visualize the delays by weekdays
    ggplot(weekday_delays, aes(x = reorder(Weekday, Total_Delays), y = Total_Delays)) +
      geom_bar(stat = "identity", fill = "steelblue") +
      coord_flip() +
      labs(title = "Total Bus Delays by Weekdays",
           x = "Weekday",
           y = "Total Delays") +
      theme_minimal()
    
    # Display the results
    print("Total Bus Delays by Weekdays:")
    print(weekday_delays)
  } else {
    print("The dataset does not contain a 'Day' column, so the analysis by weekdays could not be performed.")
  }
} else {
  print("The file does not exist. Please check the file path.")
}

# Define file path
data_path <- "C:/Users/Owner/Downloads/archive/ttc-bus-delay-data-2022.csv"

# Check if the file exists
if (file.exists(data_path)) {
  # Load the dataset
  data <- read.csv(data_path)
  
  # Check if the dataset is empty
  if (nrow(data) == 0) {
    stop("The dataset is empty.")
  }
  
  # Inspect the dataset to ensure it's loaded correctly
  print("First few rows of the dataset:")
  print(head(data))
  print("Structure of the dataset:")
  print(str(data))
  print("Summary of the dataset:")
  print(summary(data))
  
  # Data Cleaning and Preparation
  # Convert date column to Date format
  data$Date <- as.Date(data$Date, format = "%d-%b-%y")
  
  # Convert time column to POSIXct format
  data$Time <- strptime(data$Time, format="%H:%M")
  
  # Extract hour from time, remove rows with NA values in Hour
  data <- data %>%
    mutate(Hour = hour(Time))
  
  # Check if the Date and Hour extractions are successful
  print("Summary of Date column after conversion:")
  print(summary(data$Date))
  print("Summary of Hour column after extraction:")
  print(summary(data$Hour))
  
  # Group by Hour to count the frequency of delays
  hourly_delays <- data %>%
    group_by(Hour) %>%
    summarise(Total_Delays = n()) %>%
    arrange(Hour)
  
  # Visualize the delays by hour
  ggplot(hourly_delays, aes(x = Hour, y = Total_Delays)) +
    geom_line(color = "steelblue", linewidth = 1) +
    geom_point(color = "steelblue", size = 2) +
    labs(title = "Total Bus Delays by Hour of the Day",
         x = "Hour of the Day",
         y = "Total Delays") +
    theme_minimal()
  
  # Display the results
  print("Total Bus Delays by Hour of the Day:")
  print(hourly_delays)
  
  # Determine the hour with the most delays
  max_delay_hour <- hourly_delays %>%
    filter(Total_Delays == max(Total_Delays)) %>%
    pull(Hour)
  
  print(paste("The hour with the most delays is:", max_delay_hour))
} else {
  print("The file does not exist. Please check the file path.")
  
  
  # Additional Analysis: Locations with most delays
  # Group by Location to count the frequency of delays
  location_delays <- data %>%
    group_by(Location) %>%
    summarise(Total_Delays = n()) %>%
    arrange(desc(Total_Delays))
  
  # Select top 20 locations with most delays
  top_location_delays <- location_delays %>% 
    top_n(20, Total_Delays)
  
  # Visualize the locations with the most delays
  ggplot(top_location_delays, aes(x = reorder(Location, Total_Delays), y = Total_Delays)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    coord_flip() +
    labs(title = "Top 20 Locations with the Most Bus Delays",
         x = "Location",
         y = "Total Delays") +
    theme_minimal()
  
  # Display the results
  print("Top 20 Locations with the Most Bus Delays from Highest to Lowest:")
  print(top_location_delays)
  
```

### Results and Findings

**Total Aggregates**
- The dataset contains 27,351 delay records.
- Delays are distributed across various hours of the day, with peaks observed during specific times.

**Hourly Distribution**
- The hour with the most delays is 14:00 (2 PM), indicating a significant midday peak.
- Additional peaks occur during early morning and evening hours.

**Location-Specific Insights**
- Kipling Station has the highest number of delays, followed by Kennedy Station and Pioneer Village Station.
- The top 20 locations with the most delays are primarily major transit hubs and intersections.

**Causes of Bus Delays**
- The most frequent cause of bus delays is "Operations - Operator" with 9,554 instances.
- Other significant causes include mechanical issues (7,979 instances) and collisions involving TTC vehicles (1,823 instances).

### Recommendations

- **Focus on mitigating peak-hour delays:** Implement measures to reduce delays around 14:00 and other peak times.
- **Target key locations:** Address issues at high-delay locations such as Kipling and Kennedy Stations with targeted operational improvements.
- **Address common causes:** Reduce delays caused by operator and mechanical issues through enhanced training and maintenance protocols.
- **Enhance real-time monitoring:** Improve monitoring and response strategies to quickly address and mitigate delays as they occur.
- **Conduct further analysis:** Explore underlying factors contributing to delays at specific times and locations, including external influences like weather and traffic conditions.

### Limitations

- **Temporal scope:** The dataset is limited to 2022 and may not capture trends over multiple years.
- **Operational factors:** Analysis is based on reported delays without considering other operational factors that might influence delay patterns.
- **Data quality:** Data quality and completeness may affect accuracy; inconsistencies or gaps may exist in reported values.
- **External factors:** The analysis does not account for the impact of external factors such as weather, traffic conditions, or special events.
- **Generalization:** Findings may not generalize to other transit systems or regions because contributing factors vary widely.

### External Repository Access

An attempt was made to access the repository at https://github.com/Ibrahim-Sereer/Global-Data-Analysis-, but the request was blocked by the network proxy with a 403 response. External repositories cannot be reached from this environment without additional network access.

### References

- Toronto Bus Delay 2022: Toronto Transit Commission (TTC) delay data - Bus. Available on Kaggle from [here](https://www.kaggle.com/datasets/reihanenamdari/toronto-bus-delay-2022).
- R Core Team (2023). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL (https://www.R-project.org/)


