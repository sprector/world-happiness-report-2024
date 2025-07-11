library(dplyr)
library(ggplot2)
library(janitor)
library(corrplot)

## Cleaning Column Names w/ Janitor
World_happiness_report_updated_2024 <- World_happiness_report_updated_2024 %>%
  clean_names()

## EDA

# Summary Stats
summary(World_happiness_report_updated_2024)

# Standard Deviation Table
standard_deviations <- World_happiness_report_updated_2024 %>%
  summarise(lifeladdersd = sd(life_ladder, na.rm=TRUE),
            loggdpsd = sd(log_gdp_per_capita, na.rm=TRUE),
            socialsupportsd = sd(social_support, na.rm=TRUE),
            lifeexpectancysd = sd(healthy_life_expectancy_at_birth, na.rm=TRUE),
            freedomsd = sd(freedom_to_make_life_choices, na.rm=TRUE),
            generositysd = sd(generosity, na.rm=TRUE),
            corruptionsd = sd(perceptions_of_corruption, na.rm=TRUE),
            possd = sd(positive_affect, na.rm=TRUE),
            negsd = sd(negative_affect, na.rm=TRUE))

print(standard_deviations)

# Life Ladders Plots
ggplot(World_happiness_report_updated_2024, aes(x = life_ladder)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Life Ladders",
       x = "Life Ladder",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = life_ladder)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Life Ladders",
       y = "Life Ladder") +
  theme_minimal()

# Log GDP per Capita Plots
ggplot(World_happiness_report_updated_2024, aes(x = log_gdp_per_capita)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  labs(title = "Histogram of Log GDP per Capita",
       x = "Log GDP per Capita",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = log_gdp_per_capita)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Log GDP per Capita",
       y = "Log GDP per Capita") +
  theme_minimal()

# Social Support Plots
ggplot(World_happiness_report_updated_2024, aes(x = social_support)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Social Support",
       x = "Social Support",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = social_support)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Social Support",
       y = "Social Support") +
  theme_minimal()

# Life Expectancy at Birth Plots
ggplot(World_happiness_report_updated_2024, aes(x = healthy_life_expectancy_at_birth)) +
  geom_histogram(binwidth = 4, fill = "blue", color = "black") +
  labs(title = "Histogram of Healthy Life Expectancy at Birth",
       x = "Healthy Life Expectancy at Birth",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = healthy_life_expectancy_at_birth)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Healthy Life Expectancy at Birth",
       y = "Healthy Life Expectancy") +
  theme_minimal()

# Freedom to Make Life Choices Plots
ggplot(World_happiness_report_updated_2024, aes(x = freedom_to_make_life_choices)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Freedom to Make Life Choices",
       x = "Freedom to Make Life Choices",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = freedom_to_make_life_choices)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Freedom to Make Life Choices",
       y = "Freedom to Make Life Choices") +
  theme_minimal()

# Generosity Plots
ggplot(World_happiness_report_updated_2024, aes(x = generosity)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Generosity",
       x = "Generosity",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = generosity)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Generosity",
       y = "Generosity") +
  theme_minimal()

# Perceptions of Corruption Plots
ggplot(World_happiness_report_updated_2024, aes(x = perceptions_of_corruption)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Perceptions of Corruption",
       x = "Perceptions of Corruption",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = perceptions_of_corruption)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Perceptions of Corruption",
       y = "Perceptions of Corruption") +
  theme_minimal()

# Positive Affect Plots
ggplot(World_happiness_report_updated_2024, aes(x = positive_affect)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Positive Affect",
       x = "Positive Affect",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = positive_affect)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Positive Affect",
       y = "Positive Affect") +
  theme_minimal()

# Negative Affect Plots
ggplot(World_happiness_report_updated_2024, aes(x = negative_affect)) +
  geom_histogram(binwidth = 0.05, fill = "blue", color = "black") +
  labs(title = "Histogram of Negative Affect",
       x = "Negative Affect",
       y = "Frequency") +
  theme_minimal()

ggplot(World_happiness_report_updated_2024, aes(y = negative_affect)) +
  geom_boxplot(fill = "blue", color = "black") +
  labs(title = "Box Plot of Negative Affect",
       y = "Negative Affect") +
  theme_minimal()

## RESEARCH QUESTION 1

# Group by Country -> Get mean Log GDP per Capita
mean_gdp <- World_happiness_report_updated_2024 %>%
  group_by(country_name)%>%
  summarise(meangdp = mean(log_gdp_per_capita))

# Create top 5 richest table and arrange in descending order
top_5_richest = mean_gdp %>%
  arrange(desc(meangdp)) %>%
  head(5)

# Top 5 Mean GDP's Countries Bar Plot
ggplot(top_5_richest, aes(x = reorder(country_name, -meangdp), y = meangdp, fill = country_name)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(meangdp, 3)), vjust = -0.5, size = 5) +  # Add values on top of bars
  labs(title = "Top 5 Mean GDP's (2008-2023)",
       x = "Country",
       y = "Log GDP per Capita") +
  theme_minimal() +
  theme(legend.position = "none")

top_5_richest

## RESEARCH QUESTION 2

# Create top 5 poorest table and arrange in ascending order
top_5_poorest = mean_gdp %>%
  arrange(meangdp) %>%
  head(5)

# Bottom 5 Mean GDP's Bar Plot
ggplot(top_5_poorest, aes(x = reorder(country_name, meangdp), y = meangdp, fill = country_name)) +
  geom_bar(stat = "identity", color = "black") +
  geom_text(aes(label = round(meangdp, 3)), vjust = -0.5, size = 5) +  # Add values on top of bars
  labs(title = "Bottom 5 Mean GDP's (2008-2023)",
       x = "Country",
       y = "Log GDP per Capita") +
  theme_minimal() +
  theme(legend.position = "none")

top_5_poorest

## RESEARCH QUESTION 3

# Log GDP per Capita Density Plot
ggplot(World_happiness_report_updated_2024, aes(x = log_gdp_per_capita)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(title = "Density Plot of Log GDP per Capita",
       x = "Log GDP per Capita",
       y = "Density") +
  theme_minimal()

# Log GDP per Capita Box Plot
ggplot(World_happiness_report_updated_2024, aes(y = log_gdp_per_capita)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  labs(title = "Box Plot of Log GDP per Capita",
       y = "Log GDP per Capita") +
  theme_minimal() 

# Summary Stats
summary(World_happiness_report_updated_2024$log_gdp_per_capita)
sd(World_happiness_report_updated_2024$log_gdp_per_capita, na.rm=TRUE)

## RESEARCH QUESTION 4

# Creation of correlation matrix, all numeric variables
cor_matrix = World_happiness_report_updated_2024 %>%
  select(life_ladder, log_gdp_per_capita, social_support, healthy_life_expectancy_at_birth, generosity, perceptions_of_corruption, freedom_to_make_life_choices, positive_affect, negative_affect)%>%
  cor(use = 'complete.obs')
round(cor_matrix, 2)

print(cor_matrix)

# Plotting the matrix (code similar to 2021 demo)
corrplot(cor_matrix, method = "circle", type = "upper", tl.col = "black",
         title = "Correlation Matrix of Happiness Factors", mar = c(0, 0, 1, 0))


## RESEARCH QUESTION 5

# Grouping by country -> finding mean of factors generosity and freedom
# This step simplifies graphs (less points on scatter)
mean_factors <- World_happiness_report_updated_2024 %>%
  group_by(country_name) %>%
  summarise(meangenerosity = mean(generosity, na.rm=TRUE),
            meanfreedom = mean(freedom_to_make_life_choices, na.rm=TRUE))

# Creation of scatterplot w/ trend line
ggplot(mean_factors, aes(x = meanfreedom, y = meangenerosity)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Generosity vs Freedom",
  x = "Freedom to Make Life Choices",
  y = "Generosity") +
  theme_minimal()
              
# Correlation coefficient                
cor(mean_factors$meanfreedom, mean_factors$meangenerosity, use = 'complete.obs')

## RESEARCH QUESTION 6

# Grouping by country -> finding mean of factors GDP and Corruption
# This step simplifies graphs (less points on scatter)
mean_factors2 <- World_happiness_report_updated_2024 %>%
  group_by(country_name) %>%
  summarise(meangdp = mean(log_gdp_per_capita),
            meancorruption = mean(perceptions_of_corruption))

# Creation of scatterplot w/ trend line
ggplot(mean_factors2, aes(x = meancorruption, y = meangdp)) +
  geom_point(size = 1, alpha = 0.7) +
  geom_smooth(method = "lm", color = "black", se = FALSE) +
  labs(title = "Corruption vs Log GDP per Capita",
       x = "Perceptions of Corruption",
       y = "Log GDP per Capita") +
  theme_minimal()

# Correlation coefficient                
cor(mean_factors2$meancorruption, mean_factors2$meangdp, use = 'complete.obs')

## RESEARCH QUESTION 7

# Group by year -> find mean happiness
mean_happiness_over_time <- World_happiness_report_updated_2024 %>%
  group_by(year) %>%
  summarise(meanhappiness = mean(life_ladder),
            count = n()) %>%
  filter(year > 2005) # 2005 has significantly smaller sample size than the rest

# Creating mean happiness over time line graph (zoomed out)
ggplot(mean_happiness_over_time, aes(x = year, y = meanhappiness)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Mean Life Ladder Over the Years",
       x = "Year",
       y = "Life Ladder") +
  ylim(0, max(mean_happiness_over_time$meanhappiness) * 1.1) + # Zooming out the axis, to avoid distortion
  theme_minimal()

# Creating mean happiness over time line graph (zoomed in)
ggplot(mean_happiness_over_time, aes(x = year, y = meanhappiness)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Mean Life Ladder Over the Years",
       x = "Year",
       y = "Life Ladder") +
  theme_minimal() # No zooming out, to also give perspective on more granular changes

print(mean_happiness_over_time)

## RESEARCH QUESTION 8

# Group by year -> find mean Log GDP per Capita
mean_gdp_over_time <- World_happiness_report_updated_2024 %>%
  group_by(year) %>%
  summarise(meangdp = mean(log_gdp_per_capita, na.rm=TRUE),
            count = n()) %>%
  filter(year > 2005)

# Creating mean happiness over time line graph (zoomed out) 
ggplot(mean_gdp_over_time, aes(x = year, y = meangdp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Mean Log GDP per Capita Over the Years",
       x = "Year",
       y = "Log GDP per Capita") +
  ylim(0, max(mean_gdp_over_time$meangdp) * 1.1) +
  theme_minimal()

# Creating mean happiness over time line graph (zoomed in)
ggplot(mean_gdp_over_time, aes(x = year, y = meangdp)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Mean Log GDP per Capita Over the Years",
       x = "Year",
       y = "Log GDP per Capita") +
  theme_minimal()

print(mean_gdp_over_time)

## RESEARCH QUESTION 9

# Group by year -> find mean Log GDP per Capita
mean_corruption_over_time <- World_happiness_report_updated_2024 %>%
  group_by(year) %>%
  summarise(meancorruption = mean(perceptions_of_corruption, na.rm=TRUE),
            count = n()) %>%
  filter(year > 2005)

# Creating mean happiness over time line graph (zoomed out) 
ggplot(mean_corruption_over_time, aes(x = year, y = meancorruption)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Mean Perception of Corruption Over the Years",
       x = "Year",
       y = "Perception of Corruption") +
  ylim(0, max(mean_corruption_over_time$meancorruption) * 1.1) +
  theme_minimal()

# Creating mean happiness over time line graph (zoomed in)
ggplot(mean_corruption_over_time, aes(x = year, y = meancorruption)) +
  geom_line(size = 1) +
  geom_point() +
  labs(title = "Mean Perception of Corruption Over the Years",
       x = "Year",
       y = "Perception of Corruption") +
  theme_minimal()

print(mean_corruption_over_time)
