# Death-Rates-for-Suicide-by-Sex-Race-Hispanic-Origin-and-Age

<https://catalog.data.gov/dataset/death-rates-for-suicide-by-sex-race-hispanic-origin-and-age-united-states-020c1>

## Group Members: Sheridan Harney, Hely Bhavsar, and Jasjeevan Kaur
## Introduction

This is a project analyzing suicide death rates.

## Death Rates Questions

1. Has the suicide estimate changed over time?
2. How do suicide rate estimates differ across young adult age groups?


## Running Code

```{r}
library(readr)

Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States <- read_csv("data/Death_rates_for_suicide__by_sex__race__Hispanic_origin__and_age__United_States.csv")

install.packages("ggplot2")

library(ggplot2)
```

```{r}
ggplot(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States, aes(x = ESTIMATE)) +
  geom_histogram(binwidth = 1, fill = "lightblue", color = "black") +
  labs(title = "Distribution of Suicide Rates", x = "Suicide Rate (per 100,000)", y = "Frequency")

  ggplot(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States, aes(x = STUB_LABEL, y = INDICATOR, fill = STUB_LABEL)) +
    geom_boxplot() +
    labs(title = "Suicide Rates by Race", x = "race", y = "Suicide Rate (per 100,000)")
  
```

#Mean, Median, Mode, Standard Deviation, Min and Max
```{r}
install.packages("dplyr")
  library(dplyr)
  
  # Mean
mean(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE, na.rm = TRUE)

# Median
median(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE, na.rm = TRUE)

# Mode (base R way)
get_mode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
get_mode(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE)

# Standard Deviation
sd(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE, na.rm = TRUE)

# Minimum and Maximum
min(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE, na.rm = TRUE)
max(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE, na.rm = TRUE)

    colnames(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States)
```

##Anova
```{r}
str(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States)         
    unique(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$STUB_LABEL) 
  
  
    aov_result <- aov(ESTIMATE ~ STUB_LABEL, data = Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States)
    summary(aov_result)
```


```{r}

    correlation_result <- cor.test(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$"AGE_NUM", Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$ESTIMATE)
    print(correlation_result)
    
    
    Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$STUB_LABEL <- as.factor(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$STUB_LABEL)
    
  
    model <- lm(ESTIMATE ~ STUB_LABEL, data = Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States)

    summary(model)
    
    colnames(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States)
```

##Questions

1.  Has the suicide estimate changed over time?

```{r}

    library(ggplot2)
library(dplyr)  

# First: create male_female_data
male_female_data <- Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States %>%
  filter(STUB_LABEL %in% c("Male", "Female"))


lm_model <- lm(ESTIMATE ~ YEAR, data = male_female_data)
summary(lm_model)


ggplot(male_female_data, aes(x = YEAR, y = ESTIMATE)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Trend in Suicide Estimates Over Time", x = "Year", y = "Estimate") +
  theme_minimal()

```

Interpretation:

Linear regression assesses whether suicide estimates are increasing or decreasing over time. The slope coefficient of YEAR tells us the direction of the trend, and the p-value indicates if the trend is significant.

Result :

As the years increase, the estimated number of suicides slightly decreases on average. However, the data points are very spread out, meaning that year is not a strong predictor of suicide estimates by itself. The relationship is weak, even though it trends downward.

2.  How do suicide rate estimates differ across young adult age groups?

```{r}

unique(Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States$AGE)

age_data <- Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States

library(dplyr)

age_data <- filter(
  Death_rates_for_suicide_by_sex_race_Hispanic_origin_and_age_United_States,
  AGE %in% c("15-24 years", "25-34 years", "35-44 years")
)

table(age_data$AGE)


age_data$AGE <- as.factor(age_data$AGE)


anova_age <- aov(ESTIMATE ~ AGE, data = age_data)
summary(anova_age)


library(ggplot2)
ggplot(age_data, aes(x = AGE, y = ESTIMATE, fill = AGE)) +
  geom_boxplot() +
  labs(title = "Suicide Estimates by Age Group", x = "Age Group", y = "Estimate") +
  theme_minimal()





```

Interpretation:

The ANOVA tested whether there were significant differences in average suicide rate estimates among three young adult age groups: 15–24, 25–34, and 35–44 years. The resulting p-value from the ANOVA summary indicates \[whether or not\] these differences are statistically significant.

Result:

Based on the box plot, age group 35-44 years tends to have higher estimates than 15–24, 25–34 year age group.

Conclusion:

To begin our analysis, we first created a histogram to examine the distribution of suicide estimates. The histogram revealed that the data was right-skewed, indicating that while most suicide estimates were relatively low, some groups had significantly higher estimates. Next, we generated boxplots to compare the spread of suicide estimates across different demographic groups. The boxplots showed clear differences among age and racial/ethnic groups, with older males—especially those aged 85 years and over—having higher suicide estimates than younger groups. These visualizations helped us identify which groups were at the highest risk.
After visualizing the data, we calculated the mean, median, mode, standard deviation, minimum, and maximum to summarize the central tendency and variability of suicide estimates. The mean was noticeably higher than the median, further confirming the right-skewed distribution observed in the histogram. To statistically test whether there were significant differences in suicide estimates across demographic groups, we conducted an ANOVA (Analysis of Variance). The ANOVA results showed a statistically significant difference, meaning that suicide estimates varied meaningfully between groups. These findings, combined with the visualizations, suggest that age, gender, and race/ethnicity impact suicide estimates, emphasizing the need for targeted mental health interventions for high-risk populations.
