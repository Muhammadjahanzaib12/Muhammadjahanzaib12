---
title: "Srihas"
output: pdf_document
date: "2025-01-04"
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```


```{r}
# Load necessary libraries
library(ggplot2)
library(dplyr)

```


```{r}
library(readr)
data <- read_csv("Social_Network_Ads.csv")

```

```{r}
# Visualizations
# Visualization of Dependent Variable (Purchased)
ggplot(data, aes(x = Purchased)) +
  geom_histogram(aes(y = ..density..), binwidth = 0.5, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Purchased with Trendline", x = "Purchased (0 = No, 1 = Yes)", y = "Density") +
  theme_minimal()

# Visualization of Independent Variable (EstimatedSalary) with a density trendline
ggplot(data, aes(x = EstimatedSalary)) +
  geom_histogram(aes(y = ..density..), binwidth = 10000, fill = "skyblue", color = "black") +
  geom_density(color = "red", size = 1) +
  labs(title = "Distribution of Estimated Salary with Trendline", x = "Estimated Salary", y = "Density") +
  theme_minimal()

# Scatterplot of Estimated Salary vs Purchased with a trendline (useful for relationships)

ggplot(data, aes(x = EstimatedSalary, y = Purchased)) +
  geom_jitter(width = 0, height = 0.1, color = "blue") +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red", se = TRUE) +
  labs(title = "Estimated Salary vs Purchased with Trendline", x = "Estimated Salary", y = "Purchased (0 = No, 1 = Yes)") +
  theme_minimal()
```


```{r}
# Test for normality (Shapiro-Wilk test) on EstimatedSalary
shapiro_test <- shapiro.test(data$EstimatedSalary)
print(shapiro_test)

```

```{r}
# Decide parametric or non-parametric test
if (shapiro_test$p.value > 0.05) {
  print("Data is normally distributed. Proceeding with parametric test (t-test).")
  
  # Perform t-test
  t_test <- t.test(EstimatedSalary ~ Purchased, data = data)
  print(t_test)
  
} else {
  print("Data is not normally distributed. Proceeding with non-parametric test (Wilcoxon rank-sum test).")
  
  # Perform Wilcoxon rank-sum test
  wilcox_test <- wilcox.test(EstimatedSalary ~ Purchased, data = data)
  print(wilcox_test)
}

# Statistical Analysis
# Boxplot to visualize salary differences based on purchase decision
ggplot(data, aes(x = factor(Purchased), y = EstimatedSalary, fill = factor(Purchased))) +
  geom_boxplot() +
  labs(title = "Estimated Salary vs Purchased", x = "Purchased (0 = No, 1 = Yes)", y = "Estimated Salary") +
  theme_minimal()

```
