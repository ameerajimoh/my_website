---
categories:
- ""
- ""
date: "2017-10-31T22:42:51-05:00"
description: Analyzing flight details using Data Analysis and Manipulation
draft: false
image: pic11.jpg
keywords: ""
slug: aliquam
title: Various Flight Analysis
---

Problem 1: What months had the highest and lowest proportion of cancelled flights? Interpret any seasonal patterns.

```r
cancelled_flights <- flights %>%
  filter(is.na(dep_time))

#Calculate proportion of flights cancelled by month:
cancelled_flights_month <- cancelled_flights %>%
  group_by(month) %>%
  summarise(cancelled_prop = n() / nrow(flights) * 100) # Calculate the ratio of cancelled flights to the total number of flights and multiply by 100 to get percentages


ggplot(cancelled_flights_month, aes(x = month, y = cancelled_prop)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(x = "Month", y = "Proportion of Cancelled Flights (%)", title = "Proportion of Flights Cancelled by Month") +
  scale_x_continuous(breaks = 1:12)+
  theme_minimal()
```

   

