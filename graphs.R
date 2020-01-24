setwd('/Users/itlender/Projects/BLM_graphs/')
library(tidyverse)
library(ggplot2)
library(plotly)
library(readxl)
library(beeswarm)
blm_data <- read.csv("FW BLM Database deliverable_012220.csv") %>%
  dplyr::mutate_if(is.factor, as.character) %>%
  dplyr::mutate(
    FractionName = dplyr::case_when(
      FractionName %in% c("None","Dissolved","Total","Not Recorded","Filtered") ~ FractionName,
      TRUE ~ NA_character_
    )
  )

blm_cats <- read.csv("BLM_Category_table_FINAL.csv")

copper <- blm_data %>% dplyr::filter(Analyte == "Copper, Total")

# separate to two groups to simulate two ecoregions
group1 <- copper$StationCode[((length(copper$StationCode)/2) + 1):length(copper$StationCode)]
group2 <- copper$StationCode[1:(length(copper$StationCode)/2)]

copper <- copper %>%
  dplyr::mutate(
    group = dplyr::case_when(
      StationCode %in% group1 ~ "group1",
      TRUE ~ "group2"
    )
  ) %>%
  dplyr::mutate(
    EPA_Estimate = dplyr::case_when(
      group == 'group1' ~ 45,
      group == 'group2' ~ 15,
      TRUE ~ NA_real_
    )
  )

copper <- copper %>%
  dplyr::group_by(group) %>%
  dplyr::mutate(
    `10th percentile` = quantile(Result, probs = 0.1)['10%'],
    `25th percentile` = quantile(Result)['25%'],
    `median` = quantile(Result)['50%'],
    `75th percentile` = quantile(Result)['75%'],
    `90th percentile` = quantile(Result, probs = 0.9)['90%'],
    IQR = `75th percentile` - `25th percentile`
  )
copper_no_outliers <- copper %>%
  dplyr::mutate(
    Result = dplyr::case_when(
      Result < `25th percentile` - (1.5 * IQR) ~ NA_real_,
      Result > `75th percentile` + (1.5 * IQR) ~ NA_real_,
      TRUE ~ Result
    )
  ) %>%
  dplyr::filter(
    !is.na(Result)
  )

copper_10to90 <- copper %>%
  dplyr::mutate(
    Result = dplyr::case_when(
      Result < `10th percentile` ~ NA_real_,
      Result > `90th percentile` ~ NA_real_,
      TRUE ~ Result
    )
  ) %>%
  dplyr::filter(
    !is.na(Result)
  )


copper_results_g1 = copper[which(copper$group == 'group1'),]$Result
copper_results_g2 = copper[which(copper$group == 'group2'),]$Result

p <- ggplot(copper_no_outliers, aes(x=group,y=Result))

test <- p + geom_jitter(
    #color = '#0066ff',
    alpha = 0.4,
    width = 0.4,
    #aes(size = NearStreamDistance)
    aes(color = Category, size = NearStreamDistance)
  ) +
  # geom_boxplot(
  #  color = '#000066',
  #  alpha = 0.05,
  #  outlier.shape = NA,
  #  outlier.colour = 'black'
  # ) + 
  geom_violin(
    alpha = 0.7,
    draw_quantiles = c(0.1,0.25,0.5,0.75,0.9),
    linetype = 'solid'
  ) +
  geom_segment(
    x = 0.65, xend = 1.35, y = 45, yend = 45,
    color = 'red',linetype = 'dashed'
  ) +
  geom_segment(
    x = 1.6, xend = 2.4, y = 15, yend = 15,
    color = 'red',linetype = 'dashed'
  )
  


violin <- p + geom_violin()

pl <- ggplotly(p)

plotly_test <- plot_ly(
  y = ~copper_10to90[which(copper_10to90$group == 'group1'),]$Result,
  type = 'box',
  boxpoints = 'all',
  jitter = 0.3
) %>%
add_trace(
  y = ~copper_10to90[which(copper_10to90$group == 'group2'),]$Result,
  boxpoints = T
)






