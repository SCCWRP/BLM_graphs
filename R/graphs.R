# load required libraries
library(tidyverse)
library(ggplot2)

# ---------------------------------------------------------------------------------
# Load in data
# TODO: Verify that this is the most up-to-date data
data <- read.csv("data/FW BLM Database Ecoregion.csv", header = TRUE) %>%
  mutate(
    SimplifiedAnalyteName = as.character(SimplifiedAnalyteName)
  ) %>% 
  filter(Result >= 0) %>%
  filter(!((Result > 15) & (SimplifiedAnalyteName == 'pH')))


# List of all analytes
analytes = unique(data$SimplifiedAnalyteName)

# List of all ecoregions
ecoregions = unique(data$US_L3CODE)

# EPA Estimate Values
# We will list the values in the following order:
# Conductvity, Calcium, Magnesium, Sodium, Potassium, Alkalinity, 
# Chloride, Sulfate, Hardness, DOC, pH, Temperature
analytenames <- c('Conductivity', 'Calcium', 'Magnesium', 'Sodium', 
                  'Potassium', 'Alkalinity', 'Chloride', 'Sulfate', 
                  'Hardness', 'DOC', 'Copper', 'pH', 'Temperature')

#ER8_Est = EcoRegion 8 Estimates
ER8_Est <- c(772.0,63.0,25.0,63.0,3.8,150.0,54.0,171.0,260.0,0.7,NA_real_,NA_real_,NA_real_) 
names(ER8_Est) <- analytenames

#ER85_Est = EcoRegion 85 Estimates
ER85_Est <- c(600.8,35.7,14.7,53.4,3.1,84.7,60.6,99.0,150.0,NA_real_,NA_real_,NA_real_,NA_real_)
names(ER85_Est) <- analytenames

# upper/lower boundaries for the zoomed in graphs.
# I wanted to hard code it for each, since doing it by percentile might not always give us what we want
upper_bounds = c(
  NA_real_,500,500,400,70,600,1250,2500,3000,30,100,12,40
)
names(upper_bounds) <- analytenames

lower_bounds = c(
  NA_real_,0,0,0,0,0,0,0,0,0,0,4.5,0
)
names(lower_bounds) <- analytenames


# define the summary function
## This function ensures we compute the correct percentiles
f <- function(x) {
  r <- quantile(x, probs = c(0.10, 0.25, 0.5, 0.75, 0.90))
  names(r) <- c("ymin", "lower", "middle", "upper", "ymax")
  r
}

# define graphing function
## This function "cleans" the data and creates the appropriate box plots
# TODO: Talk to Ashley about the y-axis limits. There are some outliers that throw off the scale.
# TODO: Talk to Ashley about the units for pH. We may need to manually go in and get rid of "None"
graphEco <- function(data, analyte){
  
  data <- data %>%
    dplyr::filter(SimplifiedAnalyteName == analyte) %>%
    # filter out any values for ecoregions that are not 8 or 85
    dplyr::filter(US_L3CODE %in% c(8, 85)) %>%
    dplyr::mutate(US_L3CODE = as.character(US_L3CODE))

  
  # For the sake of having the horizontal lines on the ends of the boxplot.
  # without having them end up as the same exact thing........
  # yes, this is not best practice, but I can't think of an easier way to get this thing done
  # We don't quite have time to figure out the more elegant solution at this moment
  ER85_10pct <- ((data %>% filter(US_L3CODE == 85))$Result %>% quantile(c(.1)))[['10%']]
  ER85_90pct <- ((data %>% filter(US_L3CODE == 85))$Result %>% quantile(c(.9)))[['90%']]
  
  ER8_10pct <- ((data %>% filter(US_L3CODE == 8))$Result %>% quantile(c(.1)))[['10%']]
  ER8_90pct <- ((data %>% filter(US_L3CODE == 8))$Result %>% quantile(c(.9)))[['90%']]
  
  
  # create box plot
  p <- ggplot(data, aes(x = US_L3CODE, y = Result)) + stat_summary(fun.data = f, geom="boxplot") + 
    geom_segment(
      # For the EcoRegion 8 graph
      x = 0.9, xend = 1.1, y = ER8_10pct, yend = ER8_10pct, size = 1,
      color = 'black',linetype = 'solid'
    ) +
    geom_segment(
      # For the EcoRegion 8 graph
      x = 0.9, xend = 1.1, y = ER8_90pct, yend = ER8_90pct, size = 1,
      color = 'black',linetype = 'solid'
    ) +
    geom_segment(
      # For the EcoRegion 85 graph
      x = 1.9, xend = 2.1, y = ER85_10pct, yend = ER85_10pct, size = 1,
      color = 'black',linetype = 'solid'
    ) +
    geom_segment(
      # For the EcoRegion 85 graph
      x = 1.9, xend = 2.1, y = ER85_90pct, yend = ER85_90pct, size = 1,
      color = 'black',linetype = 'solid'
    ) +
    geom_jitter(
      color = '#0066ff',
      alpha = 0.3,
      width = 0.4
    ) + labs(x = "US EPA Level III Ecoregion",
             y = paste(
                  analyte, 
                  ifelse(
                    data$Unit[2] != 'none', 
                    paste("(",data$Unit[2],")"), 
                    ""
                  ) 
                )
             ) + 
    theme(text = element_text(size = 20)) + 
    # change the values y and yend to the appropriate EPA Estimate Values
    geom_segment(
      x = 0.65, xend = 1.35, y = ER8_Est[[analyte]], yend = ER8_Est[[analyte]], size = 1,
      color = 'red',linetype = 'dashed'
    ) +
    # change the values y and yend to the appropriate EPA Estimate Values
    geom_segment(
      x = 1.6, xend = 2.4, y = ER85_Est[[analyte]], yend = ER85_Est[[analyte]], size = 1,
      color = 'red',linetype = 'dashed'
    ) #+ ylim(0, 2000) #<This is the code to "crop" the y-axis
  p_cropped <- p + ylim(lower_bounds[[analyte]],upper_bounds[[analyte]])
  
  # Export the plots to photos
  ggsave(paste0("plots/",analyte,".jpg"), plot = p, device = NULL)
  ggsave(paste0("plots/",analyte,"-zoomed.jpg"), plot = p_cropped, device = NULL)
}


# Create plots for all analytes of interest
for(analyte in analytes){
  print(analyte)
  plotEco <- graphEco(data, analyte)
  print(plotEco)
}











