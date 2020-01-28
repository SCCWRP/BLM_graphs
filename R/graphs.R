# load required libraries
library(tidyverse)
library(ggplot2)

# ---------------------------------------------------------------------------------
# Load in data
# TODO: Verify that this is the most up-to-date data
data <- read.csv("data/FW BLM Database Ecoregion.csv", header = TRUE)

# List of all analytes
analytes = unique(data$SimplifiedAnalyteName)

# List of all ecoregions
ecoregions = unique(data$US_L3CODE)

# EPA Estimate Values
# TODO: Will need to get these values from Ashley
EPA_est = 100

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
graphEco <- function(analyte, EPA_est){
  data <- data %>%
    dplyr::filter(SimplifiedAnalyteName == analyte) %>%
    # filter out any values for ecoregions that are not 8 or 85
    dplyr::filter(US_L3CODE == c(8, 85)) %>%
  dplyr::mutate(US_L3CODE = as.character(US_L3CODE))

  # create box plot
  p <- ggplot(data, aes(x = US_L3CODE, y = Result)) + stat_summary(fun.data = f, geom="boxplot") + 
    stat_boxplot(geom = "errorbar", width = 0.4) +
    geom_jitter(
      color = '#0066ff',
      alpha = 0.4,
      width = 0.4
    ) + labs(x = "US EPA Level III Ecoregion",
             y = paste(analyte, "(", data$Unit[2], ")")) + 
    theme(text = element_text(size = 20)) + 
    # change the values y and yend to the appropriate EPA Estimate Values
    geom_segment(
      x = 0.65, xend = 1.35, y = EPA_est, yend = EPA_est, size = 1,
      color = 'red',linetype = 'dashed'
    ) +
    # change the values y and yend to the appropriate EPA Estimate Values
    geom_segment(
      x = 1.6, xend = 2.4, y = EPA_est, yend = EPA_est, size = 1,
      color = 'red',linetype = 'dashed'
    ) # + ylim(#min, #max) <This is the code to "crop" the y-axis
}


# Create plots for all analytes of interest
for(analyte in analytes){
  plotEco <- graphEco(analyte, EPA_est)
  print(plotEco)
}











