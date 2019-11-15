library(dplyr)
library(tidyr)

# import dataset
df <- read.csv(file = "data/uw_courses.csv", stringsAsFactors = F)

# For this report, we are only concerned with INFO department
df <- df %>% 
  filter(dept_abbrev == 'INFO')

# function that returns key metrics to generate a box plot (@Nicholas)


# function that generate a boxplot