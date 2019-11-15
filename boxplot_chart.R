library(dplyr)
library(tidyr)
library(ggplot2)

  # import dataset
df <- read.csv(file = "data/uw_courses.csv", stringsAsFactors = F)

# For this report, we are only concerned with INFO department
df <- df %>% 
  filter(dept_abbrev == 'INFO') %>% 
  filter(course_no == '200') %>% 
  mutate(prof_name = paste0(firstname, ", ",substring(lastname, 1,1)))



# function that returns key metrics to generate a box plot (@Nicholas)
compute <- function(dataframe, subject, course_no) {
  
}

# function that generate a boxplot

# create a data frame
variety=rep(LETTERS[1:7], each=40)
treatment=rep(c("high","low"),each=20)
note=seq(1:280)+sample(1:150, 280, replace=T)
data=data.frame(variety, treatment ,  note)

# grouped boxplot
ggplot(data, aes(x=variety, y=note, fill=treatment)) + 
  geom_boxplot()
