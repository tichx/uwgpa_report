library(dplyr)

# import dataset
df <- read.csv(file = "data/uw_courses.csv", stringsAsFactors = F)

# For this report, we are only concerned with INFO department
df <- df %>% 
  filter(dept_abbrev == 'INFO')

# Courses with lowest average class gpa within info department (@Nicholas)
lowest_gpa <- df %>% 
  filter(avg_gpa == min(df$avg_gpa))
  
# Courses with greatest number of Drop/Fail within info department (@Nicholas)
most_wf <- df %>% 
  mutate(withdraw_fail = as.numeric(Fail) + as.numeric(W)) %>% 
  filter(withdraw_fail == max(withdraw_fail, na.rm = T))
