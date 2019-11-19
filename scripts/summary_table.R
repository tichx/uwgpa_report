#Install package
library("dplyr")

#Import data set
df <- read.csv("data/uw_courses.csv", stringsAsFactors = FALSE)

#As for this summary table, we want to look into classes of informatics department. 
#We want to summarize the popularity of informatics classes
#Popularity of classes depends on the total enrollment of that certain class. 
#More Popular class will have more student enrollment than less popular class. 

<<<<<<< HEAD
  P_table <- df %>%
    filter(dept_abbrev == "INFO") %>%
    mutate(course_tag = paste(dept_abbrev, course_no)) %>%
    group_by(course_tag, course_title) %>%
    summarize(class_popularity = sum(as.double(student_count))) %>%
    arrange(-class_popularity)
=======
P_table <- df %>%
  filter(dept_abbrev == "INFO") %>%
  mutate(course_tag = paste(dept_abbrev, course_no)) %>%
  group_by(course_tag, course_title) %>%
  summarize(class_popularity = sum(as.double(student_count))) %>%
  arrange(-class_popularity) %>%
  head(10)
>>>>>>> 3a11e63d6a15a087bb0da51592ef4179781ca39a
