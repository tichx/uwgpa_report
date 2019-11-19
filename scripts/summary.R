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

# top 10 professor names with highest rating (@Lance)
data_with_pros <- df %>%
  mutate(pro_name = paste(lastname, firstname)) %>%
  select(professor_rating, pro_name) %>%
  filter(is.na(professor_rating) == FALSE) %>%
  group_by(pro_name) %>%
  summarise(
    ave_rate= sum(professor_rating) / n()
  ) %>%
  arrange(-ave_rate) %>%
  head(10)

# courses with the biggest enrollment (@Chloe)
biggest_class <- df %>%
  mutate(course_tag = paste(dept_abbrev, course_no)) %>%
  group_by(course_tag, course_title) %>%
  summarize(class_size = sum(as.double(student_count))) %>%
  arrange(-class_size) %>%
  head(1)

# The course with most 4.0 in percentage (@Keyan)
most_four_course <- pull(df %>%
  filter(dept_abbrev == "INFO") %>%
  arrange(course_no) %>%
  mutate(course_id = paste0(dept_abbrev, " ", course_no),
  term = factor(term, levels = c("Autumn", "Winter", "Spring", "Summer")),
                term = as.character(term)) %>%
  select(course_id, course_title, section_id, term, year, A, student_count) %>%
  group_by(course_id, course_title) %>%
  summarise(A_percentage =
    (sum(suppressWarnings(as.numeric(A)), na.rm = T) /
     sum(suppressWarnings(as.numeric(student_count)), na.rm = T) * 100)) %>%
  ungroup() %>%
  ungroup() %>%
  filter(A_percentage == max(A_percentage)) %>%
  select(course_id))
  