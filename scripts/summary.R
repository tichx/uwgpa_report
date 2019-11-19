library(dplyr)

# import dataset
df <- read.csv(file = "data/uw_courses.csv", stringsAsFactors = F)

  
# For this report, we are only concerned with INFO department
df <- df %>% 
  filter(dept_abbrev == 'INFO')

get_summary <- function(dataframe) {
  # Courses with lowest average class gpa within info department (@Nicholas)
  lowest_gpa <- df %>% 
    filter(avg_gpa == min(df$avg_gpa)) %>% 
    mutate(text=paste0(lastname, "'s ", course_code," ", course_title, " has an average GPA of ", avg_gpa)) %>% 
    head(1) %>% 
    pull()
  
  # Courses with greatest number of Drop/Fail within info department (@Nicholas)
  most_wf <- df %>% 
    mutate(withdraw_fail = as.numeric(Fail) + as.numeric(W)) %>% 
    filter(withdraw_fail == max(withdraw_fail, na.rm = T)) %>% 
    mutate(text=paste0(lastname, "'s ", course_code," ", course_title, " has a total of ", withdraw_fail, " students withdrawed or failed")) %>% 
    head(1) %>% 
    pull()
  
  # top 1 professor names with highest rating (@Lance)
  highest_rated_prof <- df %>%
    mutate(pro_name = paste(lastname, firstname)) %>%
    select(professor_rating, pro_name) %>%
    filter(is.na(professor_rating) == FALSE) %>%
    group_by(pro_name) %>%
    summarise(
      ave_rate= sum(professor_rating) / n()
    ) %>%
    arrange(-ave_rate) %>% 
    head(1) %>% 
    select(pro_name) %>% 
    pull()
  
# courses with the biggest enrollment (@Chloe)
  biggest_class <- df %>%
    mutate(course_tag = paste(dept_abbrev, course_no)) %>%
    group_by(course_tag, course_title) %>%
    summarize(class_size = sum(as.double(student_count))) %>%
    arrange(-class_size) %>% 
    mutate(course = paste(course_tag,course_title)) %>%
    ungroup() %>% 
    select(course) %>% 
    head(1) %>% 
    pull()
  
  # The course with most 4.0 in percentage (@Keyan)
  most_four_course <- df %>%
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
                             select(course_id) %>% 
    pull()
  
    summary <-list()
    summary(min_gpa = lowest_gpa,
            max_withdrawl_fail=most_wf,
            largest_class=biggest_class,
            best_performing_class=most_four_course,
            best_prof=highest_rated_prof)
  return(summary)
}
sth<-get_summary(df)
  