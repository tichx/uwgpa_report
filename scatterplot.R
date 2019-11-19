# The file contains the third chart of our midpoint deliverable.
# It demonstrates the relationship between average GPA and student enrolled for INFO courses.
# hover_information = course_id, course_title, avg_student, avg_gpa, first_offered, last_offered
df <- read.csv("data/uw_courses.csv", stringsAsFactors = F)

library(dplyr)
library(plotly)

third_chart <- function(df) {
  # Construct the general big dataframe of informatics courses.
  info_df <- df %>%
    filter(dept_abbrev == "INFO") %>%
    arrange(course_no) %>%
    mutate(course_id = paste0(dept_abbrev, " ", course_no),
           course_level = floor(course_no / 100) * 100,
           term = factor(term, levels = c("Autumn", "Winter", "Spring", "Summer")),
           term = as.character(term))

  # The next two functions construct a specific dataframe of informatics courses,
  # with the first and last offering quarters included for each course.
  # The first and last offering quarters are information needed for the interactive
  # components of my chart.
  course_period_df <- unique(info_df %>%
    group_by(course_id, course_title) %>%
    filter(row_number() == 1 | row_number() == n()) %>%
    mutate(term_year = paste0(term, " ", year)) %>%
    select(course_level, course_id, course_title, term_year))

  course_period_df <- unique(course_period_df %>%
    group_by(course_id, course_title) %>%
    mutate(first_offered = term_year[1],
           last_offered = term_year[2],
           course_level = paste0(as.character(course_level), " ", "level")) %>%
    select(course_level, course_id, course_title, first_offered, last_offered))

  # This dataframe contains the average course grade and the average number of
  # enrolled students for each INFO course.
  course_grade_df <- info_df %>%
    select(course_level, course_id, course_title,
           section_id, year, term, student_count, avg_gpa) %>%
    mutate(course_level = paste0(as.character(course_level), " ", "level")) %>%
    group_by(course_level, course_id, course_title) %>%
    summarise(avg_gpa = round(mean(avg_gpa), 1),
              avg_student = round(mean(as.numeric(student_count)), 0))

  # Combine course_grade_df and course_period_df by the columns "course_level",
  # "course_id", "course_title".
  combined_info_df <- left_join(course_grade_df, course_period_df,
                                by = c("course_level", "course_id", "course_title"))

  # Build an interactive chart that displays the average GPA for each INFO course.
  # The radius of each circle represents the average size of each course.
  plot_ly(
    data = combined_info_df,
    x = ~course_id,
    y = ~avg_gpa,
    type = "scatter",
    mode = "markers",
    hovertemplate = ifelse(is.na(combined_info_df[["last_offered"]]), 
                           paste("course id:", combined_info_df[["course_id"]],
                                 "<br>course title:", combined_info_df[["course_title"]], 
                                 "<br>avg student:", combined_info_df[["avg_student"]],
                                 "<br>avg gpa:", combined_info_df[["avg_gpa"]],
                                 "<br>first offered:", combined_info_df[["first_offered"]]),
                           paste("course id:", combined_info_df[["course_id"]],
                                 "<br>course title:", combined_info_df[["course_title"]], 
                                 "<br>avg student:", combined_info_df[["avg_student"]],
                                 "<br>avg gpa:", combined_info_df[["avg_gpa"]],
                                 "<br>first offered:", combined_info_df[["first_offered"]],
                                 "<br>last offered:", combined_info_df[["last_offered"]])),
     marker = list(size = ~avg_student),
     color = ~course_level
   ) %>%
     layout(
       title = "Average GPAs for INFO Courses",
       xaxis = list(title = "Course ID"),
       yaxis = list(title = "Average GPA")
     )
}

chart2 <- third_chart(df)

##########Section 2 Function##########
# The course with most 4.0 in percentage.
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

