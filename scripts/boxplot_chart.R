library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)

# import dataset
df <- read.csv(file = "data/uw_courses.csv", stringsAsFactors = F)

# For this report, we are only concerned with INFO department
df <- df %>%
  filter(dept_abbrev == "INFO") %>%
  filter(course_no == "200") %>%
  mutate(prof_name = paste0(firstname, ", ", substring(lastname, 1, 1)))

# function that returns key metrics to generate a box plot (@Nicholas)
compute <- function(dataframe, subject, course_number) {
  df <- dataframe %>%
    filter(dept_abbrev == subject) %>%
    filter(course_no == course_number) %>%
    mutate(gp = as.numeric(student_count) * as.numeric(avg_gpa))
  return(df)
}

class <- compute(df, "INFO", "200") %>%
  group_by(lastname) %>%
  summarize(
    firstname = firstname[1],
    student_count = sum(as.numeric(student_count)),
    section = n(),
    A = sum(as.numeric(A), na.rm = TRUE),
    Am = sum(as.numeric(Aminus), na.rm = TRUE),
    Bp = sum(as.numeric(Bplus), na.rm = TRUE),
    B = sum(as.numeric(B), na.rm = TRUE),
    Bm = sum(as.numeric(Bminus), na.rm = TRUE),
    Cp = sum(as.numeric(Cplus), na.rm = TRUE),
    C = sum(as.numeric(C), na.rm = TRUE),
    Cm = sum(as.numeric(Cminus), na.rm = TRUE),
    Dp = sum(as.numeric(Dplus), na.rm = TRUE),
    D = sum(as.numeric(D), na.rm = TRUE),
    Dm = sum(as.numeric(Dminus), na.rm = TRUE),
    Failed = sum(as.numeric(Fail), na.rm = TRUE),
    Withdrawl = sum(as.numeric(W), na.rm = TRUE),
    grade_point_total = sum(gp)
  ) %>%
  mutate(avg_gpa = round(grade_point_total / student_count, 2)) %>%
  rowwise() %>%
  mutate(count = sum(A, B, C, D, Failed, Withdrawl, Am, Bm, Cm, Dm, Bp,
                     Cp, Dp, na.rm = TRUE)) %>%
  mutate(st = student_count)
class <- add_row(class,
  lastname = "All Sections", firstname = "",
  student_count = sum(class$student_count),
  section = sum(class$section),
  A = sum(class$A),
  Am = sum(class$Am),
  Bp = sum(class$Bp),
  B = sum(class$B),
  Bm = sum(class$Bm),
  Cp = sum(class$Cp),
  C = sum(class$C),
  Cm = sum(class$Cm),
  Dp = sum(class$Dp),
  D = sum(class$D),
  Dm = sum(class$Dm),
  Failed = sum(class$Failed),
  Withdrawl = sum(class$Withdrawl),
  grade_point_total = sum(class$grade_point_total),
  avg_gpa = round(grade_point_total / student_count, 2)
)

plot_data <- class %>%
  select(lastname, A, Am, Bp, B, Bm, Cp, C, Cm, Dp, D, Dm, Failed,
         Withdrawl) %>%
  gather(key = Grade, value = GPA, -lastname)
final_plot <- ggplot(plot_data) +
  geom_col(mapping = aes(x = lastname, y = GPA, fill = Grade),
           position = "fill") +
  scale_y_continuous(sec.axis = sec_axis(~ . * 4)) +
  labs(title = "Grade disparity between INFO 200 sections at UW",
       subtitle = "Average GPA", x = "Prof. last name",
       y = "Grade Percentage", color = "Grade") +
  coord_flip()
final_plot <- ggplotly(final_plot)
