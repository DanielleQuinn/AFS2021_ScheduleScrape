# ---- Load Packages ----
library(rvest)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)

# ---- Nov 7 ----
# Note: NEED TO SCROLL TO BOTTOM OF EACH PAGE BEFORE MANUALLY COPYING HTML ('-_-)
# TO COPY ENTIRE PAGE: AFTER SCROLLING, INSPECT PAGE > "Edit as HTML"
# Currently: Saved HTML from each day as a .txt file

file <- read_html("afs_html_nov7.txt") %>%
  html_elements('section')

## Times
nov7_times <-  file %>%
  html_elements(".css-1dy340k") %>%
  html_text()

## Locations
nov7_rooms <- file %>%
  html_elements(".css-1qxrd6e") %>%
  html_text()

## Titles
nov7_titles <- file %>%
  html_elements(".css-ou7ftf") %>%
  html_text()


nov7_sessions <- file %>%
  html_elements(".css-154k25t") %>%
  html_text()

nov7 <- data.frame(times = nov7_times,
                   room = nov7_rooms,
                   title = nov7_titles,
                   date = "November 7")

# ---- Nov 8 ----
file <- read_html("afs_html_nov8.txt") %>%
  html_elements('section')

## Times
nov8_times <-  file %>%
  html_elements(".css-1dy340k") %>%
  html_text()

## Locations
nov8_rooms <- file %>%
  html_elements(".css-1qxrd6e") %>%
  html_text()

## Titles
nov8_titles <- file %>%
  html_elements(".css-ou7ftf") %>%
  html_text()

nov8 <- data.frame(times = nov8_times,
                   room = nov8_rooms,
                   title = nov8_titles,
                   date = "November 8")

# ---- Nov 9 ----
file <- read_html("afs_html_nov9.txt") %>%
  html_elements('section')

## Times
nov9_times <-  file %>%
  html_elements(".css-1dy340k") %>%
  html_text()

## Locations
nov9_rooms <- file %>%
  html_elements(".css-1qxrd6e") %>%
  html_text()

## Titles
nov9_titles <- file %>%
  html_elements(".css-ou7ftf") %>%
  html_text()

nov9 <- data.frame(times = nov9_times,
                   room = nov9_rooms,
                   title = nov9_titles,
                   date = "November 9")

# ---- Nov 10 ----
file <- read_html("afs_html_nov10.txt") %>%
  html_elements('section')

## Times
nov10_times <- file %>%
  html_elements(".css-1dy340k") %>%
  html_text()

## Locations
nov10_rooms <- file %>%
  html_elements(".css-1qxrd6e") %>%
  html_text()

## Titles
nov10_titles <- file %>%
  html_elements(".css-ou7ftf") %>%
  html_text()

nov10 <- data.frame(times = nov10_times,
                    room = nov10_rooms,
                    title = nov10_titles,
                    date = "November 10")

# ---- Combine & Reshape ----
data <- bind_rows(nov6, nov7, nov8, nov9, nov10) %>%
  separate(room, into = c("room", "location"), sep = ",") %>%
  separate(times, into = c("start", "end"), sep = "-") %>%
  mutate(end = str_remove(end, " EST"),
         start = parse_date_time(paste("2021", date, start), "%y%m%d %H%M%p"),
         end = parse_date_time(paste("2021", date, end), "%y%m%d %H%M%p"),
         start = force_tz(start, "America/New_York"),
         end = force_tz(end, "America/New_York"),
         duration = end - start,
         title = str_to_title(title)) %>%
  filter(duration == 20) %>%
  select(start, end, room, title) %>% 
  # Note: Peale A Nov 9 8:00 - 8:20 two talks scheduled; one is likely an intro to the symposium?
  # This code will combine them, separated with a slash
  group_by(start, end, room) %>% 
  summarise(title = paste(title, collapse = " / ")) %>%
  ungroup() %>%
  pivot_wider(id_cols = c(start, end),
              names_from = room, values_from = title) %>% 
  mutate(across(c(-start, -end), .fns = function(x) replace_na(x, "-")))

write.csv(data, "afs_schedule.csv", row.names = FALSE)

# for multi-sheet Excel
data2 <- data %>%
  mutate(date = date(start),
         start = str_sub(as.character(start), 12, 16),
         end = str_sub(as.character(end), 12, 16)) %>%
  select(date, start, sort(tidyselect::peek_vars()))
  
nov7_final <- data2 %>% filter(date == ymd("2021-11-07")) %>% select(-date, -end)
nov8_final <- data2 %>% filter(date == ymd("2021-11-08")) %>% select(-date, -end)
nov9_final <- data2 %>% filter(date == ymd("2021-11-09")) %>% select(-date, -end)
nov10_final <- data2 %>% filter(date == ymd("2021-11-10")) %>% select(-date, -end)

writexl::write_xlsx(x = list(Nov7 = nov7_final,
                             Nov8 = nov8_final,
                             Nov9 = nov9_final,
                             Nov10 = nov10_final),
                    path = "afs_schedule_complete.xlsx")
