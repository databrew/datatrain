library(readr)
library(dplyr)
library(ggplot2)
hours <- read_csv('Toggl_time_entries_2022-01-01_to_2022-12-31.csv')

hours$date <- as.Date(hours$`Start date`)
hours$hours <- as.numeric(hours$Duration) / 60 / 60
pd <- hours %>%
  group_by(Project, User) %>%
  summarise(hrs = sum(hours))
ggplot(data = pd,
       aes(x = User,
           y = hrs)) +
  geom_col(aes(fill = Project)) +
  labs(y = 'Total hours worked')
joined <- hours %>% dplyr::rename(person = User)
ggplot(data = joined,
       aes(x = person,
           y = hours)) +
  geom_col()

pd <- expand.grid(
  date = seq(min(joined$date),
             max(joined$date),
             by= 1),
  person = sort(unique(joined$person))
)
pd <- left_join(pd, 
                joined %>% group_by(date, person) %>% summarise(hours = sum(hours))) %>%
  mutate(hours = ifelse(is.na(hours), 0, hours)) %>%
  filter(date >= as.Date('2022-05-25')) %>%
  filter(person != 'Jostein')
ggplot(data = pd,
       aes(x = date,
           y = hours,
           fill = person)) +
  geom_bar(stat = 'identity',
           position = position_dodge(width = 0.8)) +
  geom_text(aes(label = round(hours, digits = 1),
                y = hours + 0.5),
            position = position_dodge(width = 0.8)) +
  scale_x_date(breaks = sort(unique(pd$date))) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 1))

pdx <- joined %>% mutate(date = as.Date(date)) %>% arrange(date) %>%
  group_by(person) %>%
  mutate(cs = cumsum(hours))

ggplot(data = pdx,
       aes(x = date,
           y = cs,
           color = person)) +
  geom_line() +
  scale_x_date(breaks = sort(unique(pdx$date))) +
  theme(axis.text.x = element_text(angle = 90,
                                   vjust = 1)) +
  labs(x = 'Date',
       y = 'Cumulative hours worked for hours')
