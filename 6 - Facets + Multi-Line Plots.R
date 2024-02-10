# FACETS
#   Facets allow you to use categories within your data to line up similar
#   charts next to each other, which can be a really useful way to highlight
#   differences between groups.
# 
# The two main facet functions are:
#   facet_wrap() <- usually best used to show a series of plots across a 
#                  single category variable, with you specify with `~varable`.
#                  This will show one plot per level of the category.
#   facet_grid() <- My favorite to show a "grid" of facets, with rows and 
#                  columns defined by two different variables. This is specified
#                  using the facet formula `var1~var2`
#       Note: Pay attention to the `scales` parameter in these facet functions, 
#             where you can specify whether you want to hold the x and/or y
#             axis scales constant across all of the facets.


# Facets and multi-lines --------------------------------------------------------------------------------

tips <- read_csv('https://www.dropbox.com/s/rydxlxdarjdoj7a/tips.csv?dl=1')

# Let's plot tip_percentage vs. total_bill,
# then split that across lots of categories

tips %>% 
  mutate(bad_smoker_tip = ifelse(smoker == "Yes", tip*100, tip)) %>% 
  mutate(bad_day_total = ifelse(day %in% c("Sun", "Sat"), total_bill*1000, total_bill)) %>% 
  mutate(tip_perc = tip/total_bill) %>% 
  ggplot(aes(x = bad_day_total, y = bad_smoker_tip, color = as.factor(sex))) +
  geom_point(size = 3) +
  facet_grid(smoker~day, scales = 'free')



econ <- read_csv('https://www.dropbox.com/s/8bq9rw0rk46hru2/econ.csv?dl=1')

# Let's plot two measures over time: savings rate & unemployment weeks
# It's easiest if we pivot to make this work
library(lubridate)

econ %>% 
  select(date, savings_rate, unempl_weeks) %>% 
  #filter(date < mdy('01-01-1975')) %>% 
  pivot_longer(!date, 
               names_to = 'Measure', 
               values_to = 'Rate') %>% 
  ggplot(aes(x = date, y = Rate, color = Measure)) +
  geom_line() +
  facet_wrap(~Measure, ncol = 1)


