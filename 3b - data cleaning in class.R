library(tidyverse)

raw <- read_csv('https://www.dropbox.com/scl/fi/ug8tbxsdd2qtsfqwnnox1/dollar_store.csv?rlkey=fu36g6uhfpx8u644d1rpsq11i&dl=1')

ds <- janitor::clean_names(raw)


# Convert to number formats: price, star_rating, review_count, stock_status, unit_size
#   Goals:   (1) Don't lose information
#            (2) Missing where appropriate, but only where appropriate



# Create usable brand and product_name columns from the product_info column
#   Hints:  (1) Ensure missingness is appropriate. 
#           (2) If possible, exploit any repeated structure in the column


# Convert date columns to date and/or date-time columns, then calculate how many
# days each product sat on the shelf before its first sale.

library(lubridate)


str_c()
str_remove()


ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>% 
  mutate(date_added_c = dmy(date_added),
         month = month(date_added_c, label = T),
         wday = wday(date_added_c, label = T))


ds %>% 
  select(date_added, first_sold_day, first_sold_time) %>% 
  mutate(sold_time_together = str_c(first_sold_day, first_sold_time, sep = "----")) %>% 
  mutate(sold_dt = mdy_hms(sold_time_together),
         date_added_c = dmy(date_added)) %>% 
  mutate(time_until_first_sale = as_date(sold_dt) - date_added_c)


# separate_wider_delim

str_remove()
str_extract()

# c() - The way to define a vector. Ex c('one', 'two', 'three')


ds %>% 
  select(product_info) %>% 
  separate_wider_delim(product_info, delim = " - ", names = c("brand", "product"), too_many = "merge") %>% 
  mutate(brand = str_remove(brand, "Brand:"))


ds %>% 
  select(unit_size) %>% 
  mutate(unit_type = str_extract(unit_size, "each|ounce")) %>% 
  mutate(unit_size_c = parse_number(unit_size))






