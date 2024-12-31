pacman::p_load(
  rio,           # import/export
  here,          # filepaths
  lubridate,     # working with dates
  forcats,       # factors
  aweek,         # create epiweeks with automatic factor levels
  janitor,       # tables
  tidyverse      # data mgmt and viz
)
### Import data
# import your dataset
linelist <- import("linelist_cleaned.rds")

### New categorical variable
linelist <- linelist %>% 
  mutate(delay_cat = case_when(
    # criteria                                   # new value if TRUE
    days_onset_hosp < 2                        ~ "<2 days",
    days_onset_hosp >= 2 & days_onset_hosp < 5 ~ "2-5 days",
    days_onset_hosp >= 5                       ~ ">5 days",
    is.na(days_onset_hosp)                     ~ NA_character_,
    TRUE                                       ~ "Check me"))  

### Default value order: to the new column delay_cat is a 
## categorical column of class Character - not yet a factor

table(linelist$delay_cat, useNA = "always")

### lets create a bar plot
ggplot(data = linelist)+
  geom_bar(mapping = aes(x = delay_cat))

#### Convert to factor, we must convert the above to factor

linelist <- linelist %>%
  mutate(delay_cat = fct_relevel(delay_cat))

#### now, they have been converted and are now in levels, check out the levels
levels(linelist$delay_cat)

#### The function fct_relevel() has the additional utility of allowing you to manually specify the level order.

linelist <- linelist %>%
  mutate(delay_cat = fct_relevel(delay_cat, "<2 days", "2-5 days", ">5 days"))
## check the level order now
levels(linelist$delay_cat)

#### Now plot a well orderd bar plot
ggplot(data = linelist)+
  geom_bar(mapping = aes(x = delay_cat))
