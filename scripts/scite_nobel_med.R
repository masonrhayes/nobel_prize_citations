library(tidyverse)
library(zoo)
library(ggthemes)

# See "functions.R" to see how each function is written

nobel_medicine = read_csv("data/dataverse_files/medicine_publication_record.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning")) %>% 
  mutate(field = "medicine")


scite_medicine = read_csv("data/scite_data/scite_nobel_medicine.csv")

# Combine data from scite and from nobel prize publication records -----

nobel_data = scite_join(scite_medicine, medicine)

# Aggregate all the data for all prize years, instead of separating --------

scite_summary_all = nobel_data %>% 
  scite_summarize_all(2007)

glimpse(aggregate_data)

# Group by prize year, then find citation statement tallies by years since prize -------
scite_summary = nobel_data %>% 
  scite_summarize()

glimpse(scite_summary)


## Building graph ---------
## Easy to edit the following to modify this graph:
## Change the filters to show more prize years (number of different graphs contained in the facet wrap) or to expand the range of years_since_prize (x axis)

variable = "Two Year SI"

scite_summary %>%
  filter(
    between(years_since_prize, -9, 10)
  ) %>% 
  filter(
    between(prize_year, 1992, 2000)
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes(years_since_prize, two_year_si)) +
  geom_line(aes(group = prize_year)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  facet_wrap(~prize_year)+
  labs(title = str_interp("${variable} by years since Nobel Prize in Medicine was awarded"), y = str_interp("${variable}"), x = "Years Since Prize") 


# Aggregate graph -----------

variable = "Contrasting Citation Statements"

scite_summary_all %>%
  filter(
    between(years_since_prize, -8, 8)
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes(years_since_prize, contrasting_in_year)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(title = str_interp("${variable} by years since Nobel Prize in Medicine was awarded"), y = str_interp("${variable}"), x = "Years Since Prize")




## Testing stats ---------
## Define group before vs after nobel prize: since it's announced in October, year 0 still is considered "after" the prize

scite_summary = scite_summary %>% 
  mutate(group = ifelse(years_since_prize >=0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize > -3, "after", "before")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))


scite_summary_all = scite_summary_all %>% 
  mutate(group = ifelse(years_since_prize >=0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize > -3, "after", "before")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))

## Define year range -----------

years_after = 2; years_before = -years_after - 1

# Check that the range is balanced. Year 0 belongs to "before", since Nobel prize is awarded 10 December each year.

years_before:years_after

stats = scite_summary %>% 
  filter(
    between(years_since_prize, years_before, years_after)
  ) %>% 
  group_by(group) %>% 
  mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
  summarize(
    across(ends_with("in_year"), list(mean = mean, sum = sum)),
    count = n())

## What is the mean scite index before/after the prize
## citation *sums*? --------
stats %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  select(group, mean_si, supporting_in_year_sum, contrasting_in_year_sum, total_cites_in_year_sum, count)

# Citation *means* --------
stats %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  select(group, mean_si, supporting_in_year_mean, contrasting_in_year_mean, total_cites_in_year_mean, count)

stats %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  ggplot(aes(group)) +
  geom_col(aes(y = mean_si))

view(stats)


# Aggregate stats --------

## (re)define year range -----------

years_after = 3; years_before = -years_after - 1

# Check that the range is balanced. Year 0 belongs to "after", since Nobel prize is awarded 10 December each year, but it is announce in October

years_before:years_after

stats_aggregate = aggregate_data %>% 
  filter(
    between(years_since_prize, years_before, years_after)
  ) %>% 
  group_by(group) %>% 
  mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
  summarize(
    across(ends_with("in_year"), list(mean = mean, sum = sum)),
    count = n())


### agg citation *sums*? --------
stats_aggregate %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  select(group, mean_si, supporting_in_year_sum, contrasting_in_year_sum, total_cites_in_year_sum, count)

# Agg citation *means* --------
stats_aggregate %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  select(group, mean_si, supporting_in_year_mean, contrasting_in_year_mean, total_cites_in_year_mean, count)



## Check simple linear model ----------------

aggregate_data %>% 
  mutate(ltysi = log(two_year_si)) %>% 
  group_by(group) %>% 
  mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
  ungroup() %>%
  mutate(interaction_year_group = interaction(years_since_prize, group)) %>%
  lm(formula = two_year_si ~ years_since_prize + group + mentioning_in_year) %>% 
  summary()

## Final checks ----
## Testing the arbitrary cutoff

arbitrary = final_df %>% 
  filter(
    between(years_since_prize, -4,-2)
  ) %>% 
  group_by(arbitrary_cutoff) %>% 
  mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
  summarize(
    across(ends_with("in_year"), list(mean = mean, sum = sum)),
    count = n())

arbitrary %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  select(arbitrary_cutoff, mean_si, supporting_in_year_sum, 
         contrasting_in_year_sum, total_cites_in_year_sum, count)