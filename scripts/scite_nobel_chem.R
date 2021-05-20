library(tidyverse)
library(zoo)
library(ggthemes)
library(pracma)

chemistry = read_csv("data/dataverse_files/chemistry_publication_record.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning")) %>% 
  mutate(field = "chemistry")


postgres = read_csv("data/scite_data/scite_nobel_chemistry.csv")

nobel = left_join(postgres, chemistry, by = "target_doi") %>%
  mutate(years_since_prize = source_year - prize_year) %>%
  group_by(target_doi) %>%
  mutate(supporting_cites = sum(type == "supporting"),
         contrasting_cites = sum(type == "contradicting"),
         mentioning_cites = sum(type == "mentioning")) %>%
  ungroup() %>%
  group_by(prize_year, years_since_prize) %>%
  mutate(supporting_in_year = sum(type == "supporting"),
         contrasting_in_year = sum(type == "contradicting"),
         mentioning_in_year = sum(type == "mentioning")) %>% 
  ungroup()

# Aggregate all the data for all prize years, instead of separating
aggregate_data = nobel %>% 
  group_by(years_since_prize) %>% 
  mutate(supporting_in_year = sum(type == "supporting"),
         contrasting_in_year = sum(type == "contradicting"),
         mentioning_in_year = sum(type == "mentioning")) %>% 
  ungroup() %>% 
  arrange(years_since_prize) %>% 
  filter(!duplicated(years_since_prize)) %>% 
  mutate(prev_year = lag(years_since_prize, order_by = years_since_prize)) %>% ungroup() %>%
  filter(supporting_in_year + contrasting_in_year >= 100) %>% 
  select(years_since_prize, prev_year,
         supporting_in_year, contrasting_in_year, mentioning_in_year) %>% 
  mutate(two_year_si = NA) %>% 
  mutate(two_year_si = ifelse(
    years_since_prize <= prev_year + 2,
    (supporting_in_year + lag(supporting_in_year,default=0))/(supporting_in_year + lag(supporting_in_year,default=0) + contrasting_in_year + lag(contrasting_in_year,default=0)), 
  ))

view(aggregate_data)


# Check
glimpse(nobel)

# Define function to calculate Two Year scite Index (SI) ------------
si_calc = function(df){
  df %>% 
    mutate(two_year_si = ifelse(
      years_since_prize <= prev_year + 2,
      (supporting_in_year + lag(supporting_in_year,default=0))/(supporting_in_year + lag(supporting_in_year,default=0) + contrasting_in_year + lag(contrasting_in_year,default=0)), 
    ))
}

# New data frame ---------
# 
# After calculating cites in each year and normalizing dates, we no longer need unique years_since_prize. So group by prize year, drop duplicate years_since_prize; and then add variable called prev_year which is a one-year lag of years_since_prize.
# 
# Next, select only obs where there are >=10 supporting + contrasting, select relevant variables, create new var two_year_si, and then group by prize year and nest the data frame.
# 
# Finally, map the function to calculate two year SI, select prize_year and two_year_si, and then unnest it all.

df = nobel %>% 
  group_by(prize_year) %>%
  arrange(years_since_prize) %>% 
  filter(!duplicated(years_since_prize)) %>% 
  mutate(prev_year = lag(years_since_prize, order_by = years_since_prize)) %>% ungroup() %>%
  filter(supporting_in_year + contrasting_in_year >= 10) %>% 
  select(prize_year, years_since_prize, prev_year,
         supporting_in_year, contrasting_in_year, mentioning_in_year) %>% 
  mutate(two_year_si = NA) %>% 
  group_by(prize_year) %>% 
  nest()%>% # nesting the df to map si_calc function to each prize_year
  mutate(two_year_si = map(data, si_calc)) %>% 
  select(prize_year, two_year_si) %>% 
  unnest()


view(df)


## Building graph ---------
## Easy to edit the following to modify this graph:
## Change the filters to show more prize years (number of different graphs contained in the facet wrap) or to expand the range of years_since_prize (x axis)

variable = "Two Year SI"

df %>%
  filter(
    between(years_since_prize, -9, 10)
  ) %>% 
  filter(
    between(prize_year, 1980,2000)
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes(years_since_prize, two_year_si)) +
  geom_line(aes(group = prize_year)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0)+
  facet_wrap(~prize_year)+
  labs(title = str_interp("${variable} by years since Nobel Prize in Chemistry was awarded"), y = str_interp("${variable}"), x = "Years Since Prize") 


# Aggregate graph -----------

variable = "Two Year SI"

aggregate_data %>%
  filter(
    between(years_since_prize, -30, 30)
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes(years_since_prize, two_year_si)) +
  geom_line() +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(title = str_interp("${variable} by years since Nobel Prize in Chemistry was awarded"), y = str_interp("${variable}"), x = "Years Since Prize")




## Testing stats ---------
## Define group before vs after nobel prize: since it's awarded 10 December, year 0 is considered before. 

df = df %>% 
  mutate(group = ifelse(years_since_prize >=0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize > -3, "after", "before")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))


aggregate_data = aggregate_data %>% 
  mutate(group = ifelse(years_since_prize >=0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize > -3, "after", "before")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))

## Define year range -----------

years_after = 2; years_before = -years_after - 1

# Check that the range is balanced. Year 0 belongs to "before", since Nobel prize is awarded 10 December each year.

years_before:years_after

stats = df %>% 
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