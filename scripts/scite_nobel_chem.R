
source("scripts/functions.R")


nobel_chemistry = read_csv("data/dataverse_files/chemistry_publication_record.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning")) %>% 
  mutate(field = "chemistry")


scite_chemistry = read_csv("data/scite_data/scite_nobel_chemistry.csv")

chemistry_nobel_data = scite_join(scite_chemistry, nobel_chemistry)

# Aggregate all the data for all prize years, instead of separating. Choose max pub/prize years so that the x axis, year since prize, makes sense.

chem_summary_all = chemistry_nobel_data %>% 
  scite_summarize_all(max_pub_year = 2009,
                      max_prize_year = 2009)

view(chem_summary_all)


chem_summary = chemistry_nobel_data %>% 
  scite_summarize()


# Check
glimpse(chem_summary_all)


## Building graph ---------
## Easy to edit the following to modify this graph:
## Change the filters to show more prize years (number of different graphs contained in the facet wrap) or to expand the range of years_since_prize (x axis)

variable = "Two Year SI"

chem_summary %>%
  filter(
    between(years_since_prize, -9, 10)
  ) %>% 
  filter(
    between(prize_year, 2000,2008)
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes(years_since_prize, two_year_si)) +
  geom_line(aes(group = prize_year)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  facet_wrap(~prize_year)+
  labs(title = str_interp("${variable} by years since Nobel Prize in Chemistry was awarded"), y = str_interp("${variable}"), x = "Years Since Prize") 


# Aggregate graph -----------

# Define variables that will repeat in each graph
field = "Chemistry"

date_range = c(-10,10)

# Contrasting citation statements -----
chem_summary_all %>% 
  scite_plot(field, "Contrasting", "contrasting_in_year", date_range) +
  ylim(0,0.02)

### Supporting ----------


chem_summary_all %>% 
  scite_plot(field, "Supporting", "supporting_in_year", date_range)

## Mentioning ------


chem_summary_all %>%
  scite_plot(field, "Mentioning", "mentioning_in_year", date_range)

# total citations --------



chem_summary_all %>%
  scite_plot(field, "Total", "total_cites_in_year", date_range)


## scite Index ---------

variable = "Two Year SI"

chem_summary_all %>%
  scite_index_plot(field, c(-10,10))+
  ylim(0.75,1)



## Testing stats ---------
## Define group before vs after nobel prize: since it's awarded 10 December, year 0 is considered before. 

chem_summary = chem_summary %>% 
  mutate(group = ifelse(years_since_prize >=0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize > -3, "after", "before")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))


chem_summary_all = chem_summary_all %>% 
  mutate(group = ifelse(years_since_prize >=0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize >= -3, "after_arbitrary", "before_arbitrary")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))

## Define year range -----------

years_after = 2; years_before = -years_after - 1

# Check that the range is balanced. Year 0 belongs to "before", since Nobel prize is awarded 10 December each year.

years_before:years_after

stats = chem_summary %>% 
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

stats_aggregate = chem_summary_all %>% 
  filter(
    between(years_since_prize, years_before, years_after)
  ) %>% 
  group_by(group) %>%
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


## Final checks ----
## Testing the arbitrary cutoff

arbitrary = chem_summary_all %>% 
  filter(
    between(years_since_prize, -5,-2)
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
