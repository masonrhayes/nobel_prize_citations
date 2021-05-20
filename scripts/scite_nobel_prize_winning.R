library(tidyverse)
library(zoo)
library(ggthemes)

prize_winning = read_csv("data/dataverse_files/prize_winning_paper_record_manual.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning", "field"))


postgres = read_csv("data/postgres_data/prize_winning/_SELECT_source_doi_target_doi_citations_type_year_AS_source_year_202105181350.csv")

nobel = left_join(postgres, prize_winning, by = "target_doi") %>%
  mutate(years_since_prize = source_year - prize_year) %>%
  group_by(target_doi) %>%
  mutate(supporting_cites = sum(type == "supporting"),
         contrasting_cites = sum(type == "contradicting"),
         mentioning_cites = sum(type == "mentioning")) %>%
  ungroup() %>%
  group_by(prize_year, field, years_since_prize) %>%
  mutate(supporting_in_year = sum(type == "supporting"),
         contrasting_in_year = sum(type == "contradicting"),
         mentioning_in_year = sum(type == "mentioning")) %>% 
  ungroup()




glimpse(nobel)

df = nobel %>% 
  group_by(prize_year) %>%
  arrange(years_since_prize) %>% 
  filter(!duplicated(years_since_prize)) %>% 
  mutate(prev_year = lag(years_since_prize, order_by = years_since_prize)) %>% ungroup()

df2 = df %>%
  filter(supporting_in_year + contrasting_in_year >= 12) %>% 
  select(prize_year, field, years_since_prize, prev_year,
         supporting_in_year, contrasting_in_year, mentioning_in_year) %>% 
  mutate(two_year_si = NA) %>% 
  group_by(prize_year) %>% 
  nest()


si_calc = function(df){
  df %>% 
    mutate(two_year_si = ifelse(
      years_since_prize <= prev_year + 2,
      (supporting_in_year + lag(supporting_in_year,default=0))/(supporting_in_year + lag(supporting_in_year,default=0) + contrasting_in_year + lag(contrasting_in_year,default=0)), 
    ))
}


si_df = df2 %>%
  mutate(two_year_si = map(data, si_calc))



final_df = si_df %>% 
  select(prize_year, two_year_si) %>% 
  unnest()

view(final_df)


## Building graph ---------

variable = "two_year_si"

final_df %>%
  filter(
    between(years_since_prize, -15, 16)
  ) %>% 
  filter(
    field == "medicine"
  ) %>%
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes(years_since_prize, two_year_si)) +
  geom_line(aes(group = prize_year)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  geom_vline(xintercept = 0) +
  facet_wrap(~prize_year)+
  labs(title = str_interp("${variable} by years since Nobel Prize was awarded"), y = str_interp("${variable}"), x = "Years Since Prize") 


## Testing stats ---------

final_df = final_df %>% 
  mutate(group = ifelse(years_since_prize >0, "after_prize", "before_prize")) %>%
  mutate(arbitrary_cutoff = ifelse(years_since_prize > -3, "after", "before")) %>% 
  mutate(group = as.factor(group)) %>% 
  mutate(arbitrary_cutoff = as.factor(arbitrary_cutoff))


## Change years since prize to see different numbers

stats = final_df %>% 
  filter(
    between(years_since_prize, -4,5)
  ) %>% 
  group_by(group) %>% 
  mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
  summarize(
    across(ends_with("in_year"), list(mean = mean, sum = sum)),
    count = n())

## What is the mean scite index before/after the prize?
stats %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  select(group, mean_si, supporting_in_year_sum, contrasting_in_year_sum, total_cites_in_year_sum, count)

stats %>% 
  mutate(mean_si = supporting_in_year_sum/(supporting_in_year_sum + contrasting_in_year_sum)) %>% 
  ggplot(aes(group)) +
  geom_col(aes(y = mean_si))

view(stats)

## Check simple linear model ----------------

final_df %>% 
  filter(
    between(prize_year, 2000, 2016) & 
      !is.na(two_year_si)
  ) %>% 
  mutate(ltysi = log(two_year_si)) %>% 
  filter(
    between(years_since_prize, -4, 5
    )
  ) %>% 
  group_by(group) %>% 
  mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
  ungroup() %>%
  mutate(interaction_year_group = interaction(years_since_prize, group)) %>% 
  lm(formula = ltysi ~ years_since_prize + group + mentioning_in_year + lag(mentioning_in_year)) %>% 
  summary()


## Checking code. Is df2 nested ? ---------

df2$data[[33]] %>%
  mutate(two_year_si = ifelse(
    years_since_prize <= prev_year + 2,
    (supporting_in_year + lag(supporting_in_year,default=0))/(supporting_in_year + lag(supporting_in_year,default=0) + contrasting_in_year + lag(contrasting_in_year,default=0)), 
  )) %>% 
  view()


## Final checks ----
