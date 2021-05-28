library(tidyverse)

# Join Nobel Prize winner's publication records with scite's data -----------

scite_join = function(scite_data, nobel_data){
  df_joined = left_join(scite_data, nobel_data, by = "target_doi") %>%
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
  
  return(df_joined)
}



# Create aggregate df -----------

scite_summarize_all = function(df, max_pub_year){
  scite_summary_all = df %>% 
    filter(pub_year <= max_pub_year) %>% 
    group_by(years_since_prize) %>% 
    mutate(supporting_in_year = sum(type == "supporting"),
           contrasting_in_year = sum(type == "contradicting"),
           mentioning_in_year = sum(type == "mentioning")) %>% 
    mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year) %>% 
    ungroup() %>% 
    arrange(years_since_prize) %>% 
    filter(!duplicated(years_since_prize)) %>% 
    mutate(prev_year = lag(years_since_prize, order_by = years_since_prize)) %>% ungroup() %>%
    filter(supporting_in_year + contrasting_in_year >= 100) %>% 
    select(years_since_prize, prev_year,
           supporting_in_year, contrasting_in_year, 
           mentioning_in_year, total_cites_in_year) %>% 
    mutate(two_year_si = NA) %>% 
    mutate(two_year_si = ifelse(
      years_since_prize <= prev_year + 2,
      (supporting_in_year + lag(supporting_in_year,default=0))/(supporting_in_year + lag(supporting_in_year,default=0) + contrasting_in_year + lag(contrasting_in_year,default=0)), 
    ))
  
  return(scite_summary_all)
}

# Calculate Two Year scite Index ----------

si_calc = function(df){
  df %>% 
    mutate(two_year_si = ifelse(
      years_since_prize <= prev_year + 2,
      (supporting_in_year + lag(supporting_in_year,default=0))/(supporting_in_year + lag(supporting_in_year,default=0) + contrasting_in_year + lag(contrasting_in_year,default=0)), 
    ))
}

# Calculate supporting, contrasting, mentioning in year, grouped by prize year ------
# Then calculate Two Year SI with the previous function
# 
# After calculating cites in each year and normalizing dates, we no longer need unique years_since_prize. So group by prize year, drop duplicate years_since_prize; and then add variable called prev_year which is a one-year lag of years_since_prize.
# 
# Next, select only obs where there are >=10 supporting + contrasting, select relevant variables, create new var two_year_si, and then group by prize year and nest the data frame.
# 
# Finally, map the function to calculate two year SI, select prize_year and two_year_si, and then unnest it all.
scite_summarize = function(df){
  scite_summary = df %>% 
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
  
  return(scite_summary)
}


