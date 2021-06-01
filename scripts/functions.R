library(tidyverse)
library(ggthemes)
library(pracma)

# ------ A script to store important or frequently used functions ------

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

scite_summarize_all = function(df, max_pub_year, max_prize_year){
  scite_summary_all = df %>% 
    filter(pub_year <= max_pub_year & prize_year <= max_prize_year) %>% 
    group_by(years_since_prize) %>% 
    mutate(supporting_in_year = sum(type == "supporting"),
           contrasting_in_year = sum(type == "contradicting"),
           mentioning_in_year = sum(type == "mentioning")) %>% 
    mutate(total_cites_in_year = supporting_in_year + mentioning_in_year + contrasting_in_year,
           citing_papers = sum(!duplicated(source_doi))) %>% 
    ungroup() %>% 
    arrange(years_since_prize) %>% 
    filter(!duplicated(years_since_prize)) %>% 
    mutate(prev_year = lag(years_since_prize, order_by = years_since_prize)) %>% ungroup() %>%
    filter(supporting_in_year + contrasting_in_year >= 100) %>% 
    select(years_since_prize, prev_year,
           supporting_in_year, contrasting_in_year, 
           mentioning_in_year, total_cites_in_year, citing_papers) %>%
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


# plot --------

scite_plot = function(df, field, citation_type, var, date_range = c(-10,10)){
  plot = df %>% 
    filter(
      between(years_since_prize, date_range[1], date_range[2])
    ) %>% 
    filter(!is.na(two_year_si)) %>% 
    ggplot(aes_string("years_since_prize", str_interp("${var}/citing_papers")),
           environment = environment()) +
    geom_line() +
    geom_smooth(se = FALSE) +
    theme_minimal() +
    geom_vline(xintercept = 0) +
    labs(title = str_interp("${citation_type} Citation Statements per Citing Paper by years since Nobel Prize in ${field} was awarded"), y = str_interp("${citation_type} Citation Statements per Citing Paper"), x = "Years Since Prize")
  
  return(plot)
  
}

# To be used only on "{field}_summary_all" data

scite_index_plot = function(df, field, date_range = c(-10,10)){
  plot = df %>% 
    filter(
    between(years_since_prize, date_range[1], date_range[2])
  ) %>% 
    filter(!is.na(two_year_si)) %>% 
    ggplot(aes(years_since_prize, two_year_si)) +
    geom_line() +
    geom_smooth(se = FALSE) +
    theme_minimal() +
    geom_vline(xintercept = 0) +
    labs(title = str_interp("Two Year SI by years since Nobel Prize in ${field} was awarded"), y = str_interp("Two Year SI"), x = "Years Since Prize")
  
  return(plot)
}


scite_papers = function(df, max_pub_year, max_prize_year, 
                        min_pub_year, min_prize_year, date_range = c(-10,10)){
  supporting_papers = df %>% 
    filter(pub_year <= max_pub_year & prize_year <= max_prize_year) %>%
    filter(type == "supporting") %>%
    group_by(years_since_prize) %>% 
    mutate(supporting_papers_in_year = sum(!duplicated(source_doi))) %>% 
    ungroup() %>%  
    filter(!duplicated(years_since_prize)) %>% 
    filter(between(
      years_since_prize, date_range[1], date_range[2]
    )) %>% 
    select(years_since_prize, supporting_papers_in_year)
  
  contrasting_papers = df %>% 
    filter(pub_year <= max_pub_year & prize_year <= max_prize_year) %>% 
    filter(type == "contradicting") %>%
    group_by(years_since_prize) %>% 
    mutate(contrasting_papers_in_year = sum(!duplicated(source_doi))) %>% 
    ungroup() %>% 
    filter(!duplicated(years_since_prize)) %>% 
    filter(between(
      years_since_prize, date_range[1], date_range[2]
    )) %>% 
    select(years_since_prize, contrasting_papers_in_year)
  
  mentioning_papers = df %>% 
    filter(pub_year <= max_pub_year & prize_year <= max_prize_year) %>% 
    filter(type == "mentioning") %>%
    group_by(years_since_prize) %>% 
    mutate(mentioning_papers_in_year = sum(!duplicated(source_doi))) %>% 
    ungroup() %>% 
    filter(!duplicated(years_since_prize)) %>% 
    filter(between(
      years_since_prize, date_range[1], date_range[2]
    )) %>% 
    select(years_since_prize, mentioning_papers_in_year)
  
  citing_papers = df %>% 
    filter(pub_year <= max_pub_year & prize_year <= max_prize_year) %>%
    group_by(years_since_prize) %>% 
    mutate(citing_papers_in_year = sum(!duplicated(source_doi))) %>% 
    ungroup() %>% 
    filter(!duplicated(years_since_prize)) %>% 
    filter(between(
      years_since_prize, date_range[1], date_range[2]
    )) %>% 
    select(years_since_prize, citing_papers_in_year)
  
  output = full_join(supporting_papers, contrasting_papers,
                     by = "years_since_prize") %>% 
    full_join(mentioning_papers) %>% 
    full_join(citing_papers) %>% 
    arrange(years_since_prize)
  
  return(output)
}
