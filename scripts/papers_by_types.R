source("scripts/functions.R")

# Run first the first few lines of the "scite_nobel_{field}.R" scripts (3 of them: medicine, chemistry, physics) to get the necessary "{field}_nobel_data" datasets


# Number of papers with a certain citation type --------------
date_range = c(-10,10)
max_prize_year = 2010
max_pub_year = 2010

# Chemistry --------
chem_papers_by_type = chemistry_nobel_data %>% 
  #filter(is_prize_winning == "YES") %>% 
  scite_papers(max_prize_year, max_pub_year,
               date_range = date_range) %>% 
  mutate(field = "chemistry")

# Medicine ---------

med_papers_by_type = med_nobel_data %>% 
  #filter(is_prize_winning == "YES") %>% 
  scite_papers(max_prize_year, max_pub_year,
               date_range = date_range) %>% 
  mutate(field = "medicine")

# Physics ------

phys_papers_by_type = physics_nobel_data %>% 
  #filter(is_prize_winning == "YES") %>% 
  scite_papers(max_prize_year, max_pub_year,
               date_range = date_range) %>% 
  mutate(field = "physics")

# All fields -------

papers_by_type = bind_rows(chem_papers_by_type, med_papers_by_type, phys_papers_by_type)

papers_by_type %>%
  filter(between(years_since_prize, -5,5)) %>% 
  ggplot(aes(years_since_prize, supporting_papers_in_year/(citing_papers_in_year), color = field))+
  geom_line()+
  #geom_smooth(se = FALSE, aes(color = field)) +
  scale_color_wsj()+
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(title = "Papers with supporting evidence as a share of all papers that provide evidence (supporting or contrasting)", x = "Years Since Prize", y = "Supporting paper share")
  

papers_by_type %>%
  filter(between(years_since_prize, -8,8)) %>% 
  ggplot(aes(years_since_prize, contrasting_papers_in_year, color = field))+
  geom_line()+
  scale_color_wsj()+
  theme_minimal() +
  geom_vline(xintercept = 0)+
  labs(title = "Number of papers that support the work of Nobel Prize-winners", x = "Years Since Prize", y = "Number of papers with at least one supporting citation statement")
