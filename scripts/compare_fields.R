source("scripts/functions.R")


phys_summary_all = phys_summary_all %>% 
  mutate(field = "physics")

chem_summary_all = chem_summary_all %>% 
  mutate(field = "chemistry")

med_summary_all = med_summary_all %>% 
  mutate(field = "medicine")

summary_all = bind_rows(phys_summary_all,
                        chem_summary_all,
                        med_summary_all)

view(summary_all)

var = "supporting_in_year"
citation_type = "Supporting"

date_range = c(-10,10)

summary_all %>%
  filter(
    between(years_since_prize, date_range[1], date_range[2])
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes_string("years_since_prize", str_interp("${var}/citing_papers")),
         environment = environment()) +
  geom_line(aes(color = field)) +
  scale_color_wsj()+
  theme_minimal() +
  geom_vline(xintercept = 0) +
  labs(title = str_interp("${citation_type} Citation Statements per Citing Paper by years since Nobel Prize was awarded"), y = str_interp("${citation_type} Citation Statements per Citing Paper"), x = "Years Since Prize")


summary_all %>%
  filter(
    between(years_since_prize, date_range[1], date_range[2])
  ) %>% 
  filter(!is.na(two_year_si)) %>% 
  ggplot(aes_string("years_since_prize", "two_year_si"),
         environment = environment()) +
  geom_line(aes(color = field)) +
  scale_color_wsj()+
  theme_minimal() +
  geom_vline(xintercept = 0) +
  ylim(0.75,1)+
  labs(title = str_interp("Two Year SI by years since Nobel Prize in ${field} was awarded"), y = str_interp("Two Year SI"), x = "Years Since Prize")

