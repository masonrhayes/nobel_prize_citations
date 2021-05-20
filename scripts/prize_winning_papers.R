library(tidyverse)

# Joining data to get prize-winning record

medicine = read_csv("data/medicine_publication_record.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning")) %>% 
  mutate(field = "medicine")


chemistry = read_csv("data/chemistry_publication_record.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning")) %>% 
  mutate(field = "chemistry")

physics = read_csv("data/physics_publication_record.csv") %>%
  setNames(., c("laureate_id", "laureate_name", "prize_year", "title",
                "pub_year", "paper_id", "target_doi", "journal", "affiliation",
                "is_prize_winning")) %>% 
  mutate(field = "physics")


all_fields = bind_rows(medicine, chemistry, physics)

prize_winning_papers = all_fields %>% 
  filter(is_prize_winning == "YES") %>% 
  filter(!is.na(target_doi))


## Same dimensions: good to go
publication_record_prize_winning = read_csv("data/prize-winning_paper_record.csv")

dim(prize_winning_papers)

dim(publication_record_prize_winning)

## NOTE: ----------
# there are 110 papers without DOIs; I drop these

# Write it prize_winning_papers to csv

prize_winning_papers %>% 
  write_csv("data/prize_winning_publication_record_manual.csv")
