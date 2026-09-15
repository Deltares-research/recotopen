
library(tidyverse)
library(DT)
library(htmltools)

source("r/lookuptables.R")
source("r/functions.R")

resultdir <- 'resultaat'

draaitabel <- read_csv2(file.path(resultdir, "draaitabel_alles.csv"))

## do only once, to create a mapping table for the names
# tibble(originalName = names(draaitabel1)) %>%
#   write_csv2(file.path("data/deltares/names_mapping", "names_draaitabel1.csv"))

namesMapping <- read_csv2(file.path("data/deltares/names_mapping", "names_draaitabel1.csv"))

lookup = namesMapping$originalName
names(lookup) = namesMapping$niceName

draaitabel1 <- draaitabel %>%
  rename(all_of(lookup))

draaitabel2 = draaitabel1 %>%
  mutate(
    zonering2 = case_when(
      ZONERING == "Aquatisch" ~ "Aquatisch",
      ZONERING != "Aquatisch" ~ "Oever"
    )
  ) %>%
  distinct(zonering2, systeem, ECOTOOP) %>%
  mutate(systeem = systemen[as.character(systeem)])


mogelijke_zoneringen <- draaitabel2 %>% 
  distinct(zonering2) %>%
  unlist() %>%
  unname()

# test voor verwijderen van non-word tekens en vervangen met dash
# str_replace_all("hoe is het vandaag", regex("\\W+"), "-")

for(x in mogelijke_zoneringen){
  
  cat(paste("##",                                                 # level
            x,                                                     # title
            "{#sec-", str_replace_all(x, regex("\\W+"), "-"), "}", # tag
            "\n", "\n", "\n"))                                     # line breaks

  mogelijke_systemen2 = draaitabel2 %>% 
    filter(zonering2 == x) %>% 
    distinct(systeem) %>%
    unlist() %>% 
    unname()
  
  for(y in mogelijke_systemen2){
    
    cat(paste("###", 
              x, ">", y, 
              "{#sec-", str_replace_all(y, regex("\\W+"), "-"), "}",
              "\n", "\n", "\n"))
    
    mogelijke_ecotopen <- draaitabel2 %>%
      filter(
        zonering2 == x,
        systeem == y
      ) %>%
      distinct(ECOTOOP) %>%
      unlist() %>% 
      unname()
    
    for(z in mogelijke_ecotopen){
      cat(paste("#### ", 
                 x, ">", y, ">", z, 
                " {#sec-", str_replace_all(z, regex("\\W+"), "-"), "}",
                " \n", " \n", " \n"))
    }
  }
}
