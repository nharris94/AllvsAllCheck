library(tidyverse)
library(data.table)
library(yaml)
library(here)

yaml_file <- read_yaml(here("Input", "config.yaml"))

kin <- fread(yaml_file$User_variable$kin0_path)
kin_related <- kin %>% filter(Kinship >= 0.0884)
kin_related <- kin_related %>%
    mutate("Relationship" = case_when(
        (0.177 <= Kinship & Kinship < 0.354) ~ "1st-degree",
        (0.0884 <= Kinship & Kinship < 0.177) ~ "2nd-degree",
        (0.0442 <= Kinship & Kinship < 0.0884) ~ "3rd-degree",
        0.354 <= Kinship ~ "Twin/Duplicate")) %>%
    select(ID1, ID2, Kinship, Relationship)

# use key value pairs to identify families?
family_ID_list <- list()
x <- 1
for (row in 1:nrow(kin_related)) {
    ID1 <- kin_related[row, ID1]
    ID2 <- kin_related[row, ID2]

    #check if ID1/2 is already a key in list
    if (ID1 %in% names(family_ID_list)) {
        family_ID_list[[ID2]] <- family_ID_list[[ID1]]
    } else if (ID2 %in% names(family_ID_list)) {
       family_ID_list[[ID1]] <- family_ID_list[[ID2]]
    } else {
       fam_ID <- paste0("family", x)
       family_ID_list[[ID1]] <- fam_ID
       family_ID_list[[ID2]] <- fam_ID
       x <- x + 1
    }
}