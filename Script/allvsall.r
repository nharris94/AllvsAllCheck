library(tidyverse)
library(dplyr)
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
        0.354 <= Kinship ~ "Twin/Duplicate"
    )) %>%
    select(ID1, ID2, Kinship, Relationship)

# use key value pairs to identify families?
family_ID_list <- list()
x <- 1
for (row in 1:nrow(kin_related)) {
    ID1 <- kin_related[row, ID1]
    ID2 <- kin_related[row, ID2]

    # check if ID1/2 is already a key in list
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

# determine proband by seeing who occured most in list for each family
# will be used to with master list to estimate proper family ID
fam_ID <- ""
num_max_occur <- 0
proband_list <- list()
for (ID in names(family_ID_list)) {
    if (family_ID_list[[ID]] != fam_ID) {
        fam_ID <- family_ID_list[[ID]]
        num_max_occur <- 0
    }

    num_occur <- sum(kin_related == ID)
    if (num_occur > num_max_occur) {
        proband_list[[fam_ID]] <- ID
        num_max_occur <- num_occur
    }
}

# use familyID and proband list to state who is proband and sample relation to proband
relation_list <- data.frame(ID = names(family_ID_list), generated_fam_ID = unlist(family_ID_list, use.names=FALSE))
relation_list <- relation_list %>% mutate(estimated_proband = case_when((ID %in% proband_list) ~ "Proband"))
relation_check <- function(ID) {
    proband <- proband_list[[family_ID_list[[ID]]]]

    if (proband == ID) {
        proband_relate <- "Proband"
    } else {
       relation <- kin_related %>% filter(ID1 == proband | ID2 == proband) %>% filter(ID1 == ID | ID2 == ID)
       proband_relate <- relation$Relationship[1]
    }
    return(proband_relate)
}
relation_list <- relation_list %>% mutate(Relation_to_Est_Proband =  mapply(relation_check, ID))
