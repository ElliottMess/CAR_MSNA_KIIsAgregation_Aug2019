library(dplyr)
library(tibble)

#Where the files are
CLEANED_DATASET <- paste0("input/MSNA_H2R_dataset_100919.csv")
FORM_SURVEY <- paste0("input/Questionnaire_Kobo__MSNA_ki_MF_MP_1108 - FINAL_v2_survey.csv")
AGGREGATED_DATASET <- paste0("output/REACH_CAR_MSNA_AggregatedKIs_cleanedData.csv")



# Function to aggregate results for questions where yes ("oui") takes over other responses

aok_oui <- function(x) {
  if ("oui" %in% x) {
    return("oui")
  }
  else if("non" %in% x) {
    return("non")
  }
  else {
    return("")
  }
}
# Function to decide how the ties are dealt with between type of respondents.
## Inhabitants of villages (habitant) take over all other
## Direct contacts takes over other types
## Remote contact and others (autre) are last

oui_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)]) #getting unique choices
  utype <- unique(type[!is.na(type)]) #getting unique type of respondents
  
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) { # Checks that there is a tie
    tab <- tabulate(match(type, utype)) #list all ties
    if("habitant" %in% utype[tab == max(tab)]){ 
      aok_oui(filter(data.frame(x, type), type == "habitant") %>% select(x)) # apply aggregate function
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      aok_oui(filter(data.frame(x, type), type == "direct_contact") %>% select(x)) 
    }else{
      aok_oui(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x))
    }
  }
  else {
    aok_oui(x) ## If no ties, normal decision
  }
}


# Function to aggregate results for questions where no ("non") takes over other responses

aok_non <- function(x) {
  if ("non" %in% x) {
    return("non")
  }
  else if("oui" %in% x) {
    return("oui")
  }
  
  else {
    return("")
  }
}

# Function to decide how the ties are dealt with between type of respondents.
## Inhabitants of villages (habitant) take over all other
## Direct contacts takes over other types
## Remote contact and others (autre) are last
non_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)])
  utype <- unique(type[!is.na(type)])
  
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    tab <- tabulate(match(type, utype))
    if("habitant" %in% utype[tab == max(tab)]){
      aok_non(filter(data.frame(x, type), type == "habitant") %>% select(x))
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      aok_non(filter(data.frame(x, type), type == "direct_contact") %>% select(x))
    }else{
      aok_non(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x))
    }
  }
  else {
    aok_non(x) ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
}



## Aggregating function to calculate mode (most picked answer), while outputting NC (No consensus) if we don't have a clear winner.

aok_mode <- function(x) {
  ux <- unique(x[!is.na(x)])
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    if (class(x) == "logical") {
      return(as.logical("")) # Blanks are coerced to values in logical vectors, so we specifically identify columns with TRUE/FALSE (KoBo's select multiples) and output a "logical" blank.
    }
    else {
      return("NC")
    }
  }
  else {
    ux[which.max(tabulate(match(x, ux)))] ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
  
}

# Function to decide how the ties are dealt with between type of respondents.
## Inhabitants of villages (habitant) take over all other
## Direct contacts takes over other types
## Remote contact and others (autre) are last
mode_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)])
  utype <- unique(type[!is.na(type)])
  
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    tab <- tabulate(match(type, utype))
    if("habitant" %in% utype[tab == max(tab)]){
      aok_mode(filter(data.frame(x, type), type == "habitant") %>% select(x))
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      aok_mode(filter(data.frame(x, type), type == "direct_contact") %>% select(x))
    }else{
      aok_mode(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x))
    }
  }
  else {
    aok_mode(x) ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
}

#Aggregating function to pick most grave livelihoods coping strategies
aok_lcs <- function(x){
  if("oui" %in% x) {
    return("oui")
  }
  else if("non_strategie_epuisee_12_mois" %in% x) {
    return("non_strategie_epuisee_12_mois")
  }
  else if("non_pas_besoin" %in% x) {
    return("non_pas_besoin")
  }
  else if("non_pertinent" %in% x) {
    return("non_pertinent")
  }
  else {
    return("")
  }
}

# Function to decide how the ties are dealt with between type of respondents.
## Inhabitants of villages (habitant) take over all other
## Direct contacts takes over other types
## Remote contact and others (autre) are last
lcs_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)])
  utype <- unique(type[!is.na(type)])
  
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    tab <- tabulate(match(type, utype))
    if("habitant" %in% utype[tab == max(tab)]){
      aok_non(filter(data.frame(x, type), type == "habitant") %>% select(x))
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      aok_non(filter(data.frame(x, type), type == "direct_contact") %>% select(x))
    }else{
      aok_non(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x))
    }
  }
  else {
    aok_non(x) ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
}

#Aggregating function to pick most grave statisfaction
aok_satisf <- function(x){
  if("pas suffisant du tout" %in% x) {
    return("pas suffisant du tout")
  }
  else if("insuffisant" %in% x) {
    return("insuffisant")
  }
  else if("juste assez" %in% x) {
    return("non_pas_besoin")
  }
  else if("suffisant" %in% x) {
    return("suffisant")
  }
  else if("plus_que_suffisant" %in% x) {
    return("plus_que_suffisant")
  }
  else {
    return("")
  }
}

# Function to decide how the ties are dealt with between type of respondents.
## Inhabitants of villages (habitant) take over all other
## Direct contacts takes over other types
## Remote contact and others (autre) are last
satisf_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)])
  utype <- unique(type[!is.na(type)])
  
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    tab <- tabulate(match(type, utype))
    if("habitant" %in% utype[tab == max(tab)]){
      aok_satisf(filter(data.frame(x, type), type == "habitant") %>% select(x))
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      aok_satisf(filter(data.frame(x, type), type == "direct_contact") %>% select(x))
    }else{
      aok_satisf(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x))
    }
  }
  else {
    aok_satisf(x) ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
}

#Aggregating function to pick most grave statisfaction
aok_prop <- function(x){
  if("majorite" %in% x) {
    return("majorite")
  }
  else if("plus_moitie" %in% x) {
    return("plus_moitie")
  }
  else if("moins_moitie" %in% x) {
    return("moins_moitie")
  }
  else if("quelques" %in% x) {
    return("quelques")
  }
  else {
    return("")
  }
}

# Function to decide how the ties are dealt with between type of respondents.
## Inhabitants of villages (habitant) take over all other
## Direct contacts takes over other types
## Remote contact and others (autre) are last
prop_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)])
  utype <- unique(type[!is.na(type)])
  
  # This checks to see if we have more than one mode (a tie), return blank if so.
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    tab <- tabulate(match(type, utype))
    if("habitant" %in% utype[tab == max(tab)]){
      aok_satisf(filter(data.frame(x, type), type == "habitant") %>% select(x))
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      aok_satisf(filter(data.frame(x, type), type == "direct_contact") %>% select(x))
    }else{
      aok_satisf(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x))
    }
  }
  else {
    aok_prop(x) ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
}

# Function to decide how the ties are dealt with between type of respondents.
integer_decideType <- function(x, type) {
  ux <- unique(x[!is.na(x)])
  utype <- unique(type[!is.na(type)])
  if (length(which(tabulate(match(x, ux)) == max(tabulate(match(x, ux))))) > 1) {
    tab <- tabulate(match(type, utype))
    if("habitant" %in% utype[tab == max(tab)]){
      mean(filter(data.frame(x, type), type == "habitant") %>% select(x), na.rm = TRUE)
    }else if("direct_contact" %in% utype[tab == max(tab)]){
      mean(filter(data.frame(x, type), type == "direct_contact") %>% select(x), na.rm = TRUE)
    }else{
      mean(filter(data.frame(x, type), type %in% c("remote_contact", "autre")) %>% select(x), na.rm = TRUE)
    }
  }
  else {
    mean(x, na.rm = TRUE) ## This occurs if no tie, so we return the value which has max value! Wambam.
  }
}

#Reading data and form
d_f <- read.csv(CLEANED_DATASET, stringsAsFactors = FALSE, skipNul = TRUE)
form_survey <- read.delim(FORM_SURVEY, sep= ";", stringsAsFactors = FALSE, skipNul = TRUE)

#removing "other" (autre) columns
col_autres <- form_survey[form_survey$type == "text" & grepl("_autre",form_survey$name), "name"]
#identifying select_multiple column
col_selectMultStart <- form_survey[grepl("^select_multiple ", form_survey$type), "name"]
col_selectMultChoice <- names(d_f)[grep("\\.", names(d_f))]

# Removing useless or will-be invalid columns (uuid, etc.)
col_Xs <- names(d_f)[grep("^X_", names(d_f))]
d_f <- d_f %>%
  mutate_at(col_selectMultChoice, as.logical)

d_f <- select(d_f, -start, -end, -today,-diviceid, -col_autres, -contains("note"), - "q0_2_enqueteur", - "q0_3_date", -col_Xs)

# saving starting order
base_colOrder <- names(d_f)
base_colOrder <- base_colOrder[!base_colOrder %in% "type_contact"]

# identifying localisation questions
ques_loc <- c("info_prefecture","info_sous_prefecture", "info_commune", "info_loc_H2R")

# identifying always yes questions
ques_Oui <- c("wash_9_latrines_20_personnes",
         "sante_4_1_violence","sante_4_2_violence","sante_4_3_violence","sante_4_4_violence",
         "nut_1_malnutrition", "nut_2_malnutrition_centre", "nut_4_malnutrition_deces",
         "protect_1_handicap_enfant", "protect_1_handicap_adulte",
         "protect_2", "protect_3", "protect_4", "protect_7", "protect_10",
         "protect_11_1", "protect_11_2", "protect_11_3", "protect_11_4")

# applying aggregation to questions
loc_Oui <- d_f %>%
  select( ques_loc, ques_Oui, type_contact)%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(oui_decideType, type = quo(type_contact))

# always no question
ques_non <- c("mssc_8_acces_agri",
              "wash_5_1_acces_boire","wash_5_2_acces_cuisiner","wash_5_3_acces_hygiene")

loc_Non <- d_f%>%
  select(ques_loc,ques_non, type_contact )%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(non_decideType, type = quo(type_contact))

# LCS questions
ques_lcs <- c("secal_6_1_lcs_vente_actifs_non_productifs","secal_6_2_lcs_vente_actifs_productifs", "secal_6_3_lcs_reduire_depenses_non_alimentaires",
         "secal_6_4_depenser_epargne","secal_6_5_lcs_emprunter_argent_nourriture", "secal_6_6_lcs_retirer_enfants_ecole", "secal_6_7_lcs_vendre_maison_terrain",
         "secal_6_8_lcs_activites_illegales", "secal_6_9_lcs_mendier", "secal_6_10_lcs_vendre_animaux", "secal_6_11_lcs_consommer_semences")

loc_lcs <- d_f %>%
  select(ques_loc,ques_lcs, type_contact)%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(lcs_decideType, type = quo(type_contact))

ques_satisf <- c("wash_6_assez_eau", "sante_8_services_sante")

#Satisfaction question
loc_satisf <- d_f %>%
  select(ques_loc,ques_satisf, type_contact)%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(satisf_decideType, type = quo(type_contact))

ques_prop <- c("protect_9")

#Satisfaction question
loc_prop <- d_f %>%
  select(ques_loc,ques_prop, type_contact)%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(prop_decideType, type = quo(type_contact))

# Identifying integer
integer_ques <- form_survey[form_survey$type %in% c("integer", "decimal", "calculate"),"name"]

loc_integer <- d_f%>%
  select(ques_loc, integer_ques, type_contact )%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(integer_decideType, type = quo(type_contact))



# Already analysed columns
dejaTraite_col <- unique(c( names(loc_satisf), names(loc_lcs), names(loc_Oui), names(loc_Non), names(loc_integer)))
dejaTraite_col <- dejaTraite_col[5:length(dejaTraite_col)]

# Identifying all equal questions 
equal_cols <- setdiff(setdiff(names(d_f),ques_loc), dejaTraite_col)


loc_egual <- d_f %>%
  select( ques_loc, equal_cols, type_contact)%>%
  group_by(info_prefecture,info_sous_prefecture, info_commune, info_loc_H2R)%>%
  summarize_all(mode_decideType, type = quo(type_contact))

        
# Aggregating all frames
loc_aggregated <- loc_egual%>%
  left_join(loc_satisf, by = c ("info_prefecture","info_sous_prefecture", "info_commune", "info_loc_H2R"))%>%
  left_join(loc_integer, by = c ("info_prefecture","info_sous_prefecture", "info_commune", "info_loc_H2R"))%>%
  left_join(loc_lcs, by = c ("info_prefecture","info_sous_prefecture", "info_commune", "info_loc_H2R"))%>%
  left_join(loc_Non, by = c ("info_prefecture","info_sous_prefecture", "info_commune", "info_loc_H2R"))%>%
  left_join(loc_Oui, by = c ("info_prefecture","info_sous_prefecture", "info_commune", "info_loc_H2R"))%>%
  select(base_colOrder)

write.csv(loc_aggregated, AGGREGATED_DATASET)
