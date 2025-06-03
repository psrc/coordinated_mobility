library(magrittr)
library(psrccensus)
library(dplyr)
library(stringr)
library(data.table)

# 1. Setup: List necessary direct-PUMS variables & table names ------
pvars <- c(
  "AGEP",                   # Age
  "DIS",                    # Disability
  "PRACE",                  # Individual race (PSRC categories)
  "POVPIP",                 # Income-to-poverty ratio
  "ENG",                    # Ability to speak English
  "ESR",                    # Employment Status
  "VPS",                    # Veteran status
  "SEX"                     # Female or Male
)

hvars <- c(
  "HDIS",                   # Presence of disabled persons in household
  "POVPIP",                 # Income-to-poverty ratio
  "PRACE",                  # Householder race (PSRC categories)
  "R65",                    # Presence of persons over 65 years in household
  "VEH"                     # Household vehicles
)

# 2. Setup: Helper functions ----------------------------------------
add_shared_vars <- function(df){
  df %<>% mutate(
    poc = factor(
      case_when(PRACE=="White alone" ~ "Non-POC",
                !is.na(PRACE)        ~ "POC"),
      levels=c("POC","Non-POC")),
    race = forcats::fct_relevel(
      factor(
        str_replace(
          str_replace(as.character(PRACE), "Latino", "Latinx"),
          "( or African American)? alone", "")),
      c("Some Other Race", "Two or More Races"), after = Inf),
    low_inc = factor(
      case_when(is.na(POVPIP)  ~ NA_character_,                                # Low income = 200 pct poverty level
                POVPIP < 200   ~ "Low Income",
                !is.na(POVPIP) ~ "Not Low Income"),
      levels=c("Low Income", "Not Low Income"))
  )
}

add_pp_vars <- function(df){
  df %<>% mutate(
    age5_17  = case_when(between(AGEP,5,17)  ~ "5-17",
                         !is.na(AGEP)        ~ "not 5-17"),
    over_65  = case_when(AGEP > 64           ~ "65+",
                         !is.na(AGEP)        ~ "not 65+"),
    age65_84 = case_when(between(AGEP,65,84) ~ "65-84",
                         !is.na(AGEP)        ~ "not 65-84"),
    over_85  = case_when(AGEP > 84           ~ "85+",
                         !is.na(AGEP)        ~ "not 85+"),
    lep = factor(                                                              # Low English Proficiency @ individual level
      case_when(AGEP < 5                          ~ NA_character_,
                grepl("^Very", as.character(ENG)) ~ "Speak English less than 'very well'",
                TRUE                              ~ "Speak English 'very well'")),
    employment = factor(
      case_when(AGEP < 18 | grepl("^Not in", as.character(ESR)) ~ NA_character_,
                grepl("^(Civilian|Armed) ", as.character(ESR)) ~ "Employed",
                !is.na(ESR)                                    ~ "Unemployed")),
    veteran = factor(
      case_when(AGEP < 18                ~ NA_character_,
                is.na(as.character(VPS)) ~ "Not a veteran",
                !is.na("VPS")            ~ "Veteran"),
      levels=c("Veteran","Not a veteran")),
    poc_woman = factor(
      case_when(AGEP < 18                            ~ NA_character_,
                SEX=="Female" & PRACE!="White alone" ~ "POC woman",
                !is.na(SEX) & !is.na(PRACE)          ~ "All others"),
      levels=c("POC woman","All others")))
}

add_hh_vars <- function(df){
  df %<>% mutate(
    zero_veh=factor(
      case_when(grepl("^No ", as.character(VEH)) ~"Yes",
                grepl("^\\d ", as.character(VEH)) ~"No"),
      levels=c("Yes","No")),
    h65=factor(
      case_when(grepl("^[12] ", as.character(R65))        ~"Person age 65+ in household",
                as.character(R65)=="No person 65 and over" ~"No person age 65+ in household"),
      levels=paste(c("Person", "No Person"), "age 65+ in household"))
    )
}

xtab_var1s <- c("DIS","low_inc","employment","lep","poc", "race")
xtab_var2s <- c("race", "age5_17", "over_65", "age65_84", "over_85",
                "low_inc","poc","veteran","DIS", "poc_woman", "lep")

var_vctr <- function(var){
  x <- data.frame(setdiff(xtab_var2s, var), var) %>%
    transpose() %>% c()
}

reg_pums_count <- purrr::partial(psrc_pums_count, rr=TRUE, incl_na=FALSE)      # set default data object, settings

ctyreg_pums_count <- function(so, groupvars=NULL){                             # Function for county + region counts
  rs      <- list()
  rs[[1]] <- reg_pums_count(so, group_vars=groupvars)                          # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- reg_pums_count(so, group_vars=c("COUNTY", groupvars)) %>%
    filter(COUNTY!="Region")                                                   # Remove duplicate total level
  rs %<>% rbindlist() %>% arrange(DATA_YEAR, COUNTY)                           # Combine county & regional results
  return(rs)
}

apply_cr_pums_count <- function(var){
  x <- sapply(var_vctr(var), ctyreg_pums_count, simplify=FALSE, USE.NAMES=TRUE)
}

# Write all tables to file
write_cmp_pums_xlsx <- function(result_list){
  rs <- result_list #%>% unlist(recursive=FALSE, use.names=TRUE)
  openxlsx::write.xlsx(rs, file = "cmp_outfile.xlsx",
                       sheetName = names(rs), rowNames = FALSE)
  return(invisible(NULL))
}

# 3. Retrieve data & calculate statistics ---------------------------
dyear <- 2023
pums_rds <- "C:/Users/mjensen/projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"
#pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"    # Network PUMS location,
pp_df <- get_psrc_pums(5, dyear, "p", pvars, dir=pums_rds)                     # retrieve data
hh_df <- get_psrc_pums(5, dyear, "h", hvars, dir=pums_rds)
pp_df %<>% add_pp_vars() %>% add_shared_vars()                                 # add recode variables
hh_df %<>% add_hh_vars() %>% add_shared_vars()
#cmpstats <- sapply(xtab_var1s, apply_cr_pums_count, simplify=FALSE, USE.NAMES=TRUE) # calculate all combinations

# calculate specified combinations
cmpstats <- list()
cmpstats$age65_disability      <- ctyreg_pums_count(pp_df, c("over_65","DIS"))
cmpstats$age65_84_disability   <- ctyreg_pums_count(pp_df, c("age65_84","DIS"))
cmpstats$age85_disability      <- ctyreg_pums_count(pp_df, c("over_85","DIS"))
cmpstats$age65_lowinc          <- ctyreg_pums_count(pp_df, c("over_65","low_inc"))
cmpstats$age65_84_lowinc       <- ctyreg_pums_count(pp_df, c("age65_84","low_inc"))
cmpstats$age85_lowinc          <- ctyreg_pums_count(pp_df, c("over_85","low_inc"))
cmpstats$age65_lep             <- ctyreg_pums_count(pp_df, c("over_65","lep"))
cmpstats$age65_84_lep          <- ctyreg_pums_count(pp_df, c("age65_84","lep"))
cmpstats$age85_lep             <- ctyreg_pums_count(pp_df, c("over_85","lep"))
cmpstats$age65_poc             <- ctyreg_pums_count(pp_df, c("over_65","poc"))
cmpstats$age65_84_poc          <- ctyreg_pums_count(pp_df, c("age65_84","poc"))
cmpstats$age85_poc             <- ctyreg_pums_count(pp_df, c("over_85","poc"))
cmpstats$age65_race            <- ctyreg_pums_count(pp_df, c("over_65","race"))
cmpstats$age65_84_race         <- ctyreg_pums_count(pp_df, c("age65_84","race"))
cmpstats$age85_race            <- ctyreg_pums_count(pp_df, c("over_85","race"))

cmpstats$youth_disability      <- ctyreg_pums_count(pp_df, c("age5_17","DIS"))
cmpstats$youth_lowinc          <- ctyreg_pums_count(pp_df, c("age5_17","low_inc"))
cmpstats$youth_poc             <- ctyreg_pums_count(pp_df, c("age5_17","poc"))
cmpstats$youth_race            <- ctyreg_pums_count(pp_df, c("age5_17","race"))
cmpstats$youth_lep             <- ctyreg_pums_count(pp_df, c("age5_17","lep"))

cmpstats$disability_low_inc    <- ctyreg_pums_count(pp_df, c("DIS", "low_inc"))
cmpstats$disability_employment <- ctyreg_pums_count(pp_df, c("DIS","employment"))

cmpstats$low_inc_employment    <- ctyreg_pums_count(pp_df, c("low_inc","employment"))
cmpstats$low_inc_poc_woman     <- ctyreg_pums_count(pp_df, c("low_inc", "poc_woman"))

cmpstats$veteran_age65         <- ctyreg_pums_count(pp_df, c("veteran","over_65"))
cmpstats$veteran_age65_84      <- ctyreg_pums_count(pp_df, c("veteran","age65_84"))
cmpstats$veteran_age85         <- ctyreg_pums_count(pp_df, c("veteran","over_85"))
cmpstats$veteran_disability    <- ctyreg_pums_count(pp_df, c("veteran","DIS"))
cmpstats$veteran_lowinc        <- ctyreg_pums_count(pp_df, c("veteran","low_inc"))
cmpstats$veteran_employment    <- ctyreg_pums_count(pp_df, c("veteran","employment"))

cmpstats$lep_employment        <- ctyreg_pums_count(pp_df, c("lep","employment"))
cmpstats$lep_low_inc           <- ctyreg_pums_count(pp_df, c("lep", "low_inc"))

cmpstats$poc_lowinc            <- ctyreg_pums_count(pp_df, c("poc","low_inc"))
cmpstats$race_lowinc           <- ctyreg_pums_count(pp_df, c("race","low_inc"))
cmpstats$poc_disability        <- ctyreg_pums_count(pp_df, c("poc","DIS"))
cmpstats$race_disability       <- ctyreg_pums_count(pp_df, c("race","DIS"))
cmpstats$poc_employment        <- ctyreg_pums_count(pp_df, c("poc","employment"))
cmpstats$race_employment       <- ctyreg_pums_count(pp_df, c("race","employment"))

cmpstats$zero_veh_disability   <- ctyreg_pums_count(hh_df, c("zero_veh","HDIS"))
cmpstats$zero_veh_low_inc      <- ctyreg_pums_count(hh_df, c("zero_veh","low_inc"))
cmpstats$zero_veh_age65        <- ctyreg_pums_count(hh_df, c("zero_veh","h65"))
cmpstats$zero_veh_ppoc         <- ctyreg_pums_count(hh_df, c("zero_veh","poc"))
cmpstats$zero_veh_prace        <- ctyreg_pums_count(hh_df, c("zero_veh","race"))

write_cmp_pums_xlsx(cmpstats)
