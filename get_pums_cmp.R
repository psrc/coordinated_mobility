library(magrittr)
library(psrccensus)
library(dplyr)
library(stringr)
library(data.table)

# 1. Setup: List necessary direct-PUMS variables & table names ------
pvars <- c(
  "AGEP",                   # Age
  "DIS",                    # Disability
  #  "LANP",                   # Language spoken at home
  "PRACE",                  # Individual race (PSRC categories)
  "POVPIP",                 # Income-to-poverty ratio
  "ENG",                    # Ability to speak English
  "ESR",                    # Employment Status
  "VPS"                     # Veteran status
)

# 2. Setup: Helper functions ----------------------------------------
add_vars <- function(df){
  df %<>% mutate(
    adult = case_when(AGEP>17 ~ "Adult", TRUE ~NA_character_),                 # Used largely for filtering
    age_detail = factor(                                                       # Used largely for breakouts
      case_when(is.na(AGEP) ~ NA_character_,
                between(AGEP,65,84) ~ "65-84",
                between(AGEP,5,17)  ~ "5-17",
                AGEP <= 4           ~ "0-4",
                AGEP > 84           ~ "85+",
                between(AGEP,18,64) ~ "18-64")),
    over_65 = case_when(is.na(AGEP) ~ NA_character_,                           # Used largely for filtering
                        AGEP > 64          ~ "65+",
                        between(AGEP,0,64) ~ "0-64"),
    poc = factor(
      case_when(PRACE=="White alone" ~ "Non-POC",
                !is.na(PRACE)        ~ "POC"),
      levels=c("POC","Non-POC")),
    low_inc = case_when(is.na(POVPIP) ~ NA_character_,                         # Low income = 200 pct poverty level
                        POVPIP<200 ~ "Yes",
                        TRUE       ~ "No"),
    lep = factor(                                                              # Low English Proficiency @ individual level
      case_when(AGEP<5 ~ NA_character_,
                !str_detect(ENG, "^Very") ~"Speak English less than 'very well'",
                TRUE                      ~ "Speak English 'very well'")),
    employment = factor(
      case_when(grepl("^(Civilian|Armed) ", as.character(ESR)) ~"Employed",
                !is.na(ESR)                                    ~"Unemployed")),
    veteran = case_when(!is.na("VPS") ~"Veteran",
                        TRUE          ~ NA_character_))
}

xtab_var1s <- c("DIS","low_inc","employment","lep","poc")
xtab_var2s <- c("age_detail","over_65","low_inc","poc","veteran","DIS")

var_vctr <- function(var){
  x <- data.frame(setdiff(xtab_var2s, var), var) %>%
  transpose() %>% c()
}

reg_pums_count <- purrr::partial(psrc_pums_count, pp_df, rr=TRUE, incl_na=FALSE) # set default data object, settings

ctyreg_pums_count <- function(groupvars=NULL){                                 # Function for county + region counts
  rs      <- list()
  rs[[1]] <- reg_pums_count(group_vars=groupvars)                              # incl_na=FALSE option for accurate within-subgroup shares
  rs[[2]] <- reg_pums_count(group_vars=c("COUNTY", groupvars)) %>%
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
dyear <- 2022
pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"    # Network PUMS location
pp_df <- get_psrc_pums(5, dyear, "p", pvars, dir=pums_rds)                     # retrieve data
pp_df %<>% add_vars()                                                          # add recode variables
# cmpstats <- sapply(xtab_var1s, apply_cr_pums_count, simplify=FALSE, USE.NAMES=TRUE) # calculate all combinations

# calculate specified combinations
cmpstats$disability_age_detail <- ctyreg_pums_count(pp_df, c("age_detail","DIS"))
cmpstats$disability_age65      <- ctyreg_pums_count(pp_df, c("over_65","DIS"))
cmpstats$disability_veteran    <- ctyreg_pums_count(pp_df, c("veteran","DIS"))
cmpstats$disability_low_inc    <- ctyreg_pums_count(pp_df, c("low_inc","DIS"))
cmpstats$disability_poc        <- ctyreg_pums_count(pp_df, c("poc","DIS"))

cmpstats$poc_age_detail        <- ctyreg_pums_count(pp_df, c("age_detail","poc"))
cmpstats$poc_age65             <- ctyreg_pums_count(pp_df, c("over_65","poc"))

cmpstats$lowinc_age_detail     <- ctyreg_pums_count(pp_df, c("age_detail","low_inc"))
cmpstats$lowinc_age65          <- ctyreg_pums_count(pp_df, c("over_65","low_inc"))
cmpstats$lowinc_veteran        <- ctyreg_pums_count(pp_df, c("veteran","low_inc"))
cmpstats$lowinc_poc            <- ctyreg_pums_count(pp_df, c("poc","low_inc"))
cmpstats$lowinc_disability     <- ctyreg_pums_count(pp_df, c("DIS","low_inc"))

cmpstats$employment_low_inc    <- ctyreg_pums_count(pp_df, c("low_inc","employment"))
cmpstats$employment_poc        <- ctyreg_pums_count(pp_df, c("poc","employment"))
cmpstats$employment_disability <- ctyreg_pums_count(pp_df, c("DIS","employment"))
cmpstats$employment_LEP        <- ctyreg_pums_count(pp_df, c("lep","employment"))

cmpstats$lep_age_detail        <- ctyreg_pums_count(pp_df, c("age_detail","lep"))
cmpstats$lep_age65             <- ctyreg_pums_count(pp_df, c("over_65","lep"))
cmpstats$lep_low_inc           <- ctyreg_pums_count(pp_df, c("low_inc","lep"))

write_cmp_pums_xlsx(cmpstats)
