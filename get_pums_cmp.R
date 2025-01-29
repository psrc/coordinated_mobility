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

# 2. Setup: Helper functions ----------------------------------------
add_vars <- function(df){
  df %<>% mutate(
    age5_17  = case_when(between(AGEP,5,17)  ~ "5-17",
                         !is.na(AGEP)        ~ "not 5-17"),
    over_65  = case_when(AGEP > 64           ~ "65+",
                         between(AGEP,0,64)  ~ "0-64"),
    age65_84 = case_when(between(AGEP,65,84) ~ "65-84",
                         AGEP < 65           ~ "0-64"),
    over_85  = case_when(AGEP > 84           ~ "85+",
                         between(AGEP,0,84)  ~ "0-84"),
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
      levels=c("Low Income", "Not Low Income")),
    lep = factor(                                                              # Low English Proficiency @ individual level
      case_when(AGEP < 5                          ~ NA_character_,
                grepl("^Very", as.character(ENG)) ~ "Speak English less than 'very well'",
                !is.na(ENG)                       ~ "Speak English 'very well'")),
    employment = factor(
      case_when(AGEP < 18                                      ~ NA_character_,
                grepl("^(Civilian|Armed) ", as.character(ESR)) ~ "Employed",
                !is.na(ESR)                                    ~ "Unemployed")),
    veteran = factor(
      case_when(AGEP < 18     ~ NA_character_,
                !is.na("VPS") ~ "Veteran",
                is.na("VPS")  ~ "Not a veteran"),
      levels=c("Veteran","Not a veteran")),
    poc_woman = factor(
      case_when(AGEP < 18                            ~ NA_character_,
                SEX=="Female" & PRACE!="White alone" ~ "POC woman",
                !is.na(SEX) & !is.na(PRACE)          ~ "All others"),
      levels=c("POC woman","All others")))
}

xtab_var1s <- c("DIS","low_inc","employment","lep","poc", "race")
xtab_var2s <- c("age_detail", "race", "age5_17", "over_65", "age65_84", "over_85",
                "low_inc","poc","veteran","DIS", "poc_woman")

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
dyear <- 2023
#pums_rds <- "J:/Projects/Census/AmericanCommunitySurvey/Data/PUMS/pums_rds"   # Network PUMS location, dir=pums_rds
pp_df <- get_psrc_pums(5, dyear, "p", pvars)                                   # retrieve data
pp_df %<>% add_vars()                                                          # add recode variables
#cmpstats <- sapply(xtab_var1s, apply_cr_pums_count, simplify=FALSE, USE.NAMES=TRUE) # calculate all combinations

# calculate specified combinations
cmpstats <- list()
cmpstats$age65_veteran         <- ctyreg_pums_count(c("veteran","over_65"))
cmpstats$age65_84_veteran      <- ctyreg_pums_count(c("veteran","age65_84"))
cmpstats$age85_veteran         <- ctyreg_pums_count(c("veteran","over_85"))

cmpstats$disability_youth      <- ctyreg_pums_count(c("age5_17","DIS"))
cmpstats$disability_age65      <- ctyreg_pums_count(c("over_65","DIS"))
cmpstats$disability_age65_84   <- ctyreg_pums_count(c("age65_84","DIS"))
cmpstats$disability_age85      <- ctyreg_pums_count(c("over_85","DIS"))
cmpstats$disability_veteran    <- ctyreg_pums_count(c("veteran","DIS"))
cmpstats$disability_low_inc    <- ctyreg_pums_count(c("low_inc","DIS"))
cmpstats$disability_poc        <- ctyreg_pums_count(c("poc","DIS"))
cmpstats$disability_race       <- ctyreg_pums_count(c("race","DIS"))

cmpstats$poc_youth             <- ctyreg_pums_count(c("age5_17","poc"))
cmpstats$poc_age65             <- ctyreg_pums_count(c("over_65","poc"))
cmpstats$poc_age65_84          <- ctyreg_pums_count(c("age65_84","poc"))
cmpstats$poc_age85             <- ctyreg_pums_count(c("over_85","poc"))
cmpstats$race_youth            <- ctyreg_pums_count(c("age5_17","race"))
cmpstats$race_age65            <- ctyreg_pums_count(c("over_65","race"))
cmpstats$race_age65_84         <- ctyreg_pums_count(c("age65_84","race"))
cmpstats$race_age85            <- ctyreg_pums_count(c("over_85","race"))

cmpstats$lowinc_youth          <- ctyreg_pums_count(c("age5_17","low_inc"))
cmpstats$lowinc_age65          <- ctyreg_pums_count(c("over_65","low_inc"))
cmpstats$lowinc_age65_84       <- ctyreg_pums_count(c("age65_84","low_inc"))
cmpstats$lowinc_age85          <- ctyreg_pums_count(c("over_85","low_inc"))
cmpstats$lowinc_veteran        <- ctyreg_pums_count(c("veteran","low_inc"))
cmpstats$lowinc_poc            <- ctyreg_pums_count(c("poc","low_inc"))
cmpstats$lowinc_race           <- ctyreg_pums_count(c("race","low_inc"))

cmpstats$employment_low_inc    <- ctyreg_pums_count(c("low_inc","employment"))
cmpstats$employment_poc        <- ctyreg_pums_count(c("poc","employment"))
cmpstats$employment_race       <- ctyreg_pums_count(c("race","employment"))
cmpstats$employment_disability <- ctyreg_pums_count(c("DIS","employment"))
cmpstats$employment_LEP        <- ctyreg_pums_count(c("lep","employment"))
cmpstats$employment_veteran    <- ctyreg_pums_count(c("veteran","employment"))

cmpstats$lep_youth             <- ctyreg_pums_count(c("age5_17","lep"))
cmpstats$lep_age65             <- ctyreg_pums_count(c("over_65","lep"))
cmpstats$lep_age65_84          <- ctyreg_pums_count(c("age65_84","lep"))
cmpstats$lep_age85             <- ctyreg_pums_count(c("over_85","lep"))
cmpstats$lep_low_inc           <- ctyreg_pums_count(c("low_inc","lep"))

write_cmp_pums_xlsx(cmpstats)
