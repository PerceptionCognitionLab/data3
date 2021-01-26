## Getting policy and comparison data
library(tidyr)
library(dplyr)
library(stringr)

# save(d, file = "../data/raw_dat.RData")

summarisePolicyItems <- function(df, r) {
  dat_policies <- df %>% 
    mutate(
      policy = case_when(
        !is.na(dim_workspace_closing) ~ paste0(dim_workspace_closing, dim_testing, dim_essential_workplaces, '.'),
        !is.na(dim_income_support) ~ paste0(dim_income_support, ', and ', dim_company_support, '.'),
        !is.na(dim_public_events) ~ paste0('Public events have ', dim_public_events, '. Other measured have been put in place to prevent the spread (e.g. 1.5 meters distance, face masks indoors) and they will be enforced ', dim_enforcement, '.'),
        !is.na(dim_school_closing) ~ paste0(dim_school_closing, ' in ', dim_closure_criteria, '. ', dim_testing),
        !is.na(dim_stay_home) ~ paste0('It is ', dim_stay_home, ' that citizens stay at home. When outside, non-medical face masks are ', dim_face_mask)
      )
    ) %>% 
    group_by(observation) %>% 
    fill(url_pid) %>% ## Fill in the pid before removing non item rows
    ungroup() %>% 
    select(-starts_with("dim"))
  dat_policies <- dat_policies[r, ]
  
  dat_policies <- dat_policies %>%
    unite(fairness, colnames(dat_policies)[grep("fairness", colnames(dat_policies))]
          , sep="", na.rm = TRUE)
  dat_policies <- dat_policies %>%
    unite(respect_self, colnames(dat_policies)[grep("self_respect", colnames(dat_policies))]
          , sep="", na.rm = TRUE)
  dat_policies <- dat_policies %>%
    unite(respect_other, colnames(dat_policies)[grep("_respect", colnames(dat_policies))]
          , sep="", na.rm = TRUE)
  
  ## Copy the reasoning for all item rows and remove the reasoning observations
  dat_policies <- dat_policies %>% 
    group_by(url_pid, topic) %>%
    fill(starts_with('Reasoning'),.direction = "up") %>% 
    mutate(
      reasoning_topic = case_when(
        !is.na(reasoning_work) ~ reasoning_work,
        !is.na(reasoning_econ) ~ reasoning_econ,
        !is.na(reasoning_gathering) ~ reasoning_gathering,
        !is.na(reasoning_family) ~ reasoning_family,
        !is.na(reasoning_movement) ~ reasoning_movement
      ),
      fairness = as.double(fairness),
      respect_other = as.double(respect_other),
      respect_self = as.double(respect_self)
    ) %>% 
    filter(!str_detect(sender, 'Reasoning')) %>% 
    select(- c(reasoning_work, reasoning_econ, reasoning_gathering, reasoning_family, reasoning_movement))
  
  return(dat_policies)
}

extractData <- function(d){
  # We start with the policy items
  r <- grep('Item|Reasoning', d$sender) ## Also take the reasoning items
  r <- r[- which(r %in% c(grep("Risk Propensity Item", d$sender), grep("Political Orientation Item", d$sender)))]
  
  ## Select the variables we need (at least the slider, dim and reasoning)
  var_selection <- d %>%
    select(url_pid, observation, sender, timestamp, topic
           , colnames(d)[grep("slider", colnames(d))], colnames(d)[grep("dim", colnames(d))],
           colnames(d)[grep("reasoning", colnames(d))]
           # , colnames(d)[grep("respect", colnames(d))]
    )
  dat_policies <- summarisePolicyItems(var_selection, r)
  
  good <- !duplicated(cbind(dat_policies$timestamp,dat_policies$observation))
  data_slider <- dat_policies[good,]
  data_slider$sid <- data_slider$observation

  return(data_slider)
}

extractComparisonData <- function(d){
  r <- grep('Item|Reasoning|choice', d$sender) ## Also take the reasoning and choice items
  r <- r[- which(r %in% c(grep("Risk Propensity Item", d$sender), grep("Political Orientation Item", d$sender), grep("Loop", d$sender)))]
  
  ## Select the required variables
  var_selection <- d %>%
    select(url_pid, observation, sender, timestamp, topic, response, response_action
           , colnames(d)[grep("slider", colnames(d))], colnames(d)[grep("dim", colnames(d))],
           colnames(d)[grep("reasoning", colnames(d))], colnames(d)[grep("dim", colnames(d))]
    )
  dat_comparisons <- summarisePolicyItems(var_selection, r)
  
  ## To see a normal scenario
  dat_comparisons <- dat_comparisons %>% filter(!url_pid == 205 | url_pid ==265)
  
  ## Order the policies based on choices
  comp_data <- dat_comparisons %>% 
    group_by(url_pid, topic) %>% 
    mutate(
      fairness = na_if(fairness, '')
    ) %>% 
    arrange(desc(fairness), timestamp , .by_group = TRUE) %>% 
    mutate(
      policy_id = ifelse(is.na(response), row_number(), NA_integer_),
      pref_id = ifelse(!is.na(response), row_number(), NA_integer_),
      comp_number = case_when(
        policy_id == 1 | policy_id == 2 | pref_id == 9 ~ 1,
        policy_id == 3 | policy_id == 4 | pref_id == 10 ~ 2,
        policy_id == 5 | policy_id == 6 | pref_id == 11 ~ 3,
        policy_id == 7 | policy_id == 8 | pref_id == 12 ~ 4
      )
    ) %>% 
    group_by(url_pid, topic, comp_number) %>% 
    fill(response, .direction = "downup") %>% 
    ungroup() %>% 
    mutate(
      comp_winner = case_when(
        response == 1 & (policy_id == 1 | policy_id == 3 | policy_id == 5 | policy_id == 7) ~ TRUE,
        response == 2 & (policy_id == 2 | policy_id == 4 | policy_id == 6 | policy_id == 8) ~ TRUE,
        TRUE ~ FALSE
      ),
      comp_against = ifelse((policy_id %% 2) == 0, policy_id - 1, policy_id + 1)
    ) %>% 
    filter(!str_detect(sender, 'choice')) %>% 
    select(- c(response, response_action, pref_id))
    
  good <- !duplicated(cbind(comp_data$timestamp,comp_data$observation))
  data_comparisons <- comp_data[good,]
  
  contentFile <- "../data3/covid_policy/compdata"
  write.csv(file = contentFile, data_comparisons, quote=FALSE, row.names=FALSE)
}
