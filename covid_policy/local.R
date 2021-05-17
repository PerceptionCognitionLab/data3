## Getting policy and comparison data
library(tidyr)
library(dplyr)
library(stringr)

# save(d, file = "../data/raw_dat.RData")

summarisePolicyItems <- function(df, r) {
  dat_policies <- df %>% 
    mutate(
      policy = case_when(
        !is.na(dim_workspace_closing) ~ paste0(dim_workspace_closing, replace_na(dim_testing, ''), dim_essential_workplaces, '.'),
        !is.na(dim_income_support) ~ paste0(dim_income_support, ', and ', dim_company_support, '.'),
        !is.na(dim_public_events) ~ paste0('Public events have ', dim_public_events, '. Other measured have been put in place to prevent the spread (e.g. 1.5 meters distance, face masks indoors) and they will be enforced ', dim_enforcement, '.'),
        !is.na(dim_school_closing) ~ paste0(dim_school_closing, ' in ', dim_closure_criteria, '. ', replace_na(dim_testing, '')),
        !is.na(dim_stay_home) ~ paste0('It is ', dim_stay_home, ' that citizens stay at home. When outside, non-medical face masks are ', dim_face_mask)
      )
    ) %>% 
    group_by(observation) %>% 
    fill(url_pid) %>% ## Fill in the pid before removing non item rows
    ungroup()
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
    select(- c(reasoning_work, reasoning_econ, reasoning_gathering, reasoning_family, reasoning_movement)) %>% 
    ungroup()
  
  return(dat_policies)
}

definePolicyIds <- function(df) {
  
  return(df %>% 
           mutate(
             policy_dim_Id = case_when(
               !is.na(dim_workspace_closing) ~ 'W_',
               !is.na(dim_income_support) ~ 'E_',
               !is.na(dim_public_events) ~ 'G_',
               !is.na(dim_school_closing) ~ 'S_',
               !is.na(dim_stay_home) ~ 'M_'
             ),
             policy_dim_Id = case_when(
               dim_stay_home == 'not required' ~ paste0(policy_dim_Id, 'A0'),
               dim_stay_home == 'recommended' ~ paste0(policy_dim_Id, 'A1'),
               dim_stay_home == 'required' ~ paste0(policy_dim_Id, 'A2'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_face_mask == 'not required' ~ paste0(policy_dim_Id, 'B0'),
               dim_face_mask == 'mandatory in public transport (and need to be purchased by citizens)' ~ paste0(policy_dim_Id, 'B1'),
               dim_face_mask == 'mandatory in public transport, stores, bars and restaurants (and need to be purchased by citizens)' ~ paste0(policy_dim_Id, 'B2'),
               dim_face_mask == 'mandatory whenever outside of one\'s residence (and need to be purchased by citizens)' ~ paste0(policy_dim_Id, 'B3'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_public_events == 'not been cancelled'  ~ paste0(policy_dim_Id, 'A0'),
               dim_public_events == 'been canceled above 1000 people' ~ paste0(policy_dim_Id, 'A1'),
               dim_public_events == 'been canceled above 100 people' ~ paste0(policy_dim_Id, 'A2'),
               dim_public_events == 'been canceled above 10 people' ~ paste0(policy_dim_Id, 'A3'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_enforcement == 'as usual policies (no change from non COVID related policies)' ~ paste0(policy_dim_Id, 'B0'),
               dim_enforcement == 'as usual policies, plus specific COVID related enforcement forces (city workers, or police officers managing traffic and distance)' | dim_enforcement == 'as usual policies, plus specific COVID-related enforcement forces (city workers, or police officers managing traffic and distance)' ~ paste0(policy_dim_Id, 'B1'),
               dim_enforcement == 'extensively, with police presence at every busy street, actively managing flows of people and handing out fines' ~ paste0(policy_dim_Id, 'B2'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_school_closing == 'All schools are open' ~ paste0(policy_dim_Id, 'A0'),
               dim_school_closing == 'Elementary and high schools are closed (daycare and preschool remain open as they cannot provide online classes)' ~ paste0(policy_dim_Id, 'A1'),
               dim_school_closing == 'All schools are closed (except daycare for essential workers)' ~ paste0(policy_dim_Id, 'A2'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_closure_criteria == 'the entire country' ~ paste0(policy_dim_Id, 'B0'),
               dim_closure_criteria == 'regions where less than 50% of the ICU capacity is reached, all schools are closed in other regions' ~ paste0(policy_dim_Id, 'B1'),
               dim_closure_criteria == 'some regions, while not in others' ~ paste0(policy_dim_Id, 'B2'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_testing == 'There is no specific testing policy concerning schools.' ~ paste0(policy_dim_Id, 'C0'),
               dim_testing == 'Additionally, all children and instructors are tested once a week (only negative/recovered people are allowed in the school).' ~ paste0(policy_dim_Id, 'C1'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_workspace_closing == 'It is recommended to work from home when possible, and shops can remain open' ~ paste0(policy_dim_Id, 'A0'),
               dim_workspace_closing == 'All contact professions are required to close (offices may remain open)' ~ paste0(policy_dim_Id, 'A1'),
               dim_workspace_closing == 'All non-essential workplaces are required to close(keeping open: grocery stores, medical professions, limited transport workers, food and medical manufacturing, food delivery, garbage collection)' ~ paste0(policy_dim_Id, 'A2'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_testing == '. There is no specific testing policy' ~ paste0(policy_dim_Id, 'B0'), 
               dim_testing == '. Only negative or recovered people are allowed into workplaces (the employer takes charge of tests)' ~ paste0(policy_dim_Id, 'B1'), 
               dim_testing == '. Only negative or recovered people are allowed into workplaces (the state takes charge of tests)' ~ paste0(policy_dim_Id, 'B2'), 
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_essential_workplaces == ', and no specific workplace policy has been introduced' ~ paste0(policy_dim_Id, 'C0'), 
               dim_essential_workplaces == '. Furthermore, additional safety measures have also been introduced (e.g. accommodated working hours and shifts, mandatory mask wearing, contactless delivery...)' ~ paste0(policy_dim_Id, 'C1'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_income_support == 'No income support is available for individuals' ~ paste0(policy_dim_Id, 'A0'), 
               dim_income_support == 'Income support is available for individuals who cannot work' ~ paste0(policy_dim_Id, 'A1'),
               dim_income_support == 'Income support is available in the shape of a flat sum for all workers' ~ paste0(policy_dim_Id, 'A2'),
               TRUE ~ policy_dim_Id
             ),
             policy_dim_Id = case_when(
               dim_company_support == 'no support is available for companies' ~ paste0(policy_dim_Id, 'B0'), 
               dim_company_support == 'support is available for all small and medium enterprises (SMEs)' ~ paste0(policy_dim_Id, 'B1'), 
               dim_company_support == 'support is available for affected industries' ~ paste0(policy_dim_Id, 'B2'), 
               dim_company_support == 'support is available for all industries' ~ paste0(policy_dim_Id, 'B3'), 
               TRUE ~ policy_dim_Id
             )
           )
  )
}

extractData=function(d){
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
  dat_policies <- definePolicyIds(dat_policies)
  
  good <- !duplicated(cbind(dat_policies$timestamp,dat_policies$observation))
  data_slider <- dat_policies[good,]
  data_slider$sid <- data_slider$url_pid
  
  contentFile <- "../data3/covid_policy/sliderdata"
  write.csv(file = contentFile, data_slider, quote=T, row.names=FALSE)
  
  return(data_slider)
}

extractComparisonData = function(d){
  r <- grep('Item|Reasoning|choice', d$sender) ## Also take the reasoning and choice items
  r <- r[- which(r %in% c(grep("Risk Propensity Item", d$sender), grep("Political Orientation Item", d$sender), grep("Loop", d$sender)))]
  
  ## Select the required variables
  var_selection <- d %>%
    select(url_pid, observation, sender, timestamp, topic, response, response_action
           , colnames(d)[grep("slider", colnames(d))], colnames(d)[grep("dim", colnames(d))],
           colnames(d)[grep("reasoning", colnames(d))], colnames(d)[grep("dim", colnames(d))]
    )
  dat_comparisons <- summarisePolicyItems(var_selection, r)
  dat_comparisons <- definePolicyIds(dat_comparisons)
  ## To see a normal scenario
  #dat_comparisons <- dat_comparisons %>% filter(!url_pid == 205 | url_pid ==265)
  
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
  write.csv(file = contentFile, data_comparisons, quote=TRUE, row.names=FALSE)
}

extractComparisonData(d)

## Getting respondent data

getRespondentData <- function(d){
  ## Select the variables we need (at least the slider, dim and reasoning)
  respondent_characteristics <- d %>%
    select(url_pid, observation, sender, timestamp, gender, self_desctipt, age, origin_country, migrated,
           migrated_closer, household_composition, nr_children, age_youngest_child, postcode, highest_education, work_before_covid,
           contract_type_before_covid, industry, profession, covid_work, work_hours, extra_care, physical_distancing,
           physical_distancing_others, meta_user_agent, meta_platform, meta_language,
           meta_time_zone, attitude_risk_question, attitude_political_question, starts_with('dollar')) %>% 
    group_by(observation) %>% 
    fill(url_pid) %>%
    fill(5:28, .direction = 'updown') %>% 
    filter((!is.na(attitude_risk_question)) | (!is.na(attitude_political_question))) %>%
    mutate(
      attitude = ifelse(is.na(attitude_risk_question), 'politics', 'risk'),
      item = coalesce(attitude_risk_question, attitude_political_question),
      score = rowSums(across(30:59), na.rm = T)
    ) %>% 
    select(-c(starts_with('dollar'), attitude_risk_question, attitude_political_question)) %>% 
    ungroup()
  
  contentFile <- "../data3/covid_policy/partdata"
  write.csv(file = contentFile, respondent_characteristics, quote=TRUE, row.names=FALSE)
}

getRespondentData(d)
