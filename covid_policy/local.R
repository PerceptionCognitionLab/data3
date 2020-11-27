library(tidyr)
library(dplyr)

extractData=function(d){
  # We start with the policy items
  r <- grep("Item", d$sender)
  r <- r[- which(r %in% c(grep("Risk Propensity Item", d$sender), grep("Political Orientation Item", d$sender)))]
  dat_policies <- d %>%
    select(url_pid, observation, sender, timestamp, topic
           , stay_home, face_mask, school_closing, closure_criteria
           , colnames(d)[grep("fairness", colnames(d))]
           , colnames(d)[grep("respect", colnames(d))]
           )

  dat_policies <- dat_policies %>%
    unite(fairness, colnames(dat_policies)[grep("fairness", colnames(dat_policies))]
                        , sep="", na.rm = TRUE)
  dat_policies <- dat_policies %>%
    unite(respect_self, colnames(dat_policies)[grep("self_respect", colnames(dat_policies))]
          , sep="", na.rm = TRUE)
  dat_policies <- dat_policies %>%
    unite(respect_other, colnames(dat_policies)[grep("_respect", colnames(dat_policies))]
          , sep="", na.rm = TRUE)
  dat_policies$url_pid <- rep(dat_policies$url_pid[1], nrow(dat_policies))

  dat_policies <- dat_policies[r, ]
  good <- !duplicated(cbind(dat_policies$timestamp,dat_policies$observation))
  dat <- dat_policies[good,]
  dat$sid <- dat$observation
  return(dat)
}


