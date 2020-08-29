
  library(readxl)
  library(tidyverse)
  library(janitor)
  library(testthat)
  library(summarytools)


# Data -------------------------------------------------------------------------

  data <- read_excel("data/20_08_29_patient_data.xlsx") %>%
    clean_names() %>%
    remove_empty()

  data$id <- 1:nrow(data)


# Inspect data structure -------------------------------------------------------

# view(dfSummary(data))


# Clean ------------------------------------------------------------------------

# One wrong dx date
# No obvious correction. Set for NA and query.                           # QUERY
  data$date_of_dx[data$id == 9] <- NA

# 3 missing a follow-up time. Set to max value                           # QUERY
  data$date_rip_last_f_up[is.na(data$date_rip_last_f_up)] <- "2020-03-30 00:00:00"

# Not all are followed up to the same point.                             # QUERY

# Date for TX is always before the date of DX                             #QUERY
# data_frame(
#   dx = data$date_of_dx,
#   tx = data$x1st_treatment,
#   tx_dx = dx < tx
# ) %>%
#   View()

# One patient has a EOS < Dx or Tx times. Set missng for now.            # QUERY
# View(filter(data, dx_to_eos < 5))
  data$date_rip_last_f_up[data$date_rip_last_f_up < data$x1st_treatment] <- NA


# Time to event variables starting with DX
  data <- data %>%
    mutate(
      dx_to_tx    = as.numeric(difftime(x1st_treatment,        date_of_dx,
                                        units = "weeks")),
      dx_to_eos   = as.numeric(difftime(date_rip_last_f_up,    date_of_dx,
                                        units = "weeks")),
      dx_to_rec_1 = as.numeric(difftime(relapse_1_progression, date_of_dx,
                                        units = "weeks")),
      dx_to_rec_2 = as.numeric(difftime(relapse_2_progression, date_of_dx,
                                        units = "weeks")),
      dx_to_rec_3 = as.numeric(difftime(relapse_3_progression, date_of_dx,
                                        units = "weeks"))
    )


# Time to event variables starting with TX
  data <- data %>%
    mutate(
      tx_to_dx    = as.numeric(difftime(date_of_dx,            x1st_treatment,
                                        units = "weeks")),
      tx_to_eos   = as.numeric(difftime(date_rip_last_f_up,    x1st_treatment,
                                        units = "weeks")),
      tx_to_rec_1 = as.numeric(difftime(relapse_1_progression, x1st_treatment,
                                        units = "weeks")),
      tx_to_rec_2 = as.numeric(difftime(relapse_2_progression, x1st_treatment,
                                        units = "weeks")),
      tx_to_rec_3 = as.numeric(difftime(relapse_3_progression, x1st_treatment,
                                        units = "weeks"))
    )

# Died

  data$died <- 0
  data$died[data$outcome == "RIP"] <- 1

# Factors

  data$gender <- factor(data$gender, labels = c("Female", "Male"))
  data$p53_exp <- factor(data$p53_exp)
  data$bcl2_percent[data$bcl2_percent == "negative"] <- "Negative"
  data$bcl2_percent <- factor(data$bcl2_percent)


# Save -------------------------------------------------------------------------
  save(data, file = "data/data.RData")
  rm(list = ls())
  load("data/data.RData")


# New fancy tables

# https://cran.r-project.org/web/packages/tangram/vignettes/example.html
# library(tangram)

