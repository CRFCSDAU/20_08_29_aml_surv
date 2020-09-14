
  library(readxl)
  library(tidyverse)
  library(janitor)
  library(testthat)
  library(summarytools)


# Data -------------------------------------------------------------------------

  data <- read_excel("data/20_08_29_patient_data_2.xlsx") %>%
    clean_names() %>%
    remove_empty()

  data$id <- 1:nrow(data)


# Inspect data structure -------------------------------------------------------

# view(dfSummary(data))


# Clean ------------------------------------------------------------------------

# One obs with EOS date before the dx date. Correct date from EE.
  data$date_rip_last_f_up[data$date_rip_last_f_up < data$date_of_dx] <- "2019-02-21 UTC"

# OS

  data$outcome[data$outcome == "RIP"] <- "Died"
  data$outcome <- factor(data$outcome)

  data$died_flag <- 0
  data$died_flag[data$outcome == "Died"] <- 1
  # with(data, table(died_flag, outcome))

# PFS

  data$pfs_date <- data$date_rip_last_f_up
  data$pfs_date[!is.na(data$relapse_1_progression)] <-
    data$relapse_1_progression[!is.na(data$relapse_1_progression)]

  data$pfs_outcome <- as.character(data$outcome)
  data$pfs_outcome[!is.na(data$relapse_1_progression)] <- "Remission"
  data$pfs_outcome <- factor(data$pfs_outcome)

  data$pfs_flag <- 0
  data$pfs_flag[data$pfs_outcome %in% c("Died", "Remission")] <- 1
  # with(data, table(pfs_flag, pfs_outcome))

# Time to event variables starting with DX
  data <- data %>%
    mutate(
      dx_to_eos   = as.numeric(difftime(date_rip_last_f_up,    date_of_dx,
                                        units = "weeks")),
      dx_to_pfs   = as.numeric(difftime(pfs_date,              date_of_dx,
                                        units = "weeks")),
      dx_to_rec_1 = as.numeric(difftime(relapse_1_progression, date_of_dx,
                                        units = "weeks")),
      dx_to_rec_2 = as.numeric(difftime(relapse_2_progression, date_of_dx,
                                        units = "weeks")),
      dx_to_rec_3 = as.numeric(difftime(relapse_3_progression, date_of_dx,
                                        units = "weeks"))
    )


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

