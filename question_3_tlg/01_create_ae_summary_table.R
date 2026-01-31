################################
########## Question 3 ##########
################################


library(gtsummary)
library(pharmaverseadam)
library(dplyr)


# load adverse events and subject level dataset from pharmaverseadam
adae <- pharmaverseadam::adae
adsl <- pharmaverseadam::adsl

# filter to only treatment emergent adverse events
# so only events that started after treatment began
# also exclude screen failure subjects (only keep placebo and treatment arms)
adae_te <- adae %>%
  filter(TRTEMFL == "Y") %>%
  filter(ACTARM != "Screen Failure")

adsl <- adsl %>% filter(ACTARM != "Screen Failure")

# create one row per subject id per system organ class
# this also dedups so only counts once per system organ class
# and then label and count subjects with at least one adverse event
aesoc_data <- adae_te %>%
  distinct(USUBJID, ACTARM, AESOC) %>%
  mutate(has_ae = "Yes")


# reshape wide and merge with all subjects
# starts with all subjects from subject level data
# then pivot wider so one row per subject, with columns for each system organ class affected
# merge with full dataset and then filter out subjects that didnt have that system organ class
aesoc_full <- adsl %>%
  select(USUBJID, ACTARM) %>%
  left_join(
    aesoc_data %>%
      tidyr::pivot_wider(names_from = AESOC, values_from = has_ae,
                         values_fill = "No"),
    by = c("USUBJID", "ACTARM")
  )

aesoc_full[is.na(aesoc_full)] <- "No"


# remove subject id (no need for summary)
# stratify by treatment arm
# then count and calculate percentage for each soc
tbl_aesoc <- aesoc_full %>%
  select(-USUBJID) %>%
  tbl_summary(
    by = ACTARM,
    statistic = all_categorical() ~ "{n} ({p}%)"
  ) %>%
  modify_header(label = "**System Organ Class**") %>%
  modify_caption("**Treatment Emergent AEs by System Organ Class**")

# save the table as pdf
tbl_aesoc %>%
  as_gt() %>%
  gt::gtsave("ae_summary_table.pdf")
