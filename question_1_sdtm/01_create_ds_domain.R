################################
########## Question 1 ##########
################################


library(pharmaverseraw)
library(sdtm.oak)
library(dplyr)
library(admiral)

# load data convert blank to na
raw_data <- convert_blanks_to_na(pharmaverseraw::ds_raw)

# load controlled terminology
# this contains mappings for DSDECOD, VISIT, and VISITNUM
study_ct <- read.csv("sdtm_ct.csv")


# generate_oak_id_vars: to create internal tracking column used by sdtm.oak
# oak_id: unique row identifier
# raw_source: tracks which source dataset the row came from
# patient_number: sub id from PATNUM
#
# noticed a formatting inconsistency in CT
# raw data has "Ambul Ecg Removal" but CT expects "Ambul ECG Removal"
raw_data <- raw_data %>%
  generate_oak_id_vars(
    pat_var = "PATNUM",
    raw_src = "raw_data"
  ) %>%
  mutate(
    INSTANCE = case_when(
      INSTANCE == "Ambul Ecg Removal" ~ "Ambul ECG Removal",
      TRUE ~ INSTANCE
    )
  )

# assign_no_ct: maps raw variable to SDTM variable WITHOUT controlled terminology
# maps IT.DSTERM (standard disposition terms) to DSTERM
# maps OTHERSP to DSTERM
#   this overwrites DSTERM when OTHERSP has a value (for non standard terms)
ds_data <- assign_no_ct(
  raw_dat = raw_data,
  raw_var = "IT.DSTERM",
  tgt_var = "DSTERM",
  id_vars = oak_id_vars()
) %>%
  assign_no_ct(
    raw_dat = raw_data,
    raw_var = "OTHERSP",
    tgt_var = "DSTERM",
    id_vars = oak_id_vars()
  )


ds_data <- ds_data %>%
  # DSDECOD: standardized disposition code
  # assign_ct: map raw variable using controlled terminology 
  #   - ct_spec: The CT specification file (study_ct)
  #   - ct_clst: The codelist to use (C66727 = disposition event codelist)
  assign_ct(
    raw_dat = raw_data,
    raw_var = "IT.DSDECOD",
    tgt_var = "DSDECOD",
    ct_spec = study_ct,
    ct_clst = "C66727",
    id_vars = oak_id_vars()
  ) %>%
  # for other entries use OTHERSP as DSDECOD (no CT mapping)
  assign_no_ct(
    raw_dat = raw_data,
    raw_var = "OTHERSP",
    tgt_var = "DSDECOD",
    id_vars = oak_id_vars()
  ) %>%
  # VISIT: maps INSTANCE to standardized VISIT using CT
  assign_ct(
    raw_dat = raw_data,
    raw_var = "INSTANCE",
    tgt_var = "VISIT",
    ct_spec = study_ct,
    ct_clst = "VISIT",
    id_vars = oak_id_vars()
  ) %>%
  # VISITNUM: maps INSTANCE to numeric visit number using CT
  assign_ct(
    raw_dat = raw_data,
    raw_var = "INSTANCE",
    tgt_var = "VISITNUM",
    ct_spec = study_ct,
    ct_clst = "VISITNUM",
    id_vars = oak_id_vars()
  )  %>%
  # DSDTC: combines date (DSDTCOL) and time (DSTMCOL) into ISO 8601 datetime
  # raw_fmt: specifies input formats, date can be "m-d-y" or "mm-dd-yyyy", time is "H:M"
  assign_datetime(
    raw_dat = raw_data,
    raw_var = c("DSDTCOL", "DSTMCOL"),
    raw_fmt = c(list(c("m-d-y", "mm-dd-yyyy")),"H:M"),
    tgt_var = "DSDTC"
  ) %>%
  # DSSTDTC: start date/time of disposition event (date only, from IT.DSSTDAT)
  assign_datetime(
    raw_dat = raw_data,
    raw_var = "IT.DSSTDAT",
    raw_fmt = "m-d-y",
    tgt_var = "DSSTDTC",
    id_vars = oak_id_vars()
  )


# RFSTDTC: reference start date (first dose date) needed for DSSTDY from dm data
dm_date <- pharmaversesdtm::dm %>%
  select(USUBJID, RFSTDTC)

ds <- ds_data %>%
  mutate(
    # standard SDTM variables
    STUDYID = raw_data$STUDY,
    DOMAIN = "DS",
    
    # USUBJID: unique subject ID (format: "01-" + patient number)
    USUBJID = paste0("01-", raw_data$PATNUM),
    
    # ensure DSTERM and DSDECOD are uppercase
    DSTERM = toupper(DSTERM),
    DSDECOD = toupper(DSDECOD),
    
    # DSCAT: category of disposition event
    #   - "PROTOCOL MILESTONE" for randomization
    #   - "OTHER EVENT" for final visits (lab, retrieval)
    #   - "DISPOSITION EVENT" for all other disposition records
    DSCAT = case_when(
      toupper(DSTERM) == "RANDOMIZED" ~ "PROTOCOL MILESTONE",
      toupper(DSTERM) %in% c("FINAL LAB VISIT", "FINAL RETRIEVAL VISIT") ~
        "OTHER EVENT",
      TRUE ~ "DISPOSITION EVENT")) %>%
  
  # sort by subject and date before generating sequence number
  arrange(USUBJID, DSSTDTC) %>%
  
  # DSSEQ: Sequence number - unique within subject, ordered by date
  # derive_seq: generates sequential numbers within each USUBJID
  derive_seq(
    tgt_var = "DSSEQ",
    rec_vars = c("USUBJID", "DSSTDTC")
  ) %>%
  
  # DSSTDY: study day of disposition event (relative to RFSTDTC)
  # derive_study_day: calculates days from reference date

  derive_study_day(
    sdtm_in = .,
    dm_domain = dm_date,
    tgdt = "DSSTDTC",
    refdt = "RFSTDTC",
    study_day_var = "DSSTDY",
    merge_key = "USUBJID"
  ) %>%
  
  # select final SDTM DS domain variables 
  select(STUDYID, DOMAIN, USUBJID, DSSEQ, DSTERM, DSDECOD, DSCAT, VISITNUM,
         VISIT, DSDTC, DSSTDTC, DSSTDY)

