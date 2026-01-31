################################
########## Question 2 ##########
################################


library(admiral)
library(pharmaversesdtm)
library(dplyr)
library(lubridate)
library(stringr)

# pull in data and take away blanks

dm <- convert_blanks_to_na(pharmaversesdtm::dm)
ex <- convert_blanks_to_na(pharmaversesdtm::ex)
ae <- convert_blanks_to_na(pharmaversesdtm::ae)
vs <- convert_blanks_to_na(pharmaversesdtm::vs)
ds <- convert_blanks_to_na(pharmaversesdtm::ds)

# code referenced from pharmaverse github

format_agegr9 <- function(x) {
  case_when(
    x < 18 ~ "<18",
    between(x, 18, 50) ~ "18-50",
    x > 50 ~ ">50",
    TRUE ~ NA_character_
  )
}

format_agegr9n <- function(x) {
  case_when(
    x < 18 ~ 1L,
    between(x, 18, 50) ~ 2L,
    x > 50 ~ 3L,
    TRUE ~ NA_integer_
  )
}


# derive exposure datetime variables for TRTSDTM
# use first exposure record, impute missing hours and minutes but not seconds

# highest_imputation = "h": only impute time components if missing
# time_imputation = "first": so this impute missing time as 00:00 (hours=00, minutes=00)
# ignore_seconds_flag = TRUE 
# if only seconds were missing: only flag when HH or MM imputed

ex_ext <- ex %>%
  derive_vars_dtm(
    dtc = EXSTDTC,
    new_vars_prefix = "EXST",
    highest_imputation = "h",
    time_imputation = "first",
    ignore_seconds_flag = TRUE
  ) %>%
  derive_vars_dtm(
    dtc = EXENDTC,
    new_vars_prefix = "EXEN",
    highest_imputation = "h",
    time_imputation = "first",
    ignore_seconds_flag = TRUE
  )

# start with DM and derive variables
adsl <- dm %>%
  
  # derive treatment variables (TRT01P, TRT01A)
  mutate(
    TRT01P = ARM,
    TRT01A = ACTARM
  ) %>%
  
  # derive TRTSDTM and TRTSTMF (treatment start datetime and time flag)
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) &
      !is.na(EXSTDTM),
    new_vars = exprs(TRTSDTM = EXSTDTM, TRTSTMF = EXSTTMF),
    order = exprs(EXSTDTM, EXSEQ),
    mode = "first",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  
  # derive treatment end datetime (TRTEDTM) for LSTAVLDT calculation
  derive_vars_merged(
    dataset_add = ex_ext,
    filter_add = (EXDOSE > 0 |
      (EXDOSE == 0 &
        str_detect(EXTRT, "PLACEBO"))) & !is.na(EXENDTM),
    new_vars = exprs(TRTEDTM = EXENDTM, TRTETMF = EXENTMF),
    order = exprs(EXENDTM, EXSEQ),
    mode = "last",
    by_vars = exprs(STUDYID, USUBJID)
  ) %>%
  
  # derive TRTSDT/TRTEDT from datetime variables
  derive_vars_dtm_to_dt(source_vars = exprs(TRTSDTM, TRTEDTM)) %>%
  
  # derive AGEGR9 and AGEGR9N from previous function
  mutate(
    AGEGR9 = format_agegr9(AGE),
    AGEGR9N = format_agegr9n(AGE)
  ) %>%
  
  # derive ITTFL
  # set to 'Y' if ARM is not missing, else 'N'
  mutate(
    ITTFL = if_else(!is.na(ARM), "Y", "N")
  )

# derive LSTAVLDT
# set to the last date patient has documented clinical data showing alive

adsl <- adsl %>%
  derive_vars_extreme_event(
    by_vars = exprs(STUDYID, USUBJID),
    events = list(
      
      # last complete date of vital assessment with a valid test result
      event(
        dataset_name = "vs",
        order = exprs(VSDTC, VSSEQ),
        condition = !is.na(VSDTC) & nchar(VSDTC) >= 10 &
          !(is.na(VSSTRESN) & is.na(VSSTRESC)),
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(VSDTC, highest_imputation = "n"),
          seq = VSSEQ
        )
      ),
      
      # last complete onset date of adverse events
      event(
        dataset_name = "ae",
        order = exprs(AESTDTC, AESEQ),
        condition = !is.na(AESTDTC) & nchar(AESTDTC) >= 10,
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(AESTDTC, highest_imputation = "n"),
          seq = AESEQ
        )
      ),
      
      # last completed disposition date
      event(
        dataset_name = "ds",
        order = exprs(DSSTDTC, DSSEQ),
        condition = !is.na(DSSTDTC) & nchar(DSSTDTC) >= 10,
        set_values_to = exprs(
          LSTAVLDT = convert_dtc_to_dt(DSSTDTC, highest_imputation = "n"),
          seq = DSSEQ
        )
      ),
      
      # last date of treatment administration where patient received a valid dose
      event(
        dataset_name = "adsl",
        condition = !is.na(TRTEDT),
        set_values_to = exprs(LSTAVLDT = TRTEDT, seq = NA_integer_)
      )
    ),
    source_datasets = list(vs = vs, ae = ae, ds = ds, adsl = adsl),
    tmp_event_nr_var = event_nr,
    order = exprs(LSTAVLDT, seq, event_nr),
    mode = "last",
    new_vars = exprs(LSTAVLDT)
  )

# get final variables for submission
adsl <- adsl %>%
  select(
    USUBJID,
    AGEGR9, AGEGR9N,
    TRTSDTM, TRTSTMF,
    ITTFL,
    LSTAVLDT
  )

