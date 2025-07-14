# klips_full_replication.R
# Complete pipeline: surveys → household/individual _ix → merges → cleanup

# 0. Setup --------------------------------------------------------------------

# 0.1. Set your project root
setwd("~/Desktop/data-science-presentation")

# 0.2. Load libraries
library(haven)
library(dplyr)
library(tidyr)
library(purrr)
library(readxl)
library(labelled)

# 0.3. Ensure folders exist
dir.create("rawdata/klips", recursive = TRUE, showWarnings = FALSE)
dir.create("processed_data", showWarnings = FALSE)

# 0.4. Wave identifiers
wave_numbers <- sprintf("%02d", 1:20)

# 1. Create additional‐survey files (_a_x.dta) BEFORE any merges ----------------

# Wave 08
read_dta("rawdata/klips/klips08a.dta") %>%
  select(pid, matches("^a08395[1-5]$"), matches("^a08396[1-4]$")) %>%
  write_dta("rawdata/klips/klips08a_x.dta")

# Wave 09
read_dta("rawdata/klips/klips09a.dta") %>%
  select(pid, matches("^a09425[1-4]$"), a094415, a094425, a094435) %>%
  write_dta("rawdata/klips/klips09a_x.dta")

# Wave 11
read_dta("rawdata/klips/klips11a.dta") %>%
  select(pid, matches("^a11626[1-4]$"), a116409) %>%
  write_dta("rawdata/klips/klips11a_x.dta")

# Wave 17
read_dta("rawdata/klips/klips17a.dta") %>%
  select(pid, matches("^a17770[1-9]$"), matches("^a1777[1-2][0-9]$")) %>%
  write_dta("rawdata/klips/klips17a_x.dta")

# Wave 18 part 1
read_dta("rawdata/klips/klips18a1.dta") %>%
  select(pid, starts_with("a1881")) %>%
  write_dta("rawdata/klips/klips18a1_x.dta")

# Wave 18 part 2
read_dta("rawdata/klips/klips18a2.dta") %>%
  select(pid, a188503, a188511, matches("^a18851[7-9]$"), a188520) %>%
  write_dta("rawdata/klips/klips18a2_x.dta")

# 2. Produce household (_h_ix.dta) and individual (_p_ix.dta) files ------------

select_hh <- function(df, n) {
  hhid  <- paste0("hhid", n)
  hwave <- paste0("hwave", n)
  wgh   <- paste0("w", n, "h")
  h0150 <- paste0("h",  n, "0150")
  h1501 <- paste0("h",  n, "1501")
  h1502 <- paste0("h",  n, "1502")
  if (n == "01") {
    df %>% select(
      orghid98, !!sym(hhid), !!sym(hwave), hwaveent, sample98, !!sym(wgh),
      !!sym(h0150), !!sym(h1501), !!sym(h1502),
      matches("^h0121\\d+$"), matches("^h01014\\d+$"),
      matches("^h0122\\d+$"), matches("^h0123\\d+$"),
      matches("^h0124\\d+$"), matches("^h0114\\d+$"),
      matches("^h0126\\d+$")
    )
  } else if (n == "02") {
    df %>% select(
      orghid98, !!sym(hhid), !!sym(hwave), hwaveent, sample98, htype02, !!sym(wgh),
      !!sym(h0150), matches("^h0221\\d+$"), matches("^h0214\\d+$"),
      matches("^h023\\d+$"), matches("^h024\\d+$"), matches("^h214\\d+$"),
      matches("^h025\\d+$"), matches("^h026\\d+$")
    )
  } else if (as.integer(n) <= 12) {
    df %>% select(
      orghid98, !!sym(hhid), !!sym(hwave), hwaveent, sample98, paste0("htype",n), !!sym(wgh),
      starts_with(paste0("h",n,"0150")),
      starts_with(paste0("h",n,"1501")),
      starts_with(paste0("h",n,"1502")),
      matches(paste0("^h",n,"21\\d+$")),
      matches(paste0("^h",n,"014\\d+$")),
      matches(paste0("^h",n,"23\\d+$")),
      matches(paste0("^h",n,"24\\d+$")),
      matches(paste0("^h",n,"14\\d+$")),
      matches(paste0("^h",n,"25\\d+$")),
      matches(paste0("^h",n,"26\\d+$"))
    )
  } else {
    df %>% select(
      orghid98, orghid09, !!sym(hhid), !!sym(hwave), hwaveent,
      sample98, sample09, paste0("htype",n), !!sym(wgh),
      starts_with(paste0("h",n,"0150")),
      starts_with(paste0("h",n,"1501")),
      starts_with(paste0("h",n,"1502")),
      matches(paste0("^h",n,"21\\d+$")),
      matches(paste0("^h",n,"014\\d+$")),
      matches(paste0("^h",n,"23\\d+$")),
      matches(paste0("^h",n,"24\\d+$")),
      matches(paste0("^h",n,"14\\d+$")),
      matches(paste0("^h",n,"25\\d+$")),
      matches(paste0("^h",n,"26\\d+$")),
      paste0("h",n,"4002")
    )
  }
}

select_ind <- function(df, n) {
  hhid <- paste0("hhid", n)
  hmem <- paste0("hmem", n)
  wgp  <- paste0("w", n, "p")
  pfx  <- function(x) paste0("p", n, x)
  df %>% select(
    pid, !!sym(hhid), !!sym(hmem), orghid98, sample98, hwaveent,
    matches(paste0("^", wgp, "$")),
    matches(paste0("^", pfx("0101"), "$")),
    matches(paste0("^", pfx("010[2-8]"), "$")),
    matches(paste0("^", pfx("0110"), "$")),
    matches(paste0("^", pfx("0111"), "$")),
    matches(paste0("^", pfx("020[1-4]"), "$")),
    matches(paste0("^", pfx("033\\d+$"))),
    matches(paste0("^", pfx("0402"), "$")),
    matches(paste0("^", pfx("3121"), "$")),
    matches(paste0("^", pfx("280[1-6]"), "$")),
    matches(paste0("^", pfx("3201"), "$")),
    matches(paste0("^", pfx("43\\d+$"))),
    matches(paste0("^", pfx("45\\d+$"))),
    matches(paste0("^", pfx("5501"), "$")),
    matches(paste0("^", pfx("650[1-8]"), "$")),
    matches(paste0("^", pfx("9074"), "$")),
    matches(paste0("^", pfx("9075"), "$"))
  )
}

for (n in wave_numbers) {
  # Household
  hh_in  <- if(n!="20") paste0("rawdata/klips/klips",n,"h_i.dta")
  else                paste0("rawdata/klips/klips",n,"h.dta")
  hh_df  <- read_dta(hh_in) %>% select_hh(n) %>%
    arrange(across(starts_with("hhid")))
  write_dta(hh_df, paste0("rawdata/klips/klips",n,"h_ix.dta"))
  
  # Individual
  ind_in <- if(n!="20") paste0("rawdata/klips/klips",n,"p_i.dta")
  else               paste0("rawdata/klips/klips",n,"p.dta")
  ind_df <- read_dta(ind_in) %>% select_ind(n) %>%
    arrange(across(starts_with("hhid")))
  write_dta(ind_df, paste0("rawdata/klips/klips",n,"p_ix.dta"))
}

# 2. Cross-sectional merging -----------------------------------------------

for (n in wave_numbers) {
  hh_key <- paste0("hhid",n)
  p_ix   <- read_dta(paste0("rawdata/klips/klips",n,"p_ix.dta"))
  h_ix   <- read_dta(paste0("rawdata/klips/klips",n,"h_ix.dta"))
  common <- intersect(names(p_ix), names(h_ix))
  keep   <- c(hh_key, "orghid98","sample98","hwaveent")
  h_trim <- h_ix %>% select(-all_of(setdiff(common,keep)))
  merged <- p_ix %>% left_join(h_trim, by=keep) %>%
    filter(!is.na(pid)) %>%
    arrange(pid)
  write_dta(merged, paste0("rawdata/klips/klips",n,"hp_ix.dta"))
}

# 3. Merge surveys & cleanup ------------------------------------------------

# Single-file: 08,09,11,17
for (w in c("08","09","11","17")) {
  hp <- read_dta(paste0("rawdata/klips/klips",w,"hp_ix.dta"))
  sx <- read_dta(paste0("rawdata/klips/klips",w,"a_x.dta"))
  write_dta(hp %>% left_join(sx, by="pid") %>% arrange(pid),
            paste0("rawdata/klips/klips",w,"hp_ix.dta"))
}

# Two-part survey 18
hp18 <- read_dta("rawdata/klips/klips18hp_ix.dta") %>%
  left_join(read_dta("rawdata/klips/klips18a1_x.dta"), by="pid") %>%
  left_join(read_dta("rawdata/klips/klips18a2_x.dta"), by="pid") %>%
  arrange(pid)
write_dta(hp18,"rawdata/klips/klips18hp_ix.dta")

# Remove intermediates
rm_files <- c(
  paste0("rawdata/klips/klips", wave_numbers, "h_ix.dta"),
  paste0("rawdata/klips/klips", wave_numbers, "p_ix.dta"),
  paste0("rawdata/klips/klips", c("08","09","11","17"), "a_x.dta"),
  "rawdata/klips/klips18a1_x.dta", "rawdata/klips/klips18a2_x.dta"
)
walk(rm_files, ~ if (file.exists(.x)) file.remove(.x))

message("All cleaning and cross-sectional merging complete.")
source("klips_full_replication.R")
file.exists("rawdata/klips/klips08hp_ix.dta")  # should be TRUE
read_dta("rawdata/klips/klips08hp_ix.dta") %>% dim()
# 4. Job history data (long type)
library(haven)
library(dplyr)

job_data <- read_dta("rawdata/klips/klips20w.dta") %>%
  select(
    pid,
    jobwave,
    jobclass,
    jobtype,
    mainjob,
    j145,
    j150,
    j155,
    matches("^j2\\d+$"),
    matches("^j3\\d+$"),
    j804
  )

# Save cleaned job history
write_dta(job_data, "processed_data/klips20w_x.dta")
file.exists("processed_data/klips20w_x.dta")      # should be TRUE
read_dta("processed_data/klips20w_x.dta") %>% names()
# 5. Wave‐specific variable generation (_ivar files) ---------------------------

library(haven)
library(dplyr)
library(stringr)

wave_numbers <- sprintf("%02d", 1:20)

for (n in wave_numbers) {
  n_num <- as.integer(n)
  i     <- if (n_num < 10) as.character(n_num) else n
  
  df <- read_dta(paste0("rawdata/klips/klips", n, "hp_ix.dta"))
  
  # Identification
  df <- df %>%
    rename_at(vars(paste0("hhid", n)),  ~ paste0("hhid",  i)) %>%
    rename_at(vars(paste0("hmem", n)),  ~ paste0("hmem",  i)) %>%
    rename_at(vars(paste0("hwave", n)), ~ paste0("hwave", i))
  if (n_num == 1) {
    df <- df %>% mutate(!!paste0("htype", i) := 1L)
  } else {
    df <- df %>% rename_at(vars(paste0("htype", n)), ~ paste0("htype", i))
  }
  df <- df %>% mutate(!!paste0("year", i) := 1998 + n_num - 1)
  
  # Demographics (gender → male, relation, byear/bmonth/bday/age, livein)
  df <- df %>%
    rename_at(vars(paste0("p",n,"0101")), ~ paste0("gender", i)) %>%
    mutate(!!paste0("male", i) := case_when(
      get(paste0("gender", i)) == 1 ~ 1L,
      get(paste0("gender", i)) == 0 ~ 0L,
      TRUE                          ~ NA_integer_
    )) %>%
    select(-paste0("gender", i)) %>%
    rename_at(vars(paste0("p",n,"0102")), ~ paste0("relation", i)) %>%
    mutate_at(vars(paste0("p",n,c("0104","0105","0106","0107"))),
              ~ if_else(. == -1, NA_integer_, .)) %>%
    rename_at(vars(paste0("p",n,"0104")), ~ paste0("byear",  i)) %>%
    rename_at(vars(paste0("p",n,"0105")), ~ paste0("bmonth", i)) %>%
    rename_at(vars(paste0("p",n,"0106")), ~ paste0("bday",   i)) %>%
    rename_at(vars(paste0("p",n,"0107")), ~ paste0("age",    i)) %>%
    rename_at(vars(paste0("p",n,"0108")), ~ paste0("livein", i)) %>%
    mutate(!!paste0("livein", i) := if_else(get(paste0("livein", i)) == 2, 0L, get(paste0("livein", i))))
  
  # Education & marital
  df <- df %>%
    mutate(
      !!paste0("edu",   i) := if_else(get(paste0("p",n,"0110")) == -1, NA_integer_, get(paste0("p",n,"0110"))),
      !!paste0("edu_c", i) := if_else(get(paste0("p",n,"0111")) == -1, NA_integer_, get(paste0("p",n,"0111"))),
      !!paste0("marital",i) := if_else(get(paste0("p",n,"5501")) == -1, NA_integer_, get(paste0("p",n,"5501"))),
      !!paste0("married",i) := if_else(get(paste0("marital",i)) >= 2, 1L, 0L)
    ) %>%
    rename_at(vars(paste0("h",n,"0150")), ~ paste0("nmem", i)) %>%
    mutate(!!paste0("children", i) := NA_real_)
  if (n_num != 2) {
    df <- df %>%
      mutate(!!paste0("children", i) := case_when(
        get(paste0("h",n,"1501")) == 2 ~ 0,
        get(paste0("h",n,"1501")) == 1 ~ get(paste0("h",n,"1502")),
        TRUE ~ get(paste0("children", i))
      ))
  }
  
  # Province & city (safe)
  prov <- paste0("h", n, "0141")
  city <- paste0("h", n, "0142")
  if (prov %in% names(df)) df <- df %>% rename(!!paste0("province",i) := !!sym(prov))
  if (city %in% names(df)) df <- df %>% mutate(!!paste0("city",i) := if_else(get(city)==-1, NA_integer_, get(city)))
  
  # Labor Force Status & work codes
  df <- df %>%
    mutate(
      !!paste0("LFS", i) := case_when(
        get(paste0("p",n,"0201")) == 1 ~ 1L,
        get(paste0("p",n,"2801")) == 1 & get(paste0("p",n,"2806")) == 1 ~ 2L,
        TRUE ~ 0L
      ),
      !!paste0("employed",   i) := if_else(get(paste0("LFS", i)) == 1, 1L, 0L),
      !!paste0("unemployed", i) := case_when(
        get(paste0("LFS", i)) == 2 ~ 1L,
        get(paste0("LFS", i)) == 1 ~ 0L,
        TRUE ~ NA_integer_
      ),
      !!paste0("businesscode_old",   i) := get(paste0("p",n,"0330")),
      !!paste0("occupationcode_old", i) := get(paste0("p",n,"0332"))
    )
  if (n_num >= 12) {
    df <- df %>%
      mutate(
        !!paste0("businesscode_new",   i) := get(paste0("p",n,"0331")),
        !!paste0("occupationcode_new", i) := get(paste0("p",n,"0333"))
      )
  }
  
  # Weights: wave 1 has w01p, waves>=2 have wNNp_c & wNNp_l
  if (n_num == 1) {
    df <- df %>%
      rename(!!paste0("wh", i) := paste0("w",n,"h")) %>%
      mutate(
        !!paste0("wpl", i) := get(paste0("w",n,"p")),
        !!paste0("wpc", i) := get(paste0("w",n,"p"))
      ) %>%
      select(-paste0("w",n,"p"))
  } else {
    # rename household weight
    df <- df %>% rename(!!paste0("wh", i) := paste0("w",n,"h"))
    # cross-sectional weights if they exist
    if (paste0("w",n,"p_l") %in% names(df)) {
      df <- df %>% rename(!!paste0("wpl",i) := paste0("w",n,"p_l"))
    }
    if (paste0("w",n,"p_c") %in% names(df)) {
      df <- df %>% rename(!!paste0("wpc",i) := paste0("w",n,"p_c"))
    }
  }
  
  # Drop the original p*/h* columns
  df <- df %>% select(-matches(paste0("^h",n)), -matches(paste0("^p",n)))
  
  # Save ivar file
  write_dta(df, paste0("rawdata/klips/klips",n,"hp_ivar.dta"))
}

message("Wave‐specific _ivar files complete.")
# 6. Longitudinal merge (wide format) -----------------------------------------

library(haven)
library(dplyr)

# Start from wave 1 ivar file
merged_wide <- read_dta("rawdata/klips/klips01hp_ivar.dta")

for (wave_num in 2:20) {
  # Read the next wave
  path <- sprintf("rawdata/klips/klips%02dhp_ivar.dta", wave_num)
  wave_df <- read_dta(path)
  
  # Identify any columns in common (other than pid)
  common <- intersect(names(merged_wide), names(wave_df))
  common <- setdiff(common, "pid")
  
  # Drop those from wave_df so we don't duplicate
  wave_trim <- wave_df %>% select(-all_of(common))
  
  # Full join by pid
  merged_wide <- full_join(merged_wide, wave_trim, by = "pid")
}

# Save the wide panel
write_dta(merged_wide, "processed_data/klips_imerge.dta")
message("Longitudinal wide merge complete.")

# 7. Reshape from wide to long -----------------------------------------------

library(haven)
library(dplyr)
library(tidyr)

# Read the wide file
wide <- read_dta("processed_data/klips_imerge.dta")

# ID variables (do not vary by wave)
id_vars <- c("pid", "orghid98", "orghid09", "sample98", "sample09", "hwaveent")

# Prefixes of variables that vary by wave
varying_prefixes <- c(
  "hhid","hmem","htype","hwave","year",
  "nmem","male","edu","edu_c","province","city",
  "relation","byear","bmonth","bday","age","livein",
  "marital","married","children",
  "LFS","employed","unemployed",
  "businesscode_old","businesscode_new",
  "occupationcode_old","occupationcode_new",
  "risktol","patience","impulsivity",
  "wh","wpl","wpc"
  # add other per‐wave prefixes (e.g. personality_, locus_, reciprocity_, trust_)
)

# Build regex: ^(prefix1|prefix2|...)(\d+)$
pattern <- paste0("^(", paste(varying_prefixes, collapse="|"), ")(\\d+)$")

# Pivot to long
long <- wide %>%
  pivot_longer(
    cols         = matches(pattern),
    names_to     = c(".value", "wave"),
    names_pattern= pattern
  ) %>%
  mutate(wave = as.integer(wave)) %>%
  select(all_of(id_vars), wave, everything())

# Save long panel
write_dta(long, "processed_data/klips_imerge_long.dta")
message("Reshape to long format complete.")
file.exists("processed_data/klips_imerge_long.dta")    # TRUE
read_dta("processed_data/klips_imerge_long.dta") %>% dim()
read_dta("processed_data/klips_imerge_long.dta") %>% names()
# 8. Merge job history into the long panel -----------------------------------

library(haven)
library(dplyr)

# Read long panel and job history
long <- read_dta("processed_data/klips_imerge_long.dta")
jobs <- read_dta("processed_data/klips20w_x.dta")

# Merge on pid and wave (jobwave ↔ wave)
long1 <- long %>%
  left_join(jobs, by = c("pid" = "pid", "wave" = "jobwave"))

# Save intermediate
write_dta(long1, "processed_data/klips_imerge_long1.dta")
message("Job history merged.")

# 9. Outlier treatment on job & wage variables --------------------------------

klips1 <- read_dta("processed_data/klips_imerge_long1.dta")

# Drop logically inconsistent LFS/worktype combos if worktype1 exists
if ("worktype1" %in% names(klips1)) {
  klips1 <- klips1 %>%
    filter(!(unemployed == 1 & worktype1 %in% c(1,3,5,7))) %>%
    filter(!(employed   == 1 & worktype1 %in% c(2,4,6)))
}

# Drop jobtype==3 if jobtype exists
if ("jobtype" %in% names(klips1)) {
  klips1 <- klips1 %>% filter(jobtype != 3)
}

# Function to winsorize wage variables by year‐specific lower bounds
replace_wage <- function(data, var) {
  lb <- case_when(
    data$year == 1998 ~ (1400 + 1485)/40000,
    data$year == 1999 ~ (1485 + 1525)/40000,
    data$year == 2000 ~ (1525 + 1600)/40000,
    data$year == 2001 ~ (1600 + 1865)/40000,
    data$year == 2002 ~ (1865 + 2100)/40000,
    data$year == 2003 ~ (2100 + 2275)/40000,
    data$year == 2004 ~ (2275 + 2510)/40000,
    data$year == 2005 ~ (2510 + 2840)/40000,
    data$year == 2006 ~ (2840 + 3100)/40000,
    data$year == 2007 ~ 3100/20000,
    data$year == 2008 ~ 3480/20000,
    data$year == 2009 ~ 3770/20000,
    data$year == 2010 ~ 4000/20000,
    data$year == 2011 ~ 4110/20000,
    data$year == 2012 ~ 4320/20000,
    data$year == 2013 ~ 4580/20000,
    data$year == 2014 ~ 4860/20000,
    data$year == 2015 ~ 5210/20000,
    TRUE              ~ -Inf
  )
  data[[var]] <- ifelse(data[[var]] < lb, NA, data[[var]])
  data
}

# Apply to any hourly‐wage variables present
for (wvar in intersect(c("hourlywage", "hourlywage_ov", "hourlywage_total"), names(klips1))) {
  klips1 <- replace_wage(klips1, wvar)
}

# Save cleaned wages
write_dta(klips1, "processed_data/klips_imerge_long1.dta")
message("Outlier treatment complete.")

# 10. Merge CPI & deflate money variables -------------------------------------

library(readxl)

# Read and clean CPI sheet
cpi <- read_excel("rawdata/klips/CPI_updated.xlsx", sheet = "Sheet1") %>%
  mutate(across(starts_with("CPI"), as.character)) %>%
  mutate(across(starts_with("CPI"), ~ na_if(.x, "-"))) %>%
  mutate(across(starts_with("CPI"), as.numeric)) %>%
  pivot_longer(
    cols       = starts_with("CPI"),
    names_to   = "wave",
    names_prefix = "CPI",
    values_to  = "CPI"
  ) %>%
  mutate(
    province = as.integer(province),
    wave     = as.integer(wave)
  ) %>%
  filter(province != 0)

# Merge onto klips1
klips2 <- read_dta("processed_data/klips_imerge_long1.dta") %>%
  left_join(cpi, by = c("province", "wave")) %>%
  arrange(pid, wave)

# Identify money variables present
money_vars <- intersect(
  c("annualsales", "monthlyprofit", "annualsalary", "annualsalary_ov", "annualsalary_total",
    "hourlywage", "hourlywage_ov", "hourlywage_total"),
  names(klips2)
)

# Deflate each by CPI and multiply by 100
klips2 <- klips2 %>%
  mutate(across(all_of(money_vars), ~ .x / CPI * 100))

# Save CPI-adjusted data
write_dta(klips2, "processed_data/klips_imerge_long2.dta")
message("CPI merged and money variables deflated.")

# 11. Reorder & label variables (sketched) ------------------------------------

library(labelled)

klips3 <- read_dta("processed_data/klips_imerge_long2.dta")

# Example reorder (add all your vars in the desired order)
desired_order <- c(
  "pid","hhid","hmem","hwave","htype","wave","year",
  "orghid98","orghid09","sample98","sample09","hwaveent",
  "nmem","male","edu","edu_c","province","city","relation",
  "byear","bmonth","bday","age","livein","marital","married","children",
  "LFS","employed","unemployed","businesscode_old","businesscode_new",
  "occupationcode_old","occupationcode_new","jobtype","jobclass","mainjob",
  "worktype1","worktype2","worktype3","selfemployed","overtimeunit",
  "workhour","workhour_ov","workday","workday_ov",
  "monthlywage","monthlywage_ov","monthlyprofit",
  "annualsalary","annualsalary_ov","annualsalary_total",
  "hourlywage","hourlywage_ov","hourlywage_total",
  "risktol","patience","impulsivity","CPI","wh","wpl","wpc"
  # continue with personality_, locus_, reciprocity_, trust_, etc.
)

# Reorder (will keep any variables not in desired_order at the end)
klips3 <- klips3 %>% select(any_of(desired_order), everything())

# Apply variable labels and value labels as defined previously
# var_label(klips3) <- vl
# for each val_labels(klips3)$var <- ...

# Save final labeled dataset
write_dta(klips3, "processed_data/klips_imerge_long2_reordered_labeled.dta")
message("Reordering & labeling complete.")
file.exists("processed_data/klips_imerge_long2_reordered_labeled.dta")
# should print TRUE

read_dta("processed_data/klips_imerge_long2_reordered_labeled.dta") %>% dim()
read_dta("processed_data/klips_imerge_long2_reordered_labeled.dta") %>% names()
