# Load necessary libraries
library(haven)
library(dplyr)
library(purrr)
library(tidyr)

# Clear workspace (similar to Stata's 'clear all')
rm(list = ls())

################################################################################
#                         Data Cleaning 
################################################################################

# Create a list of wave numbers (01-20 with leading zeros)
wave_numbers <- sprintf("%02d", 1:20)

### Household data processing
for (n in wave_numbers) {
  n_num <- as.numeric(n)  # Convert to numeric for comparisons
  
  # Determine file name based on wave number
  if (n_num < 20) {
    file_path <- paste0("rawdata/klips/klips", n, "h_i.dta")
  } else {
    file_path <- paste0("rawdata/klips/klips", n, "h.dta")
  }
  
  # Read the data
  hh_data <- read_dta(file_path)
  
  # Select variables based on wave number
  if (n_num == 1) {
    hh_data <- hh_data %>%
      select(orghid98, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "1501$")), 
             matches(paste0("^h", n, "1502$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "22\\d+$")),
             matches(paste0("^h", n, "23\\d+$")), matches(paste0("^h", n, "24\\d+$")),
             matches(paste0("^h", n, "14\\d+$")), matches(paste0("^h", n, "26\\d+$")))
  } else if (n_num == 2) {
    hh_data <- hh_data %>%
      select(orghid98, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, matches(paste0("^htype", n, "$")), matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "23\\d+$")),
             matches(paste0("^h", n, "24\\d+$")), matches(paste0("^h", n, "14\\d+$")),
             matches(paste0("^h", n, "25\\d+$")), matches(paste0("^h", n, "26\\d+$")),
             matches(paste0("^h", n, "08\\d+$")))
  } else if (n_num == 3) {
    hh_data <- hh_data %>%
      select(orghid98, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, matches(paste0("^htype", n, "$")), matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "1501$")), 
             matches(paste0("^h", n, "1502$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "23\\d+$")),
             matches(paste0("^h", n, "24\\d+$")), matches(paste0("^h", n, "14\\d+$")),
             matches(paste0("^h", n, "25\\d+$")), matches(paste0("^h", n, "26\\d+$")),
             matches(paste0("^h", n, "08\\d+$")))
  } else if (n_num %in% c(4, 5)) {
    hh_data <- hh_data %>%
      select(orghid98, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, matches(paste0("^htype", n, "$")), matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "1501$")), 
             matches(paste0("^h", n, "1502$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "23\\d+$")),
             matches(paste0("^h", n, "24\\d+$")), matches(paste0("^h", n, "14\\d+$")),
             matches(paste0("^h", n, "25\\d+$")), matches(paste0("^h", n, "26\\d+$")))
  } else if (n_num == 6) {
    hh_data <- hh_data %>%
      select(orghid98, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, matches(paste0("^htype", n, "$")), matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "1501$")), 
             matches(paste0("^h", n, "1502$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "23\\d+$")),
             matches(paste0("^h", n, "24\\d+$")), matches(paste0("^h", n, "14\\d+$")),
             matches(paste0("^h", n, "25\\d+$")), matches(paste0("^h", n, "26\\d+$")))
  } else if (n_num <= 12) {
    hh_data <- hh_data %>%
      select(orghid98, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, matches(paste0("^htype", n, "$")), matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "1501$")), 
             matches(paste0("^h", n, "1502$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "23\\d+$")),
             matches(paste0("^h", n, "24\\d+$")), matches(paste0("^h", n, "14\\d+$")),
             matches(paste0("^h", n, "25\\d+$")), matches(paste0("^h", n, "26\\d+$")))
  } else if (n_num >= 13) {
    hh_data <- hh_data %>%
      select(orghid98, orghid09, matches(paste0("^hhid", n, "$")), matches(paste0("^hwave", n, "$")), 
             hwaveent, sample98, sample09, matches(paste0("^htype", n, "$")), matches(paste0("^w", n, "h$")), 
             matches(paste0("^h", n, "0150$")), matches(paste0("^h", n, "1501$")), 
             matches(paste0("^h", n, "1502$")), matches(paste0("^h", n, "21\\d+$")),
             matches(paste0("^h", n, "014\\d+$")), matches(paste0("^h", n, "23\\d+$")),
             matches(paste0("^h", n, "24\\d+$")), matches(paste0("^h", n, "14\\d+$")),
             matches(paste0("^h", n, "25\\d+$")), matches(paste0("^h", n, "26\\d+$")),
             matches(paste0("^h", n, "4002$")))
  }
  
  # Sort by household ID
  hh_data <- hh_data %>% arrange(across(matches(paste0("^hhid", n, "$"))))
  
  # Save the processed data
  save_path <- paste0("rawdata/klips/klips", n, "h_ix.dta")
  write_dta(hh_data, save_path)
}

### Individual data processing
for (n in wave_numbers) {
  n_num <- as.numeric(n)  # Convert to numeric for comparisons
  
  # Determine file name based on wave number
  if (n_num < 20) {
    file_path <- paste0("rawdata/klips/klips", n, "p_i.dta")
  } else {
    file_path <- paste0("rawdata/klips/klips", n, "p.dta")
  }
  
  # Read the data
  ind_data <- read_dta(file_path)
  
  # Select variables based on wave number
  if (n_num == 1) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^w", n, "p$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "0402$")), matches(paste0("^p", n, "3121$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "3201$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "5501$")), matches(paste0("^p", n, "650[1-8]$")),
             matches(paste0("^p", n, "9074$")), matches(paste0("^p", n, "9075$")))
  } else if (n_num == 2) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "43\\d+$")),
             matches(paste0("^p", n, "45\\d+$")), matches(paste0("^p", n, "47\\d+$")),
             matches(paste0("^p", n, "5501$")), matches(paste0("^p", n, "650[1-8]$")),
             matches(paste0("^p", n, "9074$")), matches(paste0("^p", n, "9075$")))
  } else if (n_num == 3) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "43\\d+$")),
             matches(paste0("^p", n, "5501$")), matches(paste0("^p", n, "650[1-8]$")),
             matches(paste0("^p", n, "9074$")), matches(paste0("^p", n, "9075$")))
  } else if (n_num == 4) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "2904$")),
             matches(paste0("^p", n, "2905$")), matches(paste0("^p", n, "3201$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "5501$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")))
  } else if (n_num == 5) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "2904$")),
             matches(paste0("^p", n, "2905$")), matches(paste0("^p", n, "420\\d+$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "501[1-6]$")),
             matches(paste0("^p", n, "51\\d+$")), matches(paste0("^p", n, "5501$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")))
  } else if (n_num == 6) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "420\\d+$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "48\\d+$")),
             matches(paste0("^p", n, "51\\d+$")), matches(paste0("^p", n, "5501$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")), matches(paste0("^p", n, "610\\d+$")))
  } else if (n_num == 7) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "3201$")),
             matches(paste0("^p", n, "420\\d+$")), matches(paste0("^p", n, "43\\d+$")),
             matches(paste0("^p", n, "45\\d+$")), matches(paste0("^p", n, "47\\d+$")),
             matches(paste0("^p", n, "48\\d+$")), matches(paste0("^p", n, "51\\d+$")),
             matches(paste0("^p", n, "5501$")), matches(paste0("^p", n, "640[1-5]$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")), matches(paste0("^p", n, "610\\d+$")))
  } else if (n_num == 8) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "3201$")),
             matches(paste0("^p", n, "420\\d+$")), matches(paste0("^p", n, "43\\d+$")),
             matches(paste0("^p", n, "45\\d+$")), matches(paste0("^p", n, "47\\d+$")),
             matches(paste0("^p", n, "48\\d+$")), matches(paste0("^p", n, "51\\d+$")),
             matches(paste0("^p", n, "5501$")), matches(paste0("^p", n, "650[1-8]$")),
             matches(paste0("^p", n, "9074$")), matches(paste0("^p", n, "9075$")),
             matches(paste0("^p", n, "610\\d+$")))
  } else if (n_num == 9) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "420\\d+$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "48\\d+$")),
             matches(paste0("^p", n, "51\\d+$")), matches(paste0("^p", n, "5501$")),
             matches(paste0("^p", n, "6201$")), matches(paste0("^p", n, "6216$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")), matches(paste0("^p", n, "610\\d+$")))
  } else if (n_num == 10) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "420\\d+$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "48\\d+$")),
             matches(paste0("^p", n, "49\\d+$")), matches(paste0("^p", n, "51\\d+$")),
             matches(paste0("^p", n, "5501$")), matches(paste0("^p", n, "640[1-5]$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")), matches(paste0("^p", n, "610\\d+$")))
  } else if (n_num == 11) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, sample98, hwaveent, matches(paste0("^p", n, "orig98$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "420\\d+$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "48\\d+$")),
             matches(paste0("^p", n, "51\\d+$")), matches(paste0("^p", n, "5501$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")), matches(paste0("^p", n, "610\\d+$")))
  } else if (n_num >= 12) {
    ind_data <- ind_data %>%
      select(pid, matches(paste0("^hhid", n, "$")), matches(paste0("^hmem", n, "$")), 
             orghid98, orghid09, sample98, sample09, hwaveent, 
             matches(paste0("^p", n, "orig98$")), matches(paste0("^p", n, "orig09$")),
             matches(paste0("^w", n, "p_c$")), matches(paste0("^w", n, "p_l$")),
             matches(paste0("^p", n, "0101$")), matches(paste0("^p", n, "010[2-8]$")),
             matches(paste0("^p", n, "0110$")), matches(paste0("^p", n, "0111$")),
             matches(paste0("^p", n, "020[1-4]$")), matches(paste0("^p", n, "033\\d+$")),
             matches(paste0("^p", n, "280[1-6]$")), matches(paste0("^p", n, "420\\d+$")),
             matches(paste0("^p", n, "43\\d+$")), matches(paste0("^p", n, "45\\d+$")),
             matches(paste0("^p", n, "47\\d+$")), matches(paste0("^p", n, "48\\d+$")),
             matches(paste0("^p", n, "51\\d+$")), matches(paste0("^p", n, "5501$")),
             matches(paste0("^p", n, "650[1-8]$")), matches(paste0("^p", n, "9074$")),
             matches(paste0("^p", n, "9075$")), matches(paste0("^p", n, "610\\d+$")))
  }
  
  # Sort by household ID
  ind_data <- ind_data %>% arrange(across(matches(paste0("^hhid", n, "$"))))
  
  # Save the processed data
  save_path <- paste0("rawdata/klips/klips", n, "p_ix.dta")
  write_dta(ind_data, save_path)
}

### Job history data (long type)
job_data <- read_dta("rawdata/klips/klips20w.dta") %>%
  select(pid, jobwave, jobclass, jobtype, mainjob, j145, j150, j155, 
         matches("^j2\\d+$"), matches("^j3\\d+$"), j804)

### Additional survey data processing
# Wave 8 additional survey
a08_data <- read_dta("rawdata/klips/klips08a.dta") %>%
  select(pid, matches("^a08395[1-5]$"), matches("^a08396[1-4]$"))  # Sense of value
write_dta(a08_data, "rawdata/klips/klips08a_x.dta")

# Wave 9 additional survey
a09_data <- read_dta("rawdata/klips/klips09a.dta") %>%
  select(pid, matches("^a09425[1-4]$"), a094415, a094425, a094435)  # Middle school score and GPA
write_dta(a09_data, "rawdata/klips/klips09a_x.dta")

# Wave 11 additional survey
a11_data <- read_dta("rawdata/klips/klips11a.dta") %>%
  select(pid, matches("^a11626[1-4]$"), a116409)  # High school score and GPA
write_dta(a11_data, "rawdata/klips/klips11a_x.dta")

# Wave 17 additional survey
a17_data <- read_dta("rawdata/klips/klips17a.dta") %>%
  select(pid, matches("^a17770[1-9]$"), matches("^a1777[1-2][0-9]$"))  # Job stress
write_dta(a17_data, "rawdata/klips/klips17a_x.dta")

# Wave 18 additional survey part 1
a18_1_data <- read_dta("rawdata/klips/klips18a1.dta") %>%
  select(pid, matches("^a1881\\d+$"))  # Big 5 personality traits, locus of control, etc.
write_dta(a18_1_data, "rawdata/klips/klips18a1_x.dta")

# Wave 18 additional survey part 2
a18_2_data <- read_dta("rawdata/klips/klips18a2.dta") %>%
  select(pid, a188503, a188511, matches("^a18851[7-9]$"), a188520)  # Work related skills
write_dta(a18_2_data, "rawdata/klips/klips18a2_x.dta")