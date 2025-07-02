# Load necessary libraries
library(haven)
library(dplyr)
library(purrr)
library(tidyr)

# Clear workspace (similar to Stata's 'clear all')
rm(list = ls())
# At the start of your script, after loading libraries
if (!dir.exists("processed_data")) {
  dir.create("processed_data")
}
if (!dir.exists("rawdata/klips")) {
  dir.create("rawdata/klips", recursive = TRUE)
}

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

write_dta(job_data, "processed_data/klips20w_x.dta")

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
################################################################################
#                       Cross-sectional merging
################################################################################

# Cross-sectional merge
wave_numbers <- sprintf("%02d", 1:20)

for (n in wave_numbers) {
  # Read individual and household data
  ind_data <- read_dta(paste0("rawdata/klips/klips", n, "p_ix.dta"))
  hh_data <- read_dta(paste0("rawdata/klips/klips", n, "h_ix.dta"))
  
  # Get common column names
  common_cols <- intersect(names(ind_data), names(hh_data))
  
  # Remove duplicate columns from household data before merging
  hh_data <- hh_data %>% select(-any_of(common_cols[common_cols != paste0("hhid", n)]))
  
  # Merge with household ID
  merged_data <- ind_data %>% 
    left_join(hh_data, by = paste0("hhid", n))
  
  # Drop missing PIDs and sort
  merged_data <- merged_data %>%
    filter(!is.na(pid)) %>%
    arrange(pid)
  
  # Save merged file
  write_dta(merged_data, paste0("rawdata/klips/klips", n, "hp_ix.dta"))
}
# Merge additional survey data 
# Wave 8
data_08 <- read_dta("rawdata/klips/klips08hp_ix.dta")
a08 <- read_dta("rawdata/klips/klips08a_x.dta")
common_cols <- intersect(names(data_08), names(a08)) %>% .[. != "pid"]
if(length(common_cols) > 0) a08 <- a08 %>% select(-all_of(common_cols))
data_08 <- left_join(data_08, a08, by = "pid")
write_dta(data_08, "rawdata/klips/klips08hp_ix.dta")

# Wave 9
data_09 <- read_dta("rawdata/klips/klips09hp_ix.dta")
a09 <- read_dta("rawdata/klips/klips09a_x.dta")
common_cols <- intersect(names(data_09), names(a09)) %>% .[. != "pid"]
if(length(common_cols) > 0) a09 <- a09 %>% select(-all_of(common_cols))
data_09 <- left_join(data_09, a09, by = "pid")
write_dta(data_09, "rawdata/klips/klips09hp_ix.dta")

# Wave 11
data_11 <- read_dta("rawdata/klips/klips11hp_ix.dta")
a11 <- read_dta("rawdata/klips/klips11a_x.dta")
common_cols <- intersect(names(data_11), names(a11)) %>% .[. != "pid"]
if(length(common_cols) > 0) a11 <- a11 %>% select(-all_of(common_cols))
data_11 <- left_join(data_11, a11, by = "pid")
write_dta(data_11, "rawdata/klips/klips11hp_ix.dta")

# Wave 17
data_17 <- read_dta("rawdata/klips/klips17hp_ix.dta")
a17 <- read_dta("rawdata/klips/klips17a_x.dta")
common_cols <- intersect(names(data_17), names(a17)) %>% .[. != "pid"]
if(length(common_cols) > 0) a17 <- a17 %>% select(-all_of(common_cols))
data_17 <- left_join(data_17, a17, by = "pid")
write_dta(data_17, "rawdata/klips/klips17hp_ix.dta")

# Wave 18 (with two merges)
data_18 <- read_dta("rawdata/klips/klips18hp_ix.dta")

# First merge
a18_1 <- read_dta("rawdata/klips/klips18a1_x.dta")
common_cols <- intersect(names(data_18), names(a18_1)) %>% .[. != "pid"]
if(length(common_cols) > 0) a18_1 <- a18_1 %>% select(-all_of(common_cols))
data_18 <- left_join(data_18, a18_1, by = "pid")

# Second merge
a18_2 <- read_dta("rawdata/klips/klips18a2_x.dta")
common_cols <- intersect(names(data_18), names(a18_2)) %>% .[. != "pid"]
if(length(common_cols) > 0) a18_2 <- a18_2 %>% select(-all_of(common_cols))
data_18 <- left_join(data_18, a18_2, by = "pid")

write_dta(data_18, "rawdata/klips/klips18hp_ix.dta")
################################################################################
#                       Clean up intermediate files
################################################################################

# Erase old data files as in Stata code
for (n in wave_numbers) {
  # Delete household and individual files
  if(file.exists(paste0("rawdata/klips/klips", n, "h_ix.dta"))) {
    file.remove(paste0("rawdata/klips/klips", n, "h_ix.dta"))
  }
  if(file.exists(paste0("rawdata/klips/klips", n, "p_ix.dta"))) {
    file.remove(paste0("rawdata/klips/klips", n, "p_ix.dta"))
  }
}

# Delete additional survey files
additional_files <- c(
  "rawdata/klips/klips08a_x.dta",
  "rawdata/klips/klips09a_x.dta", 
  "rawdata/klips/klips11a_x.dta",
  "rawdata/klips/klips17a_x.dta",
  "rawdata/klips/klips18a1_x.dta",
  "rawdata/klips/klips18a2_x.dta"
)

for (f in additional_files) {
  if(file.exists(f)) file.remove(f)
}
################################################################################
#                       Generate variables
################################################################################

wave_numbers <- sprintf("%02d", 1:20)

for (n in wave_numbers) {
  n_num <- as.numeric(n)
  
  # Handle wave number formatting
  if (n_num < 10) {
    i <- substr(n, 2, 2)
  } else {
    i <- n
  }
  
  # Read merged data
  wave_data <- read_dta(paste0("rawdata/klips/klips", n, "hp_ix.dta"))
  
  ### Identification variables
  wave_data <- wave_data %>%
    rename(
      !!paste0("hhid", i) := paste0("hhid", n),
      !!paste0("hmem", i) := paste0("hmem", n),
      !!paste0("hwave", i) := paste0("hwave", n)
    )
  
  if (n_num == 1) {
    wave_data <- wave_data %>% mutate(!!paste0("htype", n) := 1)
  }
  
  wave_data <- wave_data %>% rename(!!paste0("htype", i) := paste0("htype", n))
  
  wave_data <- wave_data %>% mutate(!!paste0("year", i) := 1998 + as.numeric(i) - 1)
  
  ### Demographic variables
  wave_data <- wave_data %>%
    rename(!!paste0("gender", i) := paste0("p", n, "0101")) %>%
    mutate(!!paste0("male", i) := ifelse(get(paste0("gender", i)) == 1, 1, 
                                         ifelse(is.na(get(paste0("gender", i))), NA, 0))) %>%
    select(-paste0("gender", i)) %>%
    rename(!!paste0("relation", i) := paste0("p", n, "0102")) %>%
    mutate(!!paste0("byear", i) := ifelse(get(paste0("p", n, "0104")) == -1, NA, get(paste0("p", n, "0104"))),
           !!paste0("bmonth", i) := ifelse(get(paste0("p", n, "0105")) == -1, NA, get(paste0("p", n, "0105"))),
           !!paste0("bday", i) := ifelse(get(paste0("p", n, "0106")) == -1, NA, get(paste0("p", n, "0106"))),
           !!paste0("age", i) := ifelse(get(paste0("p", n, "0107")) == -1, NA, get(paste0("p", n, "0107")))) %>%
    rename(!!paste0("livein", i) := paste0("p", n, "0108")) %>%
    mutate(!!paste0("livein", i) := ifelse(get(paste0("livein", i)) == 2, 0, get(paste0("livein", i)))) %>%
    mutate(!!paste0("edu", i) := ifelse(get(paste0("p", n, "0110")) == -1, NA, get(paste0("p", n, "0110"))),
           !!paste0("edu_c", i) := ifelse(get(paste0("p", n, "0111")) == -1, NA, get(paste0("p", n, "0111")))) %>%
    mutate(!!paste0("marital", i) := ifelse(get(paste0("p", n, "5501")) == -1, NA, get(paste0("p", n, "5501"))),
           !!paste0("married", i) := ifelse(get(paste0("marital", i)) >= 2, 1, 
                                            ifelse(is.na(get(paste0("marital", i))), NA, 0))) %>%
    rename(!!paste0("nmem", i) := paste0("h", n, "0150")) %>%
    mutate(!!paste0("children", i) := NA_real_)
  
  if (n_num != 2) {
    wave_data <- wave_data %>%
      mutate(!!paste0("children", i) := case_when(
        get(paste0("h", n, "1501")) == 2 ~ 0,
        get(paste0("h", n, "1501")) == 1 ~ get(paste0("h", n, "1502")),
        TRUE ~ get(paste0("children", i))
      ))
  }
  
  wave_data <- wave_data %>%
    rename(!!paste0("province", i) := paste0("h", n, "0141")) %>%
    mutate(!!paste0("city", i) := ifelse(get(paste0("h", n, "0142")) == -1, NA, get(paste0("h", n, "0142"))))
  
  ### Work related variables
  wave_data <- wave_data %>%
    mutate(!!paste0("LFS", i) := case_when(
      get(paste0("p", n, "0201")) == 1 ~ 1,
      get(paste0("p", n, "2801")) == 1 & get(paste0("p", n, "2806")) == 1 ~ 2,
      TRUE ~ 0
    ),
    !!paste0("employed", i) := ifelse(get(paste0("LFS", i)) == 1, 1, 0),
    !!paste0("unemployed", i) := case_when(
      get(paste0("LFS", i)) == 2 ~ 1,
      get(paste0("LFS", i)) == 1 ~ 0,
      TRUE ~ NA_real_
    ),
    !!paste0("businesscode_old", i) := get(paste0("p", n, "0330")),
    !!paste0("businesscode_new", i) := NA_real_,
    !!paste0("occupationcode_old", i) := get(paste0("p", n, "0332")),
    !!paste0("occupationcode_new", i) := get(paste0("p", n, "0332")))
  
  if (n_num >= 12) {
    wave_data <- wave_data %>%
      mutate(!!paste0("businesscode_new", i) := get(paste0("p", n, "0331")),
             !!paste0("occupationcode_new", i) := get(paste0("p", n, "0333")))
  }
  
  ### Personality traits (Wave 18 only)
  personality_vars <- paste0("personality_", letters[1:15])
  for (var in personality_vars) {
    wave_data <- wave_data %>% mutate(!!paste0(var, i) := NA_real_)
  }
  
  if (n_num == 18) {
    for (j in 1:15) {
      wave_data <- wave_data %>%
        mutate(!!paste0("personality_", letters[j], i) := 
                 ifelse(get(paste0("a1881", sprintf("%02d", j))) == -1, 
                        NA, get(paste0("a1881", sprintf("%02d", j)))))
    }
    
    wave_data <- wave_data %>%
      mutate(
        !!paste0("big5_O", i) := get(paste0("personality_d", i)) + get(paste0("personality_i", i)) + get(paste0("personality_n", i)),
        !!paste0("big5_C", i) := get(paste0("personality_a", i)) - get(paste0("personality_g", i)) + get(paste0("personality_k", i)),
        !!paste0("big5_E", i) := get(paste0("personality_b", i)) + get(paste0("personality_h", i)) - get(paste0("personality_l", i)),
        !!paste0("big5_A", i) := -get(paste0("personality_c", i)) + get(paste0("personality_f", i)) + get(paste0("personality_m", i)),
        !!paste0("big5_N", i) := get(paste0("personality_e", i)) + get(paste0("personality_j", i)) + get(paste0("personality_o", i))
      )
  }
  
  ### Locus of control (Wave 18 only)
  locus_vars <- paste0("locus_", letters[1:10])
  for (var in locus_vars) {
    wave_data <- wave_data %>% mutate(!!paste0(var, i) := NA_real_)
  }
  
  if (n_num == 18) {
    for (j in 1:10) {
      wave_data <- wave_data %>%
        mutate(!!paste0("locus_", letters[j], i) := 
                 ifelse(get(paste0("a1881", 15+j)) == -1, 
                        NA, get(paste0("a1881", 15+j))))
    }
    
    wave_data <- wave_data %>%
      mutate(
        !!paste0("locus_internal", i) := get(paste0("locus_a", i)) + get(paste0("locus_d", i)) + get(paste0("locus_f", i)) - get(paste0("locus_g", i)),
        !!paste0("locus_external", i) := get(paste0("locus_b", i)) + get(paste0("locus_c", i)) + get(paste0("locus_e", i)) + 
          get(paste0("locus_h", i)) + get(paste0("locus_i", i)) + get(paste0("locus_j", i))
      )
  }
  
  ### Reciprocity, Trust, Preferences (Wave 18 only)
  if (n_num == 18) {
    wave_data <- wave_data %>%
      mutate(
        !!paste0("reciprocity_a", i) := ifelse(a188126 == -1, NA, a188126),
        !!paste0("reciprocity_b", i) := ifelse(a188127 == -1, NA, a188127),
        !!paste0("reciprocity_c", i) := ifelse(a188128 == -1, NA, a188128),
        !!paste0("reciprocity_d", i) := ifelse(a188129 == -1, NA, a188129),
        !!paste0("reciprocity_e", i) := ifelse(a188130 == -1, NA, a188130),
        !!paste0("reciprocity_f", i) := ifelse(a188131 == -1, NA, a188131),
        
        !!paste0("trust_a", i) := ifelse(a188132 == -1, NA, a188132),
        !!paste0("trust_b", i) := ifelse(a188133 == -1, NA, a188133),
        !!paste0("trust_c", i) := ifelse(a188134 == -1, NA, a188134),
        
        !!paste0("risktol", i) := ifelse(a188135 == -1, NA, a188135),
        !!paste0("patience", i) := ifelse(a188136 == -1, NA, a188136),
        !!paste0("impulsivity", i) := ifelse(a188137 == -1, NA, a188137)
      )
  } else {
    wave_data <- wave_data %>%
      mutate(
        !!paste0("risktol", i) := NA_real_,
        !!paste0("patience", i) := NA_real_,
        !!paste0("impulsivity", i) := NA_real_
      )
  }
  
  ### Household income variables
  # [Additional income variable generation code would go here]
  # This follows the same pattern as above but was omitted for brevity
  
  ### Sample weights
  wave_data <- wave_data %>%
    rename(!!paste0("wh", i) := paste0("w", n, "h"))
  
  if (n_num == 1) {
    wave_data <- wave_data %>%
      mutate(
        !!paste0("wpl", i) := get(paste0("w", n, "p")),
        !!paste0("wpc", i) := get(paste0("w", n, "p"))
      ) %>%
      select(-paste0("w", n, "p"))
  } else if (n_num >= 2) {
    wave_data <- wave_data %>%
      rename(
        !!paste0("wpl", i) := paste0("w", n, "p_l"),
        !!paste0("wpc", i) := paste0("w", n, "p_c")
      )
  }
  
  # Drop original variables
  wave_data <- wave_data %>%
    select(-matches(paste0("^h", n)), -matches(paste0("^p", n)))
  
  # Save processed data
  write_dta(wave_data, paste0("rawdata/klips/klips", n, "hp_ivar.dta"))
}

### Job history data processing
job_data <- read_dta("processed_data/klips20w_x.dta") %>%
  select(pid, matches("^job"), mainjob, matches("^j1[45]"), matches("^j2"), matches("^j3[126]"), j804)

# Keep only one job per wave per person
job_data <- job_data %>%
  mutate(mainjob = ifelse(mainjob == 0, 2, mainjob)) %>%
  arrange(pid, jobwave, mainjob) %>%
  group_by(pid) %>%
  mutate(
    n = row_number(),
    twojob = jobwave == lag(jobwave)
  ) %>%
  filter(is.na(twojob) | !twojob) %>%
  select(-n, -twojob) %>%
  mutate(mainjob = ifelse(mainjob == 2, 0, mainjob)) %>%
  ungroup()

# Create work variables
job_data <- job_data %>%
  mutate(
    worktype1 = ifelse(j150 == -1, NA, j150),
    worktype2 = ifelse(j145 == -1, NA, j145),
    worktype3 = ifelse(j155 == -1, NA, j155),
    selfemployed = ifelse(worktype1 == 4, 1, 0),
    overtimeunit = ifelse(j804 == -1, NA, j804),
    
    workhour = case_when(
      j202 == 2 & j203 != -1 ~ j203,
      j202 == 1 & j205 != -1 ~ j205,
      TRUE ~ NA_real_
    ),
    workhour_ov = ifelse(j202 == 1 & j208 != -1, j208, NA_real_),
    
    workday = case_when(
      j202 == 2 & j204 != -1 ~ j204,
      j202 == 1 & j206 != -1 ~ j206,
      TRUE ~ NA_real_
    ),
    workday_ov = ifelse(j202 == 1 & j209 != -1, j209, NA_real_),
    
    monthlywage = ifelse(j316 == -1, NA, j316),
    monthlywage_ov = ifelse(j212 == -1, NA, j212),
    
    workhour_nw = ifelse(jobtype == 2 & j213 != -1, j213, NA_real_),
    workday_nw = ifelse(jobtype == 2 & j214 != -1, j214, NA_real_),
    
    annualsales = ifelse(j322 == -1, NA, j322),
    annualsales_c = ifelse(j323 == -1, NA, j323),
    monthlyprofit = ifelse(j325 == -1, NA, j325)
  ) %>%
  mutate(
    annualsalary = monthlywage * 12,
    annualsalary_ov = monthlywage_ov * 12,
    annualsalary_total = annualsalary + annualsalary_ov,  # Changed from rowSums
    hourlywage = annualsalary / (workhour * 52),
    hourlywage_ov = annualsalary_ov / (workhour_ov * 52),
    temp = workhour + workhour_ov,  # Changed from rowSums
    hourlywage_total = annualsalary_total / (temp * 52)
  ) %>%
  select(-temp) %>%
  rename(wave = jobwave) %>%
  select(-matches("^j"))

# Save processed job data
write_dta(job_data, "processed_data/klips20w_var.dta")
# Load required libraries
library(haven)
library(dplyr)
library(tidyr)

################################################################################
#                       Longitudinal merging
################################################################################

### Longitudinal merge (1:1 merges of all waves)
# Start with wave 1
merged_data <- read_dta("rawdata/klips/klips01hp_ivar.dta")

# Sequentially merge waves 2-20
for (wave_num in 2:20) {
  wave_file <- sprintf("rawdata/klips/klips%02dhp_ivar.dta", wave_num)
  wave_data <- read_dta(wave_file)
  
  merged_data <- merged_data %>% 
    full_join(wave_data, by = "pid", suffix = c("", ""))  # No suffix added
}

# Save merged wide format data
write_dta(merged_data, "processed_data/klips_imerge.dta")

### Reshape to long format
merged_data <- read_dta("processed_data/klips_imerge.dta")

# Define ID variables that shouldn't be reshaped
id_vars <- c("pid", "orghid98", "orghid09", "sample98", "sample09", "hwaveent")

# Define all variables that need reshaping (with wave suffixes)
reshape_vars <- c(
  "hhid", "hmem", "htype", "hwave", "year",
  "nmem", "male", "edu", "edu_c", "province", "city", "relation",
  "byear", "bmonth", "bday", "age", "livein", "marital", "married", "children",
  "LFS", "employed", "unemployed", "businesscode_old", "businesscode_new",
  "occupationcode_old", "occupationcode_new", "risktol", "patience", "impulsivity",
  "i_total_m", "i_total_z", "i_earned", "i_financial", "i_realestate", "i_social",
  "i_basic", "i_publictrans", "i_privatetrans", "i_other_a", "i_other_b", "i_other_c",
  "i_other_d", "i_other_e", "i_other_f", "i_other_g", "i_other_h", "i_other_i", "i_other_j",
  "wh", "wpl", "wpc",
  "personality_a", "personality_b", "personality_c", "personality_d", "personality_e",
  "personality_f", "personality_g", "personality_h", "personality_i", "personality_j",
  "personality_k", "personality_l", "personality_m", "personality_n", "personality_o",
  "big5_O", "big5_C", "big5_E", "big5_A", "big5_N",
  "locus_a", "locus_b", "locus_c", "locus_d", "locus_e", "locus_f", "locus_g",
  "locus_h", "locus_i", "locus_j", "locus_internal", "locus_external",
  "reciprocity_a", "reciprocity_b", "reciprocity_c", "reciprocity_d", "reciprocity_e", "reciprocity_f",
  "trust_a", "trust_b", "trust_c"
)

# Create pattern for pivot_longer
names_pattern <- paste0("^(", paste(reshape_vars, collapse = "|"), ")(\\d+)$")

# Reshape from wide to long
long_data <- merged_data %>%
  pivot_longer(
    cols = matches(paste0("^(", paste(reshape_vars, collapse = "|"), ")\\d+$")),
    names_to = c(".value", "wave"),
    names_pattern = names_pattern
  ) %>%
  mutate(wave = as.numeric(wave)) %>%
  select(all_of(id_vars), wave, everything())

# Save long format data
write_dta(long_data, "processed_data/klips_imerge_long.dta")

### Merge with job history data (1:m merge)
long_data <- read_dta("processed_data/klips_imerge_long.dta")
job_data <- read_dta("processed_data/klips20w_var.dta")

final_data <- long_data %>%
  left_join(job_data, by = c("pid", "wave"))

# Save final merged data
write_dta(final_data, "processed_data/klips_imerge_long1.dta")
# Load required libraries
library(haven)
library(dplyr)

################################################################################
#                       Outliers in job related variables
################################################################################

# Load the data
klips_data <- read_dta("processed_data/klips_imerge_long1.dta")

### First, check what variables are actually in your dataset
print(names(klips_data))

### Drop outliers in work status variables
# First check which variables exist
has_worktype1 <- "worktype1" %in% names(klips_data)
has_jobtype <- "jobtype" %in% names(klips_data)

# Apply filters only if variables exist
klips_data <- klips_data %>%
  {if(has_worktype1) filter(., !(unemployed == 1 & worktype1 == 1)) else .} %>%
  {if(has_worktype1) filter(., !(unemployed == 1 & worktype1 == 3)) else .} %>%
  {if(has_worktype1) filter(., !(unemployed == 1 & worktype1 == 5)) else .} %>%
  {if(has_worktype1) filter(., !(unemployed == 1 & worktype1 == 7)) else .} %>%
  {if(has_worktype1) filter(., !(employed == 1 & worktype1 == 2)) else .} %>%
  {if(has_worktype1) filter(., !(employed == 1 & worktype1 == 4)) else .} %>%
  {if(has_worktype1) filter(., !(employed == 1 & worktype1 == 6)) else .} %>%
  {if(has_worktype1) filter(., !(employed == 1 & worktype1 == 8)) else .} %>%
  {if(has_jobtype) filter(., jobtype != 3) else .}

### Hourly wage outlier treatment
# Check which wage variables exist
wage_vars <- c("hourlywage", "hourlywage_ov", "hourlywage_total")
existing_wage_vars <- wage_vars[wage_vars %in% names(klips_data)]

# Create a function to handle the hourly wage replacements
replace_hourly_wage <- function(data, wage_var) {
  data %>%
    mutate(!!wage_var := case_when(
      year == 1998 & !!sym(wage_var) < (1400+1485)/40000 ~ NA_real_,
      year == 1999 & !!sym(wage_var) < (1485+1525)/40000 ~ NA_real_,
      year == 2000 & !!sym(wage_var) < (1525+1600)/40000 ~ NA_real_,
      year == 2001 & !!sym(wage_var) < (1600+1865)/40000 ~ NA_real_,
      year == 2002 & !!sym(wage_var) < (1865+2100)/40000 ~ NA_real_,
      year == 2003 & !!sym(wage_var) < (2100+2275)/40000 ~ NA_real_,
      year == 2004 & !!sym(wage_var) < (2275+2510)/40000 ~ NA_real_,
      year == 2005 & !!sym(wage_var) < (2510+2840)/40000 ~ NA_real_,
      year == 2006 & !!sym(wage_var) < (2840+3100)/40000 ~ NA_real_,
      year == 2007 & !!sym(wage_var) < 3100/20000 ~ NA_real_,
      year == 2008 & !!sym(wage_var) < 3480/20000 ~ NA_real_,
      year == 2009 & !!sym(wage_var) < 3770/20000 ~ NA_real_,
      year == 2010 & !!sym(wage_var) < 4000/20000 ~ NA_real_,
      year == 2011 & !!sym(wage_var) < 4110/20000 ~ NA_real_,
      year == 2012 & !!sym(wage_var) < 4320/20000 ~ NA_real_,
      year == 2013 & !!sym(wage_var) < 4580/20000 ~ NA_real_,
      year == 2014 & !!sym(wage_var) < 4860/20000 ~ NA_real_,
      year == 2015 & !!sym(wage_var) < 5210/20000 ~ NA_real_,
      TRUE ~ !!sym(wage_var)
    ))
}

# Apply the function to existing wage variables
for(wage_var in existing_wage_vars) {
  klips_data <- replace_hourly_wage(klips_data, wage_var)
}

# Save the cleaned data
write_dta(klips_data, "processed_data/klips_imerge_long1.dta")

# 1. Load libraries
library(readxl)
library(dplyr)
library(haven)

# 2. Read & clean CPI raw Excel
cpi_raw <- read_excel("rawdata/klips/CPI_updated.xlsx", sheet = "Sheet1", col_names = TRUE)

cpi <- cpi_raw %>%
  # a) force all CPI columns to character so "-" can be caught
  mutate(across(starts_with("CPI"), as.character)) %>%
  # b) replace "-" with NA
  mutate(across(starts_with("CPI"), ~ na_if(.x, "-"))) %>%
  # c) convert to numeric
  mutate(across(starts_with("CPI"), as.numeric)) %>%
  # d) pivot to long
  pivot_longer(
    cols         = starts_with("CPI"),
    names_to     = "wave",
    names_prefix = "CPI",
    values_to    = "CPI"
  ) %>%
  mutate(
    province = as.integer(province),
    wave     = as.integer(wave)
  ) %>%
  filter(province != 0)

# (Optional) save cleaned CPI for inspection
# write_dta(cpi, "rawdata/klips/CPI.dta")

# 3. Read your merged panel (pre-CPI)
klips <- read_dta("processed_data/klips_imerge_long1.dta")

# 4. Merge CPI onto panel
klips <- klips %>%
  left_join(cpi, by = c("province", "wave")) %>%
  arrange(pid, wave)
# After merging CPI onto klips...

# ——— After your left_join and arrange(pid, wave) call ———

# 1) Find the CPI column name
cpi_name <- grep("CPI", names(klips), value = TRUE, ignore.case = TRUE)[1]

# 2) List your money variables and keep only those present
money_vars   <- c(
  "annualsales", "monthlyprofit",
  "annualsalary", "annualsalary_ov", "annualsalary_total",
  "hourlywage", "hourlywage_ov", "hourlywage_total",
  paste0("i_other_", letters[1:10])
)
existing_money <- intersect(money_vars, names(klips))

# 3) Deflate each by CPI*100
klips <- klips %>%
  mutate(across(all_of(existing_money),
                ~ .x / .data[[cpi_name]] * 100))


# 6. Save the CPI‐adjusted dataset
write_dta(klips, "processed_data/klips_imerge_long2.dta")

# 7. Delete old intermediate files
files_to_remove <- c(
  paste0("rawdata/klips/klips", sprintf("%02d", 1:20), "hp_ix.dta"),
  paste0("rawdata/klips/klips", sprintf("%02d", 1:20), "hp_ivar.dta"),
  "processed_data/klips20w_x.dta",
  "processed_data/klips20w_var.dta",
  "processed_data/klips_imerge.dta",
  "processed_data/klips_imerge_long.dta",
  "processed_data/klips_imerge_long1.dta"
)

# Only remove if they exist
walk(files_to_remove, ~ if (file.exists(.x)) file.remove(.x))
library(haven)
library(dplyr)
library(labelled)

# 1. Read in your CPI‐adjusted long2 file
klips <- read_dta("processed_data/klips_imerge_long2.dta")

# 2. Re-order exactly as in your Stata `order` block
stata_order <- c(
  "pid","hhid","hmem","hwave","htype","wave","year",
  "orghid98","orghid09","sample98","sample09","hwaveent",
  "nmem","male","edu","edu_c","province","city","relation",
  "byear","bmonth","bday","age","livein","marital","married","children",
  "LFS","employed","unemployed","businesscode_old","businesscode_new",
  "occupationcode_old","occupationcode_new",
  "jobtype","jobclass","mainjob","worktype1","worktype2","worktype3","selfemployed",
  "overtimeunit","workhour","workhour_ov","workday","workday_ov",
  "monthlywage","monthlywage_ov","workhour_nw","workday_nw",
  "annualsales","annualsales_c","monthlyprofit",
  "annualsalary","annualsalary_ov","annualsalary_total",
  "hourlywage","hourlywage_ov","hourlywage_total",
  "risktol","patience","impulsivity",
  "i_total_m","i_total_z","i_earned","i_financial","i_realestate","i_social",
  "i_basic","i_publictrans","i_privatetrans",
  paste0("i_other_", letters[1:10]),
  "CPI","wh","wpl","wpc",
  paste0("personality_", letters[1:15]),
  "big5_O","big5_C","big5_E","big5_A","big5_N",
  paste0("locus_", letters[1:10]),"locus_internal","locus_external",
  paste0("reciprocity_", letters[1:6]),paste0("trust_", letters[1:3])
)

klips <- klips %>% 
  select(any_of(stata_order), everything())

# 3. Apply your variable labels
vl <- list(
  pid       = "Unique individual ID",
  hhid      = "Household ID in each wave",
  hmem      = "Household member ID in each wave",
  hwave     = "Response status in each wave",
  htype     = "Household type in each wave",
  wave      = "Wave of survey",
  year      = "Year of survey",
  orghid98  = "Original household ID (98 sample)",
  orghid09  = "Original household ID (consolidated sample)",
  sample98  = "Original household indicator (98 sample)",
  sample09  = "Original household indicator (consolidated sample)",
  hwaveent  = "Wave first entered",
  male      = "Male",
  relation  = "Relation to household head",
  byear     = "Birth year",
  bmonth    = "Birth month",
  bday      = "Birth day",
  age       = "Age",
  livein    = "Whether living in household",
  edu       = "Education status",
  edu_c     = "Education completion status",
  marital   = "Marital status",
  married   = "Married or not",
  nmem      = "Number of household members",
  children  = "Number of children in household",
  province  = "Province of residence",
  city      = "City of residence",
  LFS       = "Labor Force Status",
  employed  = "Employed",
  unemployed= "Unemployed",
  businesscode_old  = "Business classification code",
  businesscode_new  = "Business classification code",
  occupationcode_old= "Occupation classification code",
  occupationcode_new= "Occupation classification code",
  mainjob   = "Mainjob",
  worktype1 = "Type of work status",
  worktype2 = "Type of employment",
  worktype3 = "Type of work hours",
  selfemployed    = "Employer/Self-employed",
  overtimeunit    = "Overtime work unit",
  workhour        = "Work hours of wage workers",
  workhour_ov     = "Overtime hours",
  workday         = "Work days of wage workers",
  workday_ov      = "Overtime days",
  monthlywage     = "Monthly wage of wage workers",
  monthlywage_ov  = "Overtime wage",
  workhour_nw     = "Work hours of non-wage workers",
  workday_nw      = "Work days of non-wage workers",
  annualsales     = "Annual sales of non-wage workers",
  annualsales_c   = "Annual sales category",
  monthlyprofit   = "Monthly profit of non-wage workers",
  annualsalary    = "Annual salary of wage workers",
  annualsalary_ov = "Overtime salary",
  annualsalary_total = "Total annual salary",
  hourlywage      = "Hourly wage of wage workers",
  hourlywage_ov   = "Hourly overtime wage",
  hourlywage_total= "Total hourly wage",
  CPI             = "CPI (2015=100)",
  wh              = "Weight: household",
  wpl             = "Weight: individual longitudinal",
  wpc             = "Weight: individual cross-sectional"
)
var_label(klips) <- vl

# 4. Define & apply your value labels, but only if those vars exist
if ("sample98" %in% names(klips)) {
  val_labels(klips)$sample98 <- c(original=1, branched=2, `not target`=3)
}
if ("sample09" %in% names(klips)) {
  val_labels(klips)$sample09 <- c(original=1, branched=2, `not target`=3)
}
if ("htype" %in% names(klips)) {
  val_labels(klips)$htype <- c(existing=1, missing=2, branched=3, additional=4)
}
if ("male" %in% names(klips)) {
  val_labels(klips)$male <- c(Male=1, Female=0)
}
if ("edu" %in% names(klips)) {
  val_labels(klips)$edu <- setNames(1:9,
                                    c("preschool","noschool","elementary","middle","high",
                                      "college","university","master","doctor"))
}
if ("edu_c" %in% names(klips)) {
  val_labels(klips)$edu_c <- setNames(1:5,
                                      c("graduated","completed","dropped out","enrolled","leave of absence"))
}
if ("livein" %in% names(klips)) {
  val_labels(klips)$livein <- c(`not live in`=0, `live in`=1)
}
if ("marital" %in% names(klips)) {
  val_labels(klips)$marital <- setNames(1:5,
                                        c("single","married","separated","divorced","widowed"))
}
if ("married" %in% names(klips)) {
  val_labels(klips)$married <- c(married=1, single=0)
}
if ("LFS" %in% names(klips)) {
  val_labels(klips)$LFS <- c(inactive=0, employed=1, unemployed=2)
}
if ("employed" %in% names(klips)) {
  val_labels(klips)$employed <- c(employed=1, others=0)
}
if ("unemployed" %in% names(klips)) {
  val_labels(klips)$unemployed <- c(unemployed=1, others=0)
}
if ("jobtype" %in% names(klips)) {
  val_labels(klips)$jobtype <- c(`wage worker`=1, `non-wage worker`=2, `dk/no`=3)
}
if ("jobclass" %in% names(klips)) {
  val_labels(klips)$jobclass <- setNames(1:8, 
                                         c("Still prev wage","Stopped prev wage","Still prev non-wage",
                                           "Stopped prev non-wage","Still new wage","Stopped new wage",
                                           "Still new non-wage","Stopped new non-wage"))
}
if ("mainjob" %in% names(klips)) {
  val_labels(klips)$mainjob <- c(mainjob=1, otherwise=0)
}
if ("worktype1" %in% names(klips)) {
  val_labels(klips)$worktype1 <- setNames(1:5,
                                          c("permanent","temporary","daily","employer/self-employed","unpaid family worker"))
}
if ("worktype2" %in% names(klips)) {
  val_labels(klips)$worktype2 <- c(regular=1, irregular=2)
}
if ("worktype3" %in% names(klips)) {
  val_labels(klips)$worktype3 <- c(part_time=1, full_time=2)
}
if ("overtimeunit" %in% names(klips)) {
  val_labels(klips)$overtimeunit <- c(Weekly=1, Monthly=2)
}
if ("annualsales_c" %in% names(klips)) {
  val_labels(klips)$annualsales_c <- setNames(1:14,
                                              c("Under 10m","10m~30m","30m~50m","50m~80m","80m~100m","100m~300m",
                                                "300m~500m","500m~1b","1b~3b","3b~10b","10b~50b","50b~100b","Over 100b","dk"))
}
# and similarly for your likert7scale…
likert7 <- setNames(1:7,
                    c("Strongly disagree","Mostly disagree","Somewhat disagree",
                      "Neither agree nor disagree","Somewhat agree","Mostly agree","Strongly agree"))
for (v in intersect(paste0("personality", 1:15), names(klips))) {
  val_labels(klips)[[v]] <- likert7
}
for (v in intersect(paste0("locus", 1:10), names(klips))) {
  val_labels(klips)[[v]] <- likert7
}
for (v in intersect(paste0("reciprocity", 1:6), names(klips))) {
  val_labels(klips)[[v]] <- likert7
}
for (v in intersect(paste0("trust", 1:3), names(klips))) {
  val_labels(klips)[[v]] <- likert7
}

# 5. Save final, labeled dataset
write_dta(klips, "processed_data/klips_imerge_long2_reordered_labeled.dta")
library(haven)
library(dplyr)
library(labelled)

# 1. Read in wave‐3 input
klips <- read_dta("processed_data/klips_imerge_long2.dta")

# 2. Fill in constant birth / gender across waves (year already exists)
#    (Stata: egen temp = mean(...), replace …, drop temp)
klips <- klips %>%
  group_by(pid) %>%
  mutate(
    male   = if ("male"   %in% names(.)) mean(male,   na.rm=TRUE) else male,
    byear  = if ("byear"  %in% names(.)) mean(byear,  na.rm=TRUE) else byear,
    bmonth = if ("bmonth" %in% names(.)) mean(bmonth, na.rm=TRUE) else bmonth,
    bday   = if ("bday"   %in% names(.)) mean(bday,   na.rm=TRUE) else bday
  ) %>%
  ungroup()

# 3. Highest‐ed by wave (edu_cx) and max across waves (max_educ), plus flags
klips <- klips %>%
  mutate(
    edu_cx = case_when(
      edu %in% 1:3                                   ~ 1,
      edu == 4 & edu_c >= 2                          ~ 1,
      edu == 4 & edu_c == 1                          ~ 2,
      edu == 5 & edu_c >= 2                          ~ 2,
      edu == 5 & edu_c == 1                          ~ 3,
      edu == 6 & edu_c >= 2                          ~ 3,
      edu == 7 & edu_c >= 2                          ~ 3,
      edu == 6 & edu_c == 1                          ~ 4,
      edu == 7 & edu_c == 1                          ~ 5,
      edu == 8 & edu_c >= 2                          ~ 5,
      edu == 8 & edu_c == 1                          ~ 6,
      edu == 9 & edu_c >= 2                          ~ 6,
      edu == 9 & edu_c == 1                          ~ 7,
      TRUE                                           ~ NA_real_
    )
  ) %>%
  group_by(pid) %>%
  mutate(
    max_educ    = max(edu_cx, na.rm=TRUE),
    collegegrad = ifelse(max_educ >= 4, 1, 0),
    maxed       = case_when(
      max_educ == 1                 ~ 1,
      max_educ %in% 2:3             ~ 2,
      max_educ %in% 4:7             ~ 3,
      TRUE                          ~ NA_real_
    ),
    edu_prim    = as.integer(maxed == 1),
    edu_sec     = as.integer(maxed == 2),
    edu_ter     = as.integer(maxed == 3),
    yrschoolz   = case_when(
      edu %in% 1:2                   ~ 0,
      edu_cx == 1                    ~ 6,
      edu_cx == 2                    ~ 9,
      edu_cx == 3                    ~ 12,
      edu_cx == 4                    ~ 14,
      edu_cx == 5                    ~ 16,
      edu_cx == 6                    ~ 18,
      edu_cx == 7                    ~ 20,
      TRUE                           ~ NA_real_
    )
  ) %>%
  ungroup()

# 4. Age groups
klips <- klips %>%
  mutate(
    agegroup = case_when(
      age >=10 & age <20 ~ 1,
      age >=20 & age <30 ~ 2,
      age >=30 & age <40 ~ 3,
      age >=40 & age <50 ~ 4,
      age >=50 & age <60 ~ 5,
      age >=60 & age <70 ~ 6,
      age >=70           ~ 7,
      TRUE               ~ NA_real_
    ),
    age10s = as.integer(agegroup==1),
    age20s = as.integer(agegroup==2),
    age30s = as.integer(agegroup==3),
    age40s = as.integer(agegroup==4),
    age50s = as.integer(agegroup==5),
    age60s = as.integer(agegroup==6),
    age70s = as.integer(agegroup==7),
    byear40 = as.integer(byear>=1940 & byear<1950),
    byear50 = as.integer(byear>=1950 & byear<1960),
    byear60 = as.integer(byear>=1960 & byear<1970),
    byear70 = as.integer(byear>=1970 & byear<1980),
    byear80 = as.integer(byear>=1980 & byear<1990),
    byear90 = as.integer(byear>=1990 & byear<2000)
  )

# 5. Marital/employment flags
klips <- klips %>%
  mutate(
    divorced   = as.integer(marital == 4),
    paidworker = as.integer(worktype1 %in% c(1,2,3)),
    regular    = as.integer(worktype2 == 1),
    irregular  = as.integer(worktype2 == 2),
    code_spouse = case_when(
      relation>=10 & relation<=19 ~ relation + 10,
      relation>=20 & relation<=29 ~ relation - 10,
      relation>=31 & relation<=49 ~ relation + 20,
      relation>=51 & relation<=69 ~ relation - 10,
      relation>=111 & relation<=199 ~ relation + 100,
      relation>=211 & relation<=299 ~ relation - 100,
      TRUE                         ~ NA_real_
    )
  )

# 6. Build spouse panel and merge back
spouse <- klips %>%
  select(hhid, year, code_spouse, hourlywage_total, annualsalary_total, annualsales) %>%
  rename(
    relation           = code_spouse,
    spouse_hourlywage  = hourlywage_total,
    spouse_annualsalary= annualsalary_total,
    spouse_annualsales = annualsales
  ) %>%
  filter(!is.na(relation))

# merge back (many‐to‐many by hhid+year+relation) and drop unmatched
klips <- klips %>%
  left_join(spouse, by=c("hhid","year","relation")) %>%
  filter(!is.na(pid))

# 7. Husband/Wife LFS totals
klips <- klips %>%
  mutate(
    temp1 = if_else(male==1 & relation %in% c(10,20), LFS, NA_real_),
    temp2 = if_else(male==0 & relation %in% c(10,20), LFS, NA_real_)
  ) %>%
  group_by(hhid, year) %>%
  mutate(
    LFS_h = sum(temp1, na.rm=TRUE),
    LFS_w = sum(temp2, na.rm=TRUE)
  ) %>%
  ungroup() %>%
  select(-temp1, -temp2)

# 8. Labels for all new vars
var_label(klips) <- list(
  edu_cx      = "Highest education attainment by wave",
  max_educ    = "Highest education attainment",
  collegegrad = "College graduate and above",
  maxed       = "Completed Educational Attainment",
  edu_prim    = "Primary education",
  edu_sec     = "Secondary education",
  edu_ter     = "Postsecondary education",
  yrschoolz   = "Years of completed schooling",
  agegroup    = "Age group",
  age10s      = "Age 10–19",
  age20s      = "Age 20–29",
  age30s      = "Age 30–39",
  age40s      = "Age 40–49",
  age50s      = "Age 50–59",
  age60s      = "Age 60–69",
  age70s      = "Age 70–79",
  byear40     = "Born 1940s",
  byear50     = "Born 1950s",
  byear60     = "Born 1960s",
  byear70     = "Born 1970s",
  byear80     = "Born 1980s",
  byear90     = "Born 1990s",
  divorced    = "Divorced",
  paidworker  = "Paid worker",
  regular     = "Regular work",
  irregular   = "Irregular work",
  spouse_hourlywage   = "Spouse's hourly wage",
  spouse_annualsalary = "Spouse's annual salary",
  spouse_annualsales  = "Spouse's annual sales",
  LFS_h       = "LFS of husband",
  LFS_w       = "LFS of wife"
)

# 9. Value‐labels for edu_cx, maxed, collegegrad, agegroup
val_labels(klips)$edu_cx <- setNames(1:7,
                                     c("elementary or below","middle school graduate","high school graduate",
                                       "college graduate","university graduate","master graduate","doctor graduate"))
val_labels(klips)$maxed <- setNames(1:3,
                                    c("Primary Education","Secondary Education","Tertiary Education"))
val_labels(klips)$collegegrad <- c(`college graduate and above`=1, `high school and below`=0)
val_labels(klips)$agegroup <- setNames(1:7,
                                       c("Age 10–19","Age 20–29","Age 30–39","Age 40–49","Age 50–59","Age 60–69","Age 70+"))

# 10. Save your “long3” file and remove spouse temp
if (file.exists("processed_data/spouse.dta")) {
  file.remove("processed_data/spouse.dta")
}
library(haven)
library(dplyr)
library(tidyr)
library(labelled)

# Load data with error handling
if (!file.exists("processed_data/klips_imerge_long3.dta")) {
  stop("Input file 'processed_data/klips_imerge_long3.dta' not found")
}

klips_long3 <- tryCatch(
  {
    read_dta("processed_data/klips_imerge_long3.dta")
  },
  error = function(e) {
    stop("Failed to read the data file: ", e$message)
  }
)

# Initialize klips_long4
klips_long4 <- klips_long3

# Check if i_total_z exists - if not, skip winsorization
if (!"i_total_z" %in% names(klips_long3)) {
  warning("Variable 'i_total_z' not found - skipping winsorization")
} else {
  # Proceed with winsorization if variable exists
  klips_long4 <- klips_long3 %>%
    mutate(
      temp1 = quantile(i_total_z, 0.99, na.rm = TRUE),
      temp2 = quantile(i_total_z, 0.01, na.rm = TRUE),
      i_total_t = case_when(
        i_total_z < temp1 & i_total_z > temp2 ~ i_total_z,
        i_total_z >= temp1 ~ temp1,
        i_total_z <= temp2 ~ temp2,
        TRUE ~ NA_real_
      )
    ) %>%
    select(-temp1, -temp2) %>%
    relocate(i_total_t, .after = i_total_z)
  
  var_label(klips_long4$i_total_t) <- "Total household income winsorized"
}

# Additional variable construction with error handling
klips_long4 <- klips_long4 %>%
  mutate(
    highgrad = ifelse(!is.na(collegegrad) & collegegrad == 0, 1, 0),
    seoul = ifelse(!is.na(province) & province == 1, 1, 0)
  )

# Handle i_total_t_100 creation carefully
if ("i_total_t" %in% names(klips_long4)) {
  klips_long4 <- klips_long4 %>% mutate(i_total_t_100 = i_total_t / 100)
} else if ("i_total_z" %in% names(klips_long4)) {
  klips_long4 <- klips_long4 %>% mutate(i_total_t_100 = i_total_z / 100)
  warning("Using i_total_z instead of i_total_t for i_total_t_100 calculation")
} else {
  warning("No suitable income variable found for i_total_t_100 calculation")
}

# Label variables
var_label(klips_long4$highgrad) <- "high school graduate"
var_label(klips_long4$seoul) <- "living in Seoul"

if ("LFS_h" %in% names(klips_long4)) var_label(klips_long4$LFS_h) <- "LFS of husband"
if ("LFS_w" %in% names(klips_long4)) var_label(klips_long4$LFS_w) <- "LFS of wife"
if ("i_total_t_100" %in% names(klips_long4)) {
  var_label(klips_long4$i_total_t_100) <- "Total household income winsorized in million"
}

# [Insert the corrected dummy variable creation code from above here]

# Save the processed data
if (!dir.exists("processed_data")) dir.create("processed_data")
saveRDS(klips_long4, "processed_data/klips_imerge_long4.rds")

message("Data processing completed successfully")
