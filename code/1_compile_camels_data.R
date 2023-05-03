# Compile CAMELS attribute data with CAMELS-Chem and other USGS gauge information

# Load libraries
  library("tidyverse")
  library("here")       # Making file paths simple and robust
  library("fs")  
  library("data.table") # Reading in many files at once & other fast functions
  
  
# Read in data ----
  # CAMELS attributes (downloaded from CAMELS website in March 2022)
  data_dir <- here("data", "camels_attributes")
  camels <- 
    data_dir %>% 
    # List only the CSV files in the folder
    fs::dir_ls(regexp = "\\.txt$") %>% 
    # Read in all CSV files and compile into one df via row-binding
    map_dfc(~data.table::fread(.)) %>% 
    # Deal with multiple gauge_id columns
    rename(gauge_id = `gauge_id...1`) %>% 
    select(-contains("...")) %>% 
    # Add a zero to front of gauge_ids in dat that are have < 8 chars
    mutate(gauge_id = as.character(gauge_id)) %>% 
    mutate(string_length = str_length(gauge_id)) %>% 
    mutate(gauge_id = ifelse(string_length < 8, paste0("0", gauge_id), gauge_id)) %>% 
    select(-string_length)
  
  # CAMELS water chemistry dataset (from Ijaz on 2/14/23; includes new total cl, no3, so4 (filter + unfiltered))
  chem <-
    read_csv(here("data", "camels_chem_total_cl_no3_so4_included.csv"), 
             na = c("", "NULL"),
             col_types = cols(
               dic = col_double()))
  