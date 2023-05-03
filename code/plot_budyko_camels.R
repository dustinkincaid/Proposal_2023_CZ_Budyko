# Plot CAMELS sites in Budyko Space

# Example code is from Bryn Stewart (see forwarded email from Julia Perdrial on 2/2/2023 subject: Budyko curve)

# Load libraries
  library("tidyverse")
  library("lubridate")
  library("here")       # Making file paths simple and robust
  library("fs")  
  library("data.table") # Reading in many files at once & other fast functions  
  library("leaflet")
  library("htmlwidgets")
  
  
# Functions ----
  # To plot the Budyko curve w/ different beta values (see email noted above)
  # NOTE: format these correctly at some point
  Budyko_BETA1 <- function(PET_P)
    ((((1 / PET_P) ^ 1) + 1) ^ (1 / 1)) ^ -1
  Budyko_BETA2 <- function(PET_P)
    ((((1 / PET_P) ^ 2) + 1) ^ (1 / 2)) ^ -1
  Budyko_BETA3 <- function(PET_P)
    ((((1 / PET_P) ^ 3) + 1) ^ (1 / 3)) ^ -1  
  Budyko_BETA4 <- function(PET_P)
    ((((1 / PET_P) ^ 4) + 1) ^ (1 / 4)) ^ -1    
  
  
  
# Read in data ----
  ## CAMELS attributes (downloaded from CAMELS website in March 2022) ----
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
  
  ## CAMELS water chemistry dataset (from Ijaz on 2/14/23; includes new total cl, no3, so4 (filter + unfiltered)) ----
  chem <-
    read_csv(here("data", "camels_chem_total_cl_no3_so4_included.csv"), 
             na = c("", "NULL"),
             col_types = cols(
               dic = col_double()))
  
  # Read in gauge site info downloaded from USGS on 2021-12-11 (see code in summarize_camels_q.R in CZCN-CAMELS-CHEM Rproject folder)
  # Definitions of variables here: http://usgs-r.github.io/dataRetrieval/reference/readNWISsite.html
  # And more info here: https://help.waterdata.usgs.gov/codes-and-parameters  
  siteInfo <- 
    read_csv(here("data", "USGS_gauge_info.csv")) %>%
    rename(gauge_id = site_no) %>% 
    # Add a zero to front of gauge_ids in dat that are have < 8 chars
    mutate(gauge_id = as.character(gauge_id)) %>% 
    mutate(string_length = str_length(gauge_id)) %>% 
    mutate(gauge_id = ifelse(string_length < 8, paste0("0", gauge_id), gauge_id)) %>% 
    select(-string_length)
  
  # ANSI state codes
  state_codes <- 
    read_csv(here("data", "ansi_us_state_codes.csv")) %>% 
    rename(state_cd = STATE, state_abb = STUSAB, state_name = STATE_NAME) %>% 
    select(-STATENS)  
  
  # EXAMPLE OF HOW I JOINED STATE CD INFO TO DATA IN THE PAST
  # Join siteInfo to dat and add state abbrev and name
  # dat <-
  #   dat %>% 
  #   # First drop gauge_id2 from dat
  #   # select(-gauge_id2) %>% 
  #   # Add a zero to front of gauge_ids in dat that are have < 8 chars
  #   mutate(gauge_id = as.character(gauge_id)) %>% 
  #   mutate(string_length = str_length(gauge_id)) %>% 
  #   mutate(gauge_id = ifelse(string_length < 8, paste0("0", gauge_id), gauge_id)) %>% 
  #   select(-string_length) %>% 
  #   # Join by gauge_id
  #   left_join(siteInfo, by = c("gauge_id" = "site_no")) %>% 
  #   # Add state info
  #   mutate(state_cd = as.numeric(state_cd)) %>% 
  #   left_join(state_codes, by = "state_cd") %>% 
  #   # Add discharge range data
  #   left_join(q_range, by = "gauge_id") %>% 
  #   # Add a guage id with state abbrev in front
  #   mutate(gauge_id_state = paste(state_abb, gauge_id, sep = "_"))
  
  # Read in AmeriFlux sites downloaded from https://ameriflux.lbl.gov/sites/site-search/# on 2023-04-13
  af_sites <- read_tsv(here("data", "sites_ameriflux.tsv"))

# Process/tidy data ----  
  ## Calculate a few more variables in CAMELS ----
    camels <- 
      camels %>% 
             # Calculate AET (P - Q = AET); p & q units = mm/day
      mutate(AET_mmd = p_mean - q_mean, 
             # Calculate Evaporative Index (AET/P)
             AET_P = AET_mmd/p_mean) %>% 
      # Add column with aridity index renamed as PET/P for consistency 
      rename(PET_P = aridity)
      
  ## Tidy up chem
    chem <-
      chem %>% 
      # Set the date column
      mutate(date = sample_start_dt) %>%
      # Priority of discharge values to use
      mutate(q_use = ifelse(q_inst > 0, q_inst, NA),
             q_use = ifelse(is.na(q_use) & q_15 > 0, q_15, q_use),
             q_use = ifelse(is.na(q_use) & q_daily > 0, q_daily, q_use),
             q_use = ifelse(is.na(q_use) & q_derived > 0, q_derived, q_use))
      # filter(!is.na(q_use))      
      
  ## Deal with concentrations that are 0
    # First, let's calculate minimum concentrations > 0
    min_concs <-
      chem %>% 
      pivot_longer(cols = c(cl:don, total_cl:total_so4),
                   names_to = "constituent",
                   values_to = "conc") %>% 
      filter(conc > 0) %>% 
      group_by(gauge_id, constituent) %>% 
      summarize(conc_min = min(conc))
    
    # We don't know the detection limits, so for now lets set 0 values to 1/2 minimum concentration
    chem <-
      chem %>% 
      pivot_longer(cols = c(cl:don, total_cl:total_so4),
                   names_to = "constituent",
                   values_to = "conc") %>% 
      full_join(min_concs, by = c("gauge_id", "constituent")) %>% 
      mutate(conc := ifelse(conc == 0, conc_min/2, conc))
    
    rm(min_concs)      
            
  ## Summarize chem variables ---- 
    chem_summ <-
      chem %>% 
      group_by(gauge_id, constituent) %>% 
      summarize(n = sum(!is.na(conc)),
                mean_conc = mean(conc, na.rm = TRUE),
                first_yr = min(year(date)),
                last_yr = max(year(date)),
                total_yrs = last_yr - first_yr + 1) %>% 
      pivot_wider(names_from = constituent, values_from = c(n, mean_conc))
    
  ## Join chem_summ to camels
    camels_summ <-
      camels %>% 
      full_join(chem_summ, by = "gauge_id") %>% 
      mutate(geology = ifelse(geol_1st_class %in% c("Carbonate sedimentary rocks"), "Carbonate",
                              ifelse(geol_1st_class %in% c("Acid plutonic rocks", "Acid volcanic rocks", "Basic volcanic rocks",
                                                           "Intermediate plutonic rocks", "Intermediate volcanic rocks", "Pyroclastics"), "Igneous",
                                     ifelse(geol_1st_class %in% c("Metamorphics"), "Metamorphics",
                                            ifelse(geol_1st_class %in% c("Mixed sedimentary rocks", "Siliciclastic sedimentary rocks"), "Sedimentary",
                                                   ifelse(geol_1st_class %in% c("Unconsolidated sediments"), "Unconsolidated", NA))))))
    
  ## Add vars to af_sites ----
    af_sites_usa <-
      af_sites %>% 
      filter(Country == "USA") %>% 
      filter(!is.na(`Site Start`)) %>% 
      # Assuming an NA site end year means the site is still collecting data
      mutate(`Site End` = ifelse(is.na(`Site End`), lubridate::year(Sys.Date()), `Site End`),
             rec_length = `Site End` - `Site Start`,
             rec_length_cat = ifelse(rec_length < 5, "<5",
                                     ifelse(rec_length >=5 & rec_length < 10, "5-9",
                                            ifelse(rec_length >= 10, ">10", NA))),
             rec_period = paste0(`Site Start`, "-", `Site End`))
      
    
# Plot CAMELS sites on Budyko curve ----
  # CAMELS sites at PET/P boundaries >20 years of data
  camels_summ_sub <-
    camels_summ %>% 
    filter(
           # (PET_P > 0.2 & PET_P < 0.4) |
           (PET_P > 0.9 & PET_P < 1.1) |
           (PET_P > 2.5)) %>% 
    filter(total_yrs > 20)
  
  ## Budyko plot ----
  camels_summ_sub %>% 
    #Plot AET/P versus PET/P
    ggplot(aes(x = PET_P, y = AET_P)) + 
    # Beta = 1 curve
    # geom_function(fun = Budyko_BETA1, colour = "red", lwd = 0.75, linetype = 1) + 
    # Beta = 2 curve
    # geom_function(fun = Budyko_BETA2, colour = "red", lwd = 1.5, linetype = 1) + 
    # Beta = 3 curve
    geom_function(fun = Budyko_BETA4, colour = "black", linewidth = 1, linetype = 1) + 
    # Set symbol size to mean concentration ("size = Ca"), specify symbol color, etc.
    # geom_point(aes(size = Ca), shape = 21, colour = "black", fill = "darkgrey", stroke = 1) + 
    # geom_point(aes(size = n_ca), shape = 21, color = "black", stroke = 1) +
    geom_point(aes(size = geol_permeability, fill = geology), shape = 21, color = "black", stroke = 0.7, alpha = 0.8) +
    # Set breaks and size range manually, also legend title
    scale_size(name = "Geological\npermeability") +
               # range = c(0.2, 8),
               # breaks = c(10, 50, 100, 500)) +
    scale_fill_brewer(name = "Geology",
                      palette = "Dark2") +
    labs(x = "PET/P", 
         y = "AET/P",
         title = "CAMELS sites near boundaries w/ >20 years of data") +
    xlim(0, 3.5) +
    ylim(0, 1.2) +
    # Add solute label
    # annotate("text", x=0.2, y=1.1, label= "Ca", size = 14, fontface = "bold") + 
    guides(
      fill = guide_legend(override.aes = list(size = 2))) +   
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=18))
  
  ggsave("plots/budyko_camels_endmember_sites_permeability.png", width = 10, height = 6, units = "in", dpi = 150)
  
  ## leaflet: map CAMELS sites w/ AmeriFlux sites ----
  map_camels_vs_ameriflux <- 
    leaflet() %>% 
    addProviderTiles("CartoDB.Positron") %>%
    addCircleMarkers(data = af_sites_usa %>% 
                       filter(`Latitude (degrees)` < 50,
                              `Longitude (degrees)` > -125,
                              rec_length >= 5),
                     lng = ~`Longitude (degrees)`,
                     lat = ~`Latitude (degrees)`,
                     popup = ~paste0(`Site ID`, " ", rec_period),
                     radius = ~1,
                     color = "black") %>%     
    addCircleMarkers(data = camels_summ_sub,
                     lng = ~gauge_lon,
                     lat = ~gauge_lat,
                     popup = ~paste0(gauge_name, " ", "PET/P = ", format(round(PET_P, 1), nsmall = 1)),
                     radius = ~2,
                     color = ~ifelse(PET_P < 1.5, "blue", "red"))
  
  htmlwidgets::saveWidget(map_camels_vs_ameriflux, here("plots", "map_camels_vs_ameriflux.html"))

  
  
  # What are the sites in camels_summ_sub?
  camels_summ_sub_sort <-
    camels_summ_sub %>% 
    select(gauge_id, gauge_name, )    
    
# Example plot
  # CAMELS sites w/ >20 Ca measurements
  camels_summ %>% 
    filter(n_ca > 20) %>% 
    #Plot AET/P versus PET/P
    ggplot(aes(x = PET_P, y = AET_P)) + 
    # Beta = 1 curve
    # geom_function(fun = Budyko_BETA1, colour = "red", lwd = 0.75, linetype = 1) + 
    # Beta = 2 curve
    # geom_function(fun = Budyko_BETA2, colour = "red", lwd = 1.5, linetype = 1) + 
    # Beta = 3 curve
    geom_function(fun = Budyko_BETA4, colour = "black", linewidth = 1, linetype = 1) + 
    # Set symbol size to mean concentration ("size = Ca"), specify symbol color, etc.
    # geom_point(aes(size = Ca), shape = 21, colour = "black", fill = "darkgrey", stroke = 1) + 
    # geom_point(aes(size = n_ca), shape = 21, color = "black", stroke = 1) +
    geom_point(aes(size = sand_frac, fill = geol_1st_class), shape = 21, color = "black", stroke = 1) +
    # Set breaks and size range manually, also legend title
    # scale_size(name = "Ca (mg/l)", 
    #            range = c(0.2, 8), 
    #            breaks = c(10, 50, 100, 500)) + 
    labs(x = "PET/P", 
         y = "AET/P",
         title = "CAMELS sites w/ >20 Ca measurements") +
    xlim(0, 3.5) +
    ylim(0, 1.2) +
    # Add solute label
    # annotate("text", x=0.2, y=1.1, label= "Ca", size = 14, fontface = "bold") + 
    theme_bw() +
    theme(panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          axis.text=element_text(size=14),
          axis.title=element_text(size=18))
  
  ggsave("plots/budyko_camels_sites_by_bedrock_sandfrac.png", width = 10, height = 6, units = "in", dpi = 150)
  
  

  