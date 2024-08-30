# Census data manually downloaded from https://censusindia.gov.in/census.website/data/census-tables (2023-02-10)
# State-level .xlsx files were downloaded for several tables for each state in India
# All state-level files were saved in a folder corresponding to each table
# 
# The folders containing all state files for each table are:
#   /data/district_age_ed_inst_ec_activity - "C-12: Population age 5-19 attending educational institution by economic activity status and sex (total)"
#   /data/district_age_education_level - "C-08: Educational level by age and sex for population age 7 and above (total)"
#   /data/district_age_sex_hh_head - "HH-06: Households by marital status, sex and age of the head of household (total)"
#   /data/district_norm_hh_size - "HH-01: Normal households by household size (total)"
#   /data/district_sc_pop - "A-10 Appendix: District wise scheduled caste population (Appendix)"
#   /data/district_st_pop - "A-11 Appendix: District wise scheduled tribe population (Appendix)"
#   /data/district_religion_sex - "C-15: Religious community by age group and sex"
#   /data/district_year_births - "F-09: Number of women and currently married women by present age, number of births last year by sex and birth order (total)"
# 
# Two other reference tables were downloaded and saved in the /data folder:
#   data/A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx - "A-01: Number of villages, towns, households, population and area (India, states/UTs, districts and Sub-districts) - 2011"
#   data/PC11_TV_DIR.xlsx - "Census 2011 - Location Code Directory"
    
source("read_india_census_files.R")
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(geojsonsf)
library(stringr)
library(sf)
library(ggplot2)
library(utile.tools)
library(spdep)

# Read data ---------------------------------------------------------------

# Read in Excel files in each directory
ed_inst_ec <- read_all_census_excels("data/census_2011/district_age_ed_inst_ec_activity",
                                     n_header_rows = 5)
                                     
age_ed_level <- read_all_census_excels("data/census_2011/district_age_education_level",
                                       n_header_rows = 5)

age_sex_hh_head <- read_all_census_excels("data/census_2011/district_age_sex_hh_head",
                                          n_header_rows = 4)

norm_hh_size <- read_all_census_excels("data/census_2011/district_norm_hh_size", 
                                       n_header_rows = 4)

# SC and ST files have 2001 state and district codes - this is corrected later
sc_pop <- read_all_census_excels("data/census_2011/district_sc_pop",
                                 n_header_rows = 4, 
                                 data_start_row = 6,
                                 n_name_rows = 2)

st_pop <- read_all_census_excels("data/census_2011/district_st_pop",
                                 n_header_rows = 5,
                                 data_start_row = 7,
                                 n_name_rows = 2)

relig <- read_all_census_excels("data/census_2011/district_religion_sex",
                                n_header_rows = 4,
                                data_start_row = 8)

births <- read_all_census_excels("data/census_2011/district_year_births",
                                 n_header_rows = 8, n_name_rows = 4,
                                 data_start_row = 11)


# Clean total population data ---------------------------------------------

# Data for total population and area size
pop_area <- read_census_excel("data/census_2011/A-1_NO_OF_VILLAGES_TOWNS_HOUSEHOLDS_POPULATION_AND_AREA.xlsx",
                              n_header_rows = 3, n_name_rows = 1, data_start_row = 5) %>%
  filter(!is.na(state_code), india_state_union_territory_district_subdistrict == "DISTRICT") %>%
  pivot_wider(id_cols = c(state_code, district_code),
              names_from = total_rural_urban, 
              values_from = c(area_in_sq_km, population_persons)) %>% 
  mutate(perc_urban_pop = population_persons_Urban/population_persons_Total,
         pop_dens = population_persons_Total/area_in_sq_km_Total) %>% 
  select(state_code, district_code, 
         tot_pop = population_persons_Total, pop_dens, perc_urban_pop)


# Clean child work data ---------------------------------------------------

# Rearrange data for children by worker type, age band, educational inst and sex
# Only 14 and under
district_child_work_ed <- ed_inst_ec %>% 
  filter(age != "5-19") %>% 
  mutate(age = as.numeric(age),
         age_band = case_when(age >= 5 & age <= 11 ~ "5-11",
                              age >= 12 & age <= 14 ~ "12-14")) %>%
  filter(age >= 5 & age <= 14,
         area_type == "District",
         total_rural_urban == "Total") %>% 
  group_by(table_name, state_code, district_code,
           district_name = area_name, age_band) %>% 
  summarise(across(-c(area_type, total_rural_urban, age, area_name,
                      total_persons, total_males, total_females),
                   ~ sum(.x))) %>% 
  ungroup() %>%
  pivot_longer(contains("educational_institution"),
               names_pattern = "(.*attending)_educational_institution_(.*)_([^_]*)",
               names_to =  c("educational_institution","workers_type","sex"),
               values_to = "count") %>% 
  filter(sex != "persons")

# Total percentages of children (U14) doing ANY work
all_child_working <- district_child_work_ed %>%
  group_by(state_code, district_code, district_name) %>% 
  summarise(u14_pop = sum(count),
            u14_count_working = sum(ifelse(workers_type != "nonworkers",
                                            count, 0)),
            u14_perc_working = u14_count_working / u14_pop,
            u14_perc_att_ed = sum(ifelse(educational_institution == "attending",
                                         count, 0)) / u14_pop,
            u14_perc_male = sum(ifelse(sex == "males",
                                       count, 0)) / u14_pop) %>% 
  ungroup()


# Clean other covars - ed level, female HH head, HH size, migrants --------

# Education level (inc. literacy rate) for all over 18s
o18_ed_level <- age_ed_level %>%
  filter(area_type == "District",
         total_rural_urban == "Total",
         is.na(as.numeric(agegroup)) | as.numeric(agegroup) >= 18,
         !(agegroup %in% c("All ages", "0-6", "Age not stated"))) %>%
  group_by(state_code, district_code, district_name = area_name) %>%
  summarise(o18_perc_lit = sum(literate_persons) / sum(total_persons),
            o18_f_perc_lit = sum(literate_females) / sum(total_females)) %>%
  ungroup()

# HH head by sex and marital status (inc. % female headed HHs), all ages
sex_hh_head <- age_sex_hh_head %>% 
  filter(area_type == "District",
         total_rural_urban == "Total",
         age_of_the_head_of_household_in_years == "All Ages") %>% 
  group_by(state_code, district_code, district_name = area_name) %>% 
  summarise(hh_perc_f_head = sum(total_households_female_haed) / 
           (sum(total_households_female_haed) + 
              sum(total_households_male_head))) %>% 
  ungroup()

# Mean size of normal HHs and the percentage that are urban
hh_size <- norm_hh_size %>% 
  filter(area_type == "District") %>% 
  dplyr::select(-c(tahsil_code, town_code)) %>% 
  group_by(state_code, district_code, district_name = area_name) %>% 
  mutate(perc_hh = normal_households_number / 
           sum(ifelse(total_rural_urban != "Total", 
                      normal_households_number, 0))) %>%
  summarise(hh_perc_urban = sum(ifelse(total_rural_urban == "Urban",
                                       perc_hh, 0)),
            hh_mean_size = sum(ifelse(total_rural_urban == "Total",
                                      mean_household_size, 0))
            ) %>% 
  ungroup()

# Clean SC ST and Muslim data ---------------------------------------------

# Total population of all Scheduled Castes
total_sc_pop <- sc_pop %>%
  filter(sc_name == "All Schedule Castes",
         area_type == "District",
         tru == "Total") %>% 
  separate(area_name, into = c("district_name", "district_code"), sep = -3) %>% 
  mutate(district_name = trimws(district_name)) %>% 
  group_by(state_code, district_code, district_name) %>% 
  summarise(sc_pop = sum(total_population_p)) %>% 
  ungroup()

# Total population of all Scheduled Tribes
total_st_pop <- st_pop %>% 
  filter(st_name == "All Schedule Tribes",
         area_type == "District",
         tru == "Total") %>% 
  separate(area_name, into = c("district_name", "district_code"), sep = -3) %>% 
  mutate(district_name = trimws(district_name)) %>% 
  group_by(state_code, district_code, district_name) %>% 
  summarise(st_pop = sum(total_population_p)) %>% 
  ungroup()

# Total population of Muslims
total_muslim_pop <- relig %>% 
  filter(area_type == "District",
         age_group == "All ages",
         total_rural_urban == "Total") %>% 
  select(state_code, district_code, district_name = area_name, 
         muslim_pop = religious_communities_muslim_persons)

# Combine SC, ST and Muslim pop
sc_st_muslim_pop <- pop_area %>% 
  left_join(select(total_sc_pop, -district_name), 
            by = c("state_code", "district_code")) %>% 
  left_join(select(total_st_pop, -district_name), 
            by = c("state_code", "district_code")) %>%
  left_join(select(total_muslim_pop, -district_name),
            by = c("state_code", "district_code")) %>% 
  mutate(sc_perc = sc_pop / tot_pop,
         st_perc = st_pop / tot_pop) %>% 
  rowwise() %>% 
  mutate(sc_st_perc = sum(st_pop, sc_pop, na.rm = T) / tot_pop,
         muslim_perc = muslim_pop / tot_pop)

# Calculate fertility rate ------------------------------------------------

# Ages of childbearing to calculate fertility rate (15-49)
fertility_ages <- c("15-19","20-24","25-29","30-34","35-39",
                    "40-44","45-49")

# Fertility rate
birth_rate <- births %>% 
  filter(area_type == "District",
         total_rural_urban == "Total",
         present_age %in% fertility_ages) %>% 
  mutate(total_births = number_of_births_last_year_f + 
           number_of_births_last_year_m,
         age_fert_rate = (total_births/total_women)*1000) %>%
  group_by(state_code, district_code, district_name = area_name) %>% 
  summarise(total_fertility_rate = 5*sum(age_fert_rate)/1000) %>% 
  ungroup()


# Get wealth index from Mohanty paper -------------------------------------
# Wealth estimates were taken from a paper by Mohanty et al.
# "Estimates of Poverty and Inequality in the Districts of India, 2011–2012" (2016)
# Table A1, Appendix A
wealth_district <- read.csv("data/wealth_est.csv") %>% 
  mutate(mpce = gsub(",", "", mpce),
         across(-distr_name, as.numeric))

# District names don't all match - and some duplicates - and no codes so need to rename
duplicate_distr <- all_child_working %>% 
  count(district_name) %>% 
  filter(n > 1) %>% 
  pull(district_name)

# State code directory
state_code_dir <- read_excel("data/census_2011/PC11_TV_DIR.xlsx") %>% 
  janitor::clean_names() %>% 
  filter(district_code == "000", town_village_code == "000000") %>% 
  mutate(state_name = str_to_title(town_village_name))

# Write problem names to CSVs for easier renaming
# wealth_district %>% 
#   filter(!(distr_name %in% all_child_working$district_name)) %>% 
#   select(distr_name) %>% 
#   write.csv("outputs/renaming_paper.csv")
# 
# all_child_working %>% 
#   filter(!(district_name %in% wealth_district$distr_name)) %>% 
#   select(district_name)
#   write.csv("outputs/renaming_census.csv")

# Read in file to rename districts to match with Mohanty data
district_names <- read.csv("data/final_distr_names.csv")

# Rename districts in census CL data
all_child_working_renamed <- all_child_working %>% 
  left_join(state_code_dir %>% select(state_code, state_name),
            by = "state_code") %>% 
  left_join(district_names %>% select(census_distr, new_distr),
            by = c("district_name" = "census_distr")) %>% 
  mutate(district_name = case_when(!is.na(new_distr) ~ new_distr,
                                   district_name %in% duplicate_distr ~
                                     paste(district_name, state_name, 
                                           sep = ", "),
                                   TRUE ~ district_name)) %>% 
  select(-new_distr)

# Rename districts in wealth estimates data
wealth_district_renamed <- wealth_district %>% 
  left_join(district_names %>% select(paper_distr, new_distr),
            by = c("distr_name" = "paper_distr")) %>% 
  mutate(distr_name = case_when(!is.na(new_distr) ~ new_distr,
                                TRUE ~ distr_name)) %>% 
  select(-new_distr)


# Join data ---------------------------------------------------------------

# Join census data with wealth estimates by name
all_child_working_wealth <- all_child_working_renamed %>% 
  left_join(wealth_district_renamed,
            by = c("district_name" = "distr_name"))

# List of data frames to join to CL data
join_data_list <- list(o18_ed_level,
                       sex_hh_head,
                       hh_size,
                       sc_st_muslim_pop,
                       birth_rate)

join_data <- reduce(lapply(join_data_list,
                           select, -any_of("district_name")),
                    left_join,
                    by = c("state_code", "district_code"))

all_data <- all_child_working_wealth %>% 
  left_join(join_data,
            by = c("state_code", "district_code")) %>%
  replace_na(list(sc_perc = 0, st_perc = 0)) %>% 
  mutate(log_fertility = log(total_fertility_rate))

# Save data
write.csv(all_data,
          "outputs/all_district_census_data.csv",
          row.names = F)

# Join with shapefile ----------------------------------------------------------

# Read in shapefile
# From https://github.com/datameet/maps
census2011_sf <- st_read("data/datameet_shp/maps-master/maps-master/Districts/Census_2011/2011_Dist.shp") %>% 
  select(-c(ST_NM, DT_CEN_CD))

# Filter out islands, join data with shapefile and fix issue with Cuddalore district boundary
# See https://github.com/datameet/maps/issues/71
mainland_data_sf <- census2011_sf %>%
  left_join(all_data %>% 
              mutate(district_code = as.numeric(district_code)),
            by = c("censuscode" = "district_code")) %>% 
  filter(!(censuscode %in% c(638, 639, 640, 587))) %>% 
  st_make_valid()

# Save unprojected data as gpkg
st_write(mainland_data_sf,
         "outputs/mainland_data_unproj.gpkg",
         append = F)

# Project SF to India CRS
mainland_data_proj_sf <- mainland_data_sf %>% 
  st_transform(crs = "EPSG:7755")

# Save projected data
st_write(mainland_data_proj_sf,
         "outputs/mainland_data_proj.gpkg",
         append = F)
