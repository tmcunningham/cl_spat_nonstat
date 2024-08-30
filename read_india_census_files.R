library(dplyr)
library(readxl)
library(tidyr)
library(stringr)

# Define function to read in Indian Census 2011 district-level Excel files
# n_header_rows includes rows to include in header names (incl table name rows)
# data_start_row is the row number that the data starts
# n_name_rows is the number of rows given to name of the table (will be skipped)
read_census_excel <- function(filename, n_header_rows,
                              data_start_row = n_header_rows + 3, 
                              n_name_rows = 1) {
  # Read just header info
  raw_names <- suppressMessages(read_excel(filename, 
                                           col_names = F, 
                                           skip = n_name_rows, 
                                           n_max = n_header_rows - n_name_rows))
  
  clean_names <- raw_names %>%
    #filter(pmap(., lift_vd(function(x) sum(!is.na(as.numeric(x,mute = T))))) == 0) %>%
    filter_all(any_vars(!is.na(.))) %>% 
    fill(names(.), .direction = "down") %>% 
    t() %>% 
    as.data.frame() %>% 
    fill(names(.), .direction = "down") %>% 
    t() %>% 
    as.data.frame() %>% 
    mutate_all(function(x) tolower(gsub(" ", "_", 
                                        str_squish(
                                          gsub("\r", " ",
                                               gsub("\n", " ",
                                                    gsub("-", " ", 
                                                         gsub("[[:punct:]]", "", x)))))))) %>% 
    summarise_all(function(x) utile.tools::paste(unique(x), collapse = "_", na.rm = T)) %>% 
    as.character()
  
  # Read data without header info
  data <- suppressMessages(read_excel(filename, 
                                      col_names = clean_names, 
                                      skip = data_start_row-1))
  
  # Filter out rows that are only not NA in first or second columns
  final_data <- data[rowSums(!is.na(data[,-c(1,2)])) != 0,]
  
  return(final_data)
}

# Define function to apply read_census_excel over all files in a directory
read_all_census_excels <- function(folder_path, n_header_rows,
                                   data_start_row = n_header_rows + 3, 
                                   n_name_rows = 1) {
  out_df <- list.files(folder_path, full.names = T) %>% 
    lapply(read_census_excel, 
           n_header_rows = n_header_rows,
           data_start_row = data_start_row, 
           n_name_rows = n_name_rows) %>% 
    bind_rows() %>%
    rename(any_of(c(district_code = "distt_code",
                    tahsil_code = "tehsil_code"))) %>% 
    # If district_code still does not exist, use district col
    # For migration table - could be problematic for future tables if other names for column
    mutate(district_code = {if ("district_code" %in% colnames(.)) district_code else district},
           state_code = {if ("state_code" %in% colnames(.)) state_code else state}) %>% 
    # Create column for area type, based on code not being 0
    # Prefixes in area name col don't always exist
    mutate(
      area_type = if("town_code" %in% colnames(.)) {
        case_when(
          as.numeric(town_code) != 0 ~ "Town",
          as.numeric(tahsil_code) != 0 ~ "Sub-district",
          as.numeric(district_code) != 0 ~ "District",
          as.numeric(state_code) != 0 ~ "State"
        )
      } else if ("tahsil_code" %in% colnames(.)) {
        case_when(
          as.numeric(tahsil_code) != 0 ~ "Sub-district",
          as.numeric(district_code) != 0 ~ "District",
          as.numeric(state_code) != 0 ~ "State"
        )
      } else {
        case_when(
          as.numeric(district_code) != 0 ~ "District",
          as.numeric(state_code) != 0 ~ "State"
        )
      },
      # Remove prefixes in area name column (as this is now in area_type col)
      area_name = gsub(paste0("^state[ ]?-[ ]?"), "", ignore.case = T,
                       gsub(paste0("^district[ ]?-[ ]?"), "", ignore.case = T,
                            gsub(paste0("^sub-district[ ]?-[ ]?"), "", 
                                 ignore.case = T,
                                 x = area_name)))
    )
  
  return(out_df)
  
}
