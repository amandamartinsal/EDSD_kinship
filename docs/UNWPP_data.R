library(readr)
library(dplyr)

## Function to load and filter UN World Population Prospects data
UNWPP_data <- function(country, start_year, end_year, sex) {
  
  # Define file paths
  fertility_file <- "unwppdata/WPP2022_Fertility_by_Age1.zip"
  male_lifetable_file <- "unwppdata/WPP2022_Life_Table_Complete_Medium_Male_1950-2021.zip"
  female_lifetable_file <- "unwppdata/WPP2022_Life_Table_Complete_Medium_Female_1950-2021.zip"
  
  # Check if files exist
  if (!file.exists(fertility_file)) stop("Fertility data file not found: ", fertility_file)
  if (!file.exists(male_lifetable_file)) stop("Male life table data file not found: ", male_lifetable_file)
  if (!file.exists(female_lifetable_file)) stop("Female life table data file not found: ", female_lifetable_file)
  
  # Read the data files
  WPP2022_f <- read_csv(fertility_file)  # Fertility data for females
  WPP2022_male_lifetable <- read_csv(male_lifetable_file) # Male mortality data
  WPP2022_female_lifetable <- read_csv(female_lifetable_file) # Female mortality data
  
  # Combine male and female life table data
  WPP2022_lifetable <- bind_rows(WPP2022_female_lifetable, WPP2022_male_lifetable)
  
  # Filter life table data based on country, year range, and sex
  px <- WPP2022_lifetable %>%
    select(Location, Sex, Time, AgeGrpStart, px) %>%
    filter(Location == country, Time >= start_year, Time <= end_year, Sex == sex) %>%
    rename(year = Time, age = AgeGrpStart)
  
  # Initialize the final data
  if (sex == "Male") {
    data <- px
  } else if (sex == "Female") {
    # Process fertility data if sex is Female
    asfr <- WPP2022_f %>%
      select(Location, Time, AgeGrpStart, ASFR) %>%
      mutate(ASFR = ASFR / 1000) %>%
      filter(Location == country, Time >= start_year, Time <= end_year) %>%
      rename(year = Time, age = AgeGrpStart, fx = ASFR)
    
    # Join and finalize the data with fertility rates
    data <- left_join(px, asfr, by = c("Location", "year", "age")) %>%
      mutate(fx = replace_na(fx, 0))
  } else {
    stop("Invalid sex. Please specify 'Male' or 'Female'.")
  }
  
  return(data)
}
