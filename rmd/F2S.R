library(tidyverse)
library(tidycensus)
library(readxl)
library(openxlsx)
library(xlsx)

# cache shapefiles for use in future sessions
options(tigris_use_cache = TRUE)

# load table for searchability
v19 <- load_variables(2019, "acs5", cache = TRUE)
View(v19)

  
# pull data from census


  #  mean travel time to work (minutes), workers age 16 years+, 2015-2019 - (Minutes)
    # B08013_001 = Aggregate travel time to work / # B08303_001 = number of workers 16 years old and over who did not work at home

avg_commute <- get_acs(geography = "county", 
                       variables = 'B08013_001',
                       year = 2019,
                       summary_var = 'B08303_001',
                       geometry = FALSE) %>%
  mutate(avg_mins = round(estimate / summary_est)) %>%
  separate(NAME,into = c("county", "state"), sep = ",") %>%
  select(state, county, avg_mins) %>%
  


  # percent of people with no vehicle
    # B08014_002 = No vehicle available / # B08014_001 = Workers 16 years and Over in households

no_vehicle <- get_acs(geography = "county", 
                       variables = 'B08014_002',
                       year = 2019,
                       summary_var = 'B08014_001',
                       geometry = FALSE) %>%
  mutate(pct_no_vehicle = estimate / summary_est) %>%
  separate(NAME,into = c("county", "state"), sep = ",") %>%
  select(state, county, pct_no_vehicle)



  # percent in poverty
    # B17001_002 = # of people in poverty in the last 12 months / B17001_001 = estimate total (denominator)


poverty <- get_acs(geography = "county", 
                      variables = "B17001_002",
                      year = 2019,
                      summary_var = "B17001_001", # always use the 001 variable of the indicator as a denominator  
                      geometry = FALSE) %>%
  mutate(pct_in_poverty = (estimate / summary_est)*100) %>%
  separate(NAME,into = c("county", "state"), sep = ",") %>%
  select(state, county, pct_in_poverty)


poverty_check <- get_acs(
  geography = "county",
  variables = "DP03_0128P",
  year = 2019)


  # % persons of color (non-hispanic white/all races hispanic and non-hispanic)

pop_2019 <- get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("RACE", "HISP"),
  breakdown_labels = TRUE,
  year = 2019, 
  output = "wide")


poc <- pop_2019 %>%
  unite("race_ethn", RACE:HISP, remove = FALSE) %>%
  filter(race_ethn == "All races_Both Hispanic Origins" | race_ethn == "White alone_Non-Hispanic") %>%
  select(-RACE,-HISP,-GEOID) %>%
  spread(race_ethn, value) %>%
  rename(all = `All races_Both Hispanic Origins`,
         white = `White alone_Non-Hispanic`) %>%
  mutate(pct_white = white/all,           
         pct_poc = 1 - pct_white) %>%
  separate(NAME,into = c("county", "state"), sep = ",") %>%
  select(state, county, pct_poc)
  


# import and transform 2019 food insecurity data from feeding america


food_secure <- read_xlsx("C:/Users/jfain/OneDrive - Save the Children Federation Inc/USP/Projects/Feed to Succeed/data/MMG2021_2019Data_ToShare.xlsx", sheet = 2) %>%
  rename(fs_rate_2019 = 4,
         fs_rate_2019_children = 13,
         state_abb = 2,
         county = 3) %>%
  select(state_abb, county, fs_rate_2019, fs_rate_2019_children) %>%
  separate(county, c("county", "state"), sep = "([,])")

    # import metro and non-metro data

rural_status <- read_xlsx("data/RBA Data.xlsx", skip = 1) %>%
  select(3,5,19) %>%
  rename(county= County,
         state_abb = 2,
         metro_nonmetro = 3)


# Read in and merge all food access files

setwd("C:/Users/jfain/OneDrive - Save the Children Federation Inc/USP/Projects/Feed to Succeed/data/Access")
data.files = list.files(pattern = "*.xlsx")
access <- lapply(data.files, function(x) read_xlsx(x, sheet = 5, range = cell_cols(1:93)))

access_df <- as.data.frame(bind_rows(access)) %>%
  select(2,3,93)

    # change column header

names(access_df) <- as.matrix(access_df[1, ])
access_df <- access_df[-1, ]
access_df[] <- lapply(access_df, function(x) type.convert(as.character(x)))


  # filter out headers and NAs (state averages) and divide access column by 100 to create decimal

access_final <- access_df %>%
  rename(state = "State",
         county = "County",
         lim_access = `% Limited Access to Healthy Foods`) %>%
  filter(!is.na(county),
         state != "State",
         county != "County",
         lim_access != "% Limited Access to Healthy Foods") %>%
  mutate(county = as.character(county),
         state = as.character(state),
         lim_access = as.numeric(paste(lim_access))/100)

 # add "County" after county name (but not for LA)

access_final_LA <- access_final %>%
  filter(state == "Louisiana")

access_final_no_LA <- access_final %>%
  filter(state != "Louisiana")

access_final_no_LA$county <- paste(access_final_no_LA$county, "County") 
access_final_LA$county <- paste(access_final_LA$county, "Parish") 

access_final <- rbind(access_final_LA, access_final_no_LA)



# merge datasets and create new county column for powerbi

final <- food_secure %>%
  left_join(poverty, by = c("county", "state")) %>%
    left_join(avg_commute, by = c("county", "state")) %>%
      left_join(no_vehicle, by = c("county", "state")) %>%
        left_join(poc, by = c("county", "state")) %>%
         left_join(rural_status, by = c("county", "state_abb")) %>%
           mutate(county_geo = paste(county, state_abb))
            
 # fix overall left space issue on state for join to access
final$state <- trimws(final$state, which = "left")

final <-  left_join(final, access_final, by = c("county", "state"))

# add logical variables
  # if food insecurity >= 20 AND poverty rate <= 20 AND average commute <= national average, 1, 0
  # if food insecurity >= 20 AND poverty rate >= 20 AND average commute <= national average, 1, 0
  # if food insecurity >= 20 AND poverty rate >= 20 AND average commute >= national average, 1, 0
  # Then add sum column summing these columns for an index (index = ind1 + ind2 + ind3)


final %>%
  filter(!is.na(avg_mins)) %>%
  summarize(mean(avg_mins))


final_index <- final %>%
  filter(metro_nonmetro == "Nonmetropolitan", !is.na(avg_mins)) %>%
  mutate(ind1 = ifelse(fs_rate_2019_children >= 20 & pct_in_poverty <= 20 & avg_mins <= 23.8 , 1, 0),
         ind2 = ifelse(fs_rate_2019_children >= 20 & pct_in_poverty >= 20 & avg_mins <= 23.8 , 1, 0),
         ind3 = ifelse(fs_rate_2019_children >= 20 & pct_in_poverty >= 20 & avg_mins >= 23.8 , 1, 0),
         index = ind1+ind2+ind3)




write.xlsx(final_index, "F23_Index.xlsx")
