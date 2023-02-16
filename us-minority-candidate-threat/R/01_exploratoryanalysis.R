# 01_exploratoryanalysis
# Pia Deshpande
# February 15, 2022

# SZ: I'm attaching the quick code I used to count the number of elections 
# in which there is at least one black and one latino candidate running. 
# I kept thinking about the point you made about partisanship. 
# One thing we can do first is to get a sense of the share of minorities 
# that are Dem. If this is large, then I have to think more about it. 
# At minimum, we'd have to make clear that we're measuring a bundle treatment 
# of ethnicity and party. To show that what we're measuring is not only 
# a party effect, we'd ideally show no discontinuities between white Dems 
# and white Reps. (count_minority_vs_minority.R)

##### LIBRARY ####
library(dplyr)
library(here)

##### LOAD DATA ####
data <- readRDS(here("Data/deshpande_zonszein_221202.rds"))
# race_final is imputed

##### ANALYSIS ####
# prelim <- 
#   data %>%
#   group_by(fips_char, year) %>%
#   # grouping by fips_char and year to get candidates in same races (all mayoral)
#   summarize(
#     
#   )

minority_dem <- data %>%
  filter(race_final != "caucasian") %>% # 1957 rows
  group_by(pid_final) %>%
  mutate(frequency = n()/nrow(.)) %>%
  select(pid_final, frequency) %>%
  unique()

minority_dem
# 66% of minority mayoral candidates in Yamil's dataset are Dems
# 15% have an unknowm partisanship (nonpartisan races?)
# 19% are Republicans

minority_dem_black <- data %>%
  filter(race_final == "black") %>% # 985 rows
  group_by(pid_final) %>%
  mutate(frequency = n()/nrow(.)) %>%
  select(pid_final, frequency) %>%
  unique()
# 77% of black candidates are Dem
# 13% are Rep (high!)
# 10% are unknown

minority_dem_hisp <- data %>%
  filter(race_final == "hispanic") %>% # 763
  group_by(pid_final) %>%
  mutate(frequency = n()/nrow(.)) %>%
  select(pid_final, frequency) %>%
  unique()
# 55% of hisp candidates are Dem
# 22% are Rep 
# 23% are unknown

minority_dem_asian <- data %>%
  filter(race_final == "asian") %>% # 173
  group_by(pid_final) %>%
  mutate(frequency = n()/nrow(.)) %>%
  select(pid_final, frequency) %>%
  unique()
# 54% of asian candidates are Dem
# 34% are Rep (high!)
# 12% are unknown

minorityvsminority_dem <- data %>%
  group_by(fips_char, year, month) %>%
  filter(any(race_final != "caucasian")) %>% # 4512 rows
  group_by(pid_final) %>%
  mutate(frequency = n()/nrow(.)) %>%
  select(pid_final, frequency) %>%
  unique()

# TODO: How many contests have at least one minority candidate?
# minorityvsminority_dem_I <- data %>%
#   group_by(fips_char, year, month) %>%
#   summarize(minority_contest = )

minorityvsminority_dem
# 50% of minorities in minority v. white races are Dem
# 25% are unknown
# 25% are Republicans

# TODO: filter to contests where there is at least one white candidate and one
# POC candidate


minorityvsminority_dem_only <- data %>%
  group_by(fips_char, year, month) %>%
  filter(race_final != "caucasian") %>% # 1957 rows
  group_by(pid_final) %>%
  mutate(frequency = n()/nrow(.)) %>%
  select(pid_final, frequency) %>%
  unique()

minorityvsminority_dem_only
# 67% of minorities in minority v. minority races are Dem (NO WHITE CANDIDATES)
# 15% are unknown
# 19% are Republicans

# Let's limit to races with at least one minority candidate
minorityelec <- data %>%
  group_by(fips_char, year, month) %>%
  filter(any(race_final != "caucasian")) %>% # 4512 rows
  select(fips_char, year, month, race_final, winner) %>%
  filter(winner == "win") %>%
  group_by(race_final) %>%
  mutate(win_frequency = n()/nrow(.)) %>%
  select(race_final, win_frequency) %>%
  unique()

minorityelec
# TODO: Out of the races they actually contest in --- how often to they win?
# Divide over the rows where there's one [GROUP] running

# now let's limit it to races with only minority candidates
minorityelec_only <- data %>%
  group_by(fips_char, year, month) %>%
  filter(race_final != "caucasian") %>% # 1957 rows
  select(fips_char, year, month, race_final, winner) %>%
  filter(winner == "win") %>%
  group_by(race_final) %>%
  mutate(win_frequency = n()/nrow(.)) %>%
  select(race_final, win_frequency) %>%
  unique()

minorityelec_only

# For these two subpopulations, I want to figure out win rates conditional
# on what the combination is (ex. latino vs. black --- who wins?)

# Vote share for the strongest ethnic minority candidate minus the vote share
# for the strongest white candidate

# TODO:
# - Merge data with turnout data (Voting Eligible Population), check with
# state_fips_chars (MIT Election Lab?). Historical redistricting???
# - First pass: use vote turnout from dataset
# - Does voting for a minority candidate in election t, affect turnout in t+1?
# District, Year, 
# (Ethnic Margin -- The difference between the strongest minority candidate
# and the white candidate),
# Turnout (corresponds to t+1) 
# Ethnic Margin: # votes/total votes (vote share). Take difference btween two
# vote shares
# Do analysis for districts who have elections every two years and those who
# have four years.
# What percentage of relevant contests happen every four years? two years?
# dummy variable observing any effect of frequency of elections

# Read: Rocio Titiunik Cambridge Elements Regression Discontinuity.
# theory and how to apply their package. 
# A Practical Introduction to Regression Discontinuity Designs

# use tidycensus to figure out Voting Age Population by fips_char code
# Avoid using number of registered people