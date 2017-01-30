library(readr)
library(dplyr)

pop <- read_csv('/Users/Daniela/rProjects/TrumpVsClintonCountiesRpart/data/county_facts.csv')
results <- read_csv('/Users/Daniela/rProjects/TrumpVsClintonCountiesRpart/data/US_County_Level_Presidential_Results_12-16.csv')

votes <- results %>%  
  inner_join(pop, by = c("combined_fips" = "fips"))

votes <- votes %>% 
  filter(state_abbr != "AK") %>%
  mutate(ID = rank(X1)) %>% 
  select(-X1, 
         -POP010210, 
         -PST040210, 
         -NES010213, 
         -WTN220207) %>%
  rename(age18minus = AGE295214, 
         age5minus = AGE135214, 
         age65plus = AGE775214,
         american_indian = RHI325214, 
         asian = RHI425214, 
         asian_Firms = SBO215207,
         black = RHI225214, 
         black_firms = SBO315207, 
         building_permits = BPS030214,
         density = POP060210, 
         edu_batchelors = EDU685213, 
         edu_highschool = EDU635213,
         firms_num = SBO001207, 
         foreign = POP645213, 
         hisp_latin = RHI725214,
         hispanic_firms = SBO415207, 
         home_owners_rate = HSG445213, 
         households = HSD410213,
         housing_Units = HSG010214, 
         housing_units_multistruct = HSG096213, 
         income = INC910213,
         land_area = LND110210, 
         living_same_house_12m = POP715213, 
         manuf_ship = MAN450207,
         Med_house_income = INC110213, 
         Med_val_own_occup = HSG495213, 
         native_haw = RHI525214,
         native_firms = SBO115207, 
         nonenglish = POP815213, 
         pacific_isl_firms = SBO515207,
         pers_per_household = HSD310213, 
         pop_change = PST120214, 
         pop2014 = PST045214,
         poverty = PVY020213, 
         priv_nofarm_employ = BZA110213, 
         priv_nonfarm_employ_change = BZA115213,
         priv_nonfarm_estab = BZA010213, 
         retail_sales = RTN130207, 
         retail_sales_percap = RTN131207,
         sales_accomod_food = AFN120207, 
         sex_f = SEX255214, 
         travel_time_commute = LFE305213,
         two_races_plus = RHI625214, 
         veterans = VET605213, 
         white = RHI125214, 
         white_alone = RHI825214,
         women_firms = SBO015207,
         Trump = per_gop_2016, 
         Clinton = per_dem_2016, 
         Romney = per_gop_2012, 
         Obama = per_dem_2012) 

# response variable
votes <- votes %>% 
  mutate(pref_cand_T = factor(ifelse(Trump > Clinton, 1, 0)))
