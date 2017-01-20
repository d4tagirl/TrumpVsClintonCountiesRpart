# Loading and preparing data
library(dplyr)

# read data
pop <- tbl_df(read.csv('data/county_facts.csv'))
results <- tbl_df(read.csv('data/US_County_Level_Presidential_Results_12-16.csv'))

# merge
votes <- results %>%
  inner_join(pop, by = c("combined_fips" = "fips"))

## Rename and Delete Variables
votes <- votes %>% 
  # remove Alaska because there is no data
  filter(state_abbr != "AK") %>% 
  # remove variables
  select(-POP010210, -PST040210, -NES010213, -WTN220207)  %>%
  rename(
    Age18minus	                    =	AGE295214,
    Age5minus	                      =	AGE135214,
    age65plus	                      =	AGE775214,
    American	                      =	RHI325214,
    Asian	                          =	RHI425214,
    Asian_Firms	                    =	SBO215207,
    Black	                          =	RHI225214,
    Black_firms	                    =	SBO315207,
    Building_permits    	          =	BPS030214,
    Density	                        =	POP060210,
    Edu_batchelors	                =	EDU685213,
    Edu_highschool	                =	EDU635213,
    Firms_number	                  =	SBO001207,
    Foreign	                        =	POP645213,
    Hispanic    	                  =	RHI725214,
    Hispanic_firms	                =	SBO415207,
    Home_owners_rate	              =	HSG445213,
    Households	                    =	HSD410213,
    Housing_Units	                  =	HSG010214,
    Housing_units_multistruct	      =	HSG096213,
    Income	                        =	INC910213,
    Land_area	                      =	LND110210,
    Living_same_house_12m	          =	POP715213,
    Manuf_shipments	                =	MAN450207,
    Median_house_income    	        =	INC110213,
    Median_value_owner_occupied    	=	HSG495213,
    Native	                        =	RHI525214,
    Native_firms                   	=	SBO115207,
    NonEnglish                     	=	POP815213,
    Pacific_island_firms	          =	SBO515207,
    Persons_per_household	          =	HSD310213,
    population_change	              =	PST120214,
    population2014	                =	PST045214,
    Poverty	                        =	PVY020213,
    Priv_nofarm_employ	            =	BZA110213,
    Private_nonfarm_employ	        =	BZA115213,
    Private_nonfarm_establishments	=	BZA010213,
    Retail_Sales                    =	RTN130207,
    Retail_sales_pc	                =	RTN131207,
    Sales_accomod_food              = AFN120207,
    Sex_F	                          =	SEX255214,
    Travel_time_commute	            =	LFE305213,
    Two_races	                      =	RHI625214,
    Veterans	                      =	VET605213,
    White	                          =	RHI125214,
    White2	                        =	RHI825214,
    Women_Firms	                    =	SBO015207,
    # change name for candidate
    Trump = per_gop_2016,
    Clinton = per_dem_2016,
    Romney = per_gop_2012,
    Obama = per_dem_2012) %>% 
  # create pref_cand variable
  mutate(pref_cand = ifelse(Trump >= Clinton, 'T', 'C')) %>% 
  mutate(pref_cand_T = as.factor(ifelse(Trump >= Clinton, 1, 0))) %>% 
  # create quintile variables by race
  mutate(white_q    = ntile(White2, 5),
         black_q    = ntile(Black, 5),
         asian_q    = ntile(Asian, 5),
         hispanic_q = ntile(Hispanic, 5)) %>% 
  # create quintile variables by education
  mutate(Edu_batch_q    = ntile(Edu_batchelors, 5)) %>% 
  # create quintile variables by Housing_units_multistruct
  mutate(urban_q    = ntile(Housing_units_multistruct, 5)) %>% 
  # create quintile variables by Income
  mutate(Income_q    = ntile(Income, 5)) %>% 
  # create ID
  mutate(ID    = rank(X)) %>% 
  # drop previous ID (X)
  select(-X)

save.image("base.Rdata")