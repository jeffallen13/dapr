#---------------------------------------------------------------------------#
# Wrangling with Tidyverse
#---------------------------------------------------------------------------#

library(tidyverse)


# Importing and inspecting ------------------------------------------------

stecon_df <- read_csv("data-raw/state_econ.csv")

class(stecon_df)

ces_df <- readRDS("data-raw/cumulative_2006-2021.Rds")

class(ces_df)

load(file = "data-raw/1976-2020-president.RData")

class(x)

pres_df <- as_tibble(x)

rm(x)

stecon_df

summary(factor(stecon_df$series))

ces_df

pres_df


# Pipe interlude ----------------------------------------------------------

taxpayer_df <- tibble(
  taxpayer = LETTERS[1:10],
  income = round(rnorm(10, mean = 100, sd = 10), 2),
  tax = income * .30
)

taxpayer_df

high_earner_tax <- filter(taxpayer_df, income > 100)

high_earner_tax <- select(high_earner_tax, taxpayer, tax)

high_earner_tax

high_earner_tax <- taxpayer_df %>% 
  filter(income > 100) %>% 
  select(taxpayer, tax)

high_earner_tax


# Reshape --------------------------------------------------------------------

# The only wrangling imperative 
## Note, though, that when plotting, this is the form we want if want to plot
## multiple series for single state
stecon_wide <- stecon_df %>% 
  pivot_wider(names_from = series, values_from = value)

stecon_wide

# Common to see economic data with year
stur_year <- stecon_wide %>% 
  select(st:unrate) %>% 
  pivot_wider(names_from = year, values_from = unrate)

stur_year

stur_long <- stur_year %>% 
  pivot_longer(!st, names_to = "year", values_to = "unrate")

stur_long


# Data Frame Manipulation -------------------------------------------------

# Column selection 

ces_df %>% select(st, year)

ces_approval <- ces_df %>% 
  select(year:st, starts_with("approval_"), ideo5)

ces_approval

rm(ces_df) # to conserve memory

# Row filtering 

ces_approval <- ces_approval %>% filter(st != "DC")

## Below is unnecessary and gives rise to stupid name; just print examples

pres_ces <- pres_df %>% 
  filter(year >= 2004) %>% 
  select(state_po, year, candidate, candidatevotes, party_simplified)

pres_ces

rm(pres_df)

# Variable modification and creation

ces_approval

summary(ces_approval)

ces_approval$approval_pres[1:10]

ces_approval <- ces_approval %>% 
  mutate(approval_pres = haven::as_factor(approval_pres),
         approval_gov = haven::as_factor(approval_gov))

summary(ces_approval)

ces_approval <- ces_approval %>% 
  mutate(approval_pres_bin = 
           if_else(approval_pres == "Strongly Approve" | 
                    approval_pres == "Approve / Somewhat Approve", 1, 0))

ces_approval <- ces_approval %>% select(-approval_pres_bin)

ces_approval <- ces_approval %>% 
  mutate(across(starts_with("approval_"), 
                ~ if_else(.x == "Strongly Approve" | 
                           .x == "Approve / Somewhat Approve", 1, 0), 
                .names = "{.col}_bin"))

levels(ces_approval$ideo5)

ces_approval <- ces_approval %>% 
  mutate(ideo5 = as.integer(ideo5)) %>% 
  mutate(ideo5 = if_else(ideo5 == 6, NA_integer_, ideo5))

class(NA)

class(NA_real_)

class(NA_integer_)

class(NA_character_)

pres_ces <- pres_ces %>% mutate(state_po = as.character(state_po))

pres_ces

# Renaming variables

pres_ces <- pres_ces %>% rename(st = state_po)

# Sorting

stecon_wide %>% arrange(year, st)

stecon_wide %>% arrange(desc(year), desc(st))

stecon_wide %>% 
  filter(year == 2020) %>% 
  arrange(desc(rgdp)) %>% 
  select(st, rgdp)


# Grouping and summarizing part 1 (wrangling) -----------------------------

stecon_df <- stecon_wide %>% 
  group_by(st) %>% 
  mutate(across(c(population, rgdp, pcpi), ~ .x / dplyr::lag(.x) - 1, 
                .names = "{.col}_gr")) %>% 
  mutate(across(ends_with("_gr"), ~ round(.x * 100, 2))) %>% 
  ungroup()

stecon_df %>% 
  group_by(year) %>% 
  summarize(unrate_med = median(unrate))

ces_approval %>% 
  filter(st == "FL", year == 2020) %>% 
  summarize(approve_pres_uw = mean(approval_pres_bin, na.rm = TRUE),
            approve_pres_w = weighted.mean(approval_pres_bin,
                                           w = weight_cumulative,
                                           na.rm = TRUE)) %>% 
  mutate(approve_diff = approve_pres_w - approve_pres_uw) %>% 
  round(., 2)

ces_state <- ces_approval %>% 
  group_by(st, year) %>% 
  summarize(across(c(ends_with("_bin"), ideo5),
                   ~ weighted.mean(.x,
                                   w = weight_cumulative,
                                   na.rm = TRUE)),
            .groups = "drop") %>% 
  mutate(across(ends_with("_bin"), ~.x * 100))

ces_state

pres_win <- pres_ces %>%
  group_by(st, year) %>% 
  filter(candidatevotes == max(candidatevotes)) %>%
  ungroup()

state_color <- pres_win %>% 
  mutate(stcolor = if_else(party_simplified == "DEMOCRAT", "blue", "red")) %>% 
  mutate(year = if_else(year == 2004, as.integer(2006), year)) %>%
  select(st, year, stcolor)


# Joining -----------------------------------------------------------------

uspe_df <- ces_state %>% 
  left_join(stecon_df, by = c("st", "year")) %>% 
  left_join(state_color, by = c("st", "year"))

uspe_df <- uspe_df %>%
  group_by(st) %>% 
  fill(stcolor) %>% 
  mutate(stcolor = factor(stcolor)) %>% 
  ungroup()

# Additional data 
st_df <- tibble(
  state_abb = state.abb,
  name = state.name,
  area_sqm = state.area, 
  region = state.region
)

st_df <- st_df %>% 
  mutate(region = as.character(region)) %>% 
  mutate(region = if_else(region == "North Central", "Midwest", region)) %>% 
  mutate(region = as.factor(region))

# Inflation
cpi_df <- readRDS("data-raw/reg_cpi.rds")

inflation_df <- cpi_df %>% 
  group_by(region) %>% 
  mutate(inflation = (cpi / dplyr::lag(cpi) - 1) * 100) %>% 
  ungroup()

uspe_df <- uspe_df %>% 
  left_join(st_df, by = c("st" = "state_abb")) %>% 
  mutate(pop_density = population / (area_sqm / 1000)) %>%
  left_join(inflation_df, by = c("region", "year"))
  

# Finishing touches -------------------------------------------------------

uspe_df <- uspe_df %>% 
  arrange(st, year) %>% 
  rename(approve_pres = approval_pres_bin,
         approve_rep = approval_rep_bin,
         approve_sen1 = approval_sen1_bin,
         approve_sen2 = approval_sen2_bin,
         approve_gov = approval_gov_bin) %>% 
  select(name, st, year, region, stcolor, everything()) %>% 
  mutate(across(-c(name:stcolor), ~ round(.x, 2)))

uspe_df

# Save --------------------------------------------------------------------

saveRDS(uspe_df, file = "data/uspe.rds")

# Grouping and Summarizing Part 2 (analytical) ----------------------------

rm(list=ls())

uspe_df <- readRDS('data/uspe.rds')

summary(uspe_df)

# Regional growth over the last 10 years
uspe_df %>% 
  filter(year >= 2012) %>% 
  group_by(region) %>% 
  summarize(`Economic Growth` = mean(rgdp_gr), 
            `Population Growth` = mean(population_gr))

# Political support and ideology 2021
uspe_df %>% 
  filter(year == 2021) %>% 
  group_by(stcolor) %>% 
  summarize(`Presidential Approval` = mean(approve_pres), 
            `Political Ideology` = mean(ideo5))

# Political support and ideology 2020
uspe_df %>% 
  filter(year == 2020) %>% 
  group_by(stcolor) %>% 
  summarize(`Presidential Approval` = mean(approve_pres), 
            `Political Ideology` = mean(ideo5))

# Growth between red and blue last 10 years 
uspe_df %>% 
  filter(year >= 2012) %>% 
  group_by(stcolor) %>% 
  summarize(`Economic Growth` = mean(rgdp_gr), 
            `Population Growth` = mean(population_gr),
            `Population Density` = mean(pop_density))

# Sub-query (share of GDP)
uspe_df %>% 
  group_by(stcolor) %>% 
  summarize(`Real GDP` = sum(rgdp),
            `State Count` = n()) %>% 
  ungroup() %>% 
  mutate(`U.S. GDP` = sum(`Real GDP`), 
         `Share of GDP (%)` = round(`Real GDP` / `U.S. GDP` * 100, 2))
