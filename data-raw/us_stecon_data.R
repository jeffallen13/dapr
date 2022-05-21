library(fredr)
library(tidyverse)
library(stringr)
library(lubridate)

# Request Series --------------------------------------------------------

# You can't go above 120 requests per minute; split up
st.series <- c(
  paste0(state.abb, "UR"),
  paste0(state.abb, "POP"),
  paste0(state.abb, "RGSP"),
  paste0(state.abb, "PCPI"),
  paste0(state.abb, "HOWN")
)

# Unemployment
stur_df <- map_dfr(paste0(state.abb, "UR"), ~ fredr(
  series_id = .x,
  frequency = "a",
  observation_start = as.Date("2000-01-01")
))

# Population
stpop_df <- map_dfr(paste0(state.abb, "POP"), ~ fredr(
  series_id = .x,
  frequency = "a",
  observation_start = as.Date("2000-01-01")
))

# Real GDP
stgdp_df <- map_dfr(paste0(state.abb, "RGSP"), ~ fredr(
  series_id = .x,
  frequency = "a",
  observation_start = as.Date("2000-01-01")
))

# Per capita personal income
stpcpi_df <- map_dfr(paste0(state.abb, "PCPI"), ~ fredr(
  series_id = .x,
  frequency = "a",
  observation_start = as.Date("2000-01-01")
))

# Home ownership
sthown_df <- map_dfr(paste0(state.abb, "HOWN"), ~ fredr(
  series_id = .x,
  frequency = "a",
  observation_start = as.Date("2000-01-01")
))

# Education
steducation_df <- map_dfr(paste0("GCT1502", state.abb), ~ fredr(
  series_id = .x,
  frequency = "a",
  observation_start = as.Date("2000-01-01")
))

# Format education 
steducation_df <- steducation_df %>%
  mutate(year = lubridate::year(date),
         st = stringr::str_sub(series_id, 8),
         series = "ba_higher") %>%
  select(st, year, series, value)

# Row bind the other DFs
st_fred_df <- stur_df %>%
  bind_rows(stpop_df, stgdp_df, stpcpi_df, sthown_df)

# Interim formatting and combine 
stecon_df <- st_fred_df %>% 
  mutate(year = lubridate::year(date), 
         st = stringr::str_sub(series_id, 1, 2),
         series = stringr::str_sub(series_id, 3)) %>% 
  select(st, year, series, value) %>% 
  bind_rows(steducation_df)


# Final formatting and export 
stecon_out <- stecon_df %>% 
  filter(year > 2004 & year < 2022) %>% 
  mutate(series = tolower(series)) %>% 
  mutate(series = case_when(
    series == "ur" ~ "unrate", 
    series == "pop" ~ "population",
    series == "rgsp" ~ "rgdp", 
    TRUE ~ series
  ))

write.csv(stecon_out, "data-raw/state_econ.csv", row.names = FALSE)