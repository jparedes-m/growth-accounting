# [0] Preamble ----
library(tidyverse)
library(readxl)
library(purrr)
library(ggplot2)
setwd("C:/Users/jparedesm/Dropbox/BSE/second quarter/macro 1/growth accounting")

# Identify compound or industries that are too specific
no_sectors <- c("C10-C12", "C13-C15", "C16-C18", "C19", "C20", "C20-C21", "C21",
    "C22-C23", "C24-C25", "C26", "C26-C27", "C27", "C28", "C29-C30", "C31-C33",
    "D-E", "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "J58-J60", "J61", "J62-J63",
    "L68A", "M-N", "TOT_IND", "MARKTxAG", "O-Q", "Q86", "Q87-Q88", "R-S")

# [1] National accounts database ----

# We want the following variables:
# - year
# - sector
# - sector_name
# - Y_def : VA_PI/100 (Value added deflator)
# - Y_r: VA_CP/Y_def (Real value added in euros)
# - L: (EMP) Number of persons employed
# - y_r: Y_r /(L) (Real value added per worker in euros)

# Beware that originally the deflator is in percentage points, so we need to divide by 100
# Also, the real value added is in millions of euros, so we need to multiply by 10^6
# The number of persons employed is in thousands, so we need to multiply by 10^3.

read_and_process_sheet <- function(sheet_name, var_name, scale) {
    read_excel("data/ES_national accounts.xlsx", sheet = sheet_name) %>%
        select(-var, -geo_code, -geo_name) %>%
        rename(sector = nace_r2_code, sector_name = nace_r2_name) %>%
        filter(!sector %in% no_sectors) %>%
        gather(year, value, -sector, -sector_name) %>%
        mutate(year = as.integer(year), !!var_name := value * scale) %>%
        select(-value) %>%
        select(year, sector, sector_name, !!var_name) %>% 
        arrange(sector, year)
}

df_ydef <- read_and_process_sheet("VA_PI", "Y_def", 1/100)
df_y <- read_and_process_sheet("VA_CP", "Y", 10^6)
df_L <- read_and_process_sheet("EMP", "L", 10^3)

df_NA <- df_ydef %>%
    left_join(df_y, by = c("year", "sector", "sector_name")) %>%
    left_join(df_L, by = c("year", "sector", "sector_name")) %>% 
    mutate(sector = case_when(sector == "TOT" ~ "Total Economy (TOT)", 
                            sector == "MARKT" ~ "Market Economy (MARKT)", 
                            TRUE ~ sector)) %>% 
    rename(Y_r = Y) %>% 
    mutate(Y_r = Y_r/Y_def, 
           y_r = Y_r/L)
rm(df_ydef, df_y, df_L, read_and_process_sheet)
# [2] Capital accounts database ----

# We want the following variables:
# - year
# - sector
# - K: (Kq_GFCF: All assets, chained linked volumes (2020))

df_k <- read_excel("data/ES_capital accounts.xlsx", sheet = "Kq_GFCF") %>% 
  select(-var, -geo_code, -geo_name) %>%
  rename(sector = nace_r2_code, sector_name = nace_r2_name) %>% 
  filter(!sector %in% no_sectors) %>% 
  gather(year, K, -sector, -sector_name) %>% mutate(year = as.integer(year)) %>%
  relocate(year, sector, sector_name) %>%
  arrange(sector, year) %>% 
  mutate(sector = case_when(sector == "TOT" ~ "Total Economy (TOT)", 
                            sector == "MARKT" ~ "Market Economy (MARKT)", 
                            TRUE ~ sector),
         K = K * 10^6) # In euros, not in millions of euros

# [3] Labour accounts database ----

# We want the following variables:
# - year
# - sector
# - share_E: Shares of employment by type (18 types) in total industry employment, in %
# - share_W: Shares of labour compensation by type (18 types) in total industry labour compensation, in %

df_h_e <- read_excel("data/ES_labour accounts.xlsx", sheet = "Share_E") %>%
  select(-country) %>%
  rename(sector = code) %>% 
  filter(!sector %in% no_sectors) %>%
  gather(year, share_E, -sector, -education, -age, -gender) %>% 
  mutate(year = as.integer(year)) %>%
  arrange(sector, year)

df_h_w <- read_excel("data/ES_labour accounts.xlsx", sheet = "Share_W") %>%
  select(-country) %>%
  rename(sector = code) %>% 
  filter(!sector %in% no_sectors) %>%
  gather(year, share_W, -sector, -education, -age, -gender) %>% 
  mutate(year = as.integer(year)) %>%
  arrange(sector, year)

educ_years <- c("1" = 6, "2" = 12, "3" = 16)

df_h <- left_join(df_h_e, df_h_w, by = c("sector", "year", "education", "age", "gender"))%>%
  mutate(share_E = share_E/100, share_W = share_W/100) %>%
  mutate(education = as.character(education),
         Years_Edu = educ_years[education]) %>%
  group_by(sector, year) %>%
  summarise(s_e = sum(share_E * Years_Edu, na.rm = TRUE),
            s_w = sum(share_W * Years_Edu, na.rm = TRUE),
            .groups = "drop") %>% 
  mutate(
    phi_s_e = case_when(
      s_e <= 4  ~ 0.134 * s_e,
      s_e > 4 & s_e <= 8  ~ 0.536 + 0.101 * (s_e - 4),
      s_e > 8  ~ 0.94 + 0.068 * (s_e - 8)),
    phi_s_w = case_when(
      s_w <= 4  ~ 0.134 * s_w,
      s_w > 4 & s_w <= 8  ~ 0.536 + 0.101 * (s_w - 4),
      s_w > 8  ~ 0.94 + 0.068 * (s_w - 8)),
    h_e = exp(phi_s_e),
    h_w = exp(phi_s_w)) %>% select(-phi_s_e, -phi_s_w) %>% 
    mutate(sector = case_when(sector == "TOT" ~ "Total Economy (TOT)", 
                            sector == "MARKT" ~ "Market Economy (MARKT)", 
                            TRUE ~ sector))

rm(df_h_e, df_h_w, educ_years)

# [4] Merge all databases ----
data <- df_NA %>% 
    left_join(df_k, by = c("year", "sector", "sector_name")) %>%
    left_join(df_h, by = c("year", "sector"))

rm(df_NA, df_k, df_h, no_sectors)
