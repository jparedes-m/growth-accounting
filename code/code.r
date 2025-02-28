# Growth accounting

# Names: Jessica Cairns, Nikolai Lawrence, Jorge Paredes, Sally Rafter

# Preamble ----
invisible(lapply(c("ggplot2", "dplyr", "readxl", "tidyverse"), library, character.only = TRUE))

setwd("C:/Users/jparedesm/Dropbox/BSE/second quarter/macro 1/growth accounting")

# Question a: Identify a period and a country of your interest for which you can get data.

# Spain, 2000-2010
no_sectors <- c("C10-C12", "C13-C15", "C16-C18", "C19", "C20", "C20-C21", "C21", "C22-C23", "C24-C25", "C26", "C26-C27", "C27", "C28", "C29-C30", "C31-C33", "D-E", "G45", "G46", "G47", "H49", "H50", "H51", "H52", "H53", "J58-J60", "J61", "J62-J63", "L68A", "M-N", "TOT_IND", "MARKTxAG", "O-Q", "Q86", "Q87-Q88", "R-S")

# Data ----
data <- read.csv("data/national accounts.csv") %>% select(-X) %>% 
  filter(geo_name == "Spain") %>% select(-geo_name, -geo_code) %>% 
  arrange(nace_r2_code, year) %>% 
  filter(year >= 2010 & year <= 2019) %>%
  filter(!nace_r2_code %in% no_sectors) %>% 
  mutate(Y_def = VA_PI/100, # Value added deflator
         Y_r = VA_CP*10^6/Y_def, # Real value added (in euros)
         y_r = Y_r /(EMP*10^3)) %>% # Real value added per worker (EMP counts for all the employed population not only employees) (eur/worker)
  rename(sector = nace_r2_code, sector_name = nace_r2_name) %>% 
  mutate(sector = case_when(sector == "TOT" ~ "Total Economy (TOT)", 
                            sector == "MARKT" ~ "Market Economy (MARKT)", 
                            TRUE ~ sector))

data_country <- data %>% filter(sector == "Total Economy (TOT)" | sector == "Market Economy (MARKT)") 

# Question b: Output per worker ----

data_country %>%  
  ggplot(aes(x = as.integer(year), y = y_r, col = sector)) + 
  geom_line() + 
  scale_x_continuous(breaks = seq(2010, 2019, by = 1)) +
  labs(title = "Output per worker in Spain, 2010-2019", 
       subtitle = "Real Value Added (2020 = 100)", 
       x = "Year", y = "Output per worker", colour = "Sector:") + 
  theme_light()+
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"), 
    legend.position = "bottom")

## Observations: Shouldn't be otherwise and Total Economy be higher than Market Economy?
## Observations 2: The main policies from 2010 until 2019 is still missing

# Question c: TFP decomposition ----
a = 1/3
# We gotta assemble a nice database so everything is tractable

## From the national accounts database we extract the following variables:
df <- data %>% select(year, sector, sector_name, Y_def, Y_r, y_r, H_EMP, COMP)

# We need the capital series (K), we get it from capital accounts database (Kq_GFCF: All assets, chained linked volumes (2020))

data_k <- read_excel("data/ES_capital accounts.xlsx", sheet = "Kq_GFCF") %>% 
  select(-var, -geo_code, -geo_name) %>%
  rename(sector = nace_r2_code, sector_name = nace_r2_name) %>% 
  filter(!sector %in% no_sectors) %>% 
  gather(year, K, -sector, -sector_name) %>% mutate(year = as.integer(year)) %>%
  relocate(year, sector, sector_name) %>%
  filter(year %in% 2010:2019) %>% 
  arrange(sector, year) %>% 
  mutate(sector = case_when(sector == "TOT" ~ "Total Economy (TOT)", 
                            sector == "MARKT" ~ "Market Economy (MARKT)", 
                            TRUE ~ sector),
         K = K * 10^6) # In euros, not in millions

# Do a left join to add the K column to the main dataframe
df <- left_join(df, data_k, by = c("year", "sector", "sector_name"))

# We need the human capital series (h), we get it from the labour accounts database (several calculations).

# Using equations from the slides (topic 3 - pages 23 to 29)
# A = y/[(K/Y)^a/(1-a) * h]

data_h_e <- read_excel("data/ES_labour accounts.xlsx", sheet = "Share_E") %>%
  select(-country) %>%
  rename(sector = code) %>% 
  filter(!sector %in% no_sectors) %>%
  gather(year, share_E, -sector, -education, -age, -gender) %>% 
  mutate(year = as.integer(year)) %>%
  filter(year %in% 2010:2019) %>%
  arrange(sector, year)

data_h_w <- read_excel("data/ES_labour accounts.xlsx", sheet = "Share_W") %>%
  select(-country) %>%
  rename(sector = code) %>% 
  filter(!sector %in% no_sectors) %>%
  gather(year, share_W, -sector, -education, -age, -gender) %>% 
  mutate(year = as.integer(year)) %>%
  filter(year %in% 2010:2019) %>%
  arrange(sector, year)

educ_years <- c("1" = 6, "2" = 12, "3" = 16)

data_h <- left_join(data_h_e, data_h_w, by = c("sector", "year", "education", "age", "gender"))%>%
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

# Do a left join to add the h column to the main dataframe
df <- left_join(df, data_h, by = c("year", "sector"))

# Now we can calculate the TFP
df <- df %>% 
  mutate(A_e = y_r/((K/Y_r)^(a/(1-a)) * h_e),
         A_w = y_r/((K/Y_r)^(a/(1-a)) * h_w))



View(df)
