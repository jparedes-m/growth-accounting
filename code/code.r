# Growth accounting

# Names: Jessica Cairns, Nikolai Lawrence, Jorge Paredes, Sally Rafter

# Preamble (Data conformation) ----
source("code/data conformation.r")

# Question b: Output per worker ----
data %>% 
  filter(sector == "Total Economy (TOT)" | sector == "Market Economy (MARKT)") %>%  
  ggplot(aes(x = as.integer(year), y = y_r, col = sector)) + 
  geom_line() + 
  scale_x_continuous(n.breaks = max(data$year) - min(data$year)+1) +
  labs(title = "Output per worker in Spain, 2010-2019", 
       subtitle = "Real Value Added (2020 = 100)", 
       x = "Year", y = "Output per worker", colour = "Sector:") + 
  theme_light()+
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"), 
    legend.position = "bottom",
    axis.text.x = element_text(size = 10, angle = 90))

## Observations: Shouldn't be otherwise and Total Economy be higher than Market Economy?
## Observations 2: The main policies from 2010 until 2019 is still missing

# Question c: TFP decomposition ----
a = 1/3

# Now we can calculate the TFP
df <- df %>% 
  mutate(A_e = y_r/((K/Y_r)^(a/(1-a)) * h_e),
         A_w = y_r/((K/Y_r)^(a/(1-a)) * h_w))



View(df)
