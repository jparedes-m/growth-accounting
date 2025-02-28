# Growth accounting

# Names: Jessica Cairns, Nikolai Lawrence, Jorge Paredes, Sally Rafter

# Data conformation ----
source("code/data conformation.r")

data <- data %>% 
  filter(year %in% 2009:2019)

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
data <- data %>% 
  mutate(A_e = y_r/((K/Y_r)^(a/(1-a)) * h_e),
         A_w = y_r/((K/Y_r)^(a/(1-a)) * h_w)) %>% 
  group_by(sector) %>%
  mutate(gamma_y = 100*(log(y_r) - lag(log(y_r)))) %>%
  ungroup()

# Note: (written in google docs)
#For Capital: We are currently using the account called all the assets, this includes non-tangible assets (software, databases, R&D), it can be discussed that we can do the TFP decomposition only using tangible assets (fixed capital). 

#For Human Capital: We are currently using two ways to make a weighted average using the share of employed people and using the compensation share. (Elaborate more on this)

# Question d: 
sectors <- c('A', 'B', 'C', 'D-E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M-N', 'O-U')
data <- data %>% 
  filter(sector %in% sectors)

ggplot(data) +
 aes(x = year, y = A_e, size = gamma_y) +
 geom_line(colour = 'navyblue') +
 scale_x_continuous(n.breaks = max(data$year) - min(data$year)+1) +
 theme_light() +
 labs(title = "TFP Residual", subtitle = "Computed with Share_E",
      y = "TFP Residual", x = "Year", size = "Real Value Added Growth Rate (%):") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'bottom', legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10), 
        legend.key = element_rect(fill = "white", colour = "white"), 
        legend.box = "vertical",
        axis.text.x = element_text(size = 10, angle = 90))+
 facet_wrap(vars(sector))




ggplot(data) +
 aes(x = year, y = A_e) +
 geom_line(colour = "navyblue") +
 theme_light() +
 labs(title = "TFP Residual", subtitle = "Computed with Share_E",
      y = "TFP Residual", x = "Year") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'bottom', legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10), 
        legend.key = element_rect(fill = "white", colour = "white"), 
        legend.box = "vertical")+
 facet_wrap(vars(sector))


ggplot(data) +
 aes(x = year, y = gamma_y) +
 geom_line(colour = "navyblue") +
 theme_light() +
 labs(title = "Real Value Added Growth Rate", subtitle = "2020 = 100",
      y = "(%)", x = "Year") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'bottom', legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10), 
        legend.key = element_rect(fill = "white", colour = "white"), 
        legend.box = "vertical")+
 facet_wrap(vars(sector))






 ggplot(data) +
 aes(x = year, y = A_w, colour = sector, size = y_r) +
 geom_line() +
 scale_color_hue(direction = 1) +
 theme_light() +
 labs(title = "TFP Residual", subtitle = "Computed with Share_W",
      y = "TFP Residual", x = "Year",
      colour = "Sector:", size = "Real Value Added growth rate") +
  theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.subtitle = element_text(size = 14, face = "bold", hjust = 0.5),
        legend.position = 'bottom', legend.title = element_text(size = 12, face = "bold"),
        legend.text = element_text(size = 10), 
        legend.key = element_rect(fill = "white", colour = "white"), 
        legend.box = "vertical")+
 facet_wrap(vars(sector))

 # 

View(data)
