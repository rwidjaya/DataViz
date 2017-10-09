install.packages(c("maps", "mapdata"))
install.packages('packcircles')
install.packages('ggplot2', dependencies = TRUE)
library(tidyverse)
library(packcircles)
library(maps)
library(mapdata)
library(RColorBrewer)

hb_kag <- read_csv('h1b_kaggle.csv')

HB_cert <- hb_kag %>%
  filter(CASE_STATUS == "CERTIFIED")

HB_cert$prwage_cat <- cut(HB_cert$PREVAILING_WAGE, c(0, 25000, 50000, 100000, 1000000000), 
                       labels=c('low', 'middle', 'middle-high', 'high'))

hb16 <- subset(HB_cert, YEAR == "2016")

hb16$region <- (str_split_fixed(hb16$WORKSITE, ", ", 2))[, 2]
hb16$region <- tolower(hb16$region)

# Bubble Chart

## Compiling the number of H1B applicants per company
com <- hb16 %>% 
  group_by(EMPLOYER_NAME) %>%
  summarise(TOTAL = n(), MEDIAN_PWAGE = median(PREVAILING_WAGE)) %>%
  arrange(desc(TOTAL)) %>%
  head(10)

com$SHORTHAND <- c("INFOSYS", "CAPGEMINI", "TATA CONSULTANCY", "WIPRO", "ACCENTURE", "IBM INDIA", 
                   "DELOITTE CONSULTING", "TECH MAHINDRA", "HCL", "MICROSOFT")

## Creating bubble chart:
##    - The size of each bubble signifies the relative size of applicants coming from the labeled company
##    - The color signifies the median prevailing wage of h1b applicants in the company, 
##        from the highest (Green) to the lowest (Red)
p <- circleProgressiveLayout(com$TOTAL)
d <- circleLayoutVertices(p)

h1_bubble <- ggplot(d, aes(x, y)) + 
  geom_polygon(aes(group = id, fill = d$MEDIAN_PWAGE)) +
  geom_text(data = p, aes(x, y, label = com$SHORTHAND), size = 3) +
  scale_fill_continuous(low = "red", high = "green", guide = "colorbar") +
  guides(fill=guide_legend(title="Median Wage")) +
  labs(title = "Top 10 Certified H1-B Sponsors & Median Wage 2016") + 
  theme_void() +
  theme(plot.title = element_text(family = 'Helvetica', hjust = 0.5, vjust = -0.5, face = "bold", size = 15)) +
  coord_fixed(1)



# Map Chart

## Data Prep: Using only US mainland data (excluding Alaska) to simplify the current graph
hb16_us_main <- hb16 %>%
  filter(lat < 50.00 & 
           lat > 24.00 & 
           lon > (-124) &
           lon < (-67))

## Creating map chart
##    - Map is used to compare the median prevailing income of H1B applicants based on their work state,
##      the wage is displayed from the highest (GREEN) to the lowest (grey) is the context of US mainland Map.
usa <- map_data("usa")
states <- map_data("state")

state_br <- hb16_us_main %>%
  group_by(region) %>%
  summarise(TOTAL = n(), MEDIAN_PWAGE = median(PREVAILING_WAGE))

ushb <- inner_join(states, state_br, by = "region")

cnames <- aggregate(cbind(long, lat) ~ MEDIAN_PWAGE, data=ushb, 
                    FUN=function(x)mean(range(x)))

us_state <- ggplot(data = states, mapping = aes(x = long, y = lat, group = group)) + 
  coord_fixed(1.3) + 
  geom_polygon(color = "black", fill = NA)

us_state_h1b_wage <- us_state + 
  geom_polygon(data = ushb, aes(fill = ushb$MEDIAN_PWAGE)) +
  geom_polygon(color = "black", fill = NA) +
  scale_fill_continuous(high = "green", low = "grey", guide="colorbar")  +
  labs(title = "Certified H1-B Application Median Wage per State 2016") + 
  geom_text(data = cnames, aes(long, lat, group = NULL, label = cnames$MEDIAN_PWAGE), size = 2) +
  guides(fill=FALSE) +
  theme(plot.title = element_text(family = 'Helvetica', hjust = 0.5, vjust = -0.5, face = "bold", size = 15))
  
### TO THE CHART
us_state_h1b_wage


# Line Chart

## Data Prep: Counting the amount of certified applicants from year 2011-2016
h1_tot <- HB_cert %>%
  group_by(YEAR, prwage_cat) %>%
  summarise(TOTAL = n())

## Line Chart: Understanding the prevailing wage class make-up and trends of H1B applicants
ggplot() + 
  geom_line(data = h1_med_tot, aes(x = YEAR, y = TOTAL, color = prwage_cat), alpha = 0.7) +
  xlab('YEAR') +
  ylab('Number of Certified Applicants') +
  labs(title = "H1B Certified Applicants per Wage Class",colour = "Applicant Wage Class") 



# BONUS: Line and Bar Chart Combo 

## A combo of bar chart and line on high and low income median wage level over the years
h1_med <- HB_cert %>%
  group_by(YEAR, prwage_cat) %>%
  summarise(MEDIAN_PWAGE = median(PREVAILING_WAGE)) %>%
  spread(prwage_cat, MEDIAN_PWAGE)

h1_med2 <- HB_cert %>%
  group_by(YEAR) %>%
  summarise(MEDIAN_PWAGE = median(PREVAILING_WAGE))

h1_med <- left_join(h1_med, h1_med2, by = c("YEAR", "YEAR"))


## Bar & Line Chart: Comparing the median income trend of low & high income 
## applicants in comparison to the overall median income
plot_base <- ggplot(h1_med, aes(YEAR, MEDIAN_PWAGE))

plot_bar <- plot_base +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_line(data = h1_med, aes(x = YEAR, y = low), color = "red", alpha = 0.7) +
  geom_line(data = h1_med, aes(x = YEAR, y = high), color = "blue", alpha = 0.7)
  
