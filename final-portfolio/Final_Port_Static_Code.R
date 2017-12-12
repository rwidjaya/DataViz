## FINAL PORTFOLIO - STATIC 

### UCID  : 12149052 
### Name  : Regina Widjaya

library(stringr)
library(tidyverse)
library(packcircles)
library(maps)
library(mapdata)
library(RColorBrewer)
library(ggthemes)
library(scales)
library(fiftystater)
library(ggalt)
library(reshape2)
library(extrafont)
library(ggrepel)
library(rgdal)
library(sp) ## spatial data in R
library(sf)
library(maptools)
library(broom)
library(viridis)
library(raster)
library(plotly)

###
### First Chart: H1-B Petition Approval Breakdown (AREA CHART)
###

## DATA
h1b_gen <- read_csv('h1b_gen.csv')
h1b_gen_long <- gather(h1b_gen, case_status, perc_status, Denied:New_Approved, factor_key=TRUE)

## CODE
gg <- ggplot(h1b_gen_long, aes(x=Year,y=perc_status*100,group=case_status,fill=case_status)) + 
  geom_area(alpha = 1) +
  labs(title="Despite The Increasing Trend in H-1B Petitions, \nThe proportion of Approved Petitions Remains", 
       subtitle="Breakdown of Approved Petitions", 
       caption="Source: USCIS, H-1B 2007-2017 Trend Tables & \n US Dept of State, Nonimmigrant Visa Issuance by Nationality", 
       y="% of Petitions") +
  theme_few() + 
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        legend.position = "top",
        legend.text = element_text(family = 'Andale Mono', size = 10),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90")) +
  scale_fill_manual(name= NULL,
                    values=c("#dc605d", "#ffce3d", "#41b097"),
                    breaks=c("Denied", "Renew_Approved", "New_Approved"),
                    labels=c("Denied Petitions","Renewed Petitions",  "New Approved Petitions"))

ggsave(gg,file='./Picture/H-1B_Area_Graph.pdf')


###
### Second Chart: H1-B Petition Beneficiary's Education Level
###

## DATA
h1b_edu <- read_csv('h1b_edu.csv')
h1b_edu <- plyr::rename(h1b_edu, c('Doctorate Degree' = 'doctorate',
                                   'Professional Degree' = 'professional',
                                   "Master's Degree" = 'masters',
                                   "Bachelor's Degree" = 'bachelors',
                                   'Associates Degree' = 'associates',
                                   'One or More Years of College (No Degree)' = 'some_college2',
                                   'Some College Credit (Less than 1 year)' = 'some_college1',
                                   'High School Graduate' = 'highschool',
                                   'No Diploma' = 'no_diploma'))
h1b_edu <- mutate(h1b_edu, 
                  some_college = associates + 
                    no_diploma + 
                    highschool + 
                    some_college1 + 
                    some_college2)
h1b_edu_sub <- select(h1b_edu, c('Year', 'doctorate', 'masters', 'bachelors', 'professional','some_college'))
h1b_edu_flat = melt(h1b_edu_sub, id=c("Year"))

## PLOT

dd <- ggplot(h1b_edu_flat, aes(x=Year, y = value, color = variable)) +
  geom_line() + geom_point() +
  labs(title="H-1B Via Holders mostly holds Bachelor's Degree,\nbut Master's degree holder always come close second.", 
       subtitle="Visa holders without the minimum requirements of Bachelor's Degree \nare still around, but they are decreasing in numbers.", 
       caption="Source: USCIS, H-1B 2007-2017 Trend Tables", 
       y="# of Petition") +
  theme_few() +
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90"),
        legend.position = "top",
        legend.title = element_blank(),
        legend.text = element_text(family = 'Andale Mono', size = 10),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        axis.title.x = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8)) +
  scale_color_discrete(name= "Degree Level",
                       breaks=c("doctorate","professional","masters","bachelors", "some_college"), labels=c("Doctorate","Professional","Master's","Bachelor's","Some College & Below"))

ggsave('./Picture/2_Line_Chart_Education.pdf', width = 8, height = 5, units="in")


###
### Third Chart: Third Chart: H1-B Petition Income Make Up (BAR CHART)
###

## DATA
h1b_income <- read_csv('h1b_income.csv')
h1b_income <- h1b_income %>%
  filter(Year < 2017) %>%
  melt(id=c("Year"))

## PLOT

ii <- ggplot(data=h1b_income, aes(factor(Year), y = value, fill = forcats::fct_rev(variable))) +
  geom_bar(stat = 'identity', position="dodge") + 
  labs(title="H-1B visa holders earns steadily higher wage.", 
       subtitle="This trend might be caused by the incresingly lucrative demand \nfor H-1B workers in the technology industry", 
       caption="Source: USCIS, H-1B 2007-2017 Trend Tables", 
       y="# of Petition") +
  theme_few() + 
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = 'Andale Mono', size = 8),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        axis.title.x = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8)) +
  scale_fill_brewer(name= "Income Level", palette = "Spectral")

ggsave('./Picture/3_Bar_Chart_Income.pdf', width = 8, height = 5, units="in")

###
### Fourth Chart: Top 20 H1-B Sponsors of Approved Petition Wage Comparison (NEG BAR CHART)
###

## DATA
h1b_comp <- read_csv('h1b_company.csv')
h1b_comp <- plyr::rename(h1b_comp, c('Employer Name' = 'employer',
                                     'Total Number of Approved Petitions' = 'num_approved',
                                     "Average Salary ($)" = 'avg_salary',
                                     "Wage Difference" = 'wage_diff'))
h1b_comp$wage_type <- ifelse(h1b_comp$wage_diff < 0, "below", "above") 
h1b_comp <- h1b_comp %>% arrange(num_approved)
h1b_comp$employer <- factor(h1b_comp$employer, levels = h1b_comp$employer)

## PLOT
ww <- ggplot(h1b_comp, aes(x=employer, y=wage_diff, label=wage_diff)) + 
  geom_bar(stat='identity', aes(fill=wage_type), width=.5)  +
  scale_fill_manual(name= "Avg. Wage Offered",
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  guides(fill=FALSE) +
  labs(title="Offshore Outsourcing Companies Tends to \nPay Lower Wages.", 
       subtitle="Top 20 H1-B Sponsors Avg. Wage Offer Compared to the \nAverage Income of H1B Workers (Avg. 2016  H1B Wage = $89,590)", 
       caption="Source: USCIS, H-1B Disclosure Data 2016", 
       y="$ Difference to the Avg. Income",
       x= "Top 20 H1-B Sponsorship Company\n[Top-Bottom] Highest to Lowest # of Workers Sponsored")+ 
  theme_few() + 
  scale_y_continuous(labels = scales::comma) +
  theme(panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(family = 'Andale Mono', size = 10),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        axis.title.x = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 8),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8)) +
  coord_flip()

ggsave('./Picture/4_Neg_Bar_Chart_IncomeDiff.pdf', width = 8, height = 6, units="in")


###
### Fifth Chart: 5-Year Change in H1-B Approved Petition (SLOPE CHART)
###

## DATA
h1b_continent <- read_csv('h1b_continent.csv')

## PLOT
lft_label <- paste(h1b_continent$continent, round(h1b_continent$`Year2011`),sep=", ")
rgt_label <- paste(h1b_continent$continent, round(h1b_continent$`Year2016`),sep=", ")
h1b_continent$class <- ifelse((h1b_continent$`Year2016` - h1b_continent$`Year2011`) < 0, "red", "green")

cc <- ggplot(h1b_continent) + geom_segment(aes(x=1, xend=2, y=Year2011, yend=Year2016, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) + 
  labs(x="", y="# of H1-B Petitions Approved") + 
  xlim(.5, 2.5) + ylim(0,(0.8*(max(h1b_continent$Year2011, h1b_continent$Year2016)))) + 
  geom_text_repel(label=lft_label, y=h1b_continent$Year2011, x=rep(1, NROW(h1b_continent)), hjust=2, size=3) + 
  geom_text_repel(label=rgt_label, y=h1b_continent$Year2016, x=rep(2, NROW(h1b_continent)), hjust=-1, size=3) +
  geom_text(label="Time 1", x=1, y=0.8*(max(h1b_continent$Year2011, h1b_continent$Year2016)), hjust=1.5, size=5) +
  geom_text(label="Time 2", x=2, y=0.8*(max(h1b_continent$Year2011, h1b_continent$Year2016)), hjust=-0.5, size=5)

cc + 
  labs(title="Significant increase in \nAsian H-1B Workers", 
       subtitle="Change in the # of Approved H1-B Visa \nin the Last 5 Years", 
       caption="Source: US Dept of State, \nNonimmigrant Visa Issuance by Nationality") +
  theme_few() +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        axis.title.x = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_text(family = 'Courier New', face = 'bold', size = 8),
        plot.margin = unit(c(1,2,1,2), "cm"))

ggsave('./Picture/5_Slope_Chart_Continent.pdf', width = 8, height = 8, units="in")


###
### Sixth Chart: Difference in State Avg Wage and H-1B Wage (Barbell Chart)
###

## DATA
hb_kag <- read_csv('h1b_kaggle.csv')
HB_cert <- hb_kag %>%
  filter(CASE_STATUS == "CERTIFIED")
HB_cert$prwage_cat <- cut(HB_cert$PREVAILING_WAGE, c(0, 25000, 50000, 100000, 1000000000), 
                          labels=c('low', 'middle', 'middle-high', 'high'))
hb16 <- subset(HB_cert, YEAR == "2016")
hb16$region <- (str_split_fixed(hb16$WORKSITE, ", ", 2))[, 2]
hb16$region <- tolower(hb16$region)

tot_emp <- read_csv('tot_emp.csv')
tot_emp <- tot_emp[!(tot_emp$state == 'Puerto Rico'| tot_emp$state == 'Guam' | tot_emp$state == 'Virgin Islands'), ]
tot_emp$state <- tolower(tot_emp$state)

h1b_state <- hb16 %>%
  group_by(region) %>%
  summarise(median_wage = median(PREVAILING_WAGE), total_h1b = n())

h1b_state <- h1b_state[!(h1b_state$region == 'na'| h1b_state$region == 'puerto rico'), ] %>%
  plyr::rename(c('region' = 'state'))

h1b_state <- left_join(h1b_state, tot_emp, by = c('state', 'state')) 

h1b_state <- h1b_state %>% 
  mutate(h1b_1000 = round(((total_h1b/TOT_EMP)*1000), 1), wage_diff = (median_wage - MED_WAGE))

h1b_state <- arrange(h1b_state, h1b_state$wage_diff)
h1b_state$state <- factor(h1b_state$state, levels=as.character(h1b_state$state)) 

tech_states_2 = c('california', 'washington', 'massachusetts')

## PLOT
bb <- ggplot(h1b_state, aes(x=h1b_state$MED_WAGE, xend=median_wage, y=state,group = state)) + 
  geom_dumbbell(colour ="#dbdbdb", 
                size=0.75, 
                colour_x = "#999999",
                colour_xend ="#ffce3d",
                size_xend = 1.5)+
  geom_text(aes(label = scales::dollar(round(wage_diff)), family = "Andale Mono"), 
            hjust = -2, size = 2.5)+
  scale_x_continuous(label= dollar) +
  labs(x="# of H1-B Visa Approved", 
       y="State",
       title="How much more are H1-B workers get paid in \nevery state?", 
       subtitle="The H1-B workers generally receive much \nbetter payment than state median. \nTechnology 'hot-spot's likely to display larger gap.", 
       caption="Source: U.S. Department of Labor Statistics & Department of State 2016") +
  theme(plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor.y=element_line(color="gray90"),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        panel.grid.minor.x=element_line(color="gray90"),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8))

ggsave('./Picture/6_Barbell_Chart_StateDiff.pdf', width = 6, height = 8, units="in")


###
### Sevent Chart: H-1B State Density (Facet Wrap Chart)
###

# DATA
h1b_facet <- h1b_state[,c('state', 'total_h1b', 'h1b_1000')]
colnames(h1b_facet) = c('state','H1-B Approved', '# of H1-B Visa Approved per 1000 Worker') 
h1b_facet <- arrange(h1b_facet, h1b_facet$`H1-B Approved`)
h1b_facet$state <- factor(h1b_facet$state, levels=as.character(h1b_facet$state)) 
h1b_facet_long <- melt(h1b_facet)

tech_states <- c("california", "new jersey")

## PLOT
ff <- ggplot(h1b_facet_long, aes(y=value, x= state)) + 
  facet_wrap(~variable, scales="free") +
  geom_bar(stat="identity", aes(fill = (state %in% tech_states))) +
  geom_text(aes(label = scales::comma(round(value,1)), family = "Andale Mono"), 
            hjust = 0, size = 2.5,
            position = position_dodge(width = 0.9)) +
  scale_fill_manual(values = c('#999999', '#ffce3d'))+
  facet_grid(~variable, scales ="free_x") +
  coord_flip() +
  theme_few() +
  labs(title="Which state hosts the most H1-B workers?", 
       subtitle="California hosts the majority of H1-B workers, \nbut New Jersey is more likely to employ H1-B workers.", 
       caption="Source: U.S. Department of Labor Statistics & Department of State 2016",
       y = "State")+
  guides(fill=FALSE) +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        strip.text.x = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 9),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90"))

ggsave('./Picture/7_Facet_Wrap_StateDense.pdf', width = 10, height = 8, units="in")


###
### Eight & Ninth Chart: H-1B State Density (Chloropleth MAPS)
###

##
## DATA
##

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Read the shapefiles into R
states <- readOGR(dsn =path.expand("./DATA/ne_50m_admin_1_states_provinces_lakes"),
                  layer = "ne_50m_admin_1_states_provinces_lakes")

## SpatialPolygonsDataFrame
class(states)

## Return CRS information from spatial data object:
proj4string(states)


# Let's transform them into tidy data that ggplot can use
states.points <- tidy(states, region = "adm1_code")

# And join in the original variables from the shapefile
states.df <- left_join(states.points, states@data, by = c("id" = "adm1_code"))

# Using those variables, we'll filter so we just have US states
states.df <- filter(states.df, iso_a2 == "US")

## Cleaning Data
HB_cert <- hb_kag %>%
  filter(CASE_STATUS == "CERTIFIED")
HB_cert$prwage_cat <- cut(HB_cert$PREVAILING_WAGE, c(0, 25000, 50000, 100000, 1000000000), 
                          labels=c('low', 'middle', 'middle-high', 'high'))
hb16 <- subset(HB_cert, YEAR == "2016")
hb16$region <- (stringr::str_split_fixed(hb16$WORKSITE, ", ", 2))[, 2]
hb16$region <- tolower(hb16$region)

# Cleaning State Employment Data
tot_emp <- read_csv('tot_emp.csv')
tot_emp <- tot_emp[!(tot_emp$state == 'Puerto Rico'| tot_emp$state == 'Guam' | tot_emp$state == 'Virgin Islands'), ]
tot_emp <- plyr::rename(tot_emp, c('state' = 'name'))

# Cleaning State Computational & Mathematics Employment Data
cm_oes <- read_csv('OES_Report.csv')
cm_oes$state <- (stringr::str_split_fixed(cm_oes$`Area Name`, "\\(", 2))[, 1]
cm_oes <- cm_oes[!(cm_oes$state == 'Puerto Rico'| cm_oes$state == 'Guam' | cm_oes$state == 'Virgin Islands'), ]
cm_oes <- plyr::rename(cm_oes, c('state' = 'name'))

# Final Data Manipulations
h1b_state <- hb16 %>%
  group_by(region) %>%
  summarise(median_wage = median(PREVAILING_WAGE), total_h1b = n(), pop_company = getmode(EMPLOYER_NAME))

h1b_state <- h1b_state[!(h1b_state$region == 'na'| h1b_state$region == 'puerto rico'), ] %>%
  plyr::rename(c('region' = 'name'))

h1b_state$name <- tools::toTitleCase(h1b_state$name)
h1b_state <- left_join(h1b_state, tot_emp, by = c('name', 'name')) 
h1b_state <- h1b_state %>% 
  mutate(h1b_1000 = round(((total_h1b/TOT_EMP)*1000), 1), wage_diff = (median_wage - MED_WAGE))
h1b_state <- mutate(h1b_state, perc_diff = wage_diff/MED_WAGE)
h1b_state <- arrange(h1b_state, h1b_state$wage_diff)

h1b_state <- left_join(states.df, h1b_state, by = 'name')
h1b_state <- left_join(h1b_state, cm_oes, by = 'name')
h1b_state <- mutate(h1b_state, perc_diff_CM = ((median_wage - h1b_state$'Annual median wage(2)')/h1b_state$'Annual median wage(2)'))

## PLOT: CHLOROPLETH 1

mm1 <- ggplot(h1b_state, aes(long, lat, group=group, fill= perc_diff)) + 
  #geom_text(aes(long, lat, group = NULL, label = h1b_state$h1b_1000), size = 2) +
  geom_polygon() +
  #geom_text(aes(long, lat, label = scales::percent(h1b_state$perc_diff)), color = "white") +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-124.85, -66.88), ylim = c(24.4, 49.38),
            orientation = c(90, 0, -98.35)) +
  labs(title="H-1B median wage is being paid \nMUCH MORE than host state median wage...", 
       subtitle="Percent Difference in Median Wage (Local State vs. H1-B)", 
       caption="Source: USCIS, H-1B Performance Data 2016", 
       y="Latitude",
       x= "Longitude") +
  guides(fill=guide_legend(title="--% Higher")) +
  scale_fill_viridis("perc_diff", labels= scales::percent, direction = -1) +
  geom_path(color="white") + 
  theme(legend.position = "bottom",
        axis.title.x= element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90")
  ) 

ggsave('./Picture/8_Chloropleth_Map_StateAvgComp.pdf', width = 8, height = 5, units="in")

## PLOT: CHLOROPLETH 2
mm2 <- ggplot(h1b_state, aes(long, lat, group=group, fill= perc_diff_CM)) + 
  #geom_text(aes(long, lat, group = NULL, label = h1b_state$h1b_1000), size = 2) +
  geom_polygon() +
  #geom_text(aes(long, lat, label = scales::percent(h1b_state$perc_diff)), color = "white") +
  coord_map("albers", lat0 = 29.5, lat1 = 45.5,
            xlim = c(-124.85, -66.88), ylim = c(24.4, 49.38),
            orientation = c(90, 0, -98.35)) +
  labs(title="However, H1-B wage are still less than \nmedian Computer & Mathematics wage...", 
       subtitle="Percent Difference in Median Wage (Local State vs. H1-B)", 
       caption="Source: USCIS, H-1B Performance Data 2016", 
       y="Latitude",
       x= "Longitude") +
  guides(fill=guide_legend(title="--% Higher")) +
  scale_fill_viridis("perc_diff", option = "magma", labels= scales::percent) +
  geom_path(color="white") + 
  theme(legend.position = "bottom",
        axis.title.x= element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        panel.background=element_rect(fill="#f7f7f7"),
        plot.background=element_rect(fill="#f7f7f7"),
        strip.background = element_rect(fill="#f7f7f7"),
        legend.background = element_rect(fill="#f7f7f7"),
        plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90")
  ) 

ggsave('./Picture/9_Chloropleth_Map_StateCMComp.pdf', width = 8, height = 5, units="in")
