install.packages(c("maps", "mapdata"))
install.packages('packcircles')
install.packages('ggplot2', dependencies = TRUE)
install.packages("ggthemes")
install.packages('fiftystater')
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
import_fonts()


## Cleaning Kaggle Data
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

## Barbel Chart 
bb <- ggplot(h1b_state, aes(x=h1b_state$MED_WAGE, xend=median_wage, y=state,group = state)) + 
  geom_dumbbell(colour ="#dbdbdb", 
                size=0.75, 
                colour_x = "#999999",
                colour_xend ="#ffce3d",
                size_xend = 1.5)+
  geom_text(aes(label = scales::dollar(round(wage_diff)), family = "Andale Mono"), 
            hjust = -1, size = 3)+
  scale_x_continuous(label= dollar) +
  labs(x="# of H1-B Visa Approved", 
       y="State\n(Top-Bottom : Widest - Narrowest gap between H1-B and state median wage",
       title="How much more are H1-B workers get paid in every state?", 
       subtitle="The H1-B workers generally receive much better payment than state median.\nTechnology 'hot-spot's likely to display larger gap.", 
       caption="Source: U.S. Department of Labor Statistics & Department of State") +
  theme(plot.title = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 15),
        plot.subtitle = element_text(family = 'Courier New', face = "italic", size = 10),
        plot.caption = element_text(family = 'Courier New', face = "italic", size = 9),
        plot.background=element_rect(fill="#f7f7f7"),
        panel.background=element_rect(fill="#f7f7f7"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank(),
        panel.grid.major.x=element_line(),
        panel.grid.minor.x=element_line(color="gray90"),
        panel.border=element_blank(),
        axis.ticks=element_blank(),
        axis.text.x = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8),
        axis.title.y = element_text(family = 'Courier New', face = 'bold', size = 10),
        axis.text.y = element_text(angle = 0, hjust = 1, family = 'Andale Mono', size = 8))

plot(bb)



# FACET WRAP 
h1b_facet <- h1b_state[,c('state', 'total_h1b', 'h1b_1000')]
colnames(h1b_facet) = c('state','H1-B Approved', '# of H1-B Visa Approved per 1000 Worker') 
h1b_facet <- arrange(h1b_facet, h1b_facet$`H1-B Approved`)
h1b_facet$state <- factor(h1b_facet$state, levels=as.character(h1b_facet$state)) 
h1b_facet_long <- melt(h1b_facet)

tech_states <- c("california", "new jersey")

ff <- ggplot(h1b_facet_long, aes(y=value, x= state)) + 
  facet_wrap(~variable, scales="free") +
  geom_bar(stat="identity", aes(fill = (state %in% tech_states))) +
  geom_text(aes(label = scales::comma(round(value,1)), family = "Andale Mono"), 
            hjust = 0, size = 3,
            position = position_dodge(width = 1)) +
  scale_fill_manual(values = c('#999999', '#ffce3d'))+
  facet_grid(~variable, scales ="free_x") +
  coord_flip() +
  theme_few() +
  labs(title="Which state hosts the most H1-B workers?", 
       subtitle="California hosts the majority of H1-B workers, \nbut New Jersey is more likely to employ H1-B workers.", 
       caption="Source: U.S. Department of Labor Statistics & Department of State",
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
        strip.text.x = element_text(family = 'Courier New', hjust = 0, face = "bold", size = 10),
        panel.grid.major.x=element_line(color="gray100"),
        panel.grid.minor.x=element_line(color="gray90"))

plot(ff)
