# Chart 1: (Area Chart) H1-B Petition Approval Time Series

## DATA
h1b_gen <- read_csv('h1b_gen.csv')
h1b_gen <- plyr::rename(h1b_gen, c('Receipts' = 'applications', 'Approvals' = 'all_approvals', 'New Approval' = 'new_approvals'))

## PLOT
h1b_gen_approved <- ggplot(h1b_gen, aes(x=Year)) + 
  geom_area(aes(y=applications, fill="applications")) + 
  geom_area(aes(y=all_approvals, fill="all_approvals"), alpha = 0.7) +
  geom_area(aes(y=new_approvals, fill="new_approvals")) +
  labs(title="H1-B Petition 2007-2016", 
       subtitle="Breakdown of Approved Petition", 
       caption="Source: USCIS & US Department of State", 
       y="# of Petition") +
  theme_few() + 
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 9)) +
  scale_fill_manual(name= NULL,
        values=c("#3c0c59", "#b32062", "#83e2c3"),
        breaks=c("applications", "all_approvals", "new_approvals"),
        labels=c("Total","All Approved\n(Continuing + New)",  "New Approved"))


# Chart 2: (LINE CHART) H1-B Petition Beneficiary's Education Level 

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
h1b_edu_sub = h1b_edu[,c(doctorate, professional, masters, bachelors, some_college)]
h1b_edu_flat = melt(h1b_edu_sub, id=c("Year"))


## PLOT
h1b_edu_petition <- ggplot(h1b_edu_flat, aes(x=Year, y = value, color = variable)) +
  geom_line() + geom_point() +
  labs(title="H1-B Petition 2007-2016", 
       subtitle="Breakdown of Petition by Beneficiary Education", 
       caption="Source: USCIS", 
       y="# of Petition") +
  theme_few() +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 9)) +
  scale_color_discrete(name= "Degree Level",
                     breaks=c("doctorate","professional","masters","bachelors", "some_college"),
                     labels=c("Doctorate","Professional","Master's","Bachelor's","Less than a Bachelor's"))

#ggsave('h1b_line_edu.png', h1b_edu_petition)


# Chart 3: (CLUSTERED BAR) H1-B Petition Income Make Up 

## DATA
h1b_income <- read_csv('h1b_income.csv')

h1b_income <- h1b_income %>%
  filter(Year < 2017) %>%
  melt(id=c("Year"))

## PLOT
h1b_income_bar <- ggplot(data=h1b_income, aes(factor(Year), y = value, fill = forcats::fct_rev(variable))) +
  geom_bar(stat = 'identity', position="dodge") + 
  labs(title="H1-B Petition 2007-2016", 
       subtitle="Breakdown of Petition by Income Level", 
       caption="Source: USCIS", 
       y="# of Petition") +
  theme_few() + 
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 9)) +
  scale_fill_brewer(name= "Income Level", palette = "Spectral")

#ggsave('h1b_bar_income.png', h1b_income_bar )


# Chart 4: (BAR PANEL) Top 20 H1-B Sponsors of Approved Petitions Avg. Wage Comparison

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
h1b_wage_diff <- ggplot(h1b_comp, aes(x=employer, y=wage_diff, label=wage_diff)) + 
  geom_bar(stat='identity', aes(fill=wage_type), width=.5)  +
  scale_fill_manual(name= "Avg. Wage Offered",
                    labels = c("Above Average", "Below Average"), 
                    values = c("above"="#00ba38", "below"="#f8766d")) + 
  labs(title="H1-B Wage 2016", 
            subtitle="Top 20 H1B Sponsor Companies Avg. Wage Offer Compared to the Average Income of H1B Workers\n(Avg. 2016  H1B Wage = $89,590)", 
            caption="Source: USCIS", 
            y="$ Difference to the Avg. Income",
            x= "Top 20 H1-B Sponsorship Company\n[Top-Bottom] Highest to Lowest # of Workers Sponsored")+ 
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 9)) +
  coord_flip()

#ggsave('h1b_company_wage.png', h1b_wage_diff)


# Chart 5: (SLOPE Chart) Progression of Approved H1-B per Continent in the last 5 Years (2011-2016)

## DATA
h1b_continent <- read_csv('h1b_continent.csv')


## PLOT 
lft_label <- paste(h1b_continent$continent, round(h1b_continent$`Year2011`),sep=", ")
rgt_label <- paste(h1b_continent$continent, round(h1b_continent$`Year2016`),sep=", ")
h1b_continent$class <- ifelse((h1b_continent$`Year2016` - h1b_continent$`Year2011`) < 0, "red", "green")

h1b_continent_line <- ggplot(h1b_continent) + geom_segment(aes(x=1, xend=2, y=Year2011, yend=Year2016, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) + 
  labs(x="", y="# of H1-B Petitions Approved") + 
  xlim(.5, 2.5) + ylim(0,(0.8*(max(h1b_continent$Year2011, h1b_continent$Year2016)))) + 
  geom_text(label=lft_label, y=h1b_continent$Year2011, x=rep(1, NROW(h1b_continent)), hjust=1.1, size=3.5) + 
  geom_text(label=rgt_label, y=h1b_continent$Year2016, x=rep(2, NROW(h1b_continent)), hjust=-0.1, size=3.5) +
  geom_text(label="Time 1", x=1, y=0.8*(max(h1b_continent$Year2011, h1b_continent$Year2016)), hjust=1.2, size=5) +
  geom_text(label="Time 2", x=2, y=0.8*(max(h1b_continent$Year2011, h1b_continent$Year2016)), hjust=-0.1, size=5)

h1b_continent_line + 
  labs(title="H1-B Approved Petition 2011 - 2016", 
       subtitle="Progression of Approved H1-B Visa in the Last 5 Years", 
       caption="Source: US Department of State") +
  theme_few() +
  scale_y_continuous(labels = scales::comma) +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 15),
        plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 10),
        plot.caption = element_text(hjust = 0.5, face = "italic", size = 9)) +
  theme(panel.background = element_blank(), 
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        plot.margin = unit(c(1,2,1,2), "cm"))

#ggsave('h1b_continent.png', h1b_continent_line)

