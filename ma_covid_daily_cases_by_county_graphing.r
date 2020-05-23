rm(list = ls())
county_all_dates<-read.csv("daily_ma_covid_cases_by_county.csv")

#creates a simple df of new statewide positive tests
statewide<-county_all_dates %>%
  group_by(date) %>%
  summarise(new_cases = sum(new_cases))%>%
  mutate(new_cases_7d = rollmean(new_cases, k= 7, fill = NA))

#creates a simple df of county totals:
county_totals<-county_all_dates %>%
  group_by(county = as.factor(county)) %>%
  summarize(total_cases = sum(new_cases)) %>%
  filter(county != "Unknown")

county_totals$population<-as.integer(c(212990,124944,565217,17332,789034,70180,466372,160830,1611699,11399,706775,521202,803907,830622))
county_totals$per1000<-county_totals$total_cases/(county_totals$population/1000)
county_totals$county<-fct_reorder(county_totals$county, county_totals$total_cases)
county_totals$countyper<-fct_reorder(county_totals$county, county_totals$per1000)

#Faceted plot of each county's new confirmed cases curve
p1<-filter(county_all_dates, county != "Unknown") %>%
  ggplot(aes(x = date, y = new_cases))+
  geom_smooth(se = F)+
  facet_wrap(~county, scales = "free_y")+ 
  theme_hc()+
  theme(plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(size = 18),
        panel.background = element_blank(),
        strip.background = element_rect(colour="black", fill="gray95"),
        strip.text.x = element_text( size = 14),)+
  labs(title = "Positive COVID-19 Tests by Massachusetts County", subtitle = "Daily New Confirmed Cases",
       caption = "Source: MA Department of Public Health")+
  xlab("Date") + 
  ylab("New Cases")

jpeg('ma_county_test_curves.jpeg', width = 1000, height = 500, quality = 100, bg = "transparent")
p1
dev.off()


p2<-ggplot(county_totals, aes( county, total_cases))+
  geom_bar(stat = "identity", fill = "slategrey")+
  scale_y_continuous(expand = c(0,2), breaks = seq(0,15000, by = 5000),limit = c(0,20000))+
  coord_flip()+
  labs(title = "Positive COVID-19 Tests by Massachusetts County", subtitle = "Total Cases by County",
       caption = "Source: MA Department of Public Health")+
  xlab("Total Cases") + 
  ylab("County")+
  theme(plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(size = 18),
        axis.ticks.y= element_blank(),
        panel.background = element_blank(),
  )
jpeg('cases_by_county.jpeg', width = 1000, height = 500, quality = 100, bg = "transparent")
p2
dev.off()


p3<-ggplot(county_totals, aes( countyper, per1000))+
  geom_bar(stat = "identity", fill = "slategrey")+
  scale_y_continuous(expand = c(0,0), breaks = seq(0,16, by = 4),limit = c(0,21))+
  coord_flip()+
  labs(title = "Positive COVID-19 Tests by Massachusetts County", subtitle = "Total Cases by County (per 1000 residents)",
       caption = "Source: MA Department of Public Health")+
  ylab("Cases per 1000 Residents")+
  theme(plot.title = element_text(face = "bold", size = 24),
        plot.subtitle = element_text(size = 18),
        axis.ticks.y= element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(size = 10, face = "bold"),
        panel.background = element_blank(),
  )

jpeg('per_capita_cases_by_county.jpeg', width = 1000, height = 500, quality = 100, bg = "transparent")
p3
dev.off()

ggplot(statewide, aes(x = date, y = new_cases_7d))+
  geom_smooth(aes(x = date, y = total_cases), se = F)+
  geom_line()