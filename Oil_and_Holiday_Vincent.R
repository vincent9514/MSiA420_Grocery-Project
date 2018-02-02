library(lubridate)
library(ggplot2)
library(dplyr)

oil<- read.csv("oil.csv", header = T, stringsAsFactors = F)
View(oil)

oil$date<-as.Date(oil$date)
class(oil$date)

names(oil)[names(oil)=="dcoilwtico"]<-"oilprice"

par(mfrow=c(1,2))

p1 <- oil %>%
  ggplot(aes(date, oilprice)) +
  geom_line(color = "black") +
  geom_smooth(method = "loess", color = "red", span = 1/5)

p2 <- oil %>%
  mutate(lag7 = lag(oilprice,7)) %>%
  mutate(diff = oilprice - lag7) %>%
  filter(!is.na(diff)) %>%
  ggplot(aes(date, diff)) +
  geom_line(color = "black") +
  geom_smooth(method = "loess", color = "red", span = 1/5) +
  labs(y = "Weekly variations in Oil price")

#We find:

#There are strong, long-term changes in oil price with an obvious drop in the 
#second half of 2014. Overlayed on this long-term trend appear 
#to be fluctuations on time scales of weeks and months.

#The frequent downward trends are visible in the week-to-week variations. 
#The strong dips and rises in the lower plot might have a stronger influence 
#on buying behaviour than the long-term evolution.


holidays<-read.csv("holidays_events.csv",header = T, stringsAsFactors = F)
View(holidays)
holidays$date<-as.Date(holidays$date)

p1 <- holidays %>%
  ggplot(aes(type, fill = type)) +
  geom_bar() +
  theme(legend.position = "none")

p2 <- holidays %>%
  ggplot(aes(locale, fill = locale)) +
  geom_bar() +
  theme(legend.position = "none")

p3 <- holidays %>%
  group_by(description) %>%
  count() %>%
  arrange(desc(n)) %>%
  head(12) %>%
  ggplot(aes(description, n)) +
  geom_col(fill = "blue") +
  theme(legend.position = "none") +
  coord_flip() +
  labs(x = "description - most frequent", y = "Frequency")

p4 <- holidays %>%
  ggplot(aes(transferred, fill = transferred)) +
  geom_bar() +
  theme(legend.position = "none")



holidays %>%
  ggplot(aes(locale_name, fill = locale_name)) +
  geom_bar() +
  theme(legend.position = "none", axis.text.x  = element_text(angle=45, hjust=1, vjust=0.9))

#We find:

#Mosts special days are of the type “Holiday” and are either of the locale “Local” or “National”. Relatively few “Regional” holidays are present in our data.

#The large number of national holidays is emphasised in the second plot, which shows the locale_name of the event.

#The lower left plot lists a few of the most frequent holiday descriptions (i.e. their names). Carnival is clearly important.

#The majority of days off is not transferred.


#transferred features
#This transferred features works a little different from what you might assume at first. 
#It is not directly related to type == Transfer, but is assigned to the original holiday day prior to the transfer. This can be seen when we group by transferred and type:

holidays %>%
  count(transferred, type)

#What this means, is that a transferred holiday shows up twice in this data. 
#First on its original date, but with a transferred == TRUE flag. 
#This means that on this day there was no holiday. It was a (quasi) normal working day. 
#Instead, the moved holiday has a type == Transfer and a transferred == FALSE on the new date. 
#This is a non-working day now.

