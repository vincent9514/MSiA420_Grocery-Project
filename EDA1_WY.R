library(readr)
library(ggplot2)
library(lubridate)
library(dplyr)
setwd("~/Documents/Winter 2018/MSiA420 - Predictive Analytics II/project/grocery_data")

#loading
trainset<-read_csv("train_set.csv",col_names = TRUE)
testset<-read_csv("test_set.csv",col_names = TRUE)
trainset$label<-1
testset$label<-0
overall <- rbind(trainset, testset)

#oil
oil<- read.csv("oil.csv", header = T, stringsAsFactors = F)
oil$date<-as.Date(oil$date)
overall$date<-as.Date(overall$date)

#merged oil
mergeddata1<-merge(overall,oil,by.x = "date", by.y = "date",all.x = TRUE)

#merged holidays
holidays<-read.csv("holidays_events.csv",header = T, stringsAsFactors = F)
holidays$date<-as.Date(holidays$date)
mergeddata2<-merge(mergeddata1,holidays,by.x = "date", by.y = "date",all.x = TRUE)

#merged items
items<-read.csv("items.csv",header = T,stringsAsFactors = F)
head(items)
mergeddata3<-merge(mergeddata2,items,by.x = "item_nbr", by.y = "item_nbr",all.x = TRUE)

#merged stores
stores<-read.csv("stores.csv",header = T,stringsAsFactors = F)
head(stores)
mergeddata4<-merge(mergeddata3,stores,by.x = "store_nbr", by.y = "store_nbr",all.x = TRUE)

#merged transactions
#transactions<-read.csv("transactions.csv",header = T,stringsAsFactors = F)
#mergeddata5<-left_join(mergeddata4,transactions,by="store_nbr")
#data_final<-mergeddata5

# data cleaning and imputing

# checking NAs in data --> no NAs
sum(is.na(mergeddata4$family))
sum(is.na(mergeddata4$perishable))
sum(is.na(mergeddata4$city))
sum(is.na(mergeddata4$state))
sum(is.na(mergeddata4$type.y))
sum(is.na(mergeddata4$cluster))


# transforming family into factor variable
mergeddata4$family_factor <- as.factor(mergeddata4$family)
count_table <- table(mergeddata4$family_factor)
typeof(count_table)
length(count_table) 

# check the distribution of family_factor 
for (i in c(1:length(count_table))){
  count_table[i] = (count_table[i]/length(mergeddata4$family_factor))*100
}
count_table
 
# we should discuss later about how to merge factors
#other=0
#for (i in c(1:length(count_table))){
#  if (count_table[i] < 0.215859408){
#    other = other+count_table[i]
#  }
#}


# merge grocery I and grocery II as GROCERY
# merge HOME AND KITCHEN I and HOME AND KITCHEN II as KITCHEN
library(forcats)
mergeddata4$family_factor <- fct_collapse(mergeddata4$family_factor, GROCERY = c("GROCERY I","GROCERY II"), KITCHEN = c("HOME AND KITCHEN I","HOME AND KITCHEN II"))
levels(mergeddata4$family_factor)

# transforming type into factor variable
mergeddata4$type.y_factor <- as.factor(mergeddata4$type.y)
levels(mergeddata4$type.y_factor)

# cluster: a grouping of similar stores
summary(mergeddata4$cluster)






