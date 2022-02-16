#In this analysis we focus on the employment rate and 
#citation scores of different universities.
install.packages("ggplot2")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("RColorBrewer")
install.packages("stringr")
install.packages("dplyr")
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(stringr)
#reading a csv file from project directory
df_initial <- read.csv("cwurData.csv") 
#checking the structure of the csv along with the data types of attributes
str(df_initial)
summary(df_initial)

#creating a subset from the df_initial by dropping the broad_impact column
df_two <- select(df_initial, -broad_impact)
summary(df_two)
#checking the structure of the new data frame df_two
str(df_two)
head(df_two,5)
df_three <- select(df_two, institution, country, score, 
                   alumni_employment, citations, world_rank, year)
str(df_three)
head(df_three,10)
summarise(df_three, unique_univs <- n_distinct(institution))
summarise(df_three, unique_countries <- n_distinct(country))
summarise(df_three, unique_years <- n_distinct(year))
summary(df_three)

#graph1
ggplot(df_three, aes(x=alumni_employment, y=citations)) + geom_point()
# Change the point size, and shape
ggplot(df_three, aes(x=alumni_employment, y=citations)) +
  geom_point(aes(size=score), shape=21)

#graph2
options(repr.plot.width=15, repr.plot.height=5)
ggplot(df_three,aes(x=score))+geom_histogram(fill="royalblue")+facet_wrap(~year)

#graph3
options(repr.plot.width=10, repr.plot.height=5)
theme_b<-theme(axis.text.x = element_blank(),legend.position = "none")
df_three %>% select(world_rank,institution,year)%>%group_by(institution,year)%>%filter(world_rank<=20 & year ==2015)%>%arrange(world_rank)%>%ggplot(aes(x=institution,y=world_rank))+
geom_bar(stat="identity",fill="royalblue")+theme_b+labs(title="Top 20 Universities in World as on 2015")+geom_label(aes(label=institution),size=3)

#graph4
options(repr.plot.width=8, repr.plot.height=5)
topc<-df_three %>%select(world_rank,institution,country,year)%>% filter(year==2015 )%>%group_by(country)%>%summarise(university_count=n())
ggplot(topc,aes(x=country,y=university_count))+geom_bar(stat="identity",fill="royalblue")+theme(axis.text.x = element_text(angle=90))+geom_text(aes(label=university_count),vjust=-.4,size=2)+labs(title="Countrywise University Count")

#graph5
options(repr.plot.width=15, repr.plot.height=5)
df_three %>% select(institution,score,year)%>%filter(score>60)%>%ggplot(aes(x=institution,y=score,group=year,col=factor(year)))+geom_line()+theme(axis.text.x=element_text(angle=90))

#graph6
boxplot(df_three$alumni_employment,
        ylab = "alumni_employment"
)

#graph7
hist(df_three$alumni_employment,
     xlab = "alumni employment",
     main = "Histogram of alumni emplyment",
     breaks = sqrt(nrow(df_three)))

#graph8
boxplot(df_three$score,
        ylab = "score"
)

#graph9
hist(df_three$score, xlab = "score", main = "Histogram of score",breaks = sqrt(nrow(df_three)))


#displaying universities in canada and US
head(df_three %>% filter(country == c('Canada','India')) %>% arrange(alumni_employment), 15)

df_times <- read.csv("timesData.csv")
str(df_times)
#this data shows the rankings, female to male ratio, teaching score, income, number of students etc.
df_new_times <- select(df_times, -teaching, -research, -citations, -income, -student_staff_ratio)
str(df_new_times)
df_new_times
df_new_times$world_rank<-as.numeric(df_new_times$world_rank)
df_new_times$`international_students_percent`<-as.numeric(df_new_times$`international_students_percent`)
df_new_times$num_students<-as.numeric(df_new_times$num_students)
df_new_times$international <-as.numeric(df_new_times$international)
df_new_times$total_score<-as.numeric(df_new_times$total_score)
df_new_times
df_new_times %>% group_by(df_new_times$country) %>% 
  tally %>% arrange(desc(n))
str(df_new_times)

names(df_new_times)[names(df_new_times) == "international_students"] <- "international_students_percent"
head(df_new_times,10)
summary(df_new_times$num_students)
summary(df_new_times$`international_students_percent`)
summary(df_new_times)

install.packages("pastecs")
library(pastecs)
stat.desc(df_three)

