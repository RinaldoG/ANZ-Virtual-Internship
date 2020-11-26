library(readxl)
library(readr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(plotly)
library(maps)
library(sf)
library(tibble)
library(ozmaps)
library(rgeos)
library(ggspatial)
library(rnaturalearth)
library(rnaturalearthdata)
library(cowplot)
library(magick)

###Data import
df <- read_xlsx("ANZ.xlsx")

#Null value Check
colSums(is.na(df))

#Unnecessary column removal 
df <- df[,c(-3:-5,-8,-9,-19,-20,-21,-23)]

#NA Removals and variable reassignment
anz <- na.omit(df)
colSums(is.na(anz))

#Longitude and Latitude split
anz <- anz %>% separate(`long_lat`,into = c("long","lat"),
                        sep=" ")
anz$long <- as.numeric(anz$long)
anz$lat <- as.numeric(anz$lat)
anz <- anz %>% separate(`merchant_long_lat`,into = c("long2","lat2"),
                        sep=" ")
anz$long2 <- as.numeric(anz$long2)
anz$lat2 <- as.numeric(anz$lat2)

#Average transaction amount
x <- (round(mean(anz$amount),2))
paste0("$",(round(mean(anz$amount),2))," ~ ","average transaction amount")
# $40.15
anz %>% 
  group_by(week) %>% 
  summarise("Average Transaction Amount" = mean(amount))
anz %>% 
  group_by(month) %>% 
  summarise("Average Transaction Amount" = mean(amount))

#Average transactions per person (All Time)
round(mean(table(anz$first_name)))
table(anz$first_name)
# 96 Transactions 

#Date addition to Dataframe
anz$date <- as.Date(anz$date)
anz <- anz %>% mutate(week=strftime(anz$date,format="%V"))
anz <- anz %>% mutate(month=strftime(anz$date,format="%m"))

##General Summary
summary(anz[,c(7,14)])


#Weekly Transaction Plots
anz %>% 
  group_by(week) %>% 
  summarise("mean amount" = mean(amount)) %>%
  ggplot( aes(x=week, y=`mean amount`,group=1)) +
  geom_area(fill="#69b3a2",alpha=0.5) +
  geom_line(colour="#69b3a2") +
  ylab("Average Transaction Amount Weekly ($)")+
  xlab("Week")


#Weekly Transactions by Gender Plots
anz %>% 
  group_by(week,gender) %>%
  summarise(amount =mean(amount))%>%
  ggplot( aes(x=week, y=amount,group=gender,fill=gender,colour=gender)) +
  geom_line() +
  geom_point() +
  ylab("Average Transaction Amount Weekly ($)")+
  xlab("Week")

ggplotly(p,tooltip=list("week","amount"))

#Daily Transactions Plots
anz %>% 
  group_by(date) %>%
  summarise(amount =mean(amount)) %>%
  ggplot( aes(x=date, y=amount)) +
  geom_line(colour="#69b3a2") +
  geom_point(colour="#69b3a2") +
  ylab("Average Transaction Amount Daily ($)")+
  xlab("Date")

ggplotly(e)

#Daily Transactions by Gender Plots
anz %>% 
  group_by(date,gender) %>%
  summarise(amount =mean(amount))%>%
  ggplot( aes(x=date, y=amount,group=gender,fill=gender,colour=gender)) +
  geom_line()+
  ylab("Average Transaction Amount Daily ($)")+
  xlab("Date")

ggplotly(e,tooltip=list("week","amount"))

#Mean Age
anz %>% distinct(first_name,.keep_all=TRUE) %>% 
  summarise("Mean Age" = round(mean(age)))
# 32 Years Old

##Spatial Mapping
mode(anz$long)

world <- ne_countries(scale = "medium", returnclass = "sf")
world <- world %>% filter(sovereignt=="Australia")
# gene world map
ggplot(data = world) +
  geom_sf(fill="#69b3a2") +
  labs( x = "Longitude", y = "Latitude") +
  coord_sf(xlim = c(100.00, 160.00), ylim = c(-45.00, -10.00), expand = FALSE) +
  annotation_north_arrow(location = "bl", which_north = "true", 
                         pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
                         style = north_arrow_fancy_orienteering) +
  geom_point(data=anz,aes(x=long2, y=lat2),colour="#DD7524")+
  theme_bw()
ggplotly(g)

state <- data.frame(sort(table(anz$merchant_state),decreasing=TRUE))
state <- t(state)
colnames(state) <- c(state[1,])
state <- state[-1,]
paste0("Transactions Per State:",(sort(table(anz$merchant_state),decreasing=TRUE)))


x <- sort(table(anz$merchant_state),decreasing=TRUE)
print(x)


























































