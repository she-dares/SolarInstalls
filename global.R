#Author: Sheetal Darekar
#Date: January 25,2018
#Project 1 at NYCDSA due in third week
#global.R file



library(leaflet) 
library(ggplot2)
library(dplyr)
library(psych)
library(googleVis)
library(data.table)
library(DT)
library(tidyr)
library(tibble)
library(dygraphs)
library(WDI)
library(xts) #dygraph data
library(dplyr)
library(tidyverse)
library(rgdal) #load shp files
library(stringr)
library(plotly)

##############Yearly Sunlight################
data=read.csv("./Data/project-sunroof-censustract-09082017.csv",stringsAsFactors=FALSE)
data=data[is.na(data$yearly_sunlight_kwh_total)!=1,]
#data$latlong=paste(data$lat_avg, data$lng_avg,sep=":")

##############Coal Consumption by State################
coal=read.csv("./Data/Total_Coal.csv", header=TRUE )
coal=as.data.frame(t(coal)) #transpose data
colnames(coal)<-as.character(unlist(coal[1,]))  # remove line 1 / index
coal = coal[-1, ] #remove column 1
coal=rownames_to_column(coal, "date1")  #row names to columns
df2=coal %>% gather(.,key="States", value="CoalTons",2:52)
#grabbing year as required for animation slider
df2$date1=format(as.Date(paste(substr(df2$date1, 2, 5), "-", substr(df2$date1, 6, nchar(df2$date1)), "-01", sep=""),"%Y-%m-%d"),"%Y")
df2=df2[is.na(df2$CoalTons)!=1,]
#trying to normalize texas data / Try using log instead
#df2$CoalTons = ifelse( as.numeric(df2$CoalTons)>5000 & (df2$States=='US-TX' | df2$States=='US-PA' | df2$States=='US-KY' | df2$States=='US-WY' | df2$States=='US-IL' | df2$States=='US-IN' | df2$States=='US-OH' ),as.numeric(df2$CoalTons)/9, as.numeric(df2$CoalTons))

##############Natural Gas Consumption by State################
ng=read.csv("./Data/Total_natural_gas.csv", header=TRUE )
ng=as.data.frame(t(ng))  #transpose data
colnames(ng)<-as.character(unlist(ng[1,])) # remove line 1 / index
ng = ng[-1, ] #remove column 1
ng=rownames_to_column(ng, "date1") #row names to columns
df3=ng %>% gather(.,key="States", value="NGTons",2:52)
#grabbing year as required for animation slider
df3$date1=format(as.Date(paste(substr(df3$date1, 2, 5), "-", substr(df3$date1, 6, nchar(df3$date1)), "-01", sep=""),"%Y-%m-%d"),"%Y") #grabbing year as required
df3=df3[is.na(df3$NGTons)!=1,]
#df3$NGTons = ifelse( as.numeric(df3$NGTons)>15000 & (df3$States=='US-CA' | df3$States=='US-FL' | df3$States=='US-VT' ),as.numeric(df3$NGTons)/1000, as.numeric(df3$NGTons)) #trying to normalize texas data 

####################Leaflet Sunlight in USA#######################
USCounties = readOGR(dsn = "./cb_2016_us_county/cb_2016_us_county_500k.shp") #reading shp files tip: keep all files in same folder while uploading

CountyData = read_csv("./Data/project-sunroof-county-09082017.csv")
CountyData = as.data.frame(CountyData)
head(CountyData)

##############PV Cost Dygraph##############################
res<-read.csv("./Data/USAPVPrices.csv")


##################City Carbon Offset#######################
CityData=read.csv("./Data/project-sunroof-city-09082017.csv")

###################Pie Chart Data#######################
elec= read.csv("./Data/Electricity.csv")

