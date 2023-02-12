##########################

# ESP 106
# Lab 3 (Monday) - graphing

##########################

#In this lab we will start by reading merging in data on economic development and indoor and outdoor air pollution. Then we will practice making some graphs with it.

setwd("/Volumes/KYRADRIVE/ESP106/lab/ESP106_week3_lab1")

#1. First read in the three csv files: gdppercapitaandgini and airpollution
airpol <- read.csv("airpollution.csv")
gdpcap <- read.csv("gdppercapiandgini.csv")


#Both datasets are from Our World in Data: ourworldindata.org
#The GDP dataset has GDP per capita and the GINI index (a measure of income inequality: https://en.wikipedia.org/wiki/Gini_coefficient)
#The air pollution dataset has death rates from indoor and outdoor air pollution - units are in deaths per 100,000 people
#Indoor air pollution is the Household Air Pollution from Solid Fules
#Outdoor air pollution is split into particulate matter and ozone

#Hint: The column names are long and cumbersome (because they contain information about units et) - you might want to rename some of the columns to make them easier to work with
namesair = c("entity","code","year","deaths.ambientpmp", "deaths.householdfuels","deaths.ambientozone","deaths.airpol")

colnames(airpol) <- namesair 


namesgdpcap = c("entity", "code","year","population","continent","ginicoef","gdppercap")

colnames(gdpcap) <- namesgdpcap




#2. Chose two countries that you are interested in and make a plot showing the death rates from indoor air pollution and outdoor air pollution (sum of particulate matter and ozone) over time
#Distinguish the countries using different colored lines and the types of pollution using different line types
#Make sure to add a legend and appropriate titles for the axes and plot 

#Hint: you can see all the different country names using unique(x$Entity) where x is the data frame containing the air pollution data
#Then create two new data frames that countain only the rows corresponding to each of the two countries you want to look at
#Create a new column of total outdoor air pollution deaths by summing death rates from particulate matter and ozone
#Use these to make your plot and add the lines you need

#Hint: you might have to set the y scale manually to make sure your plot is wide enough to show both countries. You can do this using the "ylim" argument in plot

bhind <-match(airpol$entity, "Bhutan") #returns 1 for values where the country is Bhutan
isnabh = !is.na(bhind)
bhutanair = airpol[isnabh, ]


bhind2 = !is.na((match(gdpcap$entity, "Bhutan")))
bhutapgdp = gdpcap[bhind2, ] #bhutan gdp extracted from all gdp

bhutan = merge(bhutanair,bhutapgdp) #merged data of bhutan

#repeating above steps for country 2 (moldova)
mdind<-(match(airpol$entity, "Moldova"))
isnamo = !is.na(mdind)
moldovaair = airpol[isnamo, ]


mdind2<-(match(gdpcap$entity, "Moldova"))
isnamo2 = !is.na(mdind2)
moldovagdp = gdpcap[isnamo2, ]
moldova = merge(moldovaair,moldovagdp)
  
yr.bhu = bhutan$year
in.bhu = bhutan$deaths.householdfuels
out.bhu = bhutan$deaths.ambientozone+ bhutan$deaths.ambientpmp

yr.mol = moldova$year
in.mol = moldova$deaths.householdfuels ;out.mol = moldova$deaths.ambientpmp+moldova$deaths.ambientozone
par(mar = c(5,6,4,4),lwd = 2)
plot(yr.bhu,in.bhu, type ="l", col = "blue",
       xlab = "Year", ylab = "Death Rate",
     main = "Death Rates from Indoor and Outdoor Pollution\n each Year for Bhutan and Moldova")
lines(yr.bhu,out.bhu,type ="l", col = "blue", lty = 3)
lines(yr.mol,in.mol,type="l",col="red")
lines(yr.mol,out.mol,type = "l",col = "red",lty = 3)

legend("topright",c("Bhutan Indoor","Bhutan Outdoor","Moldova Indoor","Moldova Outdoor"),
      col = c("red","red","blue","blue"), lty=c(1,3,1,3),lwd = 2 )
#3. Merge the air pollution data with the gdp data using merge()
# Merge is a function that combines data across two data frames by matching ID rows
#By default merge will identify ID rows as those where column names are the same between datasets, but it is safer to specify the columns you want to merge by yourself using "by"
#In our case, we want to merge both by country (either the "Entity" or "Code" columns) and year columns
#Note that by default, the merge function keeps only the entries that appear in both data frames - that is fine for this lab. If you need for other applications, you can change using the all.x or all.y arguments to the function - check out the documentation at ?merge

all = merge(gdpcap,airpol,by = c("entity", "year" ),no.dups = 1)


#4. Make a plot with two subplots - one showing a scatter plot between log of per-capita GDP (x axis) and indoor air pollution death rate (y axis) and one showing log of per-capita GDP (x axis) and outdoor air pollution (y axis)
#Make sure to add appropriate titles to the plots and axes
#Use ylim to keep the range of the y axis the same between the two plots - this makes it easier for the reader to compare across the two graphs
#STRECTH GOAL CHALLENGE - color the points based on continent. NOT REQUIRED FOR FULL POINTS - a challenge if you want to push yourself - continent info is included in the GDP dataset, but it is only listed for the year 2015
#If you are trying this and getting stuck ASK FOR HELP - there are some tips and tricks for making it easier 
par(mfrow = c(1,2),mar = c(4.5,4.5,3,5.5))
plot(log(all$gdppercap),all$deaths.householdfuels,
     main = "Indoor Pollution Deaths vs. Per Capita GDP", 
     ylab = "indoor pollution deaths", xlab= "log of GDP per Capita", col =rgb(0,0,0.8,0.2))
plot(log(all$gdppercap),all$deaths.ambientozone+all$deaths.ambientpmp, 
     main = "Outdoor Pollution Deaths vs. Per Capita GDP", 
     ylab = "outdoor pollution deaths", xlab= "log of GDP per Capita",col =rgb(0,0,0.8,0.2))

cont.in = all$continent != ""
continents = all$continent[cont.in]
#t15.in = all$year == 2015

z = all[cont.in ,c("continent","entity")]
all$continent = NULL
all = merge(all, z, by="entity")
colorm = cm.colors(6,alpha = 0.5)
f = as.factor(all$continent)
levels(f)
i = as.integer(f)
coli = colorm[i]
par(mfrow = c(1,2),mar = c(4.5,4.5,3,5.5))
plot(log(all$gdppercap),all$deaths.householdfuels,
     main = "Indoor Pollution Deaths\n vs. Per Capita GDP", 
     ylab = "indoor pollution deaths", xlab= "log of GDP per Capita", col =coli)
plot(log(all$gdppercap),all$deaths.ambientozone+all$deaths.ambientpmp, 
     main = "Outdoor Pollution Deaths\n vs. Per Capita GDP", 
     ylab = "outdoor pollution deaths", xlab= "log of GDP per Capita",col =coli)


