# writing a script to create the Economist Graphic
# based on 2011 data. 
# need to bring in two excel files
library(readxl)
library(ggplot2)
library(RCurl)
library(ggthemes)

# first is the 2011 Corruption Data index
setwd("/Users/paulbrennan/Dropbox/R for Biochemists/Namabia Stuff_Mar_2017/learnR_resources/Corruption & Human Development Index resources/CPI2011_DataPackage")
corInd <- read_excel("CPI2011_Results.xls")

# Bhutan is in the Excel File
countries <- corInd$country
c("Bhutan") %in% countries
# yes in this data too...

# check structure of the dataframe
str(corInd)
# first two columns show what we want 
# country 
# cpi11 - the cpi index. 
# extract just these two columns
corInd <- corInd[,1:2]

# second bit of data is 
setwd("/Users/paulbrennan/Dropbox/R for Biochemists/Namabia Stuff_Mar_2017/learnR_resources/Corruption & Human Development Index resources")
hdi <- read_excel("2015_Statistical_Annex_Table_2Trends in the Human Development Index.xls")
# look at the data
# need to skip the first 6 rows
hdi <- read_excel("2015_Statistical_Annex_Table_2Trends in the Human Development Index.xls",
                  skip = 6)
# check structure of the dataframe
str(hdi)
# second column has the country
# value for 2011 is in column six, I think...
# check column six:
hdi[,6]
# looks OK except 2011 is not in the column name so need to make that happen
hdi <- read_excel("2015_Statistical_Annex_Table_2Trends in the Human Development Index.xls",
                  skip = 6, col_names = TRUE)

str(hdi)
# column 2 with country names
# column 6 with 2011 data
# extract just these columns
hdi <- cbind(hdi[,2], hdi[,6])
# improve column names for merge
colnames(hdi) <- c("country", "hdi2011")

countries <- hdi$country
c("Bhutan") %in% countries




# now merge the two dataframes to create a new dataframe
# by country name
data <- merge(corInd[,1:2], hdi, by="country")
# interestingly just 161 observations. 
# still it's a good start. 

countries <- data$country
c("Bhutan") %in% countries



ggplot(data, 
       aes(x = cpi11,
           y = hdi2011)) +
       geom_point(shape = 1) +
  ylab("Human Development Index, 2011 (1=best)") +
  xlab("Corruption Perceptions Index, 2011 (10=least corrupt)") +
  ggtitle(paste("Corruption & human development")) +
  theme_bw()


# add continents (not same as graphic but good enough for now)
# Colour by continent inspired by Gapminder
# Data adapted from http://www.stat.ubc.ca/~jenny/notOcto/STAT545A/examples/gapminder/data/gapminderCountryColors.txt
x <- getURL("https://raw.githubusercontent.com/brennanpincardiff/RforBiochemists/master/data/countryColors20151026")
countryColors <- read.csv(text = x)
data <- merge(data, countryColors, by="country")
# lost a few more countries - just 133 now...

ggplot(data, 
       aes(x = cpi11,
           y = hdi2011,
           color = continent)) +
  geom_point(shape = 1) +
  ylab("Human Development Index, 2011 (1=best)") +
  xlab("Corruption Perceptions Index, 2011 (10=least corrupt") +
  ggtitle(paste("Corruption & human development ")) +
  theme_bw()
  
p <- ggplot(data, 
         aes(x = cpi11,
             y = hdi2011,
             color = continent)) +
  geom_point(shape = 1,         # hollow circles
             size = 2,          # size of circles
             stroke = 1.25)  +  # thickness of lines 
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     limits = c(1,10),
                     breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     limits = c(0.2, 1.0),
                     breaks = c(0.2, 0.3, 0.4, 0.5,
                                0.6, 0.7, 0.8, 0.9, 1)) 

p +  theme_tufte()
p + theme_economist()
p + theme_few()

# OK need to change to different regions...

p



# import a different set of regions:
setwd("/Users/paulbrennan/Dropbox/R for Biochemists/Namabia Stuff_Mar_2017/learnR_resources/Corruption & Human Development Index resources")

# in Excel, I cut and pasted into long form...
regions <- read_excel("worldBankRegions_rmduplic_long_20170317.xlsx", 
                      col_names = FALSE)

regions <- cbind(regions[,4], regions[,2])  
regions <- regions[1:210,]
colnames(regions) <- c("country", "region")
# combine regions with data
data <- merge(data, regions, by="country")



p <- ggplot(data, 
            aes(x = cpi11,
                y = hdi2011,
                color = region)) +
  geom_point(shape = 1,         # hollow circles
             size = 2,          # size of circles
             stroke = 1.25)  +  # thickness of lines 
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     limits = c(1,10),
                     breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     limits = c(0.2, 1.0),
                     breaks = c(0.2, 0.3, 0.4, 0.5,
                                0.6, 0.7, 0.8, 0.9, 1)) 

p +  theme_tufte()
p + theme_economist()
p + theme_few()

# generally this looks OK but I am pretty sure I am missing some data
# look to be missing Bhutan
# why?
# no missing Cape Verde, I think....
hdi

# need to control colors
### add line
### mix up the themes!


# doesn't look exactly the same despite using the same data..?!
# let's check it's really the correct data...


# add labels....
# label selected circles
# from: http://rforbiochemists.blogspot.co.uk/2015/10/exploring-un-data-on-researchers.html
wanted <-c("Congo", "Afghanistan", "Sudan", "Myanmar", 
            "Iraq", "India", "South Africa", "Rwanda", 
            "Bhutan", "Cape Verde", "Venezuela", "Russia", 
            "Argentina", "Greece", "Brazil", "Italy", "Spain", 
            "France", "United States", "Germany", "Norway", 
            "New Zealand", "Singapore", "Japan", "Britain", 
            "Barbados", "Botswana", "China")

i <- 1
labels <- NULL
for( i in 1:length(wanted)) { 
  labels<- rbind(labels, data[data$country == wanted[i], ])
}

p + geom_text(data=labels,
               aes(x = cpi11,
                   y = hdi2011,
                   label = country),
               colour = "darkgrey",
               size = 3, hjust=1, vjust=-0.5) +
  theme_bw()
# works but missing some labels - therefor missing countries. 
# also some need left justification, some right. 
# depends on how far from the line (predicted value)


p + geom_smooth(method = "nls", 
              method.args = list(formula = y ~ HDImax * x / (cpi50 + x), 
                                 start = list(HDImax = 1, cpi50 = 0.5)),
              se = F, size = 0.5, 
              data = data)
p

# fitting the line
ggplot(data, 
       aes(x = cpi11,
           y = hdi2011)) +
  geom_point(shape = 1) +
  ylab("Human Development Index, 2011 (1=best)") +
  xlab("Corruption Perceptions Index, 2011 (10=least corrupt)") +
  ggtitle(paste("Corruption & human development")) +
  theme_bw() +
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ HDImax * x / (cpi50 + x), 
                                 start = list(HDImax = 1, cpi50 = 0.5)),
              se = F, size = 0.5, 
              data = data)

# calculate r2 for non-linear graph - shouldn't really do this!
# but can show the significance...
chd_corr <- nls(formula = hdi2011 ~ HDImax * cpi11 / (cpi50 + cpi11),
                data = data[,2:3],
                start = list(HDImax = 1, cpi50 = 0.5))
summary(chd_corr)




# can we put colours by the regions and the line together?

p <- ggplot(data, 
            aes(x = cpi11,
                y = hdi2011)) +
  scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                     limits = c(1,10),
                     breaks = c(1,2,3,4,5,6,7,8,9,10)) + 
  scale_y_continuous(name = "Human Development Index, 2011 (1=best)",
                     limits = c(0.2, 1.0),
                     breaks = c(0.2, 0.3, 0.4, 0.5,
                                0.6, 0.7, 0.8, 0.9, 1)) +
  ggtitle(paste("Corruption & human development")) +
  geom_smooth(method = "nls", 
              method.args = list(formula = y ~ HDImax * x / (cpi50 + x), 
                                 start = list(HDImax = 1, cpi50 = 0.5)),
              se = F, size = 0.5, 
              data = data, 
              colour = "red")

p

# coloured points
p <- p + geom_point(aes(x = cpi11,
                      y = hdi2011,
                      color = region),
               shape = 1,         # hollow circles
             size = 2,          # size of circles
             stroke = 1.25)  # thickness of lines 
p

# labels
# add labels....
# label selected circles
# from: http://rforbiochemists.blogspot.co.uk/2015/10/exploring-un-data-on-researchers.html
wanted <-c("Congo", "Afghanistan", "Sudan", "Myanmar", 
           "Iraq", "India", "South Africa", "Rwanda", 
           "Bhutan", "Cape Verde", "Venezuela", "Russia", 
           "Argentina", "Greece", "Brazil", "Italy", "Spain", 
           "France", "United States", "Germany", "Norway", 
           "New Zealand", "Singapore", "Japan", "Britain", 
           "Barbados", "Botswana", "China")

i <- 1
labels <- NULL
for( i in 1:length(wanted)) { 
  labels<- rbind(labels, data[data$country == wanted[i], ])
}

p + geom_text(data=labels,
              aes(x = cpi11,
                  y = hdi2011,
                  label = country),
              colour = "darkgrey",
              size = 3, hjust=1, vjust=-0.5) +
  theme_bw()

# works but missing some labels - therefor missing countries. 
# also some need left justification, some right. 
# depends on how far from the line (predicted value)

p + facet_wrap(~continent)
