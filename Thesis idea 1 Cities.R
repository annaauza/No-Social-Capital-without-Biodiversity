#Thesis idea 1 CITIES

library(dplyr)
library(tidyr)
library(writexl)
library(admisc)
library(QCA)
library(NCA)
library(corrplot)
library(ggplot2)
library(gridExtra)


#Can jump to sections by SEARCH based on numbering by Dusa's (2022) book QCA with R
#SECTION 0 | DATA 
#SECTION 4 | CALIBRATION
#SECTION 5 | ANALYSIS OF NECESSITY
#SECTION 6 | ANALYSIS OF SUFFICIENCY          
#SECTION 7 | THE TRUTH TABLE                  not yet
#SECTION 8 | THE LOGICAL MINIMISATION         not yet
#SECTION 9 | PSEUDO-COUNTERFACTUAL ANALYSIS   not yet
#SECTION 10| NCA NECESSARY CONDITION ANALYSIS

#SECTION 11 | GRAPHS


##################################################################
#SECTION 0 | DATA

###
#Inserting chemicals in the column chem
###

d2006 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2006.CSV")
matching_indicesd2006 <- dataset$year == 2006 & dataset$city %in% d2006$CITY_NAME
matching_valuesd2006 <- match(dataset$city[matching_indicesd2006], d2006$CITY_NAME)
dataset$chem[matching_indicesd2006] <- d2006$MAX_AMOUNT_OF_CHEM[matching_valuesd2006]

d2007 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2007.CSV")
matching_indicesd2007 <- dataset$year == 2007 & dataset$city %in% d2007$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2007 <- match(dataset$city[matching_indicesd2007], d2007$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2007] <- d2007$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2007]

d2008 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2008.CSV")
matching_indicesd2008 <- dataset$year == 2008 & dataset$city %in% d2008$CITY_NAME
matching_valuesd2008 <- match(dataset$city[matching_indicesd2008], d2008$CITY_NAME)
dataset$chem[matching_indicesd2008] <- d2008$MAX_AMOUNT_OF_CHEM[matching_valuesd2008]

d2009 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2009.CSV")
matching_indicesd2009 <- dataset$year == 2009 & dataset$city %in% d2009$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2009 <- match(dataset$city[matching_indicesd2009], d2009$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2009] <- d2009$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2009]

d2010 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2010.CSV")
matching_indicesd2010 <- dataset$year == 2010 & dataset$city %in% d2010$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2010 <- match(dataset$city[matching_indicesd2010], d2010$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2010] <- d2010$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2010]

d2011 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2011.CSV")
matching_indicesd2011 <- dataset$year == 2011 & dataset$city %in% d2011$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2011 <- match(dataset$city[matching_indicesd2011], d2011$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2011] <- d2011$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2011]

d2012 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2012.CSV")
matching_indicesd2012 <- dataset$year == 2012 & dataset$city %in% d2012$CITY_NAME
matching_valuesd2012 <- match(dataset$city[matching_indicesd2012], d2012$CITY_NAME)
dataset$chem[matching_indicesd2012] <- d2012$MAX_AMOUNT_OF_CHEM[matching_valuesd2012]

d2013 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2013.CSV")
matching_indicesd2013 <- dataset$year == 2013 & dataset$city %in% d2013$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2013 <- match(dataset$city[matching_indicesd2013], d2013$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2013] <- d2013$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2013]

d2014 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2014.CSV")
matching_indicesd2014 <- dataset$year == 2014 & dataset$city %in% d2014$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2014 <- match(dataset$city[matching_indicesd2014], d2014$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2014] <- d2014$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2014]

d2015 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2015.CSV")
matching_indicesd2015 <- dataset$year == 2015 & dataset$city %in% d2015$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2015 <- match(dataset$city[matching_indicesd2015], d2015$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2015] <- d2015$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2015]

d2016 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2016.CSV")
matching_indicesd2016 <- dataset$year == 2016 & dataset$city %in% d2016$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2016 <- match(dataset$city[matching_indicesd2016], d2016$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2016] <- d2016$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2016]

d2017 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2017.CSV")
matching_indicesd2017 <- dataset$year == 2017 & dataset$city %in% d2017$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2017 <- match(dataset$city[matching_indicesd2017], d2017$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2017] <- d2017$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2017]

d2018 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2018.CSV")
matching_indicesd2018 <- dataset$year == 2018 & dataset$city %in% d2018$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2018 <- match(dataset$city[matching_indicesd2018], d2018$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2018] <- d2018$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2018]

d2019 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2019.CSV")
matching_indicesd2019 <- dataset$year == 2019 & dataset$city %in% d2019$CITY_NAME
matching_valuesd2019 <- match(dataset$city[matching_indicesd2019], d2019$CITY_NAME)
dataset$chem[matching_indicesd2019] <- d2019$MAX_AMOUNT_OF_CHEM[matching_valuesd2019]

d2020 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2020.CSV")
matching_indicesd2020 <- dataset$year == 2020 & dataset$city %in% d2020$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2020 <- match(dataset$city[matching_indicesd2020], d2020$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2020] <- d2020$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2020]

d2021 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\TRI data\\2021.CSV")
matching_indicesd2021 <- dataset$year == 2021 & dataset$city %in% d2021$V_TRI_FORM_R_EZ.CITY_NAME
matching_valuesd2021 <- match(dataset$city[matching_indicesd2021], d2021$V_TRI_FORM_R_EZ.CITY_NAME)
dataset$chem[matching_indicesd2021] <- d2021$V_TRI_FORM_R_EZ.MAX_AMOUNT_OF_CHEM[matching_valuesd2021]



###
#Inserting remote work data and work from home data in columns remote_work and work_from_home
###


#2006
r2006 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2006.csv")
r2006$city <- toupper(r2006$city)
matching_indicesr2006 <- dataset$year == 2006 & dataset$city %in% r2006$city
matching_valuesr2006 <- match(dataset$city[matching_indicesr2006], r2006$city)
dataset$remote_work[matching_indicesr2006] <- r2006$remote[matching_valuesr2006]

matching_indicesw2006 <- dataset$year == 2006 & dataset$city %in% r2006$city
matching_valuesw2006 <- match(dataset$city[matching_indicesw2006], r2006$city)
dataset$work_from_home[matching_indicesw2006] <- r2006$home[matching_valuesw2006]

#2007
r2007 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2007.csv")
r2007$city <- toupper(r2007$city)
matching_indicesr2007 <- dataset$year == 2007 & dataset$city %in% r2007$city
matching_valuesr2007 <- match(dataset$city[matching_indicesr2007], r2007$city)
dataset$remote_work[matching_indicesr2007] <- r2007$remote[matching_valuesr2007]

matching_indicesw2007 <- dataset$year == 2007 & dataset$city %in% r2007$city
matching_valuesw2007 <- match(dataset$city[matching_indicesw2007], r2007$city)
dataset$work_from_home[matching_indicesw2007] <- r2007$home[matching_valuesw2007]

#2008
r2008 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2008.csv")
r2008$city <- toupper(r2008$city)
matching_indicesr2008 <- dataset$year == 2008 & dataset$city %in% r2008$city
matching_valuesr2008 <- match(dataset$city[matching_indicesr2008], r2008$city)
dataset$remote_work[matching_indicesr2008] <- r2008$remote[matching_valuesr2008]

matching_indicesw2008 <- dataset$year == 2008 & dataset$city %in% r2008$city
matching_valuesw2008 <- match(dataset$city[matching_indicesw2008], r2008$city)
dataset$work_from_home[matching_indicesw2008] <- r2008$home[matching_valuesw2008]

#2009
r2009 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2009.csv")
r2009$city <- toupper(r2009$city)
matching_indicesr2009 <- dataset$year == 2009 & dataset$city %in% r2009$city
matching_valuesr2009 <- match(dataset$city[matching_indicesr2009], r2009$city)
dataset$remote_work[matching_indicesr2009] <- r2009$remote[matching_valuesr2009]

matching_indicesw2009 <- dataset$year == 2009 & dataset$city %in% r2009$city
matching_valuesw2009 <- match(dataset$city[matching_indicesw2009], r2009$city)
dataset$work_from_home[matching_indicesw2009] <- r2009$home[matching_valuesw2009]

#2010
r2010 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2010.csv")
r2010$city <- toupper(r2010$city)
matching_indicesr2010 <- dataset$year == 2010 & dataset$city %in% r2010$city
matching_valuesr2010 <- match(dataset$city[matching_indicesr2010], r2010$city)
dataset$remote_work[matching_indicesr2010] <- r2010$remote[matching_valuesr2010]

matching_indicesw2010 <- dataset$year == 2010 & dataset$city %in% r2010$city
matching_valuesw2010 <- match(dataset$city[matching_indicesw2010], r2010$city)
dataset$work_from_home[matching_indicesw2010] <- r2010$home[matching_valuesw2010]

#2011
r2011 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2011.csv")
r2011$city <- toupper(r2011$city)
matching_indicesr2011 <- dataset$year == 2011 & dataset$city %in% r2011$city
matching_valuesr2011 <- match(dataset$city[matching_indicesr2011], r2011$city)
dataset$remote_work[matching_indicesr2011] <- r2011$remote[matching_valuesr2011]

matching_indicesw2011 <- dataset$year == 2011 & dataset$city %in% r2011$city
matching_valuesw2011 <- match(dataset$city[matching_indicesw2011], r2011$city)
dataset$work_from_home[matching_indicesw2011] <- r2011$home[matching_valuesw2011]

#2012
r2012 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2012.csv")
r2012$city <- toupper(r2012$city)
matching_indicesr2012 <- dataset$year == 2012 & dataset$city %in% r2012$city
matching_valuesr2012 <- match(dataset$city[matching_indicesr2012], r2012$city)
dataset$remote_work[matching_indicesr2012] <- r2012$remote[matching_valuesr2012]

matching_indicesw2012 <- dataset$year == 2012 & dataset$city %in% r2012$city
matching_valuesw2012 <- match(dataset$city[matching_indicesw2012], r2012$city)
dataset$work_from_home[matching_indicesw2012] <- r2012$home[matching_valuesw2012]

#2013
r2013 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2013.csv")
r2013$city <- toupper(r2013$city)
matching_indicesr2013 <- dataset$year == 2013 & dataset$city %in% r2013$city
matching_valuesr2013 <- match(dataset$city[matching_indicesr2013], r2013$city)
dataset$remote_work[matching_indicesr2013] <- r2013$remote[matching_valuesr2013]

matching_indicesw2013 <- dataset$year == 2013 & dataset$city %in% r2013$city
matching_valuesw2013 <- match(dataset$city[matching_indicesw2013], r2013$city)
dataset$work_from_home[matching_indicesw2013] <- r2013$home[matching_valuesw2013]

#2014
r2014 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2014.csv")
r2014$city <- toupper(r2014$city)
matching_indicesr2014 <- dataset$year == 2014 & dataset$city %in% r2014$city
matching_valuesr2014 <- match(dataset$city[matching_indicesr2014], r2014$city)
dataset$remote_work[matching_indicesr2014] <- r2014$remote[matching_valuesr2014]

matching_indicesw2014 <- dataset$year == 2014 & dataset$city %in% r2014$city
matching_valuesw2014 <- match(dataset$city[matching_indicesw2014], r2014$city)
dataset$work_from_home[matching_indicesw2014] <- r2014$home[matching_valuesw2014]

#2015
r2015 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2015.csv")
r2015$city <- toupper(r2015$city)
matching_indicesr2015 <- dataset$year == 2015 & dataset$city %in% r2015$city
matching_valuesr2015 <- match(dataset$city[matching_indicesr2015], r2015$city)
dataset$remote_work[matching_indicesr2015] <- r2015$remote[matching_valuesr2015]

matching_indicesw2015 <- dataset$year == 2015 & dataset$city %in% r2015$city
matching_valuesw2015 <- match(dataset$city[matching_indicesw2015], r2015$city)
dataset$work_from_home[matching_indicesw2015] <- r2015$home[matching_valuesw2015]

#2016
r2016 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2016.csv")
r2016$city <- toupper(r2016$city)
matching_indicesr2016 <- dataset$year == 2016 & dataset$city %in% r2016$city
matching_valuesr2016 <- match(dataset$city[matching_indicesr2016], r2016$city)
dataset$remote_work[matching_indicesr2016] <- r2016$remote[matching_valuesr2016]

matching_indicesw2016 <- dataset$year == 2016 & dataset$city %in% r2016$city
matching_valuesw2016 <- match(dataset$city[matching_indicesw2016], r2016$city)
dataset$work_from_home[matching_indicesw2016] <- r2016$home[matching_valuesw2016]

#2017
r2017 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2017.csv")
r2017$city <- toupper(r2017$city)
matching_indicesr2017 <- dataset$year == 2017 & dataset$city %in% r2017$city
matching_valuesr2017 <- match(dataset$city[matching_indicesr2017], r2017$city)
dataset$remote_work[matching_indicesr2017] <- r2017$remote[matching_valuesr2017]

matching_indicesw2017 <- dataset$year == 2017 & dataset$city %in% r2017$city
matching_valuesw2017 <- match(dataset$city[matching_indicesw2017], r2017$city)
dataset$work_from_home[matching_indicesw2017] <- r2017$home[matching_valuesw2017]

#2018
r2018 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2018.csv")
r2018$city <- toupper(r2018$city)
matching_indicesr2018 <- dataset$year == 2018 & dataset$city %in% r2018$city
matching_valuesr2018 <- match(dataset$city[matching_indicesr2018], r2018$city)
dataset$remote_work[matching_indicesr2018] <- r2018$remote[matching_valuesr2018]

matching_indicesw2018 <- dataset$year == 2018 & dataset$city %in% r2018$city
matching_valuesw2018 <- match(dataset$city[matching_indicesw2018], r2018$city)
dataset$work_from_home[matching_indicesw2018] <- r2018$home[matching_valuesw2018]

#2019
r2019 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2019.csv")
r2019$city <- toupper(r2019$city)
matching_indicesr2019 <- dataset$year == 2019 & dataset$city %in% r2019$city
matching_valuesr2019 <- match(dataset$city[matching_indicesr2019], r2019$city)
dataset$remote_work[matching_indicesr2019] <- r2019$remote[matching_valuesr2019]

matching_indicesw2019 <- dataset$year == 2019 & dataset$city %in% r2019$city
matching_valuesw2019 <- match(dataset$city[matching_indicesw2019], r2019$city)
dataset$work_from_home[matching_indicesw2019] <- r2019$home[matching_valuesw2019]

#2020
r2020 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2020.csv")
r2020$city <- toupper(r2020$city)
matching_indicesr2020 <- dataset$year == 2020 & dataset$city %in% r2020$city
matching_valuesr2020 <- match(dataset$city[matching_indicesr2020], r2020$city)
dataset$remote_work[matching_indicesr2020] <- r2020$remote[matching_valuesr2020]

matching_indicesw2020 <- dataset$year == 2020 & dataset$city %in% r2020$city
matching_valuesw2020 <- match(dataset$city[matching_indicesw2020], r2020$city)
dataset$work_from_home[matching_indicesw2020] <- r2020$home[matching_valuesw2020]

#2021
r2021 <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\google trends data\\2021.csv")
r2021$city <- toupper(r2021$city)
matching_indicesr2021 <- dataset$year == 2021 & dataset$city %in% r2021$city
matching_valuesr2021 <- match(dataset$city[matching_indicesr2021], r2021$city)
dataset$remote_work[matching_indicesr2021] <- r2021$remote[matching_valuesr2021]

matching_indicesw2021 <- dataset$year == 2021 & dataset$city %in% r2021$city
matching_valuesw2021 <- match(dataset$city[matching_indicesw2021], r2021$city)
dataset$work_from_home[matching_indicesw2021] <- r2021$home[matching_valuesw2021]


###
#Changing inequality from ZIP to cities and inserting into the dataset
###

exposure <- read.csv("C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\social_capital_zip.csv")
merged_data <- merge(exposure, zip_citynames, by.x = "zip", by.y = "zip", all.x = TRUE)
colnames(merged_data)[which(names(merged_data) == "city.y")] <- "city"
merged_data <- merged_data[, c("zip", "city")]
exposure <- merge(exposure, merged_data, by = "zip", all.x = TRUE)

averaged_exposure <- aggregate(. ~ city, data = exposure, FUN = mean)
averaged_exposure$city <- toupper(averaged_exposure$city)

dataset <- merge(dataset, averaged_exposure[, !(names(averaged_exposure) %in% c("zip", "county"))], by = "city")

###
#Inserting protected areas with 10km / 25km / 50km buffers from the city into the dataset
###

library(dplyr)
##load dataset "area" 1st sheet
area$city <- toupper (area$city)
biod_dataset$biod10 <- biod_dataset$biod
biod_dataset <- merge(dataset, area, by = "city", all.x = TRUE)
biod_dataset$biod10 <- ifelse(is.na(biod_dataset$biod10), biod_dataset$area, biod_dataset$biod10)

    ###the same code, but for FRAME datframe
    area$city <- toupper (area$city)
    FRAME <- merge(FRAME, area, by = "city", all.x = TRUE)
    FRAME <- FRAME %>% rename(area10 = area)
    

##load dataset "area" 2nd sheet
area$city <- toupper (area$city)
biod_dataset$biod25 <- biod_dataset$biod25
biod_dataset <- merge(dataset, area, by = "city", all.x = TRUE)
biod_dataset$biod25 <- ifelse(is.na(biod_dataset$biod25), biod_dataset$area, biod_dataset$biod25)

##load dataset "area" 3rd sheet
area$city <- toupper (area$city)
biod_dataset$biod50 <- biod_dataset$biod50
biod_dataset <- merge(dataset, area, by = "city", all.x = TRUE)
biod_dataset$biod50 <- ifelse(is.na(biod_dataset$biod50), biod_dataset$area, biod_dataset$biod50)

write.csv(biod_dataset, "C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\biod_dataset.csv", row.names = FALSE)
write_xlsx(biod_dataset, "C:\\Users\\Anna Auza\\OneDrive\\Desktop\\1 Thesis data _ city\\biod_dataset.xlsx")
library(writexl)
biod_dataset <- biod_dataset %>%
  mutate(id = as.numeric(factor(city, levels = unique(city))))

###
#Removing NA and repetitive rows to prepare for calibration and calibrating
###

biod_dataset$city_year <- paste(biod_dataset$city, biod_dataset$year, sep = "_")
unique_biod_datasets <- unique(biod_dataset$city_year, biod_dataset$biod_dataset)
print(unique_biod_datasets)

#check if the chem column has the same values for repeating combinations of year and city
biod_dataset$chem <- as.numeric(biod_dataset$chem)
summary_chem <- aggregate(chem ~ city + year, data = biod_dataset, FUN = summary)
print(summary_chem)

#check for remote_work
biod_dataset$remote_work <- substring(biod_dataset$remote_work, 1, nchar(biod_dataset$remote_work) - 1)
summary(biod_dataset$remote_work)
biod_dataset$remote_work <- as.numeric(biod_dataset$remote_work)
summary_remote_work <- aggregate(remote_work ~ city + year, data = biod_dataset, FUN = summary)
print(summary_remote_work)                                      


#check for work_from_home
biod_dataset$work_from_home <- substring(biod_dataset$work_from_home, 1, nchar(biod_dataset$work_from_home) - 1)
summary(biod_dataset$work_from_home)
biod_dataset$work_from_home <- as.numeric(biod_dataset$work_from_home)
summary_work_from_home <- aggregate(work_from_home ~ city + year, data = biod_dataset, FUN = summary)
print(summary_work_from_home)

#all data have repetitions that have the same values for chem, for remote_work and work_from_home, therefore drop the repeating observations before adding the city-lev summed values for area (biodiversity area within 10km)
library(dplyr)
uq_biod_dataset <- biod_dataset %>%
  group_by(year, city) %>%
  filter(n() == 1) %>%
  ungroup()
print(uq_biod_dataset)
summary(uq_biod_dataset)
uq_biod_dataset$area <- NULL
uq_biod_dataset$city_year <- NULL
uq_biod_dataset$biod <- NULL

#now, sum up all area falling within the same city from area dataset
#load the area dataset first sheet of 10km distance data
area <- area %>%
  group_by(city) %>%
  mutate(uq_area10 = sum(area)) %>%
  ungroup()
uq_area <- area %>% distinct(city, .keep_all = TRUE)
print(uq_area)
uq_area$area <- NULL
summary(uq_area)

#add the unique values for cities to dataframe uq_biod_datasset
biod_frame <- merge(uq_biod_dataset, uq_area, by = "city", all.x = TRUE)
summary(biod_frame)
biod_frame$biod10 <-biod_frame$uq_area10
biod_frame$uq_area10 <- NULL

#how many cases are there?
cases <- !is.na(biod_frame$chem) & !is.na(biod_frame$work_from_home) & !is.na(biod_frame$remote_work) & !is.na(biod_frame$biod10)
count_cases <- sum(cases)
print(count_cases)

cases <- !is.na(biod_frame$chem) & !is.na(biod_frame$biod10)
count_cases <- sum(cases)
print(count_cases)
#PROBLEM1: there are 103 cases for chem and biod, but it drops to 1 case when work_from_home and remote_work are added

biod_frame$biodchem <- biod_frame$biod10 / biod_frame$chem


write_xlsx(biod_frame, "C:\\Users\\Anna Auza\\OneDrive\\Desktop\\THESIS\\Article 1\\1 Thesis data _ city\\biod_frame.xlsx")
write.csv(biod_frame, "C:\\Users\\Anna Auza\\OneDrive\\Desktop\\THESIS\\Article 1\\1 Thesis data _ city\\biod_frame.csv", row.names = FALSE)


#going forward only with biodiversity and chemicals data

#checking for normality to find cutoffs for calibration
# Perform Shapiro-Wilk test for each column
shapiro_test_chem <- shapiro.test(biod_frame$chem)
shapiro_test_biod10 <- shapiro.test(biod_frame$biod10)
shapiro_test_ec_zip <- shapiro.test(biod_frame$ec_zip)
shapiro_test_nbhd_ec_zip <- shapiro.test(biod_frame$nbhd_ec_zip)
shapiro_test_ec_grp_mem_zip <- shapiro.test(biod_frame$ec_grp_mem_zip)

# Print the results
print(shapiro_test_chem)
print(shapiro_test_biod10)
print(shapiro_test_ec_zip)
print(shapiro_test_nbhd_ec_zip)
print(shapiro_test_ec_grp_mem_zip)

hist(biod_frame$chem, main = "Histogram of chem", xlab = "Value")
hist(biod_frame$biod10, main = "Histogram of biod10", xlab = "Value")
hist(biod_frame$ec_zip, main = "Histogram of ec_zip", xlab = "Value")
hist(biod_frame$nbhd_ec_zip, main = "Histogram of nbhd_ec_zip", xlab = "Value")
hist(biod_frame$ec_grp_mem_zip, main = "Histogram of ec_grp_mem_zip", xlab = "Value")

# Create Q-Q plots
qqnorm(biod_frame$chem, main = "Q-Q Plot for chem")
qqnorm(biod_frame$biod10, main = "Q-Q Plot for biod10")
qqnorm(biod_frame$ec_zip, main = "Q-Q Plot for ec_zip")
qqnorm(biod_frame$nbhd_ec_zip, main = "Q-Q Plot for nbhd_ec_zip")
qqnorm(biod_frame$ec_grp_mem_zip, main = "Q-Q Plot for ec_grp_mem_zip")

#SW test results for biod10 allow to reject normality, therefore use cutoffs of 0.2 0.5 and 0.8 motivated by Pappas and Woodside
#plots for data not from Chetty on economic connectedness show non-normality, too

#install.packages("admisc")
#install.packages("QCA")

#######################################################################
#SECTION 4 | CALIBRATION


###
# FUZZY SETS
###
summary(biod_frame)
thresholds <- c(0.2, 0.5, 0.8)
thresholds2 <- c(0.05, 0.5, 0.95)

biodNA_frame <- na.omit(biod_frame)

###
#FSIL95 GUI EYEBALLING FUZZY S-SHAPED INCREASING LOGISTIC FSIL95 0.95 CALIBRATION
###
#runGUI()

biod_frame$chemFSIL95tr_1_5_15 <- calibrate(biod_frame$chem, thresholds =
                                                "e=1, c=5, i=15", idm = 0.993)

biod_frame$pop2018FSIL95tr_2000_40000_80000 <-
  calibrate(biod_frame$pop2018, thresholds = "e=2000, c=40000, i=80000")

biod_frame$ec_zipFSIL95tr_0.4_0.9_1.4 <- calibrate(biod_frame$ec_zip,
                                                     thresholds = "e=0.4, c=0.9, i=1.4")

biod_frame$nbhd_ec_zipFSIL95tr_0.2_1_1.8 <-
  calibrate(biod_frame$nbhd_ec_zip, thresholds = "e=0.2, c=1, i=1.8")

biod_frame$ec_grp_mem_zipFSIL95tr_ <- calibrate(biod_frame$ec_grp_mem_zip,
                                                  thresholds = "e=0.4, c=0.9, i=1.5")

biod_frame$biod10FSIL95tr_2_1200.._2500.. <- calibrate(biod_frame$biod10,
                                                         thresholds = "e=2, c=120000000, i=350000000")

biod_frame$biodchemFSIL95tr_0.5_75.._300.. <-
  calibrate(biod_frame$biodchem, thresholds = "e=0.5, c=75000000,
  i=300000000")

###
#FSIE95 GUI EYEBALLING FUZZY S-SHAPED INCREASING ECDF FSIE95 0.95 CALIBRATION
###
#runGUI()

#create new data frames where only there are noNA for each condition

#chem ECDF with GUI-visual s-shape tresholds
biod_frame_noNA_chem <- subset(biod_frame, !is.na(chem), select = (c("id", "year", "chem")))

biod_frame_noNA_chem$chemFSIE95tr_1_5_15 <- calibrate(biod_frame_noNA_chem$chem, thresholds =
                                              "e=1, c=5, i=15", logistic = FALSE,
                                            ecdfx = TRUE, idm = 0.993)
summary(biod_frame_noNA_chem$chemFSIE95tr_1_5_15)
biod_frame <- merge(biod_frame, biod_frame_noNA_chem[, c("id", "year", "chemFSIE95tr_1_5_15")], by = c("id", "year"), all.x = TRUE)

#ec_zip ECDF with GUI-visual s-shape tresholds
biod_frame_noNA_ec_zip <- subset(biod_frame, !is.na(ec_zip), select = (c("id", "year", "ec_zip")))

biod_frame_noNA_ec_zip$ec_zipFSIE95tr_0.4_0.9_1.4 <- calibrate(biod_frame_noNA_ec_zip$ec_zip,
                                                   thresholds = "e=0.4, c=0.9, i=1.4", logistic = FALSE,
                                                   ecdf = TRUE)
summary(biod_frame_noNA_ec_zip$ec_zipFSIE95tr_0.4_0.9_1.4)
biod_frame <- merge(biod_frame, biod_frame_noNA_ec_zip[, c("id", "year", "ec_zipFSIE95tr_0.4_0.9_1.4")], by = c("id", "year"), all.x = TRUE)


#nbhd_ec_zip ECDF with GUI-visual s-shape tresholds
biod_frame_noNA_nbhd_ec_zip <- subset(biod_frame, !is.na(nbhd_ec_zip), select = (c("id", "year", "nbhd_ec_zip")))

biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zipFSIE95tr_0.2_1_1.8 <-
  calibrate(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, thresholds = "e=0.2, c=1, i=1.8", logistic = FALSE,
            ecdf = TRUE)
summary(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zipFSIE95tr_0.2_1_1.8)
biod_frame <- merge(biod_frame, biod_frame_noNA_nbhd_ec_zip[, c("id", "year", "nbhd_ec_zipFSIE95tr_0.2_1_1.8")], by = c("id", "year"), all.x = TRUE)


#ec_grp_mem_zip ECDF with GUI-visual s-shape tresholds
biod_frame_noNA_ec_grp_mem_zip <- subset(biod_frame, !is.na(ec_grp_mem_zip), select = (c("id", "year", "ec_grp_mem_zip")))

biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zipFSIE95tr_ <-
  calibrate(biod_frame$ec_grp_mem_zip,
            thresholds = "e=0.4, c=0.9, i=1.5", logistic = FALSE,
            ecdf = TRUE)
summary(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zipFSIE95tr_)
biod_frame <- merge(biod_frame, biod_frame_noNA_ec_grp_mem_zip[, c("id", "year", "ec_grp_mem_zipFSIE95tr_")], by = c("id", "year"), all.x = TRUE)

#biod10 ECDF with GUI-visual s-shape tresholds
biod_frame_noNA_biod10 <- subset(biod_frame, !is.na(biod10), select = (c("id", "year", "biod10")))

biod_frame_noNA_biod10$biod10FSIE95tr_2_1200.._2500.. <-calibrate(biod_frame_noNA_biod10$biod10, thresholds = "e=2, c=120000000, i=350000000", logistic = FALSE,
                      ecdfx = TRUE)
summary(biod_frame_noNA_biod10$biod10FSIE95tr_2_1200.._2500..)
biod_frame <- merge(biod_frame, biod_frame_noNA_biod10[, c("id", "year", "biod10FSIE95tr_2_1200.._2500..")], by = c("id", "year"), all.x = TRUE)

#biodchem ECDF with GUI-visual s-shape tresholds
biod_frame_noNA_biodchem <- subset(biod_frame, !is.na(biodchem), select = (c("id", "year", "biodchem")))

biod_frame_noNA_biodchem$biodchemFSIE95tr_0.5_75.._300.. <-calibrate(biod_frame_noNA_biodchem$biodchem, thresholds = "e=0.5, c=75000000,
  i=300000000", logistic = FALSE,
                                                                     ecdfx = TRUE)
summary(biod_frame_noNA_biodchem$biodchemFSIE95tr_0.5_75.._300..)
biod_frame <- merge(biod_frame, biod_frame_noNA_biodchem[, c("id", "year", "biodchemFSIE95tr_0.5_75.._300..")], by = c("id", "year"), all.x = TRUE)


###
#FSIL_COM_EUCL
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "complete", distm = "euclidian")
biod_frame$biod10_FSIL_COM_EUCL <- calibrate(biod_frame$biod10, thresholds
                                               = "e=111933906, c=229691515, i=334766042")


findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "complete", distm = "euclidian")
biod_frame$biodchem_FSIL_COM_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=38144569 , c=111588681 , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "complete", distm = "euclidian")
biod_frame$ec_zip_FSIL_COM_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=0.73986  , c=0.96500  , i=1.35478")


findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "complete", distm = "euclidian")
biod_frame$nbhd_ec_zip_FSIL_COM_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.535135   , c=0.827470   , i=1.456453")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "complete", distm = "euclidian")
biod_frame$ec_grp_mem_zip_FSIL_COM_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=0.83095    , c=1.15980    , i=1.35233")


###
#FSIE_COM_EUCL
###

findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "complete", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$biod10_FSIE_COM_EUCL <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906, c=229691515, i=334766042")


findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "complete", distm = "euclidian",logistic = "FALSE", ecdf = "TRUE")
biod_frame$biodchem_FSIE_COM_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                               = "e=38144569 , c=111588681 , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "complete", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_zip_FSIE_COM_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                             = "e=0.73986  , c=0.96500  , i=1.35478")


findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "complete", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$nbhd_ec_zip_FSIE_COM_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                  = "e=0.535135   , c=0.827470   , i=1.456453")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "complete", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_grp_mem_zip_FSIE_COM_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                     = "e=0.83095    , c=1.15980    , i=1.35233")



###
#FSIL_COM_MAN
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "complete", distm = "manhattan")
biod_frame$biod10_FSIL_COM_MAN <- calibrate(biod_frame$biod10, thresholds
                                                     = "e=111933906     , c=229691515     , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "complete", distm = "manhattan")
biod_frame$biodchem_FSIL_COM_MAN <- calibrate(biod_frame$biodchem, thresholds
                                            = "e=38144569      , c=111588681      , i=303757163")


findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "complete", distm = "manhattan")
biod_frame$ec_zip_FSIL_COM_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                              = "e=0.73986       , c=0.96500       , i=1.35478")

findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "complete", distm = "manhattan")
biod_frame$nbhd_ec_zip_FSIL_COM_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                            = "e=0.535135       , c=0.827470        , i=1.456453")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "complete", distm = "manhattan")
biod_frame$ec_grp_mem_zip_FSIL_COM_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                 = "e=0.83095        , c=1.15980         , i=1.35233")


###
#FSIE_COM_MAN
###

findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "complete", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$biod10_FSIE_COM_MAN <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906, c=229691515, i=334766042")


findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "complete", distm = "manhattan",logistic = "FALSE", ecdf = "TRUE")
biod_frame$biodchem_FSIE_COM_MAN <- calibrate(biod_frame$biodchem, thresholds
                                               = "e=38144569 , c=111588681 , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "complete", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_zip_FSIE_COM_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                             = "e=0.73986  , c=0.96500  , i=1.35478")


findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "complete", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$nbhd_ec_zip_FSIE_COM_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                  = "e=0.535135   , c=0.827470   , i=1.456453")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "complete", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_grp_mem_zip_FSIE_COM_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                     = "e=0.83095    , c=1.15980    , i=1.35233")


###
#FSIL_COM_MAX
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "complete", distm = "maximum")
biod_frame$biod10_FSIL_COM_MAX <- calibrate(biod_frame$biod10, thresholds
                                            = "e=111933906      , c=229691515      , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "complete", distm = "maximum")
biod_frame$biodchem_FSIL_COM_MAX <- calibrate(biod_frame$biodchem, thresholds
                                            = "e=38144569       , c=111588681       , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "complete", distm = "maximum")
biod_frame$ec_zip_FSIL_COM_MAX <- calibrate(biod_frame$ec_zip, thresholds
                                              = "e=0.73986        , c=0.96500        , i=1.35478")

findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "complete", distm = "maximum")
biod_frame$nbhd_ec_zip_FSIL_COM_MAX <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                            = "e=0.535135        , c=0.827470         , i=1.456453")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "complete", distm = "maximum")
biod_frame$ec_grp_mem_zip_FSIL_COM_MAX <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                 = "e=0.83095         , c=1.15980         , i=1.35233")


###
#FSIE_COM_MAX
###



###
#FSIL_COM_MINK
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "complete", distm = "minkowski")
biod_frame$biod10_FSIL_COM_MINK <- calibrate(biod_frame$biod10, thresholds
                                            = "e=111933906       , c=229691515       , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "complete", distm = "minkowski")
biod_frame$biodchem_FSIL_COM_MINK <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=38144569        , c=111588681        , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "complete", distm = "minkowski")
biod_frame$ec_zip_FSIL_COM_MINK <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=0.73986         , c=0.96500         , i=1.35478")

findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "complete", distm = "minkowski")
biod_frame$nbhd_ec_zip_FSIL_COM_MINK <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.535135          , c=0.827470          , i=1.456453")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "complete", distm = "minkowski")
biod_frame$ec_grp_mem_zip_FSIL_COM_MINK <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=0.83095           , c=1.15980           , i=1.35233")


###
#FSIE_COM_MINK
###


#########################
#Are these calibrated values with COM hclust really different?
#testing correlation
corr_col_complete <- biod_frame[c("biodchem_FSIL_COM_MAX", "biodchem_FSIL_COM_MAN", "biodchem_FSIL_COM_EUCL", "biodchem_FSIL_COM_MINK")]
corr_mat_complete <- cor(corr_col_complete)
corrplot(corr_mat_complete, method = "number", col.colnames = c("max", "man", "eucl"), number.cex = 0.7, number.digits = 2)
#All the same-hclust method calibrated values with complete option are the same

#########################
###
#FSIL_WARD_EUCL
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward", distm = "euclidian")
biod_frame$biod10_FSIL_WARD_EUCL <- calibrate(biod_frame$biod10, thresholds
                                                     = "e=111933906            , c=229691515            , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward", distm = "euclidian")
biod_frame$biodchem_FSIL_WARD_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                              = "e=38144569             , c=167479933             , i=303757163")


findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward", distm = "euclidian")
biod_frame$ec_zip_FSIL_WARD_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                                = "e=0.641295              , c=0.848230              , i=1.055228")


findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward", distm = "euclidian")
biod_frame$nbhd_ec_zip_FSIL_WARD_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                              = "e=0.506295               , c=0.788465               , i=1.015915")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward", distm = "euclidian")
biod_frame$ec_grp_mem_zip_FSIL_WARD_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                   = "e=0.6592700                , c=0.8743467                , i=1.0450300")

###
#FSIE_WARD_EUCL
###


findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$biod10_FSIE_WARD_EUCL <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906, c=229691515, i=334766042")


findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward.D", distm = "euclidian",logistic = "FALSE", ecdf = "TRUE")
biod_frame$biodchem_FSIE_WARD_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                               = "e=38144569 , c=167479933 , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward.D", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_zip_FSIE_WARD_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                             = "e=0.641295  , c=0.848230  , i=1.055228")


findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward.D", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$nbhd_ec_zip_FSIE_WARD_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                  = "e=0.506295   , c=0.788465   , i=1.015915")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward.D", distm = "euclidian", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_grp_mem_zip_FSIE_WARD_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                     = "e=0.6592700     , c=0.8743467     , i=1.0450300")



###
#FSIL_WARD_MAN
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D", distm = "manhattan")
biod_frame$biod10_FSIL_WARD_MAN <- calibrate(biod_frame$biod10, thresholds
                                              = "e=111933906             , c=229691515             , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward", distm = "manhattan")
biod_frame$biodchem_FSIL_WARD_MAN <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=38144569               , c=167479933              , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward", distm = "manhattan")
biod_frame$ec_zip_FSIL_WARD_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=0.641295                , c=0.848230               , i=1.055228")

findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward", distm = "manhattan")
biod_frame$nbhd_ec_zip_FSIL_WARD_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.506295                 , c=0.788465                , i=1.015915")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward", distm = "manhattan")
biod_frame$ec_grp_mem_zip_FSIL_WARD_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=0.6592700                  , c=0.8743467                 , i=1.0450300")

###
#FSIE_WARD_MAN
###


#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$biod10_FSIE_WARD_MAN <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906             , c=229691515             , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward.D", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$biodchem_FSIE_WARD_MAN <- calibrate(biod_frame$biodchem, thresholds
                                               = "e=38144569               , c=167479933              , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward.D", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_zip_FSIE_WARD_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                             = "e=0.641295                , c=0.848230               , i=1.055228")

findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$nbhd_ec_zip_FSIE_WARD_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                  = "e=0.506295                 , c=0.788465                , i=1.015915")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward", distm = "manhattan", logistic = "FALSE", ecdf = "TRUE")
biod_frame$ec_grp_mem_zip_FSIE_WARD_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                     = "e=0.6592700                  , c=0.8743467                 , i=1.0450300")

###
#FSIL_WARD_MAX
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward", distm = "maximum")
biod_frame$biod10_FSIL_WARD_MAX <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906              , c=229691515             , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward", distm = "maximum")
biod_frame$biodchem_FSIL_WARD_MAX <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=38144569               , c=167479933              , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward", distm = "maximum")
biod_frame$ec_zip_FSIL_WARD_MAX <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=0.641295                , c=0.848230               , i=1.055228")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward", distm = "maximum")
biod_frame$nbhd_ec_zip_FSIL_WARD_MAX <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.506295                 , c=0.788465                , i=1.015915")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward", distm = "maximum")
biod_frame$ec_grp_mem_zip_FSIL_WARD_MAX <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=0.6592700                  , c=0.8743467                 , i=1.0450300")

###
#FSIE_WARD_MAX
###

###
#FSIL_WARD_MINK
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward", distm = "minkowski")
biod_frame$biod10_FSIL_WARD_MINK <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906               , c=229691515              , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward", distm = "minkowski")
biod_frame$biodchem_FSIL_WARD_MINK <- calibrate(biod_frame$biodchem, thresholds
                                              = "e=38144569                , c=167479933               , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward", distm = "minkowski")
biod_frame$ec_zip_FSIL_WARD_MINK <- calibrate(biod_frame$ec_zip, thresholds
                                                = "e=0.641295                 , c=0.848230                , i=1.055228")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward", distm = "minkowski")
biod_frame$nbhd_ec_zip_FSIL_WARD_MINK <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                              = "e=0.506295                  , c=0.788465                 , i=1.015915")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward", distm = "minkowski")
biod_frame$ec_grp_mem_zip_FSIL_WARD_MINK <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                   = "e=0.6592700                   , c=0.8743467                  , i=1.0450300")

###
#FSIE_WARD_MINK
###


#########################
#Are these calibrated values with WARD.D, that is, WARD hclust really different?
#testing correlation
corr_col_ward <- biod_frame[c("biodchem_FSIL_WARD_MAX", "biodchem_FSIL_WARD_MAN", "biodchem_FSIL_WARD_EUCL", "biodchem_FSIL_WARD_MINK")]
corr_mat_ward <- cor(corr_col_ward)
corrplot(corr_mat_ward, method = "number", number.cex = 0.7, number.digits = 2)
#All the same-hclust method calibrated LOGISTIC values with WARD.D option are the same

#check for differences between complete and ward clustering
corr_col_com_ward <- biod_frame[c("biodchem_FSIL_WARD_MAX", "biodchem_FSIL_WARD_MAN", "biodchem_FSIL_WARD_EUCL", "biodchem_FSIL_WARD_MINK", "biodchem_FSIL_COM_MAX", "biodchem_FSIL_COM_MAN", "biodchem_FSIL_COM_EUCL", "biodchem_FSIL_COM_MINK")]
corr_mat_com_ward <- cor(corr_col_com_ward)
corrplot(corr_mat_com_ward, method = "number", number.cex = 0.7, number.digits = 2)

#########################

###
#FSIL_WARD2_EUCL
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D2", distm = "euclidian")
biod_frame$biod10_FSIL_WARD2_EUCL <- calibrate(biod_frame$biod10, thresholds
                                              = "e=111933906            , c=229691515            , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward.D2", distm = "euclidian")
biod_frame$biodchem_FSIL_WARD2_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                                = "e=38144569             , c=111588681              , i=303757163")


findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward.D2", distm = "euclidian")
biod_frame$ec_zip_FSIL_WARD2_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                              = "e=0.7052525 , c=0.8537600   , i=1.0849150")


findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward.D2", distm = "euclidian")
biod_frame$nbhd_ec_zip_FSIL_WARD2_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                   = "e=0.6972325                , c=0.8891200                , i=1.1684800")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward.D2", distm = "euclidian")
biod_frame$ec_grp_mem_zip_FSIL_WARD2_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                      = "e=0.673080  , c=0.984790 , i=1.224027")

###
#FSIE_WARD2_EUCL
###


###
#FSIL_WARD2_MAN
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D2", distm = "manhattan")
biod_frame$biod10_FSIL_WARD2_MAN <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906             , c=229691515             , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward.D2", distm = "manhattan")
biod_frame$biodchem_FSIL_WARD2_MAN <- calibrate(biod_frame$biodchem, thresholds
                                               = "e=38144569               , c=111588681               , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward.D2", distm = "manhattan")
biod_frame$ec_zip_FSIL_WARD2_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                             = "e=0.7052525                 , c=0.8537600  , i=1.0849150")

findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward.D2", distm = "manhattan")
biod_frame$nbhd_ec_zip_FSIL_WARD2_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                  = "e=0.6972325  , c=0.8891200  , i=1.1684800")

findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward.D2", distm = "manhattan")
biod_frame$ec_grp_mem_zip_FSIL_WARD2_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                     = "e=0.673080  , c=0.984790 , i=1.224027")

###
#FSIE_WARD_MAN
###

###
#FSIL_WARD2_MAX
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D2", distm = "maximum")
biod_frame$biod10_FSIL_WARD2_MAX <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906  , c=229691515  , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward.D2", distm = "maximum")
biod_frame$biodchem_FSIL_WARD2_MAX <- calibrate(biod_frame$biodchem, thresholds
                                               = "e=38144569  , c=111588681 , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward.D2", distm = "maximum")
biod_frame$ec_zip_FSIL_WARD2_MAX <- calibrate(biod_frame$ec_zip, thresholds
                                             = "e=0.7052525  , c=0.8537600 , i=1.0849150")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward.D2", distm = "maximum")
biod_frame$nbhd_ec_zip_FSIL_WARD2_MAX <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                  = "e=0.6972325   , c=0.8891200 , i=1.1684800")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward.D2", distm = "maximum")
biod_frame$ec_grp_mem_zip_FSIL_WARD2_MAX <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                     = "e=0.673080  , c=0.984790 , i=1.224027")

###
#FSIE_WARD2_MAX
###

###
#FSIL_WARD2_MINK
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "ward.D2", distm = "minkowski")
biod_frame$biod10_FSIL_WARD2_MINK <- calibrate(biod_frame$biod10, thresholds
                                              = "e=111933906               , c=229691515              , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "ward.D2", distm = "minkowski")
biod_frame$biodchem_FSIL_WARD2_MINK <- calibrate(biod_frame$biodchem, thresholds
                                                = "e=38144569   , c=111588681                , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "ward.D2", distm = "minkowski")
biod_frame$ec_zip_FSIL_WARD2_MINK <- calibrate(biod_frame$ec_zip, thresholds
                                              = "e=0.7052525  , c=0.8537600 , i=1.0849150")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "ward.D2", distm = "minkowski")
biod_frame$nbhd_ec_zip_FSIL_WARD2_MINK <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                                   = "e=0.6972325   , c=0.8891200  , i=1.1684800")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "ward.D2", distm = "minkowski")
biod_frame$ec_grp_mem_zip_FSIL_WARD2_MINK <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                      = "e=0.673080   , c=0.984790    , i=1.224027")

###
#FSIE_WARD2_MINK
###

######################

###
#FSIL_SINGLE_EUCL
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "single", distm = "euclidian")
biod_frame$biod10_FSIL_SING_EUCL <- calibrate(biod_frame$biod10, thresholds
                                              = "e=111933906               , c=229691515              , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "single", distm = "euclidian")
biod_frame$biodchem_FSIL_SING_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                              = "e=111588681                , c=167479933               , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "single", distm = "euclidian")
biod_frame$ec_zip_FSIL_SING_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                                = "e=1.25259                 , c=1.35478                , i=1.45569")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "single", distm = "euclidian")
biod_frame$nbhd_ec_zip_FSIL_SING_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                              = "e=0.206865                  , c=1.733085                 , i=1.791920")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "single", distm = "euclidian")
biod_frame$ec_grp_mem_zip_FSIL_SING_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                              = "e=1.558715                  , c=1.589450                 , i=1.621355")


###
#FSIE_SINGLE_EUCL
###


###
#FSIL_SINGLE_MAN
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "single", distm = "manhattan")
biod_frame$biod10_FSIL_SING_MAN <- calibrate(biod_frame$biod10, thresholds
                                              = "e=111933906                , c=229691515              , i=334766042")

findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "single", distm = "manhattan")
biod_frame$biodchem_FSIL_SING_MAN <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=111588681                 , c=167479933               , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "single", distm = "manhattan")
biod_frame$ec_zip_FSIL_SING_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=1.25259                  , c=1.35478               , i=1.45569")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "single", distm = "manhattan")
biod_frame$nbhd_ec_zip_FSIL_SING_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.206865                   , c=1.733085                , i=1.791920")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "single", distm = "manhattan")
biod_frame$ec_grp_mem_zip_FSIL_SING_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=1.558715                    , c=1.589450                 , i=1.621355")


###
#FSIE_SINGLE_MAN
###


###
#FSIL_SINGLE_MAX
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "single", distm = "maximum")
biod_frame$biod10_FSIL_SING_MAX <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906                , c=229691515              , i=334766042")
findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "single", distm = "maximum")
biod_frame$biodchem_FSIL_SING_MAX <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=111588681                 , c=167479933               , i=303757163")

findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "single", distm = "maximum")
biod_frame$ec_zip_FSIL_SING_MAX <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=1.25259                  , c=1.35478                , i=1.45569")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "single", distm = "maximum")
biod_frame$nbhd_ec_zip_FSIL_SING_MAX <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.206865                   , c=1.733085                 , i=1.791920")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "single", distm = "maximum")
biod_frame$ec_grp_mem_zip_FSIL_SING_MAX <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=1.558715                    , c=1.589450                  , i=1.621355")

###
#FSIE_SINGLE_MAX
###



###
#FSIL_SINGLE_MINK
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "single", distm = "minkowski")
biod_frame$biod10_FSIL_SING_MINK <- calibrate(biod_frame$biod10, thresholds
                                                     = "e=111933906 , c=229691515 , i=334766042")
findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "single", distm = "minkowski")
biod_frame$biodchem_FSIL_SING_MINK <- calibrate(biod_frame$biodchem, thresholds
                                              = "e=111588681 , c=167479933 , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "single", distm = "minkowski")
biod_frame$ec_zip_FSIL_SING_MINK <- calibrate(biod_frame$ec_zip, thresholds
                                                = "e=1.25259  , c=1.35478  , i=1.45569")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "single", distm = "minkowski")
biod_frame$nbhd_ec_zip_FSIL_SING_MINK <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                              = "e=0.206865   , c=1.733085   , i=1.791920")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "single", distm = "minkowski")
biod_frame$ec_grp_mem_zip_FSIL_SING_MINK <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                   = "e=1.558715    , c=1.589450    , i=1.621355")

###
#FSIE_SINGLE_MINK
###

####################################

###
#FSIL_AVE_EUCL
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "average", distm = "euclidian")
biod_frame$biod10_FSIL_AVE_EUCL <- calibrate(biod_frame$biod10, thresholds
                                                      = "e=111933906     , c=229691515     , i=334766042")
findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "average", distm = "euclidian")
biod_frame$biodchem_FSIL_AVE_EUCL <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=38144569      , c=111588681      , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "average", distm = "euclidian")
biod_frame$ec_zip_FSIL_AVE_EUCL <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=0.580475, c=0.853760, i=1.252590")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "average", distm = "euclidian")
biod_frame$nbhd_ec_zip_FSIL_AVE_EUCL <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.206865 , c=0.788465 , i=1.456453")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "average", distm = "euclidian")
biod_frame$ec_grp_mem_zip_FSIL_AVE_EUCL <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=0.863245  , c=1.159800  , i=1.352330")

###
#FSIE_AVE_EUCL
###


###
#FSIL_AVE_MAN
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "average", distm = "manhattan")
biod_frame$biod10_FSIL_AVE_MAN <- calibrate(biod_frame$biod10, thresholds
                                             = "e=111933906 , c=229691515, i=334766042")
findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "average", distm = "manhattan")
biod_frame$biodchem_FSIL_AVE_MAN <- calibrate(biod_frame$biodchem, thresholds
                                            = "e=38144569  , c=111588681 , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "average", distm = "manhattan")
biod_frame$ec_zip_FSIL_AVE_MAN <- calibrate(biod_frame$ec_zip, thresholds
                                              = "e=0.580475   , c=0.853760  , i=1.252590")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "average", distm = "manhattan")
biod_frame$nbhd_ec_zip_FSIL_AVE_MAN <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                            = "e=0.206865    , c=0.788465   , i=1.456453")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "average", distm = "manhattan")
biod_frame$ec_grp_mem_zip_FSIL_AVE_MAN <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                 = "e=0.863245     , c=1.159800    , i=1.352330")

###
#FSIE_AVE_MAN
###

###
#FSIL_AVE_MAX
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "average", distm = "maximum")
biod_frame$biod10_FSIL_AVE_MAX <- calibrate(biod_frame$biod10, thresholds
                                            = "e=111933906 , c=229691515, i=334766042")
findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "average", distm = "maximum")
biod_frame$biodchem_FSIL_AVE_MAX <- calibrate(biod_frame$biodchem, thresholds
                                            = "e=38144569 , c=111588681 , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "average", distm = "maximum")
biod_frame$ec_zip_FSIL_AVE_MAX <- calibrate(biod_frame$ec_zip, thresholds
                                              = "e=0.580475  , c=0.853760  , i=1.252590")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "average", distm = "maximum")
biod_frame$nbhd_ec_zip_FSIL_AVE_MAX <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                            = "e=0.206865   , c=0.788465   , i=1.456453")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "average", distm = "maximum")
biod_frame$ec_grp_mem_zip_FSIL_AVE_MAX <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                 = "e=0.863245    , c=1.159800    , i=1.352330")

###
#FSIE_AVE_MAX
###


###
#FSIL_AVE_MINK
###

#find tresholds
findTh(biod_frame_noNA_biod10$biod10, n=3, hclustm = "average", distm = "minkowski")
biod_frame$biod10_FSIL_AVE_MINK <- calibrate(biod_frame$biod10, thresholds
                                            = "e=111933906 , c=229691515, i=334766042")
findTh(biod_frame_noNA_biodchem$biodchem, n=3, hclustm = "average", distm = "minkowski")
biod_frame$biodchem_FSIL_AVE_MINK <- calibrate(biod_frame$biodchem, thresholds
                                             = "e=38144569  , c=111588681 , i=303757163")
findTh(biod_frame_noNA_ec_zip$ec_zip, n=3, hclustm = "average", distm = "minkowski")
biod_frame$ec_zip_FSIL_AVE_MINK <- calibrate(biod_frame$ec_zip, thresholds
                                               = "e=0.580475   , c=0.853760  , i=1.252590")
findTh(biod_frame_noNA_nbhd_ec_zip$nbhd_ec_zip, n=3, hclustm = "average", distm = "minkowski")
biod_frame$nbhd_ec_zip_FSIL_AVE_MINK <- calibrate(biod_frame$nbhd_ec_zip, thresholds
                                             = "e=0.206865    , c=0.788465   , i=1.456453")
findTh(biod_frame_noNA_ec_grp_mem_zip$ec_grp_mem_zip, n=3, hclustm = "average", distm = "minkowski")
biod_frame$ec_grp_mem_zip_FSIL_AVE_MINK <- calibrate(biod_frame$ec_grp_mem_zip, thresholds
                                                  = "e=0.863245     , c=1.159800    , i=1.352330")

###
#FSIE_AVE_MINK
###

#############################################################


##
# LOGISTIC FUNCTION CALIBRATION | variables generated: ___calL 
##

biod_frame$chem_calL <- calibrate(biod_frame$chem, 
                                  type = "fuzzy",
                                  method = "direct",
                                  thresholds = thresholds,idm = 0.95,
                                  logistic = TRUE,
                                  ecdf = TRUE)
biod_frame$chem_calL<-as.numeric(biod_frame$chem_calL)
summary(biod_frame$chem_calL)

#because the yielded calibrates values do not cover the range of 0 to 1 and are too close to 1, use diff idm=0.6
biod_frame$chem_calL6 <- calibrate(biod_frame$chem, 
                                  type = "fuzzy",
                                  method = "direct",
                                  thresholds = thresholds,idm = 0.6,
                                  logistic = TRUE,
                                  ecdf = TRUE)
biod_frame$chem_calL6<-as.numeric(biod_frame$chem_calL6)
summary(biod_frame$chem_calL6)

biod_frame$biod10<-as.numeric(biod_frame$biod10)
biod_frame$biod10_calL <- calibrate(biod_frame$biod10, 
                                    type = "fuzzy",
                                    method = "direct",
                                    thresholds = thresholds2,idm = 0.95,
                                    logistic = TRUE,
                                    ecdf = TRUE)
biod_frame$biod10_calL<-as.numeric(biod_frame$biod10_calL)
summary(biod_frame$biod10_calL)


biod_frame$biod10_calL51 <- calibrate(biod_frame$biod10, 
                                    type = "fuzzy",
                                    method = "direct",
                                    thresholds = thresholds,idm = 0.51,
                                    logistic = TRUE,
                                    ecdf = TRUE)

biod_frame$biod10_calL51<-as.numeric(biod_frame$biod10_calL51)
summary(biod_frame$biod10_calL51)

#because the biod10 cannot be calibrated to cover the 0-1 distribution of membership,check for !NA cases in biod10
countnabiod10 <- (sum(!is.na(biod_frame$biod10_calL)))
summary(countnabiod10)
#quite many --> go to new way of calibrating in Section #EMPIRICAL CDF CALIBRATION


biod_frame$biodchem_calL <- calibrate(biod_frame$biodchem, 
                                      type = "fuzzy",
                                      method = "direct",
                                      thresholds = thresholds,idm = 0.95,
                                      logistic = TRUE,
                                      ecdf = TRUE)
biod_frame$biodchem_calL<-as.numeric(biod_frame$biodchem_calL)

biod_frame$ec_zip_calL <- calibrate(biod_frame$ec_zip, 
                                    type = "fuzzy",
                                    method = "direct",
                                    thresholds = thresholds,idm = 0.95,
                                    logistic = TRUE,
                                    ecdf = TRUE)
biod_frame$ec_zip_calL<-as.numeric(biod_frame$ec_zip_calL)


biod_frame$nbhd_ec_zip_calL <- calibrate(biod_frame$nbhd_ec_zip, 
                                         type = "fuzzy",
                                         method = "direct",
                                         thresholds = thresholds,idm = 0.95,
                                         logistic = TRUE,
                                         ecdf = TRUE)
biod_frame$nbhd_ec_zip_calL<-as.numeric(biod_frame$nbhd_ec_zip_calL)


biod_frame$ec_grp_mem_zip_calL <- calibrate(biod_frame$ec_grp_mem_zip, 
                                            type = "fuzzy",
                                            method = "direct",
                                            thresholds = thresholds,idm = 0.95,
                                            logistic = TRUE, ecdf = TRUE)
biod_frame$ec_grp_mem_zip_calL<-as.numeric(biod_frame$ec_grp_mem_zip_calL)

biod_frame$chem_calL <- calibrate(biod_frame$chem, 
                                  type = "fuzzy",
                                  method = "direct",
                                  thresholds = thresholds,idm = 0.95,
                                  logistic = TRUE,
                                  ecdf = TRUE)
biod_frame$chem_calL<-as.numeric(biod_frame$chem_calL)

biod_frame$biod10_calL <- calibrate(biod_frame$biod10, 
                                    type = "fuzzy",
                                    method = "direct",
                                    thresholds = thresholds,idm = 0.95,
                                    logistic = TRUE,
                                    ecdf = TRUE)
biod_frame$biod10_calL<-as.numeric(biod_frame$biod10_calL)

biod_frame$biodchem_calL <- calibrate(biod_frame$biodchem, 
                                      type = "fuzzy",
                                      method = "direct",
                                      thresholds = thresholds,idm = 0.95,
                                      logistic = TRUE,
                                      ecdf = TRUE)
biod_frame$biodchem_calL<-as.numeric(biod_frame$biodchem_calL)

biod_frame$ec_zip_calL <- calibrate(biod_frame$ec_zip, 
                                    type = "fuzzy",
                                    method = "direct",
                                    thresholds = thresholds,idm = 0.95,
                                    logistic = TRUE,
                                    ecdf = TRUE)
biod_frame$ec_zip_calL<-as.numeric(biod_frame$ec_zip_calL)


biod_frame$nbhd_ec_zip_calL <- calibrate(biod_frame$nbhd_ec_zip, 
                                         type = "fuzzy",
                                         method = "direct",
                                         thresholds = thresholds,idm = 0.95,
                                         logistic = TRUE,
                                         ecdf = TRUE)
biod_frame$nbhd_ec_zip_calL<-as.numeric(biod_frame$nbhd_ec_zip_calL)


biod_frame$ec_grp_mem_zip_calL <- calibrate(biod_frame$ec_grp_mem_zip, 
                                            type = "fuzzy",
                                            method = "direct",
                                            thresholds = thresholds,idm = 0.95,
                                            logistic = TRUE, ecdf = TRUE)
biod_frame$ec_grp_mem_zip_calL<-as.numeric(biod_frame$ec_grp_mem_zip_calL)

###
#EMPIRICAL CDF CALIBRATION | variables generated: ___calE 
###

biod_frame_EXIST_biod10 <- biod_frame[!is.na(biod_frame$biod10), ]
biod_frame_EXIST_biod10 <- biod_frame_EXIST_biod10[, c("city", "year", "biod10")]
biod_frame_EXIST_biod10$biod10_calE <- calibrate(biod_frame_EXIST_biod10$biod10, 
                                    type = "fuzzy",
                                    method = "direct",
                                    thresholds = thresholds,idm = 0.95,
                                    logistic = FALSE,
                                    ecdf = TRUE)
#only keep rows with unique values
biod_frame_EXIST_biod10_uq <- biod_frame_EXIST_biod10 %>% distinct(biod10, .keep_all = TRUE)
biod_frame_EXIST_biod10_uq$biod10_calE99 <- calibrate(biod_frame_EXIST_biod10_uq$biod10, 
                                                 type = "fuzzy",
                                                 method = "direct",
                                                 thresholds = thresholds,idm = 0.99,
                                                 logistic = FALSE,
                                                 ecdfx = TRUE)

###
#CRISP SETS one-value
###

#Find calibration tresholds based on function findTh for calibration in crisp sets
findTh(biod_frame$chem, n=1, hclustm="complete", distm="euclidian")
findTh(biod_frame$biod10, n=1, hclustm="complete", distm="euclidian")
findTh(biod_frame$biodchem, n=1, hclustm="complete", distm="euclidian")
findTh(biod_frame$ec_zip, n=1, hclustm="complete", distm="euclidian")
findTh(biod_frame$nbhd_ec_zip, n=1, hclustm="complete", distm="euclidian")
findTh(biod_frame$ec_grp_mem_zip, n=1, hclustm="complete", distm="euclidian")

#Calibrate into crisp sets with extension ___C
#create new data frames with only !NA values and then merge to principal data frame as crisp values
biod_frame_CHEM_noNA <- biod_frame[complete.cases(biod_frame$chem),]
findTh(biod_frame_CHEM_noNA$chem, n=1, hclustm="complete", distm="euclidian")
biod_frame_CHEM_noNA$chemC <- calibrate(biod_frame_CHEM_noNA$chem, type = "crisp", thresholds = 10.5, na.rm=TRUE)
biod_frame_CHEM_noNA <- subset(biod_frame_CHEM_noNA, !is.na(chemC), select = c(id, year, chemC))
biod_frame <- merge(biod_frame, biod_frame_CHEM_noNA[c("id", "year", "chemC")], by = c("id", "year"), all.x = TRUE)

biod_frame_BIOD10_noNA <- biod_frame[complete.cases(biod_frame$biod10),]
findTh(biod_frame_BIOD10_noNA$biod10, n=1, hclustm="complete", distm="euclidian")
biod_frame_BIOD10_noNA$biod10C <- calibrate(biod_frame_BIOD10_noNA$biod10, type = "crisp", thresholds = 111933906, na.rm=TRUE)
biod_frame_BIOD10_noNA <- subset(biod_frame_BIOD10_noNA, !is.na(biod10C), select = c(id, year, biod10C))
biod_frame <- merge(biod_frame, biod_frame_BIOD10_noNA[c("id", "year", "biod10C")], by = c("id", "year"), all.x = TRUE)

biod_frame_BIODCHEM_noNA <- biod_frame[complete.cases(biod_frame$biodchem),]
findTh(biod_frame_BIODCHEM_noNA$biodchem, n=1, hclustm="complete", distm="euclidian")
biod_frame_BIODCHEM_noNA$biodchemC <- calibrate(biod_frame_BIODCHEM_noNA$biodchem, type = "crisp", thresholds = 303757163, na.rm=TRUE)
biod_frame_BIODCHEM_noNA <- subset(biod_frame_BIODCHEM_noNA, !is.na(biodchemC), select = c(id, year, biodchemC))
biod_frame <- merge(biod_frame, biod_frame_BIODCHEM_noNA[c("id", "year", "biodchemC")], by = c("id", "year"), all.x = TRUE)

biod_frame_EC_ZIP_noNA <- biod_frame[complete.cases(biod_frame$ec_zip),]
findTh(biod_frame_EC_ZIP_noNA$ec_zip, n=1, hclustm="complete", distm="euclidian")
biod_frame_EC_ZIP_noNA$ec_zipC <- calibrate(biod_frame_EC_ZIP_noNA$ec_zip, type = "crisp", thresholds = 0.965, na.rm=TRUE)
biod_frame_EC_ZIP_noNA <- subset(biod_frame_EC_ZIP_noNA, !is.na(ec_zipC), select = c(id, year, ec_zipC))
biod_frame <- merge(biod_frame, biod_frame_EC_ZIP_noNA[c("id", "year", "ec_zipC")], by = c("id", "year"), all.x = TRUE)

biod_frame_NBHD_EC_ZIP_noNA <- biod_frame[complete.cases(biod_frame$nbhd_ec_zip),]
findTh(biod_frame_NBHD_EC_ZIP_noNA$nbhd_ec_zip, n=1, hclustm="complete", distm="euclidian")
biod_frame_NBHD_EC_ZIP_noNA$nbhd_ec_zipC <- calibrate(biod_frame_NBHD_EC_ZIP_noNA$nbhd_ec_zip, type = "crisp", thresholds = 0.82747, na.rm=TRUE)
biod_frame_NBHD_EC_ZIP_noNA <- subset(biod_frame_NBHD_EC_ZIP_noNA, !is.na(nbhd_ec_zipC), select = c(id, year, nbhd_ec_zipC))
biod_frame <- merge(biod_frame, biod_frame_NBHD_EC_ZIP_noNA[c("id", "year", "nbhd_ec_zipC")], by = c("id", "year"), all.x = TRUE)

biod_frame_EC_GRP_MEM_ZIP_noNA <- biod_frame[complete.cases(biod_frame$ec_grp_mem_zip),]
findTh(biod_frame_EC_GRP_MEM_ZIP_noNA$ec_grp_mem_zip, n=1, hclustm="complete", distm="euclidian")
biod_frame_EC_GRP_MEM_ZIP_noNA$ec_grp_mem_zipC <- calibrate(biod_frame_EC_GRP_MEM_ZIP_noNA$ec_grp_mem_zip, type = "crisp", thresholds = 1.1598, na.rm=TRUE)
biod_frame_EC_GRP_MEM_ZIP_noNA <- subset(biod_frame_EC_GRP_MEM_ZIP_noNA, !is.na(ec_grp_mem_zipC), select = c(id, year, ec_grp_mem_zipC))
biod_frame <- merge(biod_frame, biod_frame_EC_GRP_MEM_ZIP_noNA[c("id", "year", "ec_grp_mem_zipC")], by = c("id", "year"), all.x = TRUE)


###
#MULTIVALUE CRISP SET CALIBRATION
###

#calibrate into 5 values with 6 tresholds

biod_frame_CHEM_noNA <- biod_frame[complete.cases(biod_frame$chem),]
findTh(biod_frame_CHEM_noNA$chem, n=6, hclustm="complete", distm="euclidian")
biod_frame_CHEM_noNA$chemM5 <- calibrate(biod_frame_CHEM_noNA$chem, type = "crisp", thresholds = "2.5, 4.5, 6.5, 10.5, 14.0, 16.0", na.rm=TRUE)
biod_frame_CHEM_noNA <- subset(biod_frame_CHEM_noNA, !is.na(chemM5), select = c(id, year, chemM5))
biod_frame <- merge(biod_frame, biod_frame_CHEM_noNA[c("id", "year", "chemM5")], by = c("id", "year"), all.x = TRUE)


biod_frame_BIOD10_noNA <- biod_frame[complete.cases(biod_frame$biod10),]
findTh(biod_frame_BIOD10_noNA$biod10, n=6, hclustm="complete", distm="euclidian")
biod_frame_BIOD10_noNA$biod10M5 <- calibrate(biod_frame_BIOD10_noNA$biod10, type = "crisp", thresholds = "4560970, 10329838,  18783080, 111933906, 229691515, 334766042", na.rm=TRUE)
biod_frame_BIOD10_noNA <- subset(biod_frame_BIOD10_noNA, !is.na(biod10M5), select = c(id, year, biod10M5))
biod_frame <- merge(biod_frame, biod_frame_BIOD10_noNA[c("id", "year", "biod10M5")], by = c("id", "year"), all.x = TRUE)

biod_frame_BIODCHEM_noNA <- biod_frame[complete.cases(biod_frame$biodchem),]
findTh(biod_frame_BIODCHEM_noNA$biodchem, n=6, hclustm="complete", distm="euclidian")
biod_frame_BIODCHEM_noNA$biodchemM5 <- calibrate(biod_frame_BIODCHEM_noNA$biodchem, type = "crisp", thresholds = "16790117,  38144569,  77519373, 111588681, 167479933, 303757163", na.rm=TRUE)
biod_frame_BIODCHEM_noNA <- subset(biod_frame_BIODCHEM_noNA, !is.na(biodchemM5), select = c(id, year, biodchemM5))
biod_frame <- merge(biod_frame, biod_frame_BIODCHEM_noNA[c("id", "year", "biodchemM5")], by = c("id", "year"), all.x = TRUE)

biod_frame_EC_ZIP_noNA <- biod_frame[complete.cases(biod_frame$ec_zip),]
findTh(biod_frame_EC_ZIP_noNA$ec_zip, n=6, hclustm="complete", distm="euclidian")
biod_frame_EC_ZIP_noNA$ec_zipM5 <- calibrate(biod_frame_EC_ZIP_noNA$ec_zip, type = "crisp", thresholds = "0.580475, 0.739860, 0.859310, 0.965000, 1.121140, 1.354780", na.rm=TRUE)
biod_frame_EC_ZIP_noNA <- subset(biod_frame_EC_ZIP_noNA, !is.na(ec_zipM5), select = c(id, year, ec_zipM5))
biod_frame <- merge(biod_frame, biod_frame_EC_ZIP_noNA[c("id", "year", "ec_zipM5")], by = c("id", "year"), all.x = TRUE)

biod_frame_NBHD_EC_ZIP_noNA <- biod_frame[complete.cases(biod_frame$nbhd_ec_zip),]
findTh(biod_frame_NBHD_EC_ZIP_noNA$nbhd_ec_zip, n=6, hclustm="complete", distm="euclidian")
biod_frame_NBHD_EC_ZIP_noNA$nbhd_ec_zipM5 <- calibrate(biod_frame_NBHD_EC_ZIP_noNA$nbhd_ec_zip, type = "crisp", thresholds = "0.394925, 0.535135, 0.827470, 1.168480, 1.456453, 1.613416", na.rm=TRUE)
biod_frame_NBHD_EC_ZIP_noNA <- subset(biod_frame_NBHD_EC_ZIP_noNA, !is.na(nbhd_ec_zipM5), select = c(id, year, nbhd_ec_zipM5))
biod_frame <- merge(biod_frame, biod_frame_NBHD_EC_ZIP_noNA[c("id", "year", "nbhd_ec_zipM5")], by = c("id", "year"), all.x = TRUE)

biod_frame_EC_GRP_MEM_ZIP_noNA <- biod_frame[complete.cases(biod_frame$ec_grp_mem_zip),]
findTh(biod_frame_EC_GRP_MEM_ZIP_noNA$ec_grp_mem_zip, n=6, hclustm="complete", distm="euclidian")
biod_frame_EC_GRP_MEM_ZIP_noNA$ec_grp_mem_zipM5 <- calibrate(biod_frame_EC_GRP_MEM_ZIP_noNA$ec_grp_mem_zip, type = "crisp", thresholds = "0.60831, 0.83095, 0.97372, 1.15980, 1.35233, 1.51359", na.rm=TRUE)
biod_frame_EC_GRP_MEM_ZIP_noNA <- subset(biod_frame_EC_GRP_MEM_ZIP_noNA, !is.na(ec_grp_mem_zipM5), select = c(id, year, ec_grp_mem_zipM5))
biod_frame <- merge(biod_frame, biod_frame_EC_GRP_MEM_ZIP_noNA[c("id", "year", "ec_grp_mem_zipM5")], by = c("id", "year"), all.x = TRUE)

write_xlsx(biod_frame, "C:\\Users\\Anna Auza\\OneDrive\\Desktop\\THESIS\\Article 1\\1 Thesis data _ city\\biod_frame_091123.xlsx")


#######################################################################
#SECTION 5 | ANALYSIS OF NECESSITY

###
#FSIL95 LOGISTIC FUNCTION-CALIBRATED VALUES VIA GRAPGICAL USER INTERFACE GUI
###

#x1y1
selected_columns_FSIL95_x1y1 <- c("ec_zipFSIL95tr_0.4_0.9_1.4", "biodchemFSIL95tr_0.5_75.._300..")
FSIL95_noNA_x1y1 <- biod_frame %>%
  select(all_of(selected_columns_FSIL95_x1y1)) %>%
  drop_na()
with(FSIL95_noNA_x1y1, sum(fuzzyand(ec_zipFSIL95tr_0.4_0.9_1.4, biodchemFSIL95tr_0.5_75.._300..))/sum(biodchemFSIL95tr_0.5_75.._300..))
pof("ec_zipFSIL95tr_0.4_0.9_1.4 <- biodchemFSIL95tr_0.5_75.._300..", data = FSIL95_noNA_x1y1)


#x1y2
selected_columns_FSIL95_x1y2 <- c("nbhd_ec_zipFSIL95tr_0.2_1_1.8", "biodchemFSIL95tr_0.5_75.._300..")
FSIL95_noNA_x1y2 <- biod_frame %>%
  select(all_of(selected_columns_FSIL95_x1y2)) %>%
  drop_na()
with(FSIL95_noNA_x1y2, sum(fuzzyand(nbhd_ec_zipFSIL95tr_0.2_1_1.8, biodchemFSIL95tr_0.5_75.._300..))/sum(biodchemFSIL95tr_0.5_75.._300..))
pof("nbhd_ec_zipFSIL95tr_0.2_1_1.8 <- biodchemFSIL95tr_0.5_75.._300..", data = FSIL95_noNA_x1y2)

#x1y3
selected_columns_FSIL95_x1y3 <- c("ec_grp_mem_zipFSIL95tr_", "biodchemFSIL95tr_0.5_75.._300..")
FSIL95_noNA_x1y3 <- biod_frame %>%
  select(all_of(selected_columns_FSIL95_x1y3)) %>%
  drop_na()
with(FSIL95_noNA_x1y3, sum(fuzzyand(ec_grp_mem_zipFSIL95tr_, biodchemFSIL95tr_0.5_75.._300..))/sum(biodchemFSIL95tr_0.5_75.._300..))
pof("ec_grp_mem_zipFSIL95tr_ <- biodchemFSIL95tr_0.5_75.._300..", data = FSIL95_noNA_x1y3)

#----

#x2y1
selected_columns_FSIL95_x2y1 <- c("ec_zipFSIL95tr_0.4_0.9_1.4", "biod10FSIL95tr_2_1200.._2500..")
FSIL95_noNA_x2y1 <- biod_frame %>%
  select(all_of(selected_columns_FSIL95_x2y1)) %>%
  drop_na()
with(FSIL95_noNA_x2y1, sum(fuzzyand(ec_zipFSIL95tr_0.4_0.9_1.4, biod10FSIL95tr_2_1200.._2500..))/sum(biod10FSIL95tr_2_1200.._2500..))
pof("ec_zipFSIL95tr_0.4_0.9_1.4 <- biod10FSIL95tr_2_1200.._2500..", data = FSIL95_noNA_x2y1)

#x2y2
selected_columns_FSIL95_x2y2 <- c("nbhd_ec_zipFSIL95tr_0.2_1_1.8", "biod10FSIL95tr_2_1200.._2500..")
FSIL95_noNA_x2y2 <- biod_frame %>%
  select(all_of(selected_columns_FSIL95_x2y2)) %>%
  drop_na()
with(FSIL95_noNA_x2y2, sum(fuzzyand(nbhd_ec_zipFSIL95tr_0.2_1_1.8, biod10FSIL95tr_2_1200.._2500..))/sum(biod10FSIL95tr_2_1200.._2500..))
pof("nbhd_ec_zipFSIL95tr_0.2_1_1.8 <- biod10FSIL95tr_2_1200.._2500..", data = FSIL95_noNA_x2y2)

#x2y3
selected_columns_FSIL95_x2y3 <- c("ec_grp_mem_zipFSIL95tr_", "biod10FSIL95tr_2_1200.._2500..")
FSIL95_noNA_x2y3 <- biod_frame %>%
  select(all_of(selected_columns_FSIL95_x2y3)) %>%
  drop_na()
with(FSIL95_noNA_x2y3, sum(fuzzyand(ec_grp_mem_zipFSIL95tr_, biod10FSIL95tr_2_1200.._2500..))/sum(biod10FSIL95tr_2_1200.._2500..))
pof("ec_grp_mem_zipFSIL95tr_ <- biod10FSIL95tr_2_1200.._2500..", data = FSIL95_noNA_x2y3)

#x1y1 illustrated solutions
XYplot(FSIL95_noNA_x1y1$biodchemFSIL95tr_0.5_75.._300.., FSIL95_noNA_x1y1$ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x1y1, jitter=TRUE, relation = "necessity")
XYplot(FSIL95_noNA_x1y1$biodchemFSIL95tr_0.5_75.._300.., FSIL95_noNA_x1y1$ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchemFSIL95tr_0.5_75.._300.., ~ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchemFSIL95tr_0.5_75.._300.., ~ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x1y1, jitter=TRUE, relation = "sufficiency")


#x1y2 illustrated solutions
XYplot(FSIL95_noNA_x1y2$biodchemFSIL95tr_0.5_75.._300.., FSIL95_noNA_x1y2$nbhd_ec_zipFSIL95tr_0.2_1_1.8, data = FSIL95_noNA_x1y2, jitter=TRUE, relation = "necessity")
XYplot(FSIL95_noNA_x1y2$biodchemFSIL95tr_0.5_75.._300.., FSIL95_noNA_x1y2$nbhd_ec_zipFSIL95tr_0.2_1_1.8, data = FSIL95_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchemFSIL95tr_0.5_75.._300.., ~nbhd_ec_zipFSIL95tr_0.2_1_1.8, data = FSIL95_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchemFSIL95tr_0.5_75.._300.., ~nbhd_ec_zipFSIL95tr_0.2_1_1.8, data = FSIL95_noNA_x1y2, jitter=TRUE, relation = "sufficiency")

#x1y3 illustrated solutions
XYplot(FSIL95_noNA_x1y3$biodchemFSIL95tr_0.5_75.._300.., FSIL95_noNA_x1y3$ec_grp_mem_zipFSIL95tr_, data = FSIL95_noNA_x1y3, jitter=TRUE, relation = "necessity")
XYplot(FSIL95_noNA_x1y3$biodchemFSIL95tr_0.5_75.._300.., FSIL95_noNA_x1y3$ec_grp_mem_zipFSIL95tr_, data = FSIL95_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchemFSIL95tr_0.5_75.._300.., ~ec_grp_mem_zipFSIL95tr_ , data = FSIL95_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchemFSIL95tr_0.5_75.._300.., ~ec_grp_mem_zipFSIL95tr_, data = FSIL95_noNA_x1y3, jitter=TRUE, relation = "sufficiency")

#x2y1 illustrated solutions
XYplot(FSIL95_noNA_x2y1$biod10FSIL95tr_2_1200.._2500.. , FSIL95_noNA_x2y1$ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x2y1, jitter=TRUE, relation = "necessity")
XYplot(FSIL95_noNA_x2y1$biod10FSIL95tr_2_1200.._2500.., FSIL95_noNA_x2y1$ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10FSIL95tr_2_1200.._2500.., ~ec_zipFSIL95tr_0.4_0.9_1.4 , data = FSIL95_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10FSIL95tr_2_1200.._2500.., ~ec_zipFSIL95tr_0.4_0.9_1.4, data = FSIL95_noNA_x2y1, jitter=TRUE, relation = "sufficiency")

#x2y2 illustrated solutions
XYplot(FSIL95_noNA_x2y2$biod10FSIL95tr_2_1200.._2500.. , FSIL95_noNA_x2y2$nbhd_ec_zipFSIL95tr_0.2_1_1.8 , data = FSIL95_noNA_x2y2, jitter=TRUE, relation = "necessity")
XYplot(FSIL95_noNA_x2y2$biod10FSIL95tr_2_1200.._2500.., FSIL95_noNA_x2y2$nbhd_ec_zipFSIL95tr_0.2_1_1.8 , data = FSIL95_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10FSIL95tr_2_1200.._2500.., ~nbhd_ec_zipFSIL95tr_0.2_1_1.8 , data = FSIL95_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10FSIL95tr_2_1200.._2500.., ~nbhd_ec_zipFSIL95tr_0.2_1_1.8, data = FSIL95_noNA_x2y2, jitter=TRUE, relation = "sufficiency")

#x2y3 illustrated solutions
XYplot(FSIL95_noNA_x2y3$biod10FSIL95tr_2_1200.._2500.. , FSIL95_noNA_x2y3$ec_grp_mem_zipFSIL95tr_ , data = FSIL95_noNA_x2y3, jitter=TRUE, relation = "necessity")
XYplot(FSIL95_noNA_x2y3$biod10FSIL95tr_2_1200.._2500.., FSIL95_noNA_x2y3$ec_grp_mem_zipFSIL95tr_ , data = FSIL95_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10FSIL95tr_2_1200.._2500.., ~ec_grp_mem_zipFSIL95tr_ , data = FSIL95_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10FSIL95tr_2_1200.._2500.., ~ec_grp_mem_zipFSIL95tr_, data = FSIL95_noNA_x2y3, jitter=TRUE, relation = "sufficiency")


#PROBLEM - DOESN'T WORK try out replicating all combinations as superSubset function fails FOR OUTCOME Y1
#BIOD +CHEM
conditionsBIODCHEM <- c("biod10FSIL95tr_2_1200.._2500..", "chemFSIL95tr_1_5_15")
biod_frame_y1_GUI_noNA$combinedconditionsBIODCHEM <- paste (conditionsBIODCHEM, collapse = "&")
combinedres1 <- pof(setms = "N", conditions = "combinedconditionsBIODCHEM", outcome = ec_zipFSIL95tr_0.4_0.9_1.4, data = biod_frame_y1_GUI_noNA)
print (combinedres1)
#BIOD + CHEM
#BIODCHEM + CHEM
#BIOD + ~CHEM
#BIOD + CHEM + BIODCHEM
#

###
#FSIE95 ECDF FUNCTION-CALIBRATED VALUES VIA GRAPGICAL USER INTERFACE GUI
###

#x1y1
selected_columns_FSIE95_x1y1 <- c("ec_zipFSIE95tr_0.4_0.9_1.4", "biodchemFSIE95tr_0.5_75.._300..")
FSIE95_noNA_x1y1 <- biod_frame %>%
  select(all_of(selected_columns_FSIE95_x1y1)) %>%
  drop_na()
with(FSIE95_noNA_x1y1, sum(fuzzyand(ec_zipFSIE95tr_0.4_0.9_1.4, biodchemFSIE95tr_0.5_75.._300..))/sum(biodchemFSIE95tr_0.5_75.._300..))
pof("ec_zipFSIE95tr_0.4_0.9_1.4 <- biodchemFSIE95tr_0.5_75.._300..", data = FSIE95_noNA_x1y1)

#x1y2
selected_columns_FSIE95_x1y2 <- c("nbhd_ec_zipFSIE95tr_0.2_1_1.8", "biodchemFSIE95tr_0.5_75.._300..")
FSIE95_noNA_x1y2 <- biod_frame %>%
  select(all_of(selected_columns_FSIE95_x1y2)) %>%
  drop_na()
with(FSIE95_noNA_x1y2, sum(fuzzyand(nbhd_ec_zipFSIE95tr_0.2_1_1.8, biodchemFSIE95tr_0.5_75.._300..))/sum(biodchemFSIE95tr_0.5_75.._300..))
pof("nbhd_ec_zipFSIE95tr_0.2_1_1.8 <- biodchemFSIE95tr_0.5_75.._300..", data = FSIE95_noNA_x1y2)      #sufficiency

#x1y3
selected_columns_FSIE95_x1y3 <- c("ec_grp_mem_zipFSIE95tr_", "biodchemFSIE95tr_0.5_75.._300..")
FSIE95_noNA_x1y3 <- biod_frame %>%
  select(all_of(selected_columns_FSIE95_x1y3)) %>%
  drop_na()
with(FSIE95_noNA_x1y3, sum(fuzzyand(ec_grp_mem_zipFSIE95tr_, biodchemFSIE95tr_0.5_75.._300..))/sum(biodchemFSIE95tr_0.5_75.._300..))
pof("ec_grp_mem_zipFSIE95tr_ <- biodchemFSIE95tr_0.5_75.._300..", data = FSIE95_noNA_x1y3)

#----

#x2y1
selected_columns_FSIE95_x2y1 <- c("ec_zipFSIE95tr_0.4_0.9_1.4", "biod10FSIE95tr_2_1200.._2500..")
FSIE95_noNA_x2y1 <- biod_frame %>%
  select(all_of(selected_columns_FSIE95_x2y1)) %>%
  drop_na()
with(FSIE95_noNA_x2y1, sum(fuzzyand(ec_zipFSIE95tr_0.4_0.9_1.4, biod10FSIE95tr_2_1200.._2500..))/sum(biod10FSIE95tr_2_1200.._2500..))
pof("ec_zipFSIE95tr_0.4_0.9_1.4 <- biod10FSIE95tr_2_1200.._2500..", data = FSIE95_noNA_x2y1)

#x2y2
selected_columns_FSIE95_x2y2 <- c("nbhd_ec_zipFSIE95tr_0.2_1_1.8", "biod10FSIE95tr_2_1200.._2500..")
FSIE95_noNA_x2y2 <- biod_frame %>%
  select(all_of(selected_columns_FSIE95_x2y2)) %>%
  drop_na()
pof("nbhd_ec_zipFSIE95tr_0.2_1_1.8 <- biod10FSIE95tr_2_1200.._2500..", data = FSIE95_noNA_x2y2)

#x2y3
selected_columns_FSIE95_x2y3 <- c("ec_grp_mem_zipFSIE95tr_", "biod10FSIE95tr_2_1200.._2500..")
FSIE95_noNA_x2y3 <- biod_frame %>%
  select(all_of(selected_columns_FSIE95_x2y3)) %>%
  drop_na()
with(FSIE95_noNA_x2y3, sum(fuzzyand(ec_grp_mem_zipFSIE95tr_, biod10FSIE95tr_2_1200.._2500..))/sum(biod10FSIE95tr_2_1200.._2500..))
pof("ec_grp_mem_zipFSIE95tr_ <- biod10FSIE95tr_2_1200.._2500..", data = FSIE95_noNA_x2y3)

#FSIE95 x1y1 illustrated solutions
XYplot(FSIE95_noNA_x1y1$biodchemFSIE95tr_0.5_75.._300.., FSIE95_noNA_x1y1$ec_zipFSIE95tr_0.4_0.9_1.4, data = FSIE95_noNA_x1y1, jitter=TRUE, relation = "necessity")
XYplot(FSIE95_noNA_x1y1$biodchemFSIE95tr_0.5_75.._300.., FSIE95_noNA_x1y1$ec_zipFSIE95tr_0.4_0.9_1.4, data = FSIE95_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
  #negation
  XYplot(biodchemFSIE95tr_0.5_75.._300.., ~ec_zipFSIE95tr_0.4_0.9_1.4 , data = FSIE95_noNA_x1y1, jitter=TRUE, relation = "necessity")
  XYplot(biodchemFSIE95tr_0.5_75.._300.., ~ec_zipFSIE95tr_0.4_0.9_1.4 , data = FSIE95_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
  

#FSIE95x1y2 illustrated solutions
XYplot(FSIE95_noNA_x1y2$biodchemFSIE95tr_0.5_75.._300.., FSIE95_noNA_x1y2$nbhd_ec_zipFSIE95tr_0.2_1_1.8, data = FSIE95_noNA_x1y2, jitter=TRUE, relation = "necessity")
XYplot(FSIE95_noNA_x1y2$biodchemFSIE95tr_0.5_75.._300.., FSIE95_noNA_x1y2$nbhd_ec_zipFSIE95tr_0.2_1_1.8, data = FSIE95_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
  #negation
  XYplot(biodchemFSIE95tr_0.5_75.._300.., ~nbhd_ec_zipFSIE95tr_0.2_1_1.8 , data = FSIE95_noNA_x1y2, jitter=TRUE, relation = "necessity")
  XYplot(biodchemFSIE95tr_0.5_75.._300.., ~nbhd_ec_zipFSIE95tr_0.2_1_1.8 , data = FSIE95_noNA_x1y2, jitter=TRUE, relation = "sufficiency")

#FSIE95 x1y3 illustrated solutions
XYplot(FSIE95_noNA_x1y3$biodchemFSIE95tr_0.5_75.._300.., FSIE95_noNA_x1y3$ec_grp_mem_zipFSIE95tr_, data = FSIE95_noNA_x1y3, jitter=TRUE, relation = "necessity")
XYplot(FSIE95_noNA_x1y3$biodchemFSIE95tr_0.5_75.._300.., FSIE95_noNA_x1y3$ec_grp_mem_zipFSIE95tr_, data = FSIE95_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchemFSIE95tr_0.5_75.._300.., ~ec_grp_mem_zipFSIE95tr_ , data = FSIE95_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchemFSIE95tr_0.5_75.._300.., ~ec_grp_mem_zipFSIE95tr_ , data = FSIE95_noNA_x1y3, jitter=TRUE, relation = "sufficiency")

#FSIE95 x2y1 illustrated solutions
XYplot(FSIE95_noNA_x2y1$biod10FSIE95tr_2_1200.._2500.. , FSIE95_noNA_x2y1$ec_zipFSIE95tr_0.4_0.9_1.4, data = FSIE95_noNA_x2y1, jitter=TRUE, relation = "necessity")
XYplot(FSIE95_noNA_x2y1$biod10FSIE95tr_2_1200.._2500.., FSIE95_noNA_x2y1$ec_zipFSIE95tr_0.4_0.9_1.4, data = FSIE95_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10FSIE95tr_2_1200.._2500.., ~ec_zipFSIE95tr_0.4_0.9_1.4 , data = FSIE95_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10FSIE95tr_2_1200.._2500.., ~ec_zipFSIE95tr_0.4_0.9_1.4 , data = FSIE95_noNA_x2y1, jitter=TRUE, relation = "sufficiency")

#FSIE95 x2y2 illustrated solutions
XYplot(FSIE95_noNA_x2y2$biod10FSIE95tr_2_1200.._2500.. , FSIE95_noNA_x2y2$nbhd_ec_zipFSIE95tr_0.2_1_1.8 , data = FSIE95_noNA_x2y2, jitter=TRUE, relation = "necessity")
XYplot(FSIE95_noNA_x2y2$biod10FSIE95tr_2_1200.._2500.., FSIE95_noNA_x2y2$nbhd_ec_zipFSIE95tr_0.2_1_1.8 , data = FSIE95_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10FSIE95tr_2_1200.._2500.., ~nbhd_ec_zipFSIE95tr_0.2_1_1.8 , data = FSIE95_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10FSIE95tr_2_1200.._2500.., ~nbhd_ec_zipFSIE95tr_0.2_1_1.8 , data = FSIE95_noNA_x2y2, jitter=TRUE, relation = "sufficiency")

#FSIE95 x2y3 illustrated solutions
XYplot(FSIE95_noNA_x2y3$biod10FSIE95tr_2_1200.._2500.. , FSIE95_noNA_x2y3$ec_grp_mem_zipFSIE95tr_ , data = FSIE95_noNA_x2y3, jitter=TRUE, relation = "necessity")
XYplot(FSIE95_noNA_x2y3$biod10FSIL95tr_2_1200.._2500.., FSIL95_noNA_x2y3$ec_grp_mem_zipFSIL95tr_ , data = FSIL95_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10FSIE95tr_2_1200.._2500.., ~ec_grp_mem_zipFSIE95tr_ , data = FSIE95_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10FSIE95tr_2_1200.._2500.., ~ec_grp_mem_zipFSIE95tr_ , data = FSIE95_noNA_x2y3, jitter=TRUE, relation = "sufficiency")

#####################################
###
#FSIL_COM_EUCL
###
    #x1y1
    selected_columns_FSIL_COM_EUCL_x1y1 <- c("ec_zip_FSIL_COM_EUCL", "biodchem_FSIL_COM_EUCL")
    FSIL_COM_EUCL_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_EUCL_x1y1)) %>%
      drop_na()
    pof("ec_zip_FSIL_COM_EUCL <- biodchem_FSIL_COM_EUCL", data = FSIL_COM_EUCL_noNA_x1y1)
    
    
    #x1y2
    selected_columns_FSIL_COM_EUCL_x1y2 <- c("nbhd_ec_zip_FSIL_COM_EUCL", "biodchem_FSIL_COM_EUCL")
    FSIL_COM_EUCL_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_EUCL_x1y2)) %>%
      drop_na()
    pof("nbhd_ec_zip_FSIL_COM_EUCL <- biodchem_FSIL_COM_EUCL", data = FSIL_COM_EUCL_noNA_x1y2)
    
    #x1y3
    selected_columns_FSIL_COM_EUCL_x1y3 <- c("ec_grp_mem_zip_FSIL_COM_EUCL", "biodchem_FSIL_COM_EUCL")
    FSIL_COM_EUCL_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_EUCL_x1y3)) %>%
      drop_na()
    pof("ec_grp_mem_zip_FSIL_COM_EUCL <- biodchem_FSIL_COM_EUCL", data = FSIL_COM_EUCL_noNA_x1y3)
    
    #----
    
    #x2y1
    selected_columns_FSIL_COM_EUCL_x2y1 <- c("ec_zip_FSIL_COM_EUCL", "biod10_FSIL_COM_EUCL")
    FSIL_COM_EUCL_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_EUCL_x2y1)) %>%
      drop_na()
    pof("ec_zip_FSIL_COM_EUCL <- biod10_FSIL_COM_EUCL", data = FSIL_COM_EUCL_noNA_x2y1)
    
    #x2y2
    selected_columns_FSIL_COM_EUCL_x2y2 <- c("nbhd_ec_zip_FSIL_COM_EUCL", "biod10_FSIL_COM_EUCL")
    FSIL_COM_EUCL_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_EUCL_x2y2)) %>%
      drop_na()
    pof("nbhd_ec_zip_FSIL_COM_EUCL <- biod10_FSIL_COM_EUCL", data = FSIL_COM_EUCL_noNA_x2y2)
    
    #x2y3
    selected_columns_FSIL_COM_EUCL_x2y3 <- c("ec_grp_mem_zip_FSIL_COM_EUCL", "biod10_FSIL_COM_EUCL")
    FSIL_COM_EUCL_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_EUCL_x2y3)) %>%
      drop_na()
    pof("ec_grp_mem_zip_FSIL_COM_EUCL <- biod10_FSIL_COM_EUCL", data = FSIL_COM_EUCL_noNA_x2y3)
    
    #x1y1 illustrated solutions
    XYplot(FSIL_COM_EUCL_noNA_x1y1$biodchem_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x1y1$ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_EUCL_noNA_x1y1$biodchem_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x1y1$ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation PROBLEM
    XYplot(biodchem_FSIL_COM_EUCL, ~ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_EUCL, ~ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y2 illustrated solutions
    XYplot(FSIL_COM_EUCL_noNA_x1y2$biodchem_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x1y2$nbhd_ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_EUCL_noNA_x1y2$biodchem_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x1y2$nbhd_ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation

    

    #x1y3 illustrated solutions
    XYplot(FSIL_COM_EUCL_noNA_x1y3$biodchem_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x1y3$ec_grp_mem_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_EUCL_noNA_x1y3$biodchem_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x1y3$ec_grp_mem_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_EUCL, ~ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_EUCL, ~ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    #x2y1 illustrated solutions
    XYplot(FSIL_COM_EUCL_noNA_x2y1$biod10_FSIL_COM_EUCL , FSIL_COM_EUCL_noNA_x2y1$ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_EUCL_noNA_x2y1$biod10_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x2y1$ec_zip_FSIL_COM_EUCL, data = FSIL_COM_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    
    #x2y2 illustrated solutions
    XYplot(FSIL_COM_EUCL_noNA_x2y2$biod10_FSIL_COM_EUCL , FSIL_COM_EUCL_noNA_x2y2$nbhd_ec_zip_FSIL_COM_EUCL , data = FSIL_COM_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_EUCL_noNA_x2y2$biod10_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x2y2$nbhd_ec_zip_FSIL_COM_EUCL , data = FSIL_COM_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    
    #x2y3 illustrated solutions
    XYplot(FSIL_COM_EUCL_noNA_x2y3$biod10_FSIL_COM_EUCL , FSIL_COM_EUCL_noNA_x2y3$ec_grp_mem_zip_FSIL_COM_EUCL , data = FSIL_COM_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_EUCL_noNA_x2y3$biod10_FSIL_COM_EUCL, FSIL_COM_EUCL_noNA_x2y3$ec_grp_mem_zip_FSIL_COM_EUCL , data = FSIL_COM_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    
    #####################################
    ###
    #FSIL_COM_MAN
    ###
    #x1y1
    selected_columns_FSIL_COM_MAN_x1y1 <- c("ec_zip_FSIL_COM_MAN", "biodchem_FSIL_COM_MAN")
    FSIL_COM_MAN_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAN_x1y1)) %>%
      drop_na()
    pof("ec_zip_FSIL_COM_MAN <- biodchem_FSIL_COM_MAN", data = FSIL_COM_MAN_noNA_x1y1)
    
    
    #x1y2
    selected_columns_FSIL_COM_MAN_x1y2 <- c("nbhd_ec_zip_FSIL_COM_MAN", "biodchem_FSIL_COM_MAN")
    FSIL_COM_MAN_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAN_x1y2)) %>%
      drop_na()
    pof("nbhd_ec_zip_FSIL_COM_MAN <- biodchem_FSIL_COM_MAN", data = FSIL_COM_MAN_noNA_x1y2)
    
    #x1y3
    selected_columns_FSIL_COM_MAN_x1y3 <- c("ec_grp_mem_zip_FSIL_COM_MAN", "biodchem_FSIL_COM_MAN")
    FSIL_COM_MAN_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAN_x1y3)) %>%
      drop_na()
    pof("ec_grp_mem_zip_FSIL_COM_MAN <- biodchem_FSIL_COM_MAN", data = FSIL_COM_MAN_noNA_x1y3)
    
    #----
    
    #x2y1
    selected_columns_FSIL_COM_MAN_x2y1 <- c("ec_zip_FSIL_COM_MAN", "biod10_FSIL_COM_MAN")
    FSIL_COM_MAN_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAN_x2y1)) %>%
      drop_na()
    pof("ec_zip_FSIL_COM_MAN <- biod10_FSIL_COM_MAN", data = FSIL_COM_MAN_noNA_x2y1)
    
    #x2y2
    selected_columns_FSIL_COM_MAN_x2y2 <- c("nbhd_ec_zip_FSIL_COM_MAN", "biod10_FSIL_COM_MAN")
    FSIL_COM_MAN_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAN_x2y2)) %>%
      drop_na()
    pof("nbhd_ec_zip_FSIL_COM_MAN <- biod10_FSIL_COM_MAN", data = FSIL_COM_MAN_noNA_x2y2)
    
    #x2y3
    selected_columns_FSIL_COM_MAN_x2y3 <- c("ec_grp_mem_zip_FSIL_COM_MAN", "biod10_FSIL_COM_MAN")
    FSIL_COM_MAN_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAN_x2y3)) %>%
      drop_na()
    pof("ec_grp_mem_zip_FSIL_COM_MAN <- biod10_FSIL_COM_MAN", data = FSIL_COM_MAN_noNA_x2y3)
    
    #x1y1 illustrated solutions
    XYplot(FSIL_COM_MAN_noNA_x1y1$biodchem_FSIL_COM_MAN, FSIL_COM_MAN_noNA_x1y1$ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_MAN_noNA_x1y1$biodchem_FSIL_COM_MAN, FSIL_COM_MAN_noNA_x1y1$ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MAN, ~ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAN, ~ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y2 illustrated solutions
    XYplot(FSIL_COM_MAN_noNA_x1y2$biodchem_FSIL_COM_MAN, FSIL_COM_MAN_noNA_x1y2$nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_MAN_noNA_x1y2$biodchem_FSIL_COM_MAN, FSIL_COM_MAN_noNA_x1y2$nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MAN, ~nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAN, ~nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    
    
    #x1y3 illustrated solutions
    XYplot(biodchem_FSIL_COM_MAN, ec_grp_mem_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAN, ec_grp_mem_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MAN, ~ec_grp_mem_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAN, ~ec_grp_mem_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y1 illustrated solutions
    XYplot(biod10_FSIL_COM_MAN, ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAN, ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MAN, ~ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAN, ~ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y2 illustrated solutions
    XYplot(biod10_FSIL_COM_MAN , nbhd_ec_zip_FSIL_COM_MAN , data = FSIL_COM_MAN_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAN, nbhd_ec_zip_FSIL_COM_MAN , data = FSIL_COM_MAN_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MAN, ~nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAN, ~nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y3 illustrated solutions
    XYplot(biod10_FSIL_COM_MAN , ec_grp_mem_zip_FSIL_COM_MAN , data = FSIL_COM_MAN_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAN, ec_grp_mem_zip_FSIL_COM_MAN , data = FSIL_COM_MAN_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MAN , ~ec_grp_mem_zip_FSIL_COM_MAN , data = FSIL_COM_MAN_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAN, ~ec_grp_mem_zip_FSIL_COM_MAN , data = FSIL_COM_MAN_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
    #####################################
    ###
    #FSIL_COM_MAX
    ###
    #x1y1
    selected_columns_FSIL_COM_MAX_x1y1 <- c("ec_zip_FSIL_COM_MAX", "biodchem_FSIL_COM_MAX")
    FSIL_COM_MAX_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAX_x1y1)) %>%
      drop_na()

    #x1y2
    selected_columns_FSIL_COM_MAX_x1y2 <- c("nbhd_ec_zip_FSIL_COM_MAX", "biodchem_FSIL_COM_MAX")
    FSIL_COM_MAX_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAX_x1y2)) %>%
      drop_na()
 
    #x1y3
    selected_columns_FSIL_COM_MAX_x1y3 <- c("ec_grp_mem_zip_FSIL_COM_MAX", "biodchem_FSIL_COM_MAX")
    FSIL_COM_MAX_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAX_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_COM_MAX_x2y1 <- c("ec_zip_FSIL_COM_MAX", "biod10_FSIL_COM_MAX")
    FSIL_COM_MAX_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAX_x2y1)) %>%
      drop_na()

    #x2y2
    selected_columns_FSIL_COM_MAX_x2y2 <- c("nbhd_ec_zip_FSIL_COM_MAX", "biod10_FSIL_COM_MAX")
    FSIL_COM_MAX_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAX_x2y2)) %>%
      drop_na()

    #x2y3
    selected_columns_FSIL_COM_MAX_x2y3 <- c("ec_grp_mem_zip_FSIL_COM_MAX", "biod10_FSIL_COM_MAX")
    FSIL_COM_MAX_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MAX_x2y3)) %>%
      drop_na()

    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_COM_MAX <- biodchem_FSIL_COM_MAX", data = FSIL_COM_MAX_noNA_x1y1)
    XYplot(biodchem_FSIL_COM_MAX, ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAX, ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MAX, ~ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAX, ~ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y1, jitter=TRUE, relation = "sufficiency")

    
    #x1y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_COM_MAX <- biodchem_FSIL_COM_MAX", data = FSIL_COM_MAX_noNA_x1y2)
    XYplot(biodchem_FSIL_COM_MAX, nbhd_ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_COM_MAN_noNA_x1y2$biodchem_FSIL_COM_MAN, FSIL_COM_MAN_noNA_x1y2$nbhd_ec_zip_FSIL_COM_MAN, data = FSIL_COM_MAN_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MAX, ~nbhd_ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAX, ~nbhd_ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_COM_MAX <- biodchem_FSIL_COM_MAX", data = FSIL_COM_MAX_noNA_x1y3)
    XYplot(biodchem_FSIL_COM_MAX, ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAX, ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MAX, ~ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MAX, ~ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIL_COM_MAX <- biod10_FSIL_COM_MAX", data = FSIL_COM_MAX_noNA_x2y1)
    XYplot(biod10_FSIL_COM_MAX, ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAX, ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MAX, ~ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAX, ~ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_COM_MAX <- biod10_FSIL_COM_MAX", data = FSIL_COM_MAX_noNA_x2y2)
    XYplot(biod10_FSIL_COM_MAX,nbhd_ec_zip_FSIL_COM_MAX , data = FSIL_COM_MAX_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAX,nbhd_ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MAX, ~nbhd_ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAX, ~nbhd_ec_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_COM_MAX <- biod10_FSIL_COM_MAX", data = FSIL_COM_MAX_noNA_x2y3)
    XYplot(biod10_FSIL_COM_MAX , ec_grp_mem_zip_FSIL_COM_MAX , data = FSIL_COM_MAX_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAX, ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MAX, ~ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MAX, ~ec_grp_mem_zip_FSIL_COM_MAX, data = FSIL_COM_MAX_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
#####################################
    ###
    #FSIL_COM_MINK
    ###
    #x1y1
    selected_columns_FSIL_COM_MINK_x1y1 <- c("ec_zip_FSIL_COM_MINK", "biodchem_FSIL_COM_MINK")
    FSIL_COM_MINK_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MINK_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_COM_MINK_x1y2 <- c("nbhd_ec_zip_FSIL_COM_MINK", "biodchem_FSIL_COM_MINK")
    FSIL_COM_MINK_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MINK_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_COM_MINK_x1y3 <- c("ec_grp_mem_zip_FSIL_COM_MINK", "biodchem_FSIL_COM_MINK")
    FSIL_COM_MINK_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MINK_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_COM_MINK_x2y1 <- c("ec_zip_FSIL_COM_MINK", "biod10_FSIL_COM_MINK")
    FSIL_COM_MINK_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MINK_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_COM_MINK_x2y2 <- c("nbhd_ec_zip_FSIL_COM_MINK", "biod10_FSIL_COM_MINK")
    FSIL_COM_MINK_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MINK_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_COM_MINK_x2y3 <- c("ec_grp_mem_zip_FSIL_COM_MINK", "biod10_FSIL_COM_MINK")
    FSIL_COM_MINK_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_COM_MINK_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_COM_MINK <- biodchem_FSIL_COM_MINK", data = FSIL_COM_MINK_noNA_x1y1)
    XYplot(biodchem_FSIL_COM_MINK, ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MINK, ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MINK, ~ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MINK, ~ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_COM_MINK <- biodchem_FSIL_COM_MINK", data = FSIL_COM_MINK_noNA_x1y2)
    XYplot(biodchem_FSIL_COM_MINK, nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MINK, nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MINK, ~nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MINK, ~nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_COM_MINK <- biodchem_FSIL_COM_MINK", data = FSIL_COM_MINK_noNA_x1y3)
    XYplot(biodchem_FSIL_COM_MINK, ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MINK, ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_COM_MINK, ~ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_COM_MINK, ~ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIL_COM_MINK <- biod10_FSIL_COM_MINK", data = FSIL_COM_MINK_noNA_x2y1)
    XYplot(biod10_FSIL_COM_MINK, ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MINK, ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MINK, ~ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MINK, ~ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_COM_MINK <- biod10_FSIL_COM_MINK", data = FSIL_COM_MINK_noNA_x2y2)
    XYplot(biod10_FSIL_COM_MINK,nbhd_ec_zip_FSIL_COM_MINK , data = FSIL_COM_MINK_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MINK,nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MINK, ~nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MINK, ~nbhd_ec_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_COM_MINK <- biod10_FSIL_COM_MINK", data = FSIL_COM_MINK_noNA_x2y3)
    XYplot(biod10_FSIL_COM_MINK , ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MINK, ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_COM_MINK, ~ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_COM_MINK, ~ec_grp_mem_zip_FSIL_COM_MINK, data = FSIL_COM_MINK_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
#####################################
    
###
#FSIL_WARD_EUCL
###
    #x1y1
    selected_columns_FSIL_WARD_EUCL_x1y1 <- c("ec_zip_FSIL_WARD_EUCL", "biodchem_FSIL_WARD_EUCL")
    FSIL_WARD_EUCL_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_EUCL_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_WARD_EUCL_x1y2 <- c("nbhd_ec_zip_FSIL_WARD_EUCL", "biodchem_FSIL_WARD_EUCL")
    FSIL_WARD_EUCL_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_EUCL_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_WARD_EUCL_x1y3 <- c("ec_grp_mem_zip_FSIL_WARD_EUCL", "biodchem_FSIL_WARD_EUCL")
    FSIL_WARD_EUCL_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_EUCL_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_WARD_EUCL_x2y1 <- c("ec_zip_FSIL_WARD_EUCL", "biod10_FSIL_WARD_EUCL")
    FSIL_WARD_EUCL_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_EUCL_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_WARD_EUCL_x2y2 <- c("nbhd_ec_zip_FSIL_WARD_EUCL", "biod10_FSIL_WARD_EUCL")
    FSIL_WARD_EUCL_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_EUCL_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_WARD_EUCL_x2y3 <- c("ec_grp_mem_zip_FSIL_WARD_EUCL", "biod10_FSIL_WARD_EUCL")
    FSIL_WARD_EUCL_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_EUCL_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_WARD_EUCL <- biodchem_FSIL_WARD_EUCL", data = FSIL_WARD_EUCL_noNA_x1y1)
    XYplot(biodchem_FSIL_WARD_EUCL, ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD_EUCL, ~ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, ~ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_WARD_EUCL <- biodchem_FSIL_WARD_EUCL", data = FSIL_WARD_EUCL_noNA_x1y2)
    XYplot(biodchem_FSIL_WARD_EUCL, nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD_EUCL, ~nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, ~nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_WARD_EUCL <- biodchem_FSIL_WARD_EUCL", data = FSIL_WARD_EUCL_noNA_x1y3)
    XYplot(biodchem_FSIL_WARD_EUCL, ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD_EUCL, ~ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, ~ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIL_WARD_EUCL <- biod10_FSIL_WARD_EUCL", data = FSIL_WARD_EUCL_noNA_x2y1)
    XYplot(biod10_FSIL_WARD_EUCL, ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD_EUCL, ~ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ~ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_WARD_EUCL <- biod10_FSIL_WARD_EUCL", data = FSIL_WARD_EUCL_noNA_x2y2)
    XYplot(biod10_FSIL_WARD_EUCL,nbhd_ec_zip_FSIL_WARD_EUCL , data = FSIL_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL,nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD_EUCL, ~nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ~nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_WARD_EUCL <- biod10_FSIL_WARD_EUCL", data = FSIL_WARD_EUCL_noNA_x2y3)
    XYplot(biod10_FSIL_WARD_EUCL , ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD_EUCL, ~ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ~ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
####################################
    ###
    #FSIL_WARD_MAN
    ###
    #x1y1
    selected_columns_FSIL_WARD_MAN_x1y1 <- c("ec_zip_FSIL_WARD_MAN", "biodchem_FSIL_WARD_MAN")
    FSIL_WARD_MAN_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_MAN_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_WARD_MAN_x1y2 <- c("nbhd_ec_zip_FSIL_WARD_MAN", "biodchem_FSIL_WARD_MAN")
    FSIL_WARD_MAN_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_MAN_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_WARD_MAN_x1y3 <- c("ec_grp_mem_zip_FSIL_WARD_MAN", "biodchem_FSIL_WARD_MAN")
    FSIL_WARD_MAN_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_MAN_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_WARD_MAN_x2y1 <- c("ec_zip_FSIL_WARD_MAN", "biod10_FSIL_WARD_MAN")
    FSIL_WARD_MAN_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_MAN_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_WARD_MAN_x2y2 <- c("nbhd_ec_zip_FSIL_WARD_MAN", "biod10_FSIL_WARD_MAN")
    FSIL_WARD_MAN_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_MAN_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_WARD_MAN_x2y3 <- c("ec_grp_mem_zip_FSIL_WARD_MAN", "biod10_FSIL_WARD_MAN")
    FSIL_WARD_MAN_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD_MAN_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_WARD_MAN <- biodchem_FSIL_WARD_MAN", data = FSIL_WARD_MAN_noNA_x1y1)
    XYplot(biodchem_FSIL_WARD_MAN, ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_MAN, ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD_MAN, ~ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_MAN, ~ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #discontinued WARD _MAN, _MAX, _MINK

#####################################

###
#FSIE_WARD_EUCL
###
    #x1y1
    selected_columns_FSIE_WARD_EUCL_x1y1 <- c("ec_zip_FSIE_WARD_EUCL", "biodchem_FSIE_WARD_EUCL")
    FSIE_WARD_EUCL_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIE_WARD_EUCL_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIE_WARD_EUCL_x1y2 <- c("nbhd_ec_zip_FSIE_WARD_EUCL", "biodchem_FSIE_WARD_EUCL")
    FSIE_WARD_EUCL_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIE_WARD_EUCL_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIE_WARD_EUCL_x1y3 <- c("ec_grp_mem_zip_FSIE_WARD_EUCL", "biodchem_FSIE_WARD_EUCL")
    FSIE_WARD_EUCL_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIE_WARD_EUCL_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIE_WARD_EUCL_x2y1 <- c("ec_zip_FSIE_WARD_EUCL", "biod10_FSIE_WARD_EUCL")
    FSIE_WARD_EUCL_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIE_WARD_EUCL_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIE_WARD_EUCL_x2y2 <- c("nbhd_ec_zip_FSIE_WARD_EUCL", "biod10_FSIE_WARD_EUCL")
    FSIE_WARD_EUCL_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIE_WARD_EUCL_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIE_WARD_EUCL_x2y3 <- c("ec_grp_mem_zip_FSIE_WARD_EUCL", "biod10_FSIE_WARD_EUCL")
    FSIE_WARD_EUCL_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIE_WARD_EUCL_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIE_WARD_EUCL <- biodchem_FSIE_WARD_EUCL", data = FSIE_WARD_EUCL_noNA_x1y1)
    XYplot(FSIE_WARD_EUCL_noNA_x1y1$biodchem_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x1y1$ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIE_WARD_EUCL, ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD_EUCL, ~ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_EUCL, ~ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y2 illustrated solutions
    pof("nbhd_ec_zip_FSIE_WARD_EUCL <- biodchem_FSIE_WARD_EUCL", data = FSIE_WARD_EUCL_noNA_x1y2)
    XYplot(FSIE_WARD_EUCL_noNA_x1y2$biodchem_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x1y2$nbhd_ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIE_WARD_EUCL_noNA_x1y2$biodchem_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x1y2$nbhd_ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIE_WARD_EUCL, ~nbhd_ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIE_WARD_EUCL, ~nbhd_ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIE_WARD_EUCL <- biodchem_FSIE_WARD_EUCL", data = FSIE_WARD_EUCL_noNA_x1y3)
    XYplot(FSIE_WARD_EUCL_noNA_x1y3$biodchem_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x1y3$ec_grp_mem_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(FSIE_WARD_EUCL_noNA_x1y3$biodchem_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x1y3$ec_grp_mem_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIE_WARD_EUCL, ~ec_grp_mem_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIE_WARD_EUCL, ~ec_grp_mem_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIE_WARD_EUCL <- biod10_FSIE_WARD_EUCL", data = FSIE_WARD_EUCL_noNA_x2y1)
    XYplot(FSIE_WARD_EUCL_noNA_x2y1$biod10_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x2y1$ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIE_WARD_EUCL_noNA_x2y1$biod10_FSIE_WARD_EUCL, FSIE_WARD_EUCL_noNA_x2y1$ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIE_WARD_EUCL, ~ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIE_WARD_EUCL, ~ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIE_WARD_EUCL <- biod10_FSIE_WARD_EUCL", data = FSIE_WARD_EUCL_noNA_x2y2)
    XYplot(FSIE_WARD_EUCL_noNA_x2y2$biod10_FSIE_WARD_EUCL,FSIE_WARD_EUCL_noNA_x2y2$nbhd_ec_zip_FSIE_WARD_EUCL , data = FSIE_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIE_WARD_EUCL_noNA_x2y2$biod10_FSIE_WARD_EUCL,FSIE_WARD_EUCL_noNA_x2y2$nbhd_ec_zip_FSIE_WARD_EUCL, data = FSIE_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD_EUCL, ~nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ~nbhd_ec_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIE_WARD_EUCL <- biod10_FSIE_WARD_EUCL", data = FSIE_WARD_EUCL_noNA_x2y3)
    XYplot(biod10_FSIL_WARD_EUCL , ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD_EUCL, ~ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD_EUCL, ~ec_grp_mem_zip_FSIL_WARD_EUCL, data = FSIL_WARD_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
####################################
    
###
#FSIL_WARD2_EUCL
###
    #x1y1
    selected_columns_FSIL_WARD2_EUCL_x1y1 <- c("ec_zip_FSIL_WARD2_EUCL", "biodchem_FSIL_WARD2_EUCL")
    FSIL_WARD2_EUCL_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_EUCL_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_WARD2_EUCL_x1y2 <- c("nbhd_ec_zip_FSIL_WARD2_EUCL", "biodchem_FSIL_WARD2_EUCL")
    FSIL_WARD2_EUCL_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_EUCL_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_WARD2_EUCL_x1y3 <- c("ec_grp_mem_zip_FSIL_WARD2_EUCL", "biodchem_FSIL_WARD2_EUCL")
    FSIL_WARD2_EUCL_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_EUCL_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_WARD2_EUCL_x2y1 <- c("ec_zip_FSIL_WARD2_EUCL", "biod10_FSIL_WARD2_EUCL")
    FSIL_WARD2_EUCL_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_EUCL_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_WARD2_EUCL_x2y2 <- c("nbhd_ec_zip_FSIL_WARD2_EUCL", "biod10_FSIL_WARD2_EUCL")
    FSIL_WARD2_EUCL_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_EUCL_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_WARD2_EUCL_x2y3 <- c("ec_grp_mem_zip_FSIL_WARD2_EUCL", "biod10_FSIL_WARD2_EUCL")
    FSIL_WARD2_EUCL_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_EUCL_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_WARD2_EUCL <- biodchem_FSIL_WARD2_EUCL", data = FSIL_WARD2_EUCL_noNA_x1y1)
    XYplot(biodchem_FSIL_WARD2_EUCL, ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD2_EUCL, ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD2_EUCL, ~ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD2_EUCL, ~ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #x1y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_WARD2_EUCL <- biodchem_FSIL_WARD2_EUCL", data = FSIL_WARD2_EUCL_noNA_x1y2)
    XYplot(biodchem_FSIL_WARD2_EUCL, nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD2_EUCL, nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD2_EUCL, ~nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD2_EUCL, ~nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_WARD2_EUCL <- biodchem_FSIL_WARD2_EUCL", data = FSIL_WARD2_EUCL_noNA_x1y3)
    XYplot(biodchem_FSIL_WARD2_EUCL, ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD2_EUCL, ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD2_EUCL, ~ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD2_EUCL, ~ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIL_WARD2_EUCL <- biod10_FSIL_WARD2_EUCL", data = FSIL_WARD2_EUCL_noNA_x2y1)
    XYplot(biod10_FSIL_WARD2_EUCL, ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD2_EUCL, ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD2_EUCL, ~ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD2_EUCL, ~ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_WARD2_EUCL <- biod10_FSIL_WARD2_EUCL", data = FSIL_WARD2_EUCL_noNA_x2y2)
    XYplot(biod10_FSIL_WARD2_EUCL, nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD2_EUCL, nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD2_EUCL, ~nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD2_EUCL, ~nbhd_ec_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_WARD2_EUCL <- biod10_FSIL_WARD2_EUCL", data = FSIL_WARD2_EUCL_noNA_x2y3)
    XYplot(biod10_FSIL_WARD2_EUCL, ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD2_EUCL, ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_WARD2_EUCL, ~ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_WARD2_EUCL, ~ec_grp_mem_zip_FSIL_WARD2_EUCL, data = FSIL_WARD2_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
    
###################################################
    
###
#FSIL_WARD2_MAN
###
    #x1y1
    selected_columns_FSIL_WARD2_MAN_x1y1 <- c("ec_zip_FSIL_WARD2_MAN", "biodchem_FSIL_WARD2_MAN")
    FSIL_WARD2_MAN_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_MAN_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_WARD2_MAN_x1y2 <- c("nbhd_ec_zip_FSIL_WARD2_MAN", "biodchem_FSIL_WARD2_MAN")
    FSIL_WARD2_MAN_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_MAN_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_WARD2_MAN_x1y3 <- c("ec_grp_mem_zip_FSIL_WARD2_MAN", "biodchem_FSIL_WARD2_MAN")
    FSIL_WARD2_MAN_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_MAN_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_WARD2_MAN_x2y1 <- c("ec_zip_FSIL_WARD2_MAN", "biod10_FSIL_WARD2_MAN")
    FSIL_WARD2_MAN_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_MAN_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_WARD2_MAN_x2y2 <- c("nbhd_ec_zip_FSIL_WARD2_MAN", "biod10_FSIL_WARD2_MAN")
    FSIL_WARD2_MAN_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_MAN_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_WARD2_MAN_x2y3 <- c("ec_grp_mem_zip_FSIL_WARD2_MAN", "biod10_FSIL_WARD2_MAN")
    FSIL_WARD2_MAN_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_WARD2_MAN_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_WARD2_MAN <- biodchem_FSIL_WARD2_MAN", data = FSIL_WARD2_MAN_noNA_x1y1)
    XYplot(biodchem_FSIL_WARD_MAN, ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_MAN, ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_WARD_MAN, ~ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_WARD_MAN, ~ec_zip_FSIL_WARD_MAN, data = FSIL_WARD_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #discontinued WARD2 _MAN, _MAX, _MINK
    
###################################################

    ###
    #FSIL_SING_EUCL
    ###
    #x1y1
    selected_columns_FSIL_SING_EUCL_x1y1 <- c("ec_zip_FSIL_SING_EUCL", "biodchem_FSIL_SING_EUCL")
    FSIL_SING_EUCL_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_EUCL_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_SING_EUCL_x1y2 <- c("nbhd_ec_zip_FSIL_SING_EUCL", "biodchem_FSIL_SING_EUCL")
    FSIL_SING_EUCL_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_EUCL_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_SING_EUCL_x1y3 <- c("ec_grp_mem_zip_FSIL_SING_EUCL", "biodchem_FSIL_SING_EUCL")
    FSIL_SING_EUCL_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_EUCL_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_SING_EUCL_x2y1 <- c("ec_zip_FSIL_SING_EUCL", "biod10_FSIL_SING_EUCL")
    FSIL_SING_EUCL_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_EUCL_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_SING_EUCL_x2y2 <- c("nbhd_ec_zip_FSIL_SING_EUCL", "biod10_FSIL_SING_EUCL")
    FSIL_SING_EUCL_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_EUCL_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_SING_EUCL_x2y3 <- c("ec_grp_mem_zip_FSIL_SING_EUCL", "biod10_FSIL_SING_EUCL")
    FSIL_SING_EUCL_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_EUCL_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_SING_EUCL <- biodchem_FSIL_SING_EUCL", data = FSIL_SING_EUCL_noNA_x1y1)
    XYplot(biodchem_FSIL_SING_EUCL, ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_EUCL, ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_SING_EUCL, ~ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_EUCL, ~ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #x1y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_SING_EUCL <- biodchem_FSIL_SING_EUCL", data = FSIL_SING_EUCL_noNA_x1y2)
    XYplot(biodchem_FSIL_SING_EUCL, nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_EUCL, nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_SING_EUCL, ~nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_EUCL, ~nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_SING_EUCL <- biodchem_FSIL_SING_EUCL", data = FSIL_SING_EUCL_noNA_x1y3)
    XYplot(biodchem_FSIL_SING_EUCL, ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_EUCL, ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_SING_EUCL, ~ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_EUCL, ~ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIL_SING_EUCL <- biod10_FSIL_SING_EUCL", data = FSIL_SING_EUCL_noNA_x2y1)
    XYplot(biod10_FSIL_SING_EUCL, ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_SING_EUCL, ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_SING_EUCL, ~ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_SING_EUCL, ~ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_SING_EUCL <- biod10_FSIL_SING_EUCL", data = FSIL_SING_EUCL_noNA_x2y2)
    XYplot(biod10_FSIL_SING_EUCL, nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_SING_EUCL, nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_SING_EUCL, ~nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_SING_EUCL, ~nbhd_ec_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_SING_EUCL <- biod10_FSIL_SING_EUCL", data = FSIL_SING_EUCL_noNA_x2y3)
    XYplot(biod10_FSIL_SING_EUCL, ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_SING_EUCL, ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_SING_EUCL, ~ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_SING_EUCL, ~ec_grp_mem_zip_FSIL_SING_EUCL, data = FSIL_SING_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
    
    ###################################################
    
    ###
    #FSIL_SING_MAN
    ###
    
    #x1y1
    selected_columns_FSIL_SING_MAN_x1y1 <- c("ec_zip_FSIL_SING_MAN", "biodchem_FSIL_SING_MAN")
    FSIL_SING_MAN_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_SING_MAN_x1y1)) %>%
      drop_na()
    
    #discontinued SING _MAN, _MAX, _MINK
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_SING_MAN <- biodchem_FSIL_SING_MAN", data = FSIL_SING_MAN_noNA_x1y1)
    XYplot(biodchem_FSIL_SING_MAN, ec_zip_FSIL_SING_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_MAN, ec_zip_FSIL_SING_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_SING_MAN, ~ec_zip_FSIL_SING_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_MAN, ~ec_zip_FSIL_SING_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #discontinued WARD2 _MAN, _MAX, _MINK
    

####################################
    
###
#FSIL_AVE_EUCL
###
    #x1y1
    selected_columns_FSIL_AVE_EUCL_x1y1 <- c("ec_zip_FSIL_AVE_EUCL", "biodchem_FSIL_AVE_EUCL")
    FSIL_AVE_EUCL_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_EUCL_x1y1)) %>%
      drop_na()
    
    #x1y2
    selected_columns_FSIL_AVE_EUCL_x1y2 <- c("nbhd_ec_zip_FSIL_AVE_EUCL", "biodchem_FSIL_AVE_EUCL")
    FSIL_AVE_EUCL_noNA_x1y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_EUCL_x1y2)) %>%
      drop_na()
    
    #x1y3
    selected_columns_FSIL_AVE_EUCL_x1y3 <- c("ec_grp_mem_zip_FSIL_AVE_EUCL", "biodchem_FSIL_AVE_EUCL")
    FSIL_AVE_EUCL_noNA_x1y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_EUCL_x1y3)) %>%
      drop_na()
    
    #x2y1
    selected_columns_FSIL_AVE_EUCL_x2y1 <- c("ec_zip_FSIL_AVE_EUCL", "biod10_FSIL_AVE_EUCL")
    FSIL_AVE_EUCL_noNA_x2y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_EUCL_x2y1)) %>%
      drop_na()
    
    #x2y2
    selected_columns_FSIL_AVE_EUCL_x2y2 <- c("nbhd_ec_zip_FSIL_AVE_EUCL", "biod10_FSIL_AVE_EUCL")
    FSIL_AVE_EUCL_noNA_x2y2 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_EUCL_x2y2)) %>%
      drop_na()
    
    #x2y3
    selected_columns_FSIL_AVE_EUCL_x2y3 <- c("ec_grp_mem_zip_FSIL_AVE_EUCL", "biod10_FSIL_AVE_EUCL")
    FSIL_AVE_EUCL_noNA_x2y3 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_EUCL_x2y3)) %>%
      drop_na()
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_AVE_EUCL <- biodchem_FSIL_AVE_EUCL", data = FSIL_AVE_EUCL_noNA_x1y1)
    XYplot(biodchem_FSIL_AVE_EUCL, ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_AVE_EUCL, ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(FSIL_AVE_EUCL_noNA_x1y1$biodchem_FSIL_AVE_EUCL, ~FSIL_AVE_EUCL_noNA_x1y1$ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_AVE_EUCL_noNA_x1y1$biodchem_FSIL_AVE_EUCL, ~FSIL_AVE_EUCL_noNA_x1y1$ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #x1y2 illustrated solutions
    FSIL_AVE_EUCL_noNA_x1y2$biodchem_FSIL_AVE_EUCL <- as.numeric(FSIL_AVE_EUCL_noNA_x1y2$biodchem_FSIL_AVE_EUCL)
    pof("nbhd_ec_zip_FSIL_AVE_EUCL <- biodchem_FSIL_AVE_EUCL", data = FSIL_AVE_EUCL_noNA_x1y2)
    XYplot(FSIL_AVE_EUCL_noNA_x1y2$biodchem_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x1y2$nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_AVE_EUCL_noNA_x1y2$biodchem_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x1y2$nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_AVE_EUCL, ~nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y2, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_AVE_EUCL, ~nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y2, jitter=TRUE, relation = "sufficiency")
    
    #x1y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_AVE_EUCL <- biodchem_FSIL_AVE_EUCL", data = FSIL_AVE_EUCL_noNA_x1y3)
    XYplot(FSIL_AVE_EUCL_noNA_x1y3$biodchem_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x1y3$ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_AVE_EUCL, ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_AVE_EUCL, ~ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y3, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_AVE_EUCL, ~ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x1y3, jitter=TRUE, relation = "sufficiency")
    
    #x2y1 illustrated solutions
    pof("ec_zip_FSIL_AVE_EUCL <- biod10_FSIL_AVE_EUCL", data = FSIL_AVE_EUCL_noNA_x2y1)
    XYplot(FSIL_AVE_EUCL_noNA_x2y1$biod10_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x2y1$ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_AVE_EUCL_noNA_x2y1$biod10_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x2y1$ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_AVE_EUCL, ~ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y1, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_AVE_EUCL, ~ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y1, jitter=TRUE, relation = "sufficiency")
    
    #x2y2 illustrated solutions
    pof("nbhd_ec_zip_FSIL_AVE_EUCL <- biod10_FSIL_AVE_EUCL", data = FSIL_AVE_EUCL_noNA_x2y2)
    XYplot(FSIL_AVE_EUCL_noNA_x2y2$biod10_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x2y2$nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_AVE_EUCL, nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_AVE_EUCL, ~nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_SING_EUCL_noNA_x2y2, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_AVE_EUCL, ~nbhd_ec_zip_FSIL_AVE_EUCL, data = FSIL_SING_EUCL_noNA_x2y2, jitter=TRUE, relation = "sufficiency")
    
    #x2y3 illustrated solutions
    pof("ec_grp_mem_zip_FSIL_AVE_EUCL <- biod10_FSIL_AVE_EUCL", data = FSIL_AVE_EUCL_noNA_x2y3)
    XYplot(FSIL_AVE_EUCL_noNA_x2y3$biod10_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x2y3$ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_AVE_EUCL_noNA_x2y3$biod10_FSIL_AVE_EUCL, FSIL_AVE_EUCL_noNA_x2y3$ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biod10_FSIL_AVE_EUCL, ~ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y3, jitter=TRUE, relation = "necessity")
    XYplot(biod10_FSIL_AVE_EUCL, ~ec_grp_mem_zip_FSIL_AVE_EUCL, data = FSIL_AVE_EUCL_noNA_x2y3, jitter=TRUE, relation = "sufficiency")
    
    
    ###################################################
    
    ###
    #FSIL_AVE_MAN
    ###
    
    #x1y1
    selected_columns_FSIL_AVE_MAN_x1y1 <- c("ec_zip_FSIL_AVE_MAN", "biodchem_FSIL_AVE_MAN")
    FSIL_AVE_MAN_noNA_x1y1 <- biod_frame %>%
      select(all_of(selected_columns_FSIL_AVE_MAN_x1y1)) %>%
      drop_na()
    
    #discontinued SING _MAN, _MAX, _MINK
    
    #x1y1 illustrated solutions
    pof("ec_zip_FSIL_AVE_MAN <- biodchem_FSIL_AVE_MAN", data = FSIL_AVE_MAN_noNA_x1y1)
    XYplot(FSIL_AVE_MAN_noNA_x1y1$biodchem_FSIL_AVE_MAN, FSIL_AVE_MAN_noNA_x1y1$ec_zip_FSIL_AVE_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(FSIL_AVE_MAN_noNA_x1y1$biodchem_FSIL_AVE_MAN, FSIL_AVE_MAN_noNA_x1y1$ec_zip_FSIL_AVE_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    #negation
    XYplot(biodchem_FSIL_AVE_MAN, ~ec_zip_FSIL_SING_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "necessity")
    XYplot(biodchem_FSIL_SING_MAN, ~ec_zip_FSIL_SING_MAN, data = FSIL_SING_MAN_noNA_x1y1, jitter=TRUE, relation = "sufficiency")
    
    #discontinued WARD2 _MAN, _MAX, _MINK
    
    ###################################################
    
    
###
#LOGISTIC FUNCTION-CALIBRATED VALUES 
###

#Check with XY plots, as suggested in Mello (2022, p. 101)
x1 <- biod_frame$biodchem_calL
x2 <- biod_frame$biod10_calL
x31 <- biod_frame$chem_calL
x32 <- biod_frame$biod10_calL

y1 <- biod_frame$ec_zip_calL
y2 <- biod_frame$nbhd_ec_zip_calL
y3 <- biod_frame$ec_grp_mem_zip_cal
  
###Cond 1-1
plot(x1, y1, type = "o", pch = 16, col = "darkseagreen", xlab = "biod/chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 1-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#it's a necessary condition and fulfilll the cond for necessity THE BEST (because not on the diagonal line)

###Cond 1-2
plot(x1, y2, type = "o", pch = 16, col = "darkseagreen", xlab = "biod/chem", ylab = "nbhd_ec_zip", main = "Neccessary and Sufficient Conditions 1-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#it's a necessary condition

###Cond 1-3
plot(x1, y3, type = "o", pch = 16, col = "darkseagreen", xlab = "biod/chem", ylab = "ec_grp_mem_zip", main = "Neccessary and Sufficient Conditions 1-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#it's a necessary condition


###Cond 2-1
plot(x2, y1, type = "o", pch = 16, col = "darkseagreen", xlab = "biod10", ylab = "nbhd_ec_zip", main = "Neccessary and Sufficient Conditions 2-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#it's a necessary

###Cond 2-2
plot(x2, y2, type = "o", pch = 16, col = "darkseagreen", xlab = "biod10", ylab = "ec_grp_mem_zip", main = "Neccessary and Sufficient Conditions 2-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#it's a necessary

###Cond 2-3
plot(x2, y3, type = "o", pch = 16, col = "darkseagreen", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 2-3", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#it's a necessary


###Cond 31-1
plot(x31, y1, type = "o", pch = 16, col = "darkseagreen", xlab = "chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 31-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")

plot(x32, y1, type = "o", pch = 16, col = "darkseagreen", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 32-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#x31 has many more cases (chemicals more than biodiversity), therefore may enter separately as a better option?

###Cond 31-2
plot(x31, y2, type = "o", pch = 16, col = "darkseagreen", xlab = "chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 31-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")

plot(x32, y2, type = "o", pch = 16, col = "darkseagreen", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 32-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#x31 has many more cases (chemicals more than biodiversity), therefore may enter separately as a better option?

###Cond 31-3
plot(x31, y3, type = "o", pch = 16, col = "darkseagreen", xlab = "chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 31-3", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")

plot(x32, y3, type = "o", pch = 16, col = "darkseagreen", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 32-3", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "salmon")
#x31 has many more cases (chemicals more than biodiversity), therefore may enter separately as a better option?

###go further with testing only the combination of both x31 and x32 explaining y1

###
# INCLUSION SCORES
###

with(biod_frame, sum(biod10_cal & chem_cal) / sum(chem_cal))
with(biod_frame, sum(biod10_cal & chem_cal) / sum(biod10_cal))

pof(setms=biod10_cal, outcome=ec_zip_cal,data=biod_frame, relation = "necessity")
pof(setms=chem_cal, outcome=ec_zip_cal,data=biod_frame, relation = "necessity")
#don't care (only?)

with(biod_frame, sum(fuzzyand(biod10_cal, chem_cal)) / sum(biod10_cal))

###
#NECESSITY WITH XY PLOT FOR THE 1-TRESHOLD CRISP CALIBRATED VALUES 
###

#Check with XY plots, as suggested in Mello (2022, p. 101)
x2C <- biod_frame$biod10C
x31C <- biod_frame$chemC
x32C <- biod_frame$biod10C

y1C <- biod_frame$ec_zipC
y2C <- biod_frame$nbhd_ec_zipC
y3C <- biod_frame$ec_grp_mem_zipC

###Cond 1-1
plot(x1C, y1C, type = "o", pch = 16, col = "chocolate", xlab = "biod/chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 1-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#not fully necessary (includes 0)

###Cond 1-2
plot(x1C, y2C, type = "o", pch = 16, col = "chocolate", xlab = "biod/chem", ylab = "nbhd_ec_zip", main = "Neccessary and Sufficient Conditions 1-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#not fully sufficient (includes o)

###Cond 1-3
plot(x1C, y3C, type = "o", pch = 16, col = "chocolate", xlab = "biod/chem", ylab = "ec_grp_mem_zip", main = "Neccessary and Sufficient Conditions 1-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none


###Cond 2-1
plot(x2C, y1C, type = "o", pch = 16, col = "chocolate", xlab = "biod10", ylab = "nbhd_ec_zip", main = "Neccessary and Sufficient Conditions 2-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none

###Cond 2-2
plot(x2C, y2C, type = "o", pch = 16, col = "chocolate", xlab = "biod10", ylab = "ec_grp_mem_zip", main = "Neccessary and Sufficient Conditions 2-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none

###Cond 2-3
plot(x2C, y3C, type = "o", pch = 16, col = "chocolate", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 2-3", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#not necessary


###Cond 31-1
plot(x31C, y1C, type = "o", pch = 16, col = "chocolate", xlab = "chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 31-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none

plot(x32C, y1C, type = "o", pch = 16, col = "chocolate", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 32-1", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none

###Cond 31-2
plot(x31C, y2C, type = "o", pch = 16, col = "chocolate", xlab = "chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 31-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none

plot(x32C, y2C, type = "o", pch = 16, col = "chocolate", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 32-2", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#not sufficient

###Cond 31-3
plot(x31C, y3C, type = "o", pch = 16, col = "chocolate", xlab = "chem", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 31-3", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#none

plot(x32C, y3C, type = "o", pch = 16, col = "chocolate", xlab = "biod10", ylab = "ec_zip", main = "Neccessary and Sufficient Conditions 32-3", xlim = c(0,1), ylim = c(0,1))
abline(a = 0, b = 1, col = "darkslategray3")
#not necessary

#Check with cross-table, as suggested in Dusa (2022, p. 115) for crisp sets
table(biod_frame$x1C, biod_frame$y1C)
table(biod_frame$x1C, biod_frame$y2C)
table(biod_frame$x1C, biod_frame$y3C)

table(biod_frame$x2C, biod_frame$y1C)
table(biod_frame$x2C, biod_frame$y2C)
table(biod_frame$x2C, biod_frame$y3C)

table(biod_frame$x31C, biod_frame$y1C)
table(biod_frame$x32C, biod_frame$y2C)
table(biod_frame$x31C, biod_frame$y3C)
table(biod_frame$x32C, biod_frame$y1C)
table(biod_frame$x31C, biod_frame$y2C)
table(biod_frame$x32C, biod_frame$y3C)
#no observed association between the biod10 and outcome for the crisp set
#no observed PERFECT association, there may be mostly-inclusion, check next:

###
#INCLUSION SCORES FOR THE 1-TRESHOLD CRISP CALIBRATED VALUES
###

#inclusion score for binary value 1-condition combinations
sum(x1C & y1C)/sum(y1C)
sum(x1C & y2C)/sum(y2C)
sum(x1C & y3C)/sum(y3C)

sum(x2C & y1C)/sum(y1C)
sum(x2C & y2C)/sum(y2C)
sum(x2C & y3C)/sum(y3C)

with(biod_frame, table(biod_frame$ec_zipC, biod_frame$biodchemC))
with(biod_frame, table(biod_frame$ec_zipC, biod_frame$biodchemC)/sum(biod_frame$biodchemC))

with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$biodchemC))
with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$biodchemC)/sum(biod_frame$biodchemC))
#10 cases as necessary

with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$biodchemC))
with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$biodchemC)/sum(biod_frame$biodchemC))



with(biod_frame, table(biod_frame$ec_zipC, biod_frame$biod10C))
with(biod_frame, table(biod_frame$ec_zipC, biod_frame$biod10C)/sum(biod_frame$biod10C))
#16 cases

with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$biod10C))
with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$biod10C)/sum(biod_frame$biod10C))
#48 cases

with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$biod10C))
with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$biod10C)/sum(biod_frame$biod10C))
#16 cases


with(biod_frame, table(biod_frame$ec_zipC, biod_frame$chemC))
with(biod_frame, table(biod_frame$ec_zipC, biod_frame$chemC)/sum(biod_frame$chemC))

with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$chemC))
with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$chemC)/sum(biod_frame$chemC))
#5 cases

with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$chemC))
with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$chemC)/sum(biod_frame$chemC))

###
#COVERAGE FOR THE 1-TRESHOLD CRISP CALIBRATED VALUES 
###

#continue only with cases for which there are non-0 1-1 condition-outcome
tbl12 <- with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$biodchemC))
fisher.test(tbl12)  #independent when testing trivialness

tbl21 <- with(biod_frame, table(biod_frame$ec_zipC, biod_frame$biod10C))
fisher.test(tbl21)  #independent

tbl22 <- with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$biod10C))
fisher.test(tbl22)  #independent but Inf!

tbl23 <- with(biod_frame, table(biod_frame$ec_grp_mem_zipC, biod_frame$biod10C))
fisher.test(tbl23)  #independent but Inf!

tbl32 <- with(biod_frame, table(biod_frame$nbhd_ec_zipC, biod_frame$chemC))
fisher.test(tbl32)  #independent

#test the necessity for conjunction of chem and biod10 for the 3 ec connectedness outcomes
selected_columns_y1C <- c("chemC", "biodchemC", "ec_zipC", "biod10C")
biod_frame_outcome_ec_zipC <- biod_frame[complete.cases(biod_frame[, selected_columns_y1C]), selected_columns_y1C]
superSubset(biod_frame_outcome_ec_zipC, outcome = "ec_zipC", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_ec_zipC, outcome = "ec_zipC", incl.cut = 0.9, relation = "sufficiency")

selected_columns_y2C <- c("chemC", "biodchemC", "nbhd_ec_zipC", "biod10C")
biod_frame_outcome_nbhd_ec_zipC <- biod_frame[complete.cases(biod_frame[, selected_columns_y2C]), selected_columns_y2C]
superSubset(biod_frame_outcome_nbhd_ec_zipC, outcome = "nbhd_ec_zipC", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_nbhd_ec_zipC, outcome = "nbhd_ec_zipC", incl.cut = 0.9, relation = "sufficiency")

selected_columns_y3C <- c("chemC", "biodchemC", "ec_grp_mem_zipC", "biod10C")
biod_frame_outcome_ec_grp_mem_zipC <- biod_frame[complete.cases(biod_frame[, selected_columns_y3C]), selected_columns_y3C]
superSubset(biod_frame_outcome_ec_grp_mem_zipC, outcome = "ec_grp_mem_zipC", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_ec_grp_mem_zipC, outcome = "ec_grp_mem_zipC", incl.cut = 0.9, relation = "sufficiency")
#relevant but insufficient conditions 

###
#GUI-GENERATED FSIL95
###

#plotting some values on scatter x1y1 and x1y2
plot(biod_frame$biodchemFSIL95tr_0.5_75.._300.., biod_frame$ec_zipFSIL95tr_0.4_0.9_1.4, type = "o", pch = 20, col = "rosybrown3", xlab = "biodchem", ylab = "ec_zip", main = "Scatterplot for fuzzy sets", xlim = c(0,1), ylim = c(0,1))

plot(biod_frame$biodchemFSIL95tr_0.5_75.._300.., biod_frame$nbhd_ec_zipFSIL95tr_0.2_1_1.8, type = "o", pch = 20, col = "rosybrown3", xlab = "biodchem", ylab = "nbhd_ec_zip", main = "Scatterplot for fuzzy sets", xlim = c(0,1), ylim = c(0,1))


# INCLUSION SCORES

pof(setms=biodchemFSIL95tr_0.5_75.._300.., outcome=biod_frame$ec_zip,data=biod_frame, relation = "necessity")
pof(setms=chem_cal, outcome=ec_zip_cal,data=biod_frame, relation = "necessity")
#don't care (only?)

with(biod_frame, sum(fuzzyand(biod10_cal, chem_cal)) / sum(biod10_cal))


#test the necessity for conjunction of chem and biod10 for the 3 ec connectedness outcomes
selected_columns_y1FSIL95 <- c("chem", "biodchemC", "ec_zipC", "biod10C")
biod_frame_outcome_ec_zipC <- biod_frame[complete.cases(biod_frame[, selected_columns_y1C]), selected_columns_y1C]
superSubset(biod_frame_outcome_ec_zipC, outcome = "ec_zipC", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_ec_zipC, outcome = "ec_zipC", incl.cut = 0.9, relation = "sufficiency")


###
#MULTIVALUE CRISP SET 
###

#Check necessity with cross-table, as suggested in Dusa (2022, p. 115) for crisp sets
table(biod_frame$biodchemM5, biod_frame$ec_zipM5)
table(biod_frame$biodchemM5, biod_frame$nbhd_ec_zipM5)
table(biod_frame$biodchemM5, biod_frame$ec_grp_mem_zipM5)

table(biod_frame$biod10M5, biod_frame$ec_zipM5)
table(biod_frame$biod10M5, biod_frame$nbhd_ec_zipM5)
table(biod_frame$biod10M5, biod_frame$ec_grp_mem_zipM5)

table(biod_frame$chemM5, biod_frame$ec_zipM5)
table(biod_frame$biod10M5, biod_frame$ec_zipM5)
table(biod_frame$chemM5, biod_frame$nbhd_ec_zipM5)
table(biod_frame$biod10M5, biod_frame$nbhd_ec_zipM5)
table(biod_frame$chemM5, biod_frame$ec_grp_mem_zipM5)
table(biod_frame$biod10M5, biod_frame$ec_grp_mem_zipM5)

###
#INCLUSION SCORES FOR THE 6-TRESHOLD CRISP CALIBRATED VALUES
###

#inclusion score for binary value 1-condition combinations
sum(biod_frame$biodchemM5 & biod_frame$ec_zipM5)/sum(biod_frame$ec_zipM5)
sum(biod_frame$biodchemM5 & biod_frame$nbhd_ec_zipM5)/sum(biod_frame$nbhd_ec_zipM5)
sum(biod_frame$biodchemM5 & biod_frame$ec_grp_mem_zipM5)/sum(biod_frame$ec_grp_mem_zipM5)

sum(biod_frame$biod10M5 & biod_frame$ec_zipM5)/sum(biod_frame$ec_zipM5)
sum(biod_frame$biod10M5 & biod_frame$nbhd_ec_zipM5)/sum(biod_frame$nbhd_ec_zipM5)
sum(biod_frame$biod10M5 & biod_frame$ec_grp_mem_zipM5)/sum(biod_frame$ec_grp_mem_zipM5)

with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$biodchemM5))
with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$biodchemM5)/sum(biod_frame$biodchemM5))

with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$biodchemM5))
with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$biodchemM5)/sum(biod_frame$biodchemM5))

with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$biodchemM5))
with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$biodchemM5)/sum(biod_frame$biodchemM5))

with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$biod10M5))
with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$biod10M5)/sum(biod_frame$biod10M5))

with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$biod10M5))
with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$biod10M5)/sum(biod_frame$biod10M5))

with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$biod10M5))
with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$biod10M5)/sum(biod_frame$biod10M5))


with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$chemM5))
with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$chemM5)/sum(biod_frame$chemM5))

with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$chemM5))
with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$chemM5)/sum(biod_frame$chemM5))

with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$chemM5))
with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$chemM5)/sum(biod_frame$chemM5))

###
#COVERAGE FOR THE 1-TRESHOLD CRISP CALIBRATED VALUES 
###

#confused about the previous result tables, theerfore continue only with cases for which in case of 0/1 binary calibration there are non-0 1-1 condition-outcome
tbl12 <- with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$biodchemM5))
fisher.test(tbl12)  #dependent when testing trivialness

tbl21 <- with(biod_frame, table(biod_frame$ec_zipM5, biod_frame$biod10M5))
fisher.test(tbl21)  #insufficient data for calculation

tbl22 <- with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$biod10M5))
fisher.test(tbl22)  #dependent

tbl23 <- with(biod_frame, table(biod_frame$ec_grp_mem_zipM5, biod_frame$biod10M5))
fisher.test(tbl23)  #insufficient data for calculation 

tbl32 <- with(biod_frame, table(biod_frame$nbhd_ec_zipM5, biod_frame$chemM5))
fisher.test(tbl32)  #error in caluclating - must reduce the matrix (?)

#test the necessity for conjunction of chem and biod10 for the 3 ec connectedness outcomes
selected_columns_y1M5 <- c("chemM5", "biodchemM5", "ec_zipM5", "biod10M5")
biod_frame_outcome_ec_zipM5 <- biod_frame[complete.cases(biod_frame[, selected_columns_y1M5]), selected_columns_y1M5]
superSubset(biod_frame_outcome_ec_zipM5, outcome = "ec_zipM5", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_ec_zipM5, outcome = "ec_zipM5", incl.cut = 0.9, relation = "sufficiency")
#no necessary, only sufficient 

selected_columns_y2M5 <- c("chemM5", "biodchemM5", "nbhd_ec_zipM5", "biod10M5")
biod_frame_outcome_nbhd_ec_zipM5 <- biod_frame[complete.cases(biod_frame[, selected_columns_y2M5]), selected_columns_y2M5]
superSubset(biod_frame_outcome_nbhd_ec_zipM5, outcome = "nbhd_ec_zipM5", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_nbhd_ec_zipM5, outcome = "nbhd_ec_zipM5", incl.cut = 0.9, relation = "sufficiency")

selected_columns_y3M5 <- c("chemM5", "biodchemM5", "ec_grp_mem_zipM5", "biod10M5")
biod_frame_outcome_ec_grp_mem_zipM5 <- biod_frame[complete.cases(biod_frame[, selected_columns_y3M5]), selected_columns_y3M5]
superSubset(biod_frame_outcome_ec_grp_mem_zipM5, outcome = "ec_grp_mem_zipM5", incl.cut = 0.9, relation = "necessity")
superSubset(biod_frame_outcome_ec_grp_mem_zipM5, outcome = "ec_grp_mem_zipM5", incl.cut = 0.9, relation = "sufficiency")

#no necessary, only sufficient conditions

#######################################################################
#SECTION 6 | ANALYSIS OF SUFFICIENCY 

###
#SUFFICIENCY FOR THE 6-TRESHOLD CRISP CALIBRATED VALUES 
###
#none is necessary (look up to the previous section)

###
#PRI SCORE FOR THE 6-TRESHOLD CRISP CALIBRATED VALUES 
###

###
#COVERAGE FOR THE 6-TRESHOLD CRISP CALIBRATED VALUES 
###

#######################################################################
#SECTION 7 | THE TRUTH TABLE

#TT1
truthTable(FSIL95_noNA_x1y3, outcome= "ec_grp_mem_zipFSIL95tr_" , complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT2
truthTable(FSIL95_noNA_x2y3, outcome= "ec_grp_mem_zipFSIL95tr_" , complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT3
truthTable(FSIE95_noNA_x1y2, outcome= "nbhd_ec_zipFSIE95tr_0.2_1_1.8" , complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT4
truthTable(FSIE95_noNA_x1y3, outcome= "ec_grp_mem_zipFSIE95tr_" , complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT5
truthTable(FSIE95_noNA_x2y3, outcome= "ec_grp_mem_zipFSIE95tr_", complete = TRUE, show.cases = TRUE, incl.cut = 0.8)
truthTable(FSIE95_noNA_x2y3, outcome= "ec_grp_mem_zipFSIE95tr_", complete = TRUE, show.cases = TRUE, incl.cut = 0.8, dcc=TRUE, sort.by = "incl")

#TT6
FSIL_COM_EUCL_noNA_x1y2 <- as.data.frame(FSIL_COM_EUCL_noNA_x1y2)
truthTable(FSIL_COM_EUCL_noNA_x1y2, outcome= "nbhd_ec_zip_FSIL_COM_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT7
truthTable(FSIL_COM_EUCL_noNA_x2y2, outcome= "nbhd_ec_zip_FSIL_COM_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT8
truthTable(FSIL_WARD_EUCL_noNA_x1y2, outcome= "nbhd_ec_zip_FSIL_WARD_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT9
truthTable(FSIL_WARD_EUCL_noNA_x1y3, outcome= "ec_grp_mem_zip_FSIL_WARD_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT10
truthTable(FSIL_WARD_EUCL_noNA_x2y2, outcome= "nbhd_ec_zip_FSIL_WARD_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT11
truthTable(FSIL_WARD_EUCL_noNA_x2y3, outcome= "ec_grp_mem_zip_FSIL_WARD_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT12
FSIL_AVE_EUCL_noNA_x1y2 <- as.data.frame(FSIL_AVE_EUCL_noNA_x1y2)
truthTable(FSIL_AVE_EUCL_noNA_x1y2, outcome= "nbhd_ec_zip_FSIL_AVE_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT13
FSIL_AVE_EUCL_noNA_x2y2 <- as.data.frame(FSIL_AVE_EUCL_noNA_x2y2)
truthTable(FSIL_AVE_EUCL_noNA_x2y2, outcome= "nbhd_ec_zip_FSIL_AVE_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT14
truthTable(FSIL_WARD2_EUCL_noNA_x1y2, outcome= "nbhd_ec_zip_FSIL_WARD2_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT15
truthTable(FSIL_WARD2_EUCL_noNA_x1y3, outcome= "ec_grp_mem_zip_FSIL_WARD2_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT16
truthTable(FSIL_WARD2_EUCL_noNA_x2y2, outcome= "nbhd_ec_zip_FSIL_WARD2_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#TT17
truthTable(FSIL_WARD2_EUCL_noNA_x2y3, outcome= "ec_grp_mem_zip_FSIL_WARD2_EUCL", complete = TRUE, show.cases = TRUE, incl.cut = 0.5)

#######################################################################
#SECTION 8 | THE LOGICAL MINIMISATION

#TT5


#######################################################################
#SECTION 9 | PSEUDO-COUNTERFACTUAL ANALYSIS   not yet


#######################################################################

#SECTION 10 | NCA NECESSARY CONDITION ANALYSIS

#CEILINGS 

ceil_x1y1 <- nca(biod_frame, "biodchem","ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x1y2 <- nca(biod_frame, "biodchem","nbhd_ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x1y3 <- nca(biod_frame, "biodchem","ec_grp_mem_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
#print
print(ceil_x1y1)
print(ceil_x1y2)
print(ceil_x1y3)

ceil_x2y1 <- nca(biod_frame, "biod10","ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x2y2 <- nca(biod_frame, "biod10","nbhd_ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x2y3 <- nca(biod_frame, "biod10","ec_grp_mem_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
#print
print(ceil_x2y1)
print(ceil_x2y2)
print(ceil_x2y3)

ceil_x1x2y1 <- nca(biod_frame, c("chem", "biod10"),"ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp"))
ceil_x1x2y2 <- nca(biod_frame, c("chem", "biod10"),"nbhd_ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x1x2y3 <- nca(biod_frame, c("chem", "biod10"),"ec_grp_mem_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
#print
print(ceil_x1x2y1)
print(ceil_x1x2y2)
print(ceil_x1x2y3)


#ANALYSIS

ceil_x1y1 <- nca_analysis(biod_frame, "biodchem","ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x1y2 <- nca(biod_frame, "biodchem","nbhd_ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x1y3 <- nca(biod_frame, "biodchem","ec_grp_mem_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
#print
print(ceil_x1y1)
value()
print(ceil_x1y2)
print(ceil_x1y3)

ceil_x2y1 <- nca(biod_frame, "biod10","ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x2y2 <- nca(biod_frame, "biod10","nbhd_ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x2y3 <- nca(biod_frame, "biod10","ec_grp_mem_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
#print
print(ceil_x2y1)
print(ceil_x2y2)
print(ceil_x2y3)

#NCA analyse single conditions only! -discontinue
ceil_x1x2y1 <- nca(biod_frame, c("chem", "biod10"),"ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp"))
ceil_x1x2y2 <- nca(biod_frame, c("chem", "biod10"),"nbhd_ec_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
ceil_x1x2y3 <- nca(biod_frame, c("chem", "biod10"),"ec_grp_mem_zip", ceilings = c("cols", "qr", "ce_vrs", "cr_vrs", "ce_fdh", "cr_fdh", "c_lp" ))
#print
print(ceil_x1x2y1)
print(ceil_x1x2y2)
print(ceil_x1x2y3)





###
#PLOTS 
###

###
#SUMMARIES
###

###
#BOTTLENECK TABLES
###


###
#PEERS
###

###
#TESTS
###



#SECTION 11 | GRAPHS

#Figure 1

#Table 1
summary(biod_frame$ec_zip)
summary(biod_frame$nbhd_ec_zip )
summary(biod_frame$ec_grp_mem_zip)
summary(biod_frame$biod10)




create_violin_plot <- function(variable) {
  ggplot(biod_frame, aes(x = "", y = !!sym(variable), fill = variable)) +
    geom_violin(trim = FALSE, draw_quantiles = c(0.25, 0.5, 0.75), color = "darkslategray") +
    geom_boxplot(width = 0.1, fill = "white", color = "darkslategray", outlier.shape = NA) +
    coord_cartesian(ylim = c(0, 2)) +  # Set y-axis limits
    theme_minimal() +
    labs(title = paste(variable),
         x = NULL, y = NULL) +  # Remove axis titles
    theme(axis.title = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())
}

# Apply the functions to create the combined plots
ec_zip_plot <- create_violin_plot("ec_zip")
nbhd_ec_zip_plot <- create_violin_plot("nbhd_ec_zip")
ec_grp_mem_zip_plot <- create_violin_plot("ec_grp_mem_zip")

# Combine the plots using grid.arrange
grid.arrange(ec_zip_plot, nbhd_ec_zip_plot, ec_grp_mem_zip_plot,
             ncol = 3, heights = c(3, 3, 3))

