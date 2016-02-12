## read in most recent version
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish")
data <- read.csv("test2.csv", 
                 header = TRUE, stringsAsFactors = FALSE)      
head(data)
nrow(data)
names(data) 

unique(data$species)






###OLD
## read in Fraser data
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish/WCS")
fjh <- read.csv("Fraser_LW conversion list.csv", 
                 header = TRUE, stringsAsFactors = FALSE)      
head(fjh)
nrow(fjh)
names(fjh)  

#remove blank trophic levels
fjh2 <- fjh[-which(fjh$trophic == ""),]
unique(fjh2$trophic)

fjh_species <- fjh2 %>%
  select(species,trophic)

head(fjh)
fjh_c <- fjh %>%
  select(species,c) %>%
  group_by(species) %>%
  summarize(c = mean(c, na.rm = TRUE))
fjh_c <- na.omit(fjh_c)

fjh_species2 <- full_join(fjh_species,fjh_c, by="species")
write.csv(fjh_species2, "Fraser species with c, trophic.csv", row.names = FALSE)

## calculate location specific a,b for Fraser
head(fjh)
fjh_ab <- melt(fjh[,1:4], id.var = 1:2)
head(fjh_ab)

fjh_ab$name <- paste(fjh_ab$variable, fjh_ab$location, sep = "_")

check <- fjh_ab %>%
  group_by(species,name) %>%
  summarize(n = n())
subset(check, n > 1)

head(fjh_ab)
names(fjh_ab)
fjh_ab2 <- dcast(fjh_ab[,c(2,4,5)], species ~ name)
head(fjh_ab2)

names(fjh_ab2)
fjh_ab3 <- fjh_ab2[,c(1:2,5,3,6,4,7)]
names(fjh_ab3)
write.csv(fjh_ab3, "Fraser clean a,b by location.csv", row.names = FALSE)

#wrangle fraser clean dataset into master list
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish")
data <- read.csv("test.csv", 
                header = TRUE, stringsAsFactors = FALSE)      
head(data)
nrow(data)
names(data) 

unique(data$species)

data2 <- full_join(data,fjh_ab3, by = "species")
data3 <- data2  %>% 
  arrange(species)

View(data3)
View(data3[1000:1820,])
write.csv(data3, "test2.csv", row.names =FALSE)







## add trophic group, functional group
## for genus, will pull up most dominant group, or average of a/b within genus


###OLD 
##1. read in BHS fish species list_cleaned
setwd("/Users/emilydarling/Documents/Work/GitHub/CollabDB/fish files")
data <- read.csv("BHS fish attributes_5Feb2016_ESD.csv", 
                 header = TRUE, stringsAsFactors = FALSE)      
head(data)
nrow(data)
names(data)  

#check for duplicates - none
nrow(data)
unique(data$species)

unique(data$FAMILY)

data$FAMILY <- CA(data$FAMILY)
unique(data$FAMILY)

names(data)[1] <- "family"
names(data)

#write out cleaned BHS dataset
write.csv(data,"BHS fish attributes_5Feb2016_ESD_v2.csv",row.names = FALSE)

##2. use rfishbase to fill in BHS dataset
##play around with rfishbase
library(rfishbase)

#create species list to call from rfishbase API 
names(data)
bhs_esd <- data$species
head(bhs_esd)

test2 <- length_weight("Lutjanus bohar")

length_weight_data <- length_weight(bhs_esd)
lw_data <- length_weight_data
write.csv(lw_data, "lw_data from rfishbase original.csv", row.names = FALSE)
nrow(lw_data)

#read length weight data downloaded from rfishbase
lw_data <- read.csv("lw_data from rfishbase original.csv", 
                 header = TRUE, stringsAsFactors = FALSE)  
names(lw_data)

## from rfishbase
lw_data2 <- lw_data %>%
  select(SpecCode,sciname,a,b,LengthMin,LengthMax) %>%
  group_by(SpecCode,sciname) %>%
  summarize(a = mean(a, na.rm = TRUE),
            b = mean(b, na.rm = TRUE),
            LengthMin = mean(LengthMin, na.rm=TRUE),
            LengthMax = mean(LengthMax, na.rm = TRUE))

#good check on SpecCode and sciname
length(unique(lw_data2$SpecCode))
length(unique(lw_data2$sciname))
data <- lw_data2

write.csv(lw_data2, "rfishbase_ab_6Feb2015.csv", row.names = FALSE)

##3. import gabby's cleanest spreadsheets
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish/from Gabby")
wwf <- read.csv("WWF Fish_attributes_esd.csv", header = TRUE, 
                stringsAsFactors = FALSE, strip.white = TRUE)
unique(wwf$species)

#dplyr code to check for duplicates
wwf %>%
  group_by(species) %>%
  filter(n() >1)

#merge wwf values with rfishbase data
names(data)
names(wwf)
names(wwf)[3] <- "sciname"
names(wwf)[4] <- "a_wwf"
names(wwf)[5] <- "b_wwf"
head(wwf)

check <- left_join(data[,1:4], wwf[,3:5], by="sciname")

#check two outliers
#Aluterus scriptus - replace with WWF, fishbase estimate from South Florida with crazy a
subset(check, b_wwf>2.9 & b <2)

#Epinephelus tukula - not sure, emailed Twitter
subset(check, b_wwf<1)
subset(check, sciname == "Epinephelus tukula")

names(check)
ggplot(aes(x = a_wwf, y = a), data = check) +
  geom_point()

ggplot(aes(x = b_wwf, y = b), data = check) +
  geom_point()

subset(check, a > 0.8)

#write fishbase and wwf a/b lists
write.csv(check, "species with a, b_from rfishbase_wwf.csv", row.names = FALSE)

### import latest a,b file
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish")
data <- read.csv("species with a, b_from rfishbase_wwf.csv", header = TRUE, 
                 stringsAsFactors = FALSE, strip.white = TRUE)
names(data)[1] <- "FB_SpecCode"
names(data)[2] <- "species"
head(data)

##merge into Erdmann list
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish")
master <- read.csv("BHS fish attributes_5Feb2016_ESD_v2.csv", header = TRUE, 
                   stringsAsFactors = FALSE, strip.white = TRUE)
head(master)

master2 <- left_join(master,data)
#512/1671 match from fishbase and Gabby BHS

#import WCS Fiji list from Stacy - Feb 2016
setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish/WCS")
fiji <- read.csv("Updated fish info Fiji cleaned 20091118 FINAL.csv", header = TRUE, 
                stringsAsFactors = FALSE, strip.white = TRUE)

fiji2 <- fiji %>%
  select(species, a,b)
head(fiji2)
unique(fiji2$species)

names(fiji2)[2] <- "a_wcsfiji"
names(fiji2)[3] <- "b_wcsfiji"
head(fiji2)

head(master2)
master3 <- full_join(master2, fiji2, by = "species") 
head(master3)

master3 <- master3 %>%
  arrange(species)

setwd("/Users/emilydarling/Google Drive/Coral Collaboration/Attribute tables/Fish")
write.csv(master3, "test.csv", row.names = FALSE)





