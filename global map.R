#map for WWF Innovation Fund of partner sites and proposed sites

#use Darling global coral points 
setwd ("/Users/emilydarling/Dropbox/1-On the go/Coral Database/GLOBAL CORAL PAPERS/Paper1 - IP coral communities/Analysis/Models/2016/data")  
d <- read.csv("data_v4_2299sites.csv", header = TRUE, stringsAsFactors = FALSE)  
head(d)

unique(d$Source)

d2 <- d %>%
  select(Source,Country,Site,Latitude,Longitude) %>%
  filter(Source == "McClanahan_CRCP" | Source == "NOAA_CRED" | 
           Source == "WCS PNG" | Source == "WCS Indo" | 
           Source == "Nat_Geo" | Source == "WWF-US" | 
           Source == "Jupiter" | Source == "WCS Mada") 

#add WCS Indonesia fish biomass lat/long
setwd ("/Users/emilydarling/Dropbox/4_WCS MacMon/Seascapes/Indonesia MacMon/Indo Biomass paper/for R") 
wcs_indo <- read.csv("DATA_Fish_Transect_All_2015_STP_v4.csv", 
              header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(wcs_indo)
nrow(wcs_indo) 

wcs_indo2 <- wcs_indo %>%
  select(Region2, Site.Name, latitude, longitude) %>%
  group_by(Region2,Site.Name) %>%
  summarize(Latitude = mean(latitude, na.rm = TRUE), 
            Longitude = mean(longitude, na.rm = TRUE))
wcs_indo2$Source <- "WCS Indonesia biomass"
wcs_indo2$Country <- "Indonesia"

names(wcs_indo2)[2] <- "Site"
wcs_indo2 <- wcs_indo2[,c(5,6,2:4)]
head(wcs_indo2)

global <- rbind.fill(d2, wcs_indo2)

#add David Gill lat/longs 
setwd ("/Users/emilydarling/Documents/Work/GitHub/CollabDB")
gill <- read.csv("coral site coordinates.csv", 
                     header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(gill)
nrow(gill) 
unique(gill$Ecoregn)

gill2 <- gill %>%
  group_by(Ecoregn,SiteLat,SiteLong) %>%
  summarize(n = n())

hist(gill2$n)

names(gill2)[2] <- "Latitude"
names(gill2)[3] <- "Longitude"
names(gill2)[1] <- "Country"
gill2$Source <- "Gill SESYNC"
names(gill2)

global2 <- rbind.fill(global,gill2[,-4])
hist(global2$Latitude)
hist(global2$Longitude)

#add Gabby WWF sites in Indonesia (SBS, BHS) lat/longs
setwd ("/Users/emilydarling/Documents/Work/GitHub/CollabDB")
bhs <- read.csv("BHS_site_metadata_10.28.2015.csv", 
                 header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE) 
head(bhs)
nrow(bhs) 

names(global2)
global3 <- rbind.fill(global2, bhs)



#then summarize to remove duplicate sites by organiation (e.g., WCS, WWF will both have Forgotten Island/ Solor-Alor)
head(global3)
unique(global3$Country)
unique(global3$Source)

global3 <- global3 %>%
  group_by(Source,Country,Latitude,Longitude) %>%
  summarize(n = n())
unique(global3$Source)

hist(global3$n)

#geom_point AES by source, e.g., NOAA = potential
unique(global3$Source)
global3$Source2 <- recode(global3$Source, 
        "'Jupiter' = 'WCS';
          'McClanahan_CRCP' = 'WCS';
          'WCS Indo' = 'WCS';
          'WCS Mada' = 'WCS';
          'WCS PNG' = 'WCS';
          'WCS Indonesia biomass' = 'WCS';
          'NOAA_CRED' = 'NOAA';
          'Nat_Geo' = 'National Geographic'")
unique(global3$Source2)

global3$access <- ifelse(global3$Source2 == "National Geographic" |
                           global3$Source2 == "NOAA" |
                           global3$Source2 == "Gill SESYNC", "proposed partner",
                         "core team")


#export to keynote and add other global datasets as polygons
#e.g., CTI, Fish Forever


#################
#map
#longitude 360
#Google is using [0,360] Longitude values, 360 - value = converted longitude         
global3$Longitude360 <- ifelse(global3$Longitude < 0, 
                               360 - abs(global3$Longitude), global3$Longitude)
hist(global3$Latitude); length(global3$Latitude)
hist(global3$Longitude360); length(global3$Longitude360)   


#left side of world map - google earth
table(global3$Source2, global3$access)

map <- get_map(location=c(lon = 120, lat = 19.37), zoom=2, 
               maptype="satellite", source="google", crop = FALSE)         
p <- ggmap(map)
p  

#check point locations, fix any errant latitude and longitudes
names(global3)
left <- p + geom_point(aes(x = Longitude360, y = Latitude, colour = Source2), 
               size = 2, alpha = 0.6,
               position = position_jitter(width = 1, height = 1), 
               data = global3) + 
  scale_colour_manual(values = c("white","grey80","grey65","turquoise","yellow"))

pdf("sites_left map.pdf", width = 15.5, height = 15.5)
left
dev.off()    


#right side of map, Pacific to Americas
map2 <- get_map(location=c(lon = -140, lat = 19.37), zoom=2, 
               maptype="satellite", source="google", crop = FALSE)         

p2 <- ggmap(map2)
p2     

right <- p2 + geom_point(aes(x = Longitude, y = Latitude, colour = Source2), 
                         size = 2, alpha = 0.6,
                         position = position_jitter(width = 1, height = 1), 
                         data = global3) + 
  scale_colour_manual(values = c("white","grey80","grey65","turquoise","yellow"))
right

pdf("sites_right map.pdf", width = 15.5, height = 15.5)
right
dev.off()    




#in R, overlay two plots
