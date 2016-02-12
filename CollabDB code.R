
# Make fish attributes tables
##rfishbase
#install.packages("rfishbase")
library(rfishbase)
require(fishbase)

fish <- species_list(Genus = "Labroides")
fish

## Not run:
tables <- docs()
# Describe the diet table
dplyr::filter(tables, table == "diet")$description

tables
setwd()

setwd("/Users/emilydarling/Documents/Work/GitHub/CollabDB/outputs")
write.csv(tables, "rfishbase table description.csv")



## Code to merge fish attributes data

d <- read.csv("Fish_attributes.csv", header = TRUE, stringsAsFactors = FALSE)      
head(d)
nrow(d)
names(d)   

nick <- read.csv("Fish climate vulnerability for Emily.csv", header = TRUE, stringsAsFactors = FALSE)      
head(nick)
names(nick)[4] <- "Fish_Species"
names(nick)

# merge nick's Func_group with Gabby's table

d2 <- left_join(d, nick[,c(4:6)])
names(d2)

d2 <- d2[,c(2,1,3:6,9,10)]
head(d2)

names(d2)[7] <- "Trophic group"
names(d2)[8] <- "Climate susceptibility"
write.csv(d2, "Fish_attributes_v2.csv", row.names = FALSE)
