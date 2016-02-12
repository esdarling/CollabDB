# Make coral attributes tables
##coraltraits.org

#cleaning and check with ESD global corals genera
setwd("/Users/emilydarling/Documents/Work/GitHub/CollabDB/coral attributes/from coraltraits.org")
data <- read.csv("Scleractinian coral attributes.csv", 
                 header = TRUE, stringsAsFactors = FALSE)      
head(data)
nrow(data)
names(data)  


#downloaded data - with taxonomic detail - on growth form Veron and typical growth form

setwd("/Users/emilydarling/Documents/Work/GitHub/CollabDB/coral attributes/from coraltraits.org")
d <- read.csv("data_20160130.csv", header = TRUE, stringsAsFactors = FALSE)      
head(d)
nrow(d)
names(d)   

#download and add region, re: caribb/ip
region <- read.csv("data_20160131.csv", header = TRUE, stringsAsFactors = FALSE)      
head(region)
nrow(region)
names(region) 

unique(region$trait_id)

region2 <- region %>%
  select(specie_id,trait_id,trait_name,value) %>%
  filter(trait_id == 35)
head(region2)

names(region2)
names(region2)[4] <- "region"

d <- left_join(d,region2[,c(1,4)], by = "specie_id")
head(d)


d2 <- d %>%
  select(specie_id,specie_name, family_morphology,region,trait_id,trait_name,value)
head(d2)

genus_split <- strsplit(d2$specie_name, " ")
head(genus_split)

d2$genus <- sapply(genus_split, function(x) x[1]) 
head(d2$genus)
names(d2)
head(d2)

d3 <- d2 %>%
  select(region,specie_id,specie_name,genus,family_morphology,trait_id,trait_name,value)
head(d3)
write.csv(d3, "coraltraits_specieslist_30Jan2016.csv", row.names = FALSE)

d3$esd_value <- recode(d3$value, 
    "'branching_closed' = 'branching';
    'branching_open' = 'branching';
    'corymbose' = 'branching';
    'digitate' = 'branching';
    'laminar' = 'tables_or_plates';
    'encrusting_long_uprights' = 'encrusting';
    'columnar' = 'branching';
    'hispidose' ='branching'")
unique(d3$esd_value)

unique(d3$region)

d3$esd_region <- recode(d3$region,
    "'Indian Ocean' = 'Indo-Pacific';
    'Western and Central Pacific' = 'Indo-Pacific';
    'Eastern Pacific' = 'Indo-Pacific';
    'Western Atlantic' = 'Caribbean';
    'Eastern Atlantic' = 'Caribbean'")
unique(d3$esd_region)
subset(d3, esd_region == NA)

#curate genus list
#subset to veron morphology
d4 <- d3 %>%
  filter(trait_id == 180) %>%
  group_by(esd_region,genus,family_morphology,trait_id,trait_name,esd_value) %>%
  summarize(n())
head(d4)

unique(d4$esd_value)

d4$genus_growthform <- paste(d4$genus,d4$esd_value,sep =" ")
head(d4$genus_growthform)

#coral attribute table
d5 <- d4 %>%
  select(esd_region,family_morphology,genus,genus_growthform)
head(d5)

d5 <- d5[,-c(1:2)]
head(d5)
unique(d5$genus)
d5$group <- "Hard coral"

d5 <- d5[,c("group","family_morphology","genus","genus_growthform","esd_region")]
head(d5)
write.csv(d5, "Scleractinian coral attributes.csv", row.names = FALSE)

