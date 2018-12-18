ap <- read.csv("all_validated_tags_lure_study.csv",
							 stringsAsFactors = FALSE)

library(dplyr)
library(lubridate)

# get only the columns we want
ap <- ap[,c("photoName", "speciesID", "photoDatetime", "fullName", "utmEast",
						"utmNorth", "utmZone", "visitID", "visitDatetime")]
ap$photoDatetime <- ymd_hms(ap$photoDatetime)
ap$visitDatetime <- ymd_hms(ap$visitDatetime)
ap$photoDate <- ymd(paste0(year(ap$photoDatetime),"-",
													 month(ap$photoDatetime),"-",
													 day(ap$photoDatetime)))

# get only photos between 8/28 and 9/27
ap <- ap[between(ap$photoDate, ymd("2018-8-27"), ymd("2018-9-25")),]

#ap$photoDate <- factor(as.character(ap$photoDate), levels = sort(unique(ap$photoDate)))
# remove duplicates

ap <- ap[!duplicated(ap),]

# grab only speciesID = 1

human <- ap[ap$speciesID == 1,]

# make a detection matrix

dm <- matrix(NA, ncol = 30, nrow = length(unique(ap$fullName)))

photos_per_day_and_visit <- ap %>% group_by(fullName, visitID,photoDate) %>% 
	summarise(day_photo = n_distinct(photoName))

range_sampled <- photos_per_day_and_visit %>% 
	group_by(fullName, visitID) %>% 
	summarise(min = min(photoDate), max = max(photoDate))


hm <- vector("list", length = nrow(range_sampled))
for(i in 1:length(hm)){
	hm[[i]] <- seq(range_sampled$min[i], range_sampled$max[i], by = "1 day")
	hm[[i]] <- as.numeric(julian(as.Date(hm[[i]]), origin = as.Date("2018-08-26")))
}
# fill in dm
sites_numeric <- as.numeric(factor(range_sampled$fullName))

for(i in 1:length(sites_numeric)){
	dm[sites_numeric[i], hm[[i]]] <- 0
}

colnames(dm) <- paste0("day_", seq(1:30))
row.names(dm) <- unique(range_sampled$fullName)


days_seen <- human %>% group_by(fullName, photoDate) %>% 
	summarise(hum_count = n_distinct(photoName))

days_seen$days <- as.numeric(julian(as.Date(days_seen$photoDate), origin = as.Date("2018-08-26")))

unq_si <- sort(unique(ap$fullName))

for(i in 1:40){
	if(unq_si[i] %in% days_seen$fullName){
		dm[i,days_seen$days[days_seen$fullName == unq_si[i]] ] <- 1
	}
}
edit(dm)
