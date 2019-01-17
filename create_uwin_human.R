ap <- read.csv("all_human_tags_lure_study.csv",
							 stringsAsFactors = FALSE)

vis <- read.csv("all_visits_chmf.csv",
								stringsAsFactors = FALSE)
vis <- vis[order(vis$locationAbbr),]

# returned 1460 photos


library(dplyr)
library(lubridate)

# get only the columns we want
ap <- ap[,c("photoName", "speciesID", "photoDatetime", "locationAbbr", "utmEast",
						"utmNorth", "utmZone", "visitID", "visitDatetime",
						"activeStart", "activeEnd")]
ap <- ap[order(ap$locationAbbr),]
ap$photoDatetime <- ymd_hms(ap$photoDatetime)
ap$visitDatetime <- ymd_hms(ap$visitDatetime)
ap$photoDate <- ymd(paste0(year(ap$photoDatetime),"-",
													 month(ap$photoDatetime),"-",
													 day(ap$photoDatetime)))

# get only photos between 8/28 and 9/27
ap <- ap[between(ap$photoDate, ymd("2018-8-27"), ymd("2018-9-25")),]

# collect only photos between returns 1447
#ap$photoDate <- factor(as.character(ap$photoDate), levels = sort(unique(ap$photoDate)))
# remove duplicates, now down to 800

ap <- ap[!duplicated(ap),]

# grab only speciesID = 1

human <- ap[ap$speciesID == 1,]

# make a detection matrix

dm <- matrix(NA, ncol = 30, nrow = length(unique(ap$locationAbbr)))

photos_per_day_and_visit <- ap %>% group_by(locationAbbr, visitID,photoDate) %>% 
	summarise(day_photo = n_distinct(photoName))


hm <- vector("list", length = nrow(vis))
for(i in 1:length(hm)){
	hm[[i]] <- seq(as.Date(vis$activeStart[i]), 
								 as.Date(vis$activeEnd[i]),
								 by = "1 day")
	hm[[i]] <- as.numeric(julian(as.Date(hm[[i]]), origin = as.Date("2018-08-26")))
}
# fill in dm
sites_numeric <- as.numeric(factor(vis$locationAbbr))

for(i in 1:length(sites_numeric)){
	dm[sites_numeric[i], hm[[i]]] <- 0
}

colnames(dm) <- paste0("day_", seq(1:30))
row.names(dm) <- unique(vis$locationAbbr)


days_seen <- human %>% group_by(locationAbbr, photoDate) %>% 
	summarise(hum_count = n_distinct(photoName))

days_seen$days <- as.numeric(julian(as.Date(days_seen$photoDate), origin = as.Date("2018-08-26")))

unq_si <- sort(unique(ap$locationAbbr))

for(i in 1:40){
	if(unq_si[i] %in% days_seen$locationAbbr){
		if(all(dm[i,days_seen$days[days_seen$locationAbbr == unq_si[i]]] == 0)){
		dm[i,days_seen$days[days_seen$locationAbbr == unq_si[i]] ] <- 1
		} else {
			stop("you screwed up")
		}
	}
}
edit(dm)


for_nate <- matrix("", ncol = ncol(dm) + 3, nrow = nrow(dm) + 4)
for_nate[1,1:3] <- c("Seasons Legend:", "Start Date", "End Date")
for_nate[2,1:3] <- c("Season 1", "8/27/2018", "9/25/2018")
for_nate[4,] <- c("Species", "Season", "Site", colnames(dm))
for_nate[5:nrow(for_nate), 1] <- "Human"
for_nate[5:nrow(for_nate), 2] <- "1"
for_nate[5:nrow(for_nate), 3] <- row.names(dm)
dm[is.na(dm)] <- "NA"
for_nate[5:nrow(for_nate), 4:ncol(for_nate)] <- dm

write.table(for_nate, "human_1_season.csv", row.names = FALSE, col.names = FALSE,
						sep = ",")
