
ap <- read.csv("all_tags_lure_study.csv",
							 stringsAsFactors = FALSE)

vis <- read.csv("all_visits_chmf.csv",
								stringsAsFactors = FALSE)
vis <- vis[order(vis$locationAbbr),]

site_vis <- read.csv("names.csv", 
										 stringsAsFactors = FALSE)
# drop the last visit of each visit for site_vis
site_vis <- site_vis[-seq(5,200,5),]

site_vis$week <- rep(1:4, 160/4)

# get only the columns we want
ap <- ap[,c("photoName", "speciesID", "photoDatetime", "locationAbbr", "utmEast",
						"utmNorth", "utmZone", "visitID", "visitDatetime",
						"activeStart", "activeEnd", "commonName")]
ap <- ap[order(ap$locationAbbr),]
ap$photoDatetime <- ymd_hms(ap$photoDatetime)
ap$visitDatetime <- ymd_hms(ap$visitDatetime)
ap$photoDate <- ymd(paste0(year(ap$photoDatetime),"-",
													 month(ap$photoDatetime),"-",
													 day(ap$photoDatetime)))

# get only photos between 8/28 and 9/27
ap <- ap[between(ap$photoDate, ymd("2018-8-27"), ymd("2018-9-25")),]


ap <- ap[!duplicated(ap),]

# grab a specific species
species <- ap[ap$commonName == my_species,]


# grab first detection per visit ID
species_first <- species %>% group_by(visitID) %>% 
	summarise(first_photo = min(photoDatetime),
						site = unique(locationAbbr),
						start_date = ymd(as.Date(ymd_hms(min(activeStart)))))
ttd <- rep(NA, nrow(species_first))
for(i in 1:nrow(species_first)) {
	tmp_date <- site_vis[site_vis$site ==  species_first$site[i],]
	# determines which week of sampling
	tmp_date$visit_date <- mdy_hm(paste(tmp_date$date, tmp_date$time))
	tmp <- sum(tmp_date$visit_date < species_first$first_photo[i])
	
	
ttd[i]	<- difftime(species_first$first_photo[i], tmp_date$visit_date[tmp],
										units = "days")
}

species_first$ttd <- as.numeric(ttd)

# add other relevant info

species_seen <- inner_join(species_first, 
									species[,c("visitID", "locationAbbr", "visitDatetime")],
									by = "visitID") %>% 
									distinct %>% 
									arrange(locationAbbr, visitDatetime)
#species_seen$ttd <- species_seen$first_photo - species_seen$start

species_seen$start_date <- as.character(species_seen$start_date)

site_vis$date <- as.character(mdy(site_vis$date))

ttd <- left_join(site_vis, species_seen, by = c("site" = "locationAbbr",
																								 "date" = "start_date")) %>% 
	arrange(site, date)


# now we need to make the detection matrix for the ttd stuff. We can
#  fill it by row.

ttd_mat <- matrix(ttd$ttd, ncol = 4, nrow = 40, byrow = TRUE)

# make the d matrix, this take a value of 0 if detected, 1 otherwise.


d <- matrix(0, ncol = 4, nrow = 40)

d[is.na(ttd_mat)] <- 1
# need to calculate tmax for each sampling occasion. To do this we need
#  to bring in the detection matrix from the first analysis.

write.csv(ttd_mat, paste0("./data/time_to_detection/", my_species, ".csv"),
					row.names = FALSE)

