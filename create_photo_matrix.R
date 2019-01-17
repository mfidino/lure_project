

ap <- read.csv("all_tags_lure_study.csv",
							 stringsAsFactors = FALSE)

vis <- read.csv("all_visits_chmf.csv",
								stringsAsFactors = FALSE)
vis <- vis[order(vis$locationAbbr),]

site_vis <- read.csv("names.csv", 
										 stringsAsFactors = FALSE)
# drop the last visit of each visit for site_vis

unq_si <- unique(site_vis$site)

site_df <- vector("list", length = 40)
for(site in 1:40){
tmp <- site_vis[site_vis$site == unq_si[site],]	
tmp$date <- mdy(tmp$date)
test <- seq(min(tmp$date), max(tmp$date), 1)[1:28]
test <- data.frame(site = unq_si[site], date = test,
									 stringsAsFactors = FALSE)
site_df[[site]] <- test
}



site_df <- rbind.fill(site_df)
site_df$week <- rep(1:4, each = 7)


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

# get only photos between 8/27 and 9/27
ap <- ap[between(ap$photoDate, ymd("2018-8-27"), ymd("2018-9-25")),]

# collect only photos between returns 1447
#ap$photoDate <- factor(as.character(ap$photoDate), levels = sort(unique(ap$photoDate)))
# remove duplicates, now down to 800

ap <- ap[!duplicated(ap),]

# grab only speciesID = 1

species <- ap[ap$commonName == my_species,]

# make a detection matrix
dm <- matrix(NA, ncol = 29, nrow = length(unique(ap$locationAbbr)))

photos_per_day_and_visit <- species %>% group_by(locationAbbr, visitID,photoDate) %>% 
	summarise(day_photo = n_distinct(photoName))

start_date <- rep(mdy(site_vis$date[seq(1, 160, 4)]), each = 4) - 1

test <- left_join(site_df, photos_per_day_and_visit,
									 by = c("site" = "locationAbbr",
									 			  "date" = "photoDate"))

hm <- test %>% group_by(site, week) %>% 
	summarise(n = sum(day_photo, na.rm = TRUE))


dx <- read.csv("./data/lure_position.csv")

aa <- as.numeric(t(dx[,-1]))

hm$lure <- aa

top <- hm %>% group_by(site,lure) %>% 
	summarise(n = sum(n))

y <- matrix(hm$n, ncol = 4, nrow = 40, byrow = TRUE)

write.csv(y, paste0("./data/number_of_photos/", my_species,".csv"),
					row.names = FALSE)
