
library(dplyr)
library(lubridate)
library(runjags)
library(rjags)

species_to_use <- c("Eastern gray squirrel", "White tailed deer",
										"Eastern cottontail", "Virginia opossum",
										"Raccoon", "Fox squirrel",
										"Chipmunk", "Coyote")



model_array <- array(NA, dim = c(60000, 25, length(species_to_use)))
for(sp_iter in 1:length(species_to_use)){
	
	my_species <- species_to_use[sp_iter]

dmat <- read.csv(paste0("./data/detection_history/", my_species,".csv"))[,-1]

# calculate the number of non-NA days per week.
my_week <- matrix(1:28, ncol = 4, nrow = 7)
tmax <- matrix(NA, ncol = 4, nrow = 40)

for(week in 1:4){
	tmax[,week] <- 7 - rowSums(is.na(dmat[,my_week[,week]]))
}


# read in the lure position matrix
dx <- as.matrix(read.csv("./data/lure_position.csv")[,-1])
dy <- dx
dy[,2:4] <- 1 - dy[,2:4]
# create the data list we need for the time_to_event analysis.
ttd <- read.csv(paste0("./data/time_to_detection/", my_species,".csv"))
d <- ttd
d[!is.na(d)] <- 0
d[is.na(d)] <- 1
if(any(ttd > tmax, na.rm = TRUE)){
	stop("whoops")
}

data_list <- list(ttd = as.matrix(ttd),
									tmax = as.matrix(tmax),
									d = as.matrix(d),
									dx = as.matrix(dx),
									dy = as.matrix(dy),
									site_vec = rep(1:20, each = 2),
									ncamera = 40,
									nweek = 4,
									nsite = 20)
# make updated z
z <- rep(1, 20)

ttdst <- data_list$tmax + 1
ttdst[data_list$d == 0] <- NA
inits <- function(chain){
	gen_list <- function(chain = chain){
		list( 
			z = z,
			psi_mu = rbeta(1,1,1),
			D = rnorm(3),
			ttd = ttdst,
			D_ran = rnorm(20),
			.RNG.name = switch(chain,
												 "1" = "base::Wichmann-Hill",
												 "2" = "base::Marsaglia-Multicarry",
												 "3" = "base::Super-Duper",
												 "4" = "base::Mersenne-Twister",
												 "5" = "base::Wichmann-Hill",
												 "6" = "base::Marsaglia-Multicarry",
												 "7" = "base::Super-Duper",
												 "8" = "base::Mersenne-Twister"),
			.RNG.seed = sample(1:1e+06, 1)
		)
	}
	return(switch(chain,           
								"1" = gen_list(chain),
								"2" = gen_list(chain),
								"3" = gen_list(chain),
								"4" = gen_list(chain),
								"5" = gen_list(chain),
								"6" = gen_list(chain),
								"7" = gen_list(chain),
								"8" = gen_list(chain)
	)
	)
}

model_output <- run.jags(
	model = "./jags_scripts/time_to_event_posthoc.R",
	data = data_list,
	n.chains = 6,
	monitor = c("psi_mu", "D","tau_sd", "d_mu"),
	adapt = 1000,
	burnin = 50000, 
	sample = 10000, thin = 5, method = "parallel",
	inits = inits, summarise = FALSE, modules = "glm")

model_array[,,sp_iter] <- as.matrix(as.mcmc.list(model_output), chains = TRUE)[,-1]

}

saveRDS(model_array, "exponential_results_posthoc.RDS")