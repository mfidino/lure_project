library(runjags)
library(rjags)


species_to_use <- c("Eastern gray squirrel", "White tailed deer",
										"Eastern cottontail", "Virginia opossum",
										"Raccoon", "Fox squirrel",
										"Chipmunk", "Coyote")


# read in the detection matrices and fit the binomial model
model_array <- array(NA, dim = c(60000, 25, length(species_to_use)))
for(sp_iter in 1:length(species_to_use)){
	my_species <- species_to_use[sp_iter]
	dm <- read.csv(paste0("./data/detection_history/", my_species, ".csv"))
	
	# convert the daily dm to weekly
	#  We do this because the placement of lures changes each week.
	week_dm <- J <-matrix(NA, ncol = 4, nrow = nrow(dm))
  
	# indexes what week a given day falls in
	my_week <- matrix(1:28, ncol = 4, nrow = 7)
	
	# rowsums of each 7 days. +1 because first column in dm
	#  is the name of the site.
	for(week in 1:4){
	week_dm[,week] <- rowSums(dm[,my_week[,week] + 1], na.rm = TRUE)	
	J[,week] <- 7 - rowSums(is.na(dm[,my_week[,week] + 1]))
	}
	
	# create data list for model
	data_list <- list(y = week_dm,
										J = J,
										dx = as.matrix(read.csv("./data/lure_position.csv")[,-1]),
										ncamera = 40,
										nweek = 4,
										nsite = 20,
										site_vec = rep(1:20, each = 2))
	dy <- data_list$dx
	dy[,2:4] <- 1 - dy[,2:4]
	data_list$dy <- dy
# initial values
	inits <- function(chain){
		gen_list <- function(chain = chain){
			list( 
				z = rep(1,20),
				psi = rbeta(1,1,1),
				D = rnorm(3),
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
		model = "./jags_scripts/lure_occ_model_posthoc.R",
		data = data_list,
		n.chains = 6,
		monitor = c("psi", "D", "tau_sd", "d_mu"),
		adapt = 1000,
		burnin = 50000, 
		sample = 10000, thin = 5, method = "parallel",
		inits = inits, summarise = FALSE, modules = "glm")
	
	model_array[,,sp_iter] <- as.matrix(as.mcmc.list(model_output),
																chains = TRUE)[,-1]
}

# save the model array of analysis for all species.
saveRDS(model_array, "det_prob_posthoc.RDS")


