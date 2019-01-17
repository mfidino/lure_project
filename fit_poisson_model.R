library(plyr)
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
	
	y <- read.csv(paste0("./data/number_of_photos/", my_species,".csv"))
	dx <- read.csv("./data/lure_position.csv")

	data_list <- list(y = as.matrix(y),
										ncamera = 40,
										nsite = 20,
										nweek = 4,
										site_vec = rep(1:20, each = 2),
										dx = as.matrix(dx[,-1]))
	
	
	
	inits <- function(chain){
		gen_list <- function(chain = chain){
			list( 
				z = rep(1,20),
				psi = rbeta(1,1,1),
				D = rnorm(2),
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
		model = "./jags_scripts/lure_occ_poisson.R",
		data = data_list,
		n.chains = 6,
		monitor = c("psi", "D", "tau_sd", "d_mu", "lure_effect"),
		adapt = 1000,
		burnin = 10000, 
		sample = 10000, thin = 5, method = "parallel",
		inits = inits, summarise = FALSE, modules = "glm")
	
	model_array[,,sp_iter] <- as.matrix(as.mcmc.list(model_output), chains = TRUE)[,-1]
	}

# check out results
res <- exp(apply(model_array[,2,], 2, HDIofMCMC))
colnames(res) <- species_to_use
t(res)
windows(4,4,xpos = 10)
tiff("photos_per_week.tiff", height = 4, width = 4,
		 units = "in", res = 800, 
		 compression = "lzw")
# two figures in this plot.

# large right margin (2.5) because we are going to be putting species
#  names in between the two plots.
par(mar = c(4,6,1,0.5))
plot(1~1, type = "n", xlim = c(0,2.5), ylim = c(1,8.5), xlab = "",
		 ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "l")

# fancy shorter names for the species.
fancy_sp <- c("E. gray sq.", "W.t. deer", 
							"E. cottontail", "V. opossum",
							"Raccoon", "Fox sq.",#, "Str. skunk",
							"E. chipmunk", "Coyote")

# sorting the species by their baseline detection probability.
mu_t <- order(apply(model_array[,25,], 2 ,median), decreasing = FALSE)

fancy_sp <- fancy_sp[mu_t]
#axis(2, at= seq(1,9), labels = F, tck = -.025)

# x axis
axis(1, at= seq(0,3, 0.5), labels = F, tck = -0.025)
axis(1, at= seq(0,3, 0.25), labels = F, tck = -0.0125)
mtext(text = sprintf("%.1f", seq(0,3, 0.5)), 
			1, line = 0.35, at = seq(0,3, 0.5), las = 1)


axis(2, at= seq(1,8, 1), labels = F, tck = -0.025)

mtext(text = fancy_sp, 
			2, line = 0.8, at = seq(1,8, 1), las = 1)

mtext(text = "Proportion change in photos taken\nper week from lure",1,
			at = 1.25, line = 2.5)

#text(x = rep(19, 8) - 0.04, y = (1:8 + 0.4) , labels = fancy_sp, pos = 1)
#tg <- 0.3
#for(sp_iter in 1:2){
#	lines(x = c(0,60), y = rep(sp_iter,2),
#				col = "gray70")
#}


for(sp_iter in 1:8){

	posterior <- model_array[,25,mu_t[sp_iter]]
	#posterior2 <- exp(rowSums(model_array[,2:3,mu_t[sp_iter]]))
	#posterior <- posterior2 / posterior
	posterior <- posterior[between(posterior,HDIofMCMC(posterior)[1],
																 HDIofMCMC(posterior)[3])]
	
	my_vioplot(posterior, at = sp_iter , horizontal = TRUE, add = TRUE,
						 side = "right", col = "gray80", drawRect = TRUE,
						 wex = 1.5)
	#text(y = sp_iter +0.4, x = median(posterior), 
	#		 labels = sprintf("%.0f", median(posterior)), cex = 0.7)
}
abline(v = 1, lwd = 2, lty = 3)
dev.off()

# do the next plot, swapping the margins

par(mar = c(4,2.5,0,1), xpd = NA)
plot(1~1, type = "n", xlim = c(-.15,0.15), ylim = c(1,9), xlab = "",
		 ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n")

axis(1, at= seq(-.15,0.15, 0.1), labels = F, tck = -0.025)
axis(1, at= seq(-.15,0.15, 0.05), labels = F, tck = -0.0125)
mtext(text = sprintf("%.2f", seq(-.15,0.15, 0.1)), 
			1, line = 0.35, at = seq(-.15,0.15, 0.1))

mtext(text = "Difference in daily detection\n probability from lure", 1, line = 2.7, at = 0,
			cex = 1)

for(sp_iter in 2:8){
	lines(x = c(-0.15, 0.15), y = rep(sp_iter,2) - tg,
				col = "gray70")
}

for(sp_iter in 1:8){
	
	posterior <- plogis(model_array[,2,mu_t[sp_iter]])
	posterior2 <- plogis(model_array[,2,mu_t[sp_iter]] + model_array[,3,mu_t[sp_iter]])
	posterior <- posterior2 - posterior
	
	posterior <- posterior[between(posterior,HDIofMCMC(posterior)[1],
																 HDIofMCMC(posterior)[3])]
	
	my_vioplot(posterior, at = sp_iter -tg, horizontal = TRUE, add = TRUE,
						 side = "right", col = "gray80", drawRect = TRUE,
						 wex = 1.2 )
	text_loc <- switch(sp_iter,
										 median(posterior),
										 median(posterior),
										 median(posterior),
										 median(posterior),
										 median(posterior),
										 median(posterior),
										 median(posterior),
										 0.02)
	the_text <-  switch(sp_iter,
											sprintf("%.2f", median(posterior)),
											sprintf("%.2f", median(posterior)),
											sprintf("%.2f", median(posterior)),
											sprintf("%.2f", median(posterior)),
											sprintf("%.2f", median(posterior)),
											sprintf("%.2f", median(posterior)),
											sprintf("%.2f", median(posterior)),
											"0.00")
	text(y = sp_iter +0.4, x = text_loc, 
			 labels = the_text, cex = 0.7)
}
# add vertical line
lines(x = c(0,0), y = c(0.7,8.7), lty = 3, lwd = 2)

dev.off()