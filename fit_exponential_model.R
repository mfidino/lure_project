
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

# create the data list we need for the time_to_event analysis.
ttd <- read.csv(paste0("./data/time_to_detection/", my_species,".csv"))

# d is an indicator that = 0 if we detected a species within a week
#  and is 1 otherwise.
d <- ttd
d[!is.na(d)] <- 0
d[is.na(d)] <- 1

data_list <- list(ttd = as.matrix(ttd),
									tmax = as.matrix(tmax),
									d = as.matrix(d),
									dx = as.matrix(dx),
									precip = as.matrix(read.csv("./data/precip.csv")),
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
	model = "./jags_scripts/time_to_event.R",
	data = data_list,
	n.chains = 6,
	monitor = c("psi_mu", "D","tau_sd", "d_mu"),
	adapt = 1000,
	burnin = 50000, 
	sample = 10000, thin = 5, method = "parallel",
	inits = inits, summarise = FALSE, modules = "glm")

model_array[,,sp_iter] <- as.matrix(as.mcmc.list(model_output), chains = TRUE)[,-1]

}

saveRDS(model_array, "exponential_results_precip.RDS")
a <- apply(exp(model_array[,3,]), 2, quantile, probs = c(0.025,0.5,0.975))
b <- apply((1 / exp(model_array[,3,] + model_array[,2,])), 
2, quantile, probs = c(0.025,0.5,0.975))

a <- model_array[,3,1]
b <- model_array[,2,1]

h <- median(1 / exp(b))
h2 <- median(1 / exp(b+a))

h2 - h

tiff("combo_lure_time.tiff", height = 4, width = 6,
		 units = "in", res = 800, 
		 compression = "lzw")
#windows(height = 4, width = 6, xpos = 10)
# two figures in this plot.
layout(matrix(1:2, ncol = 2, nrow = 1))

# large right margin (2.5) because we are going to be putting species
#  names in between the two plots.
par(mar = c(4,1,0,2.5), xpd = NA)
plot(1~1, type = "n", xlim = c(0,8), ylim = c(1,9), xlab = "",
		 ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n")

# fancy shorter names for the species.
fancy_sp <- c("G. squirrel", "W.t. deer", 
							"E. cottontail", "V. opossum",
							"Raccoon", "F. squirrel",# "Str. skunk",
							"E. chipmunk", "Coyote")

# sorting the species by their baseline detection probability.
mu_t <- order(colMeans(model_array[,2,]))

fancy_sp <- fancy_sp[mu_t]
#axis(2, at= seq(1,9), labels = F, tck = -.025)


minor.ticks.axis <- function(ax,n,t.ratio=0.5,mn,mx,...){
	
	lims <- par("usr")
	if(ax %in%c(1,3)) lims <- lims[1:2] else lims[3:4]
	
	major.ticks <- pretty(lims,n=5)
	if(missing(mn)) mn <- min(major.ticks)
	if(missing(mx)) mx <- max(major.ticks)
	
	major.ticks <- major.ticks[major.ticks >= mn & major.ticks <= mx]
	
	labels <- sapply(major.ticks,function(i)
		2^i
	)
	axis(ax,at=major.ticks,tck = -0.025, labels = FALSE)
	mtext(text = sprintf("%.0f", 2^seq(0,8, 2)), 
				1, line = 0.35, at = seq(0,8, 2))
	
	n <- n+2
	minors <- log2(pretty(2^major.ticks[1:2],n))-major.ticks[1]
	minors <- minors[-c(1,n)]
	
	minor.ticks = c(outer(minors,major.ticks,`+`))
	minor.ticks <- minor.ticks[minor.ticks > mn & minor.ticks < mx]
	
	
	axis(ax,at=minor.ticks,tcl=par("tcl")*t.ratio,labels=FALSE, tck = -0.0125)
}


minor.ticks.axis(1,1,mn=0, mx=8 )



# x axis
#axis(1, at= seq(0,8, 2), labels = F, tck = -0.025)
#axis(1, at= seq(0,2.5, 0.25), labels = F, tck = -0.0125)
#mtext(text = sprintf("%.0f", 2^seq(0,8, 2)), 
#			1, line = 0.35, at = seq(0,25, 5))
mtext(text = 
				"Expected number of\ndays to first detection\nwithout lure (log2 scale)", 1, 
			line = 3, at = 4 ,
			cex = 0.9)


text(x = rep(10.25, 8) - 0.04, y = (1:8 + 0.4) , labels = fancy_sp, pos = 1)
tg <- 0.3
for(sp_iter in 2:8){
	lines(x = c(0,8), y = rep(sp_iter,2) - tg,
				col = "gray70")
}


for(sp_iter in 1:8){
	
	posterior <- log2((1 / exp(model_array[,2,mu_t[sp_iter]])))
	posterior <- posterior[between(posterior,HDIofMCMC(posterior)[1],
																 HDIofMCMC(posterior)[3])]


	my_vioplot(posterior, at = sp_iter - tg, horizontal = TRUE, add = TRUE,
						 side = "right", col = "gray80", drawRect = TRUE,
						 wex = 1.2)
	text(y = sp_iter +0.4, x = median(posterior), 
			 labels = sprintf("%.1f", 2^median(posterior)), cex = 0.7)
	
}

# do the next plot, swapping the margins

par(mar = c(4,2.5,0,1), xpd = NA)
plot(1~1, type = "n", xlim = c(0,3), ylim = c(1,9), xlab = "",
		 ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n")

axis(1, at= seq(0,3, 1), labels = F, tck = -0.025)
axis(1, at= seq(0,3, 0.5), labels = F, tck = -0.0125)
mtext(text = sprintf("%.0f", seq(0,3, 1)), 
			1, line = 0.35, at = seq(0,3, 1))

mtext(text = "Proportional change\nin time to first\ndetection from lure", 1, line = 3, at = 1.75,
			cex = 0.9)

for(sp_iter in 2:8){
	lines(x = c(0, 3), y = rep(sp_iter,2) - tg,
				col = "gray70")
}

for(sp_iter in 1:8){
	
	posterior <- 1 / exp(model_array[,2,mu_t[sp_iter]])
	posterior2 <- 1 / exp(rowSums(model_array[,2:3,mu_t[sp_iter]]))
	posterior <- posterior2 / posterior
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
								 0.8,
								 median(posterior),
								 0.8)
	text(y = sp_iter +0.4, x = text_loc, 
			 labels = sprintf("%.1f", median(posterior)), cex = 0.7)
}
# add vertical line
lines(x = c(1,1), y = c(0.7,8.7), lty = 3, lwd = 2)

dev.off()


