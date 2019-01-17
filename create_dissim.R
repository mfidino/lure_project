# we need to 


species_to_use <- c("Eastern gray squirrel", "White tailed deer",
										"Eastern cottontail", "Virginia opossum",
										"Raccoon", "Fox squirrel",
										"Chipmunk", "Coyote")

commat <- array(NA, dim = c(40,4,8))
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
week_dm[week_dm > 1] <- 1
commat[,,sp_iter] <- week_dm

}

# now, we need to figure out what the community looks like at each site
#  based on whether there was lure or not out there.

dx <- as.matrix(read.csv("./data/lure_position.csv")[,-1])

lure_pos <- which(dx == 1)
lure <- no_lure <- array(NA, dim = c(40,2,8))
for(sp_iter in 1:length(species_to_use)){
tmp <- commat[,,sp_iter]
	lure[,,sp_iter] <- 	tmp[lure_pos]
	no_lure[,,sp_iter] <- tmp[-lure_pos]
	rm(tmp)
}

# sum across weeks
lure <- apply(lure, c(1,3), sum)
no_lure <- apply(no_lure, c(1,3), sum)

lure[lure>1] <- 1
no_lure[no_lure>1] <- 1


lure <- zoo::rollapply(lure, 2, by = 2, sum)
no_lure <- zoo::rollapply(no_lure, 2, by = 2, sum)
lure[lure>1] <- 1
no_lure[no_lure>1] <- 1




unlist(tapply(rowSums(lure), list(cumsum(seq_len(nrow(lure)) %% 2)),
							FUN=function(x) c(0, sum(x))))
hm <- rep(NA, 20)
hm_mat <- matrix(0, ncol = 3, nrow = 20)
for(site in 1:20){
	x <- rbind(no_lure[site,], lure[site,])
	
	colnames(x) <- species_to_use
	
	hm_mat[site,] <- unlist(betadiver(x))
	test <- 1 - vegan::vegdist(x, "jaccard", binary = TRUE)
	hm[site] <- test
}
colnames(hm_mat) <- c("shared", "no_lure", "lure")



data_list <- list(y = hm_mat[,3] - hm_mat[,2],
									nsite = 20)

inits <- function(chain){
	gen_list <- function(chain = chain){
		list( 
			mu = rnorm(1),
			tau = rgamma(1,1,1),
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
	model = "./jags_scripts/diff_species.R",
	data = data_list,
	n.chains = 6,
	monitor = c("mu", "sd", "y_pred"),
	adapt = 1000,
	burnin = 10000, 
	sample = 10000, thin = 5, method = "parallel",
	inits = inits, summarise = FALSE, modules = "glm")

summary(model_output)

msum <- as.matrix(as.mcmc.list(model_output), chains = TRUE)

msum <- apply(msum, 2,  quantile, probs = c(0.025,0.5,0.975))


windows(4,4, xpos = 10)
tiff("community_difference.tiff", height = 4, width = 4,
		 units = "in", res = 800, compression = "lzw")
plot(1~1, type = "n", xlim = c(-4,4), ylim = c(0,1), xlab = "",
		 ylab = "", xaxt = "n", yaxt="n", bty = "n", bty = "n")

# x axis
axis(1, at= seq(-4,4, 1), labels = F, tck = -0.025)
axis(1, at= seq(-4,4, 0.5), labels = F, tck = -0.0125)
mtext(text = sprintf("%.0f", seq(-4,4, 1)), 
			1, line = 0.35, at = seq(-4,4, 1), las = 1)
mtext(text = "Difference in community", 1, line = 2.7, at = 0,
			cex = 1)




lines(x = rep(msum[2,2],2), y= c(0,1), lwd = 2)
lines(x = rep(msum[1,2],2), y= c(0,1), lty = 2)
lines(x = rep(msum[3,2],2), y= c(0,1), lty = 2)

lines(x = rep(msum[1,4],2), y= c(0,1), lty = 3)
lines(x = rep(msum[3,4],2), y= c(0,1), lty = 3)


t1 <- sort(data_list$y)
t2 = c(0,seq(0,0.15, length.out = 4), seq(0,0.30, length.out = 8),
			 seq(0,0.20, length.out  =5), 0,0.05)
points(y = t2, x = t1, pch = 21,
			 bg = "white", cex = 1.2)
dev.off()

test <- rbind(lure, no_lure)

example <- metaMDS(test)

treat <- c(rep("lure", 20), rep("no_lure", 20))




ordiplot(example)
ordihull(example, treat, draw = "polygon", col = "grey90", label = TRUE)
