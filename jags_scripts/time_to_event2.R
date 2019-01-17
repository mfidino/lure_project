model{
	for(site in 1:nsite){
		logit(psi[site]) <- psi_mu
		z[site] ~ dbern(psi[site])
		for(week in 1:nweek){
			ttd[site,week] ~ dexp(lambda[site,week])
			log(lambda[site,week]) <- D[1] + D[2] * dx[site,week] 
			# model for censoring
			d[site, week] ~ dbern(theta[site,week])
			theta[site, week] <- z[site] * step(ttd[site, week] - tmax[site,week]) + (1 - z[site])
		}
	}
	
	psi_mu ~ dt(0, 2.5, 1)
	D[1] ~ dnorm(0, 0.0001)
	D[2] ~ dnorm(0, 0.0001)
}