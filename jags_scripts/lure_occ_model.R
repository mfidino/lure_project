model{
	for(site in 1:nsite){
		z[site] ~ dbern(psi)
	}
	# Observation model
	for(camera in 1:ncamera){
		for(week in 1:nweek){
		logit(rho[camera, week]) <- D[1] + D[2] * dx[camera,week] + D_ran[site_vec[camera]]
		y[camera, week] ~ dbin(rho[camera,week] * z[site_vec[camera]], J[camera,week])
		}
	}
	psi ~ dbeta(1,1)
	D[1] ~ dt(0, 10, 1)
	D[2] ~ dt(0, 2.5, 1)
	
	for(site in 1:nsite){
		D_ran[site] ~ dnorm(0, tau_d)
		d_mu[site] <- D_ran[site] + D[1]
	}
	tau_d ~ dgamma(0.001,0.001)
	tau_sd <- 1 / sqrt(tau_d)
}

