model{
	for(site in 1:nsite){
		logit(psi[site]) <- psi_mu
		z[site] ~ dbern(psi[site])
	}
	# Observation model
	for(camera in 1:ncamera){
		for(week in 1:nweek){
		logit(rho[camera, week]) <- D[1] + D[2] * dx[camera,week] + si[site_vec[camera]]
		y[camera, week] ~ dbin(rho[camera,week] * z[site_vec[camera]], J[camera,week])
		}
	}
	psi_mu ~ dt(0, 2.5, 1)
	D[1] ~ dt(0, 2.5, 1)
	D[2] ~ dt(1, 2.5, 1)
	
	for(site in 1:nsite){
		si[site] ~ dnorm(0, site_tau)
	}
	site_tau ~ dgamma(1,1)
}

