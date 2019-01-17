model{
	for(site in 1:nsite){
		z[site] ~ dbern(psi)
	}
	# Observation model
	for(camera in 1:ncamera){
		for(week in 1:nweek){
		log(rho[camera, week]) <- D[1] + D[2] * dx[camera,week]+ D_ran[site_vec[camera]]
		p[camera,week] <- r / (r + rho[camera,week])
		y[camera, week] ~ dnegbin(p[camera,week] * z[site_vec[camera]], r)
		}
	}
	no_lure ~ dpois(exp(D[1] + log(4)))
	lure ~ dpois(exp(D[1] + D[2] + log(4)))
	
	psi ~ dbeta(1,1)
	D[1] ~ dnorm(0, 0.0001)
	D[2] ~ dnorm(0, 0.0001)
	
	for(site in 1:nsite){
		D_ran[site] ~ dnorm(0, tau_d)
		d_mu[site] <- D_ran[site] + D[1]
	}
	tau_d ~ dgamma(0.001,0.001)
	tau_sd <- 1 / sqrt(tau_d)
	r ~ dunif(0,50)
}

