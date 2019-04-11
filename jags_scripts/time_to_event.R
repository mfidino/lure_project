model{
	for(site in 1:nsite){
		z[site] ~ dbern(psi_mu)
	}
	# Observation model
	for(camera in 1:ncamera){
		for(week in 1:nweek){
			ttd[camera,week] ~ dexp(lambda[camera,week])
			log(lambda[camera,week]) <- D[1] + D[2] * dx[camera,week] + D[3] * precip[camera,week] + D_ran[site_vec[camera]]
			# model for censoring
			d[camera, week] ~ dbern(theta[camera,week])
			theta[camera, week] <- (z[site_vec[camera]] * 
				step(ttd[camera, week] - tmax[camera,week])) + 
				(1 - z[site_vec[camera]])
		}
	}
	psi_mu ~ dbeta(1,1)
	D[1] ~ dnorm(0, 0.0001)
	D[2] ~ dnorm(0, 0.0001)
	D[3] ~ dnorm(0, 0.0001)
	
	for(site in 1:nsite){
		D_ran[site] ~ dnorm(0, tau_d)
		d_mu[site] <- D_ran[site] + D[1]
	}
	tau_d ~ dgamma(0.001,0.001)
	tau_sd <- 1 / sqrt(tau_d)
}