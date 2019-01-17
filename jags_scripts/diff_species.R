model{
	for(site in 1:nsite){
		y[site] ~ dnorm(mu, tau)
	}
	y_pred ~ dnorm(mu,tau)
	mu ~ dnorm(0, 0.001)
	tau ~ dgamma(0.001,0.001)
	sd <- 1 / sqrt(tau)
	
}