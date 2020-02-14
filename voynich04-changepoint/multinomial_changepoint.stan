data {

	int<lower=0> num_obs;					// Number of observations (rows/pages) in data.
	int<lower=0> num_cats;					// Number of categories in data.
	int y[num_obs, num_cats];  			// Matrix of observations.

	vector<lower=0>[num_cats] alpha;		// Dirichlet prior values.

}

transformed data {

	// Uniform prior across all time points for changepoint.
	real log_unif;
	log_unif = -log(num_obs);

}

parameters {

	// Two sets of parameters.
	// One (early) before changepoint, one (late) for after.
	simplex[num_cats] theta_e;
	simplex[num_cats] theta_l;

}

transformed parameters {

	//	// Slower, but easier to understand updating of log posterior.
	//  vector[num_obs] lp;
	//  lp = rep_vector(log_unif, num_obs);
	//  for (s in 1:num_obs)
	//    for (t in 1:num_obs)
	//      lp[s] = lp[s] + multinomial_lpmf(y[t,] | t < s ? theta_e : theta_l);

	// This uses dynamic programming to reduce runtime from quadratic to linear in num_obs.
	// See <https://mc-stan.org/docs/2_19/stan-users-guide/change-point-section.html>
	vector[num_obs] log_p;
	{
		vector[num_obs + 1] log_p_e;
		vector[num_obs + 1] log_p_l;

		log_p_e[1] = 0;
		log_p_l[1] = 0;

		for( i in 1:num_obs ) {
			log_p_e[i + 1] = log_p_e[i] + multinomial_lpmf(y[i,] | theta_e );
			log_p_l[i + 1] = log_p_l[i] + multinomial_lpmf(y[i,] | theta_l );
		}

		log_p = 	
			rep_vector( -log(num_obs) + log_p_l[num_obs + 1], num_obs) + 
			head(log_p_e, num_obs) - head(log_p_l, num_obs);
	}
}

model {

	// Priors
	theta_e ~ dirichlet( alpha );
	theta_l ~ dirichlet( alpha );

	target += log_sum_exp( log_p );

}

generated quantities {

	//int<lower=1,upper=num_obs> tau;			// Changepoint
	//int counts_pred[num_obs, num_cats]; 	// Posterior predictions
	simplex[num_obs] changepoint_simplex;	// Simplex of locations for changepoint.

	// Convert the log posterior to a simplex. 
	changepoint_simplex = softmax( log_p );

	// Draw from the categorical RNG. (Giving a single draw of the likely
	// changepoint.)
	//tau = categorical_rng(changepoint_simplex);

	// Log likelihood (for LOO)
	//vector[num_obs] log_lik;

	// // Draws for posterior predictive checking. 
	//	for (n in 1:num_obs) {
	//
	//		//	log_lik[n] = multinomial_lpmf( y[n,] | theta );
	//		counts_pred[n,] = multinomial_rng( n < tau ? theta_e : theta_l, num_cats );
	//
	//	}

}


