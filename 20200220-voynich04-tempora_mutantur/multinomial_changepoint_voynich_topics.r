library( tidyverse )
library( tidyselect )
library( magrittr )

library( rstan )
library( bayesplot )

library( shinystan )

library( ggplot2 )

library( tidytext )

library( gtools )		# (Specifically for `mixedsort` to sort column names numerically.

# Number of topics in model
num_topics <- 12

# Load Voynich topic model data
message( "Reading Voynich topic model data..." )
voynich_tbl <- 
	readRDS( paste0( "work/topic_identity-", num_topics, ".rds" )) %>%
	select( -c( "gamma", "section" ) ) %>%
	ungroup

# Pivot the data wider to be presented to Stan as a matrix of samples from a multinomial.
voynich_lengths <-
	voynich_tbl %>% 
	mutate( count=1 ) %>%
	pivot_wider( names_from = topic, values_from = "count" ) %>%
	select( -document ) %>%
	select(mixedsort(peek_vars())) %>%
	replace( is.na(.), 0 )

topic_fit_file <- paste0( "work/multinomial_changepoint_voynich_topic_fit-", num_topics, ".rds" )
if( not( file.exists( topic_fit_file ) ) ) {

	message( "Fitting multinomial model.")
	voynich_topic_multinom_fit <-
		stan( "multinomial_changepoint.stan", 
			  data=list( 
							num_obs=226, 
							num_cats=num_topics,
							y = as.matrix( voynich_lengths ),
							alpha = rep( 1, num_topics ) ),
			  iter=8000,
			  seed=19300319,
			  control=list( adapt_delta=0.9 ) )
	saveRDS( voynich_multinom_fit, topic_fit_file )

} else {
	message( "Loading saved multinomial model.")
	voynich_multinom_fit <- readRDS( topic_fit_file )
}

# Extract the calculated changepoint probabilities to a simplex.
mean_changepoint_prob <-
	extract( voynich_multinom_fit )$changepoint_simplex %>% 
	as_tibble( .name_repair="unique" ) %>%
	summarise_all( mean ) %>% 
	pivot_longer( everything() ) %>% 
	rowid_to_column() 

# Save values for plotting
saveRDS( mean_changepoint_prob, file=paste0( "work/mean_changepoint_prob_voynich_topic-", num_topics, ".rds" ) )
