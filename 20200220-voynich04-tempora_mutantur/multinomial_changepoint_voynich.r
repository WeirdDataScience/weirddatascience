library( tidyverse )
library( tidyselect )
library( magrittr )

library( rstan )

library( tidytext )

# Load Voynich data
message( "Reading raw Voynich data..." )
voynich_tbl <- 
	read_csv( "data/voynich_raw.csv", col_names=FALSE ) %>%
	rename( folio = X1, text = X2 )

# Tokenize
voynich_words <-
	voynich_tbl %>%
	unnest_tokens( word, text ) 

# Calculate the lengths of words
voynich_pure_lengths <-
	voynich_words %>%
	transmute( word_length = str_length( word ) ) 

voynich_pure_lengths$count <- 1

# Pivot the data wider to be presented to Stan as a matrix of multinomial samples.
voynich_lengths <-
	voynich_words %>% 
	mutate( word_length = str_length( word ) ) %>%
	mutate( word_length = ifelse( word_length > 8, 9, word_length )) %>%
	mutate( word_length = ifelse( word_length < 2, 2, word_length )) %>%
	group_by( folio, word_length ) %>%
	summarise( count = n( )) %>%
	pivot_wider( names_from = word_length, values_from = count ) %>%
	ungroup %>%
	select( -c("folio", "5", "6", "7", "8", "9" )) %>%
	select(sort(peek_vars())) %>%
	replace( is.na(.), 0 )

if( not( file.exists( "work/multinomial_changepoint_voynich_fit.rds" ) ) ) {

	message( "Fitting multinomial model.")
	voynich_seed <- 1912
	num_cats <- ncol( voynich_lengths )
	voynich_multinomial_fit <-
		stan( "multinomial_changepoint.stan", 
			  data=list( 
							num_obs=226, 
							num_cats=num_cats,
							y = as.matrix( voynich_lengths ),
							alpha = rep( 0.6, num_cats ) ),
			  chains=4,
			  iter=8000, seed=voynich_seed,
			  control = list( adapt_delta=0.99,
								  	max_treedepth=12 ) )

	saveRDS( voynich_multinomial_fit, "work/multinomial_changepoint_voynich_fit.rds" )

} else {
	message( "Loading saved multinomial model.")
	voynich_multinomial_fit <- readRDS( "work/multinomial_changepoint_voynich_fit.rds" )
}

# Plot the calculated changepoint probabilities.
# ('changepoint_simplex').
mean_changepoint_prob <-
	extract( voynich_multinomial_fit )$changepoint_simplex %>% 
	as_tibble( .name_repair="unique" ) %>%
	summarise_all( mean ) %>% 
	pivot_longer( everything() ) %>% 
	rowid_to_column() 

# Plot the calculated changepoint probabilities.
# ('changepoint_simplex').
mean_log_p <-
	extract( voynich_multinomial_fit )$log_p %>% 
	as_tibble( .name_repair="unique" ) %>%
	summarise_all( mean ) %>% 
	pivot_longer( everything() ) %>% 
	rowid_to_column() 

# Save values for plotting
saveRDS( mean_changepoint_prob, file="work/mean_changepoint_prob_voynich.rds" )

