library( tidyverse )
library( magrittr )

library( ggthemes )
library( showtext )

library( cowplot )
library( grimoire )

# Fonts
font_add( "main_font", "resources/fonts/alchemy/1651 Alchemy/1651AlchemyNormal.otf")
font_add( "bold_font", "resources/fonts/alchemy/1651 Alchemy/1651AlchemyNormal.otf")

showtext_auto()

mean_changepoint_prob <-
	readRDS( "work/mean_changepoint_prob_tirant.rds" )

# Remove legend from internal plot
theme_set(theme_cowplot(font_size=4, font_family = "main_font" ) )  

changepoint_plot <-
	ggplot( mean_changepoint_prob ) + 
	geom_col( aes( x=rowid, y=value ), fill=weird_colours["blood"] ) +
	labs( x="Chapter", y="Probability of Changepoint" ) +
	theme( 
			panel.background = element_rect(fill = "transparent", colour = "transparent"),
			plot.background = element_rect(fill = "transparent", colour = "transparent"),
			plot.title = element_text( family="bold_font", colour=weird_colours["ink"], size=22 ),
			plot.subtitle = element_text( family="bold_font", colour=weird_colours["ink"], size=12 ),
			axis.text = element_text( family="bold_font", colour=weird_colours["ink"], size=12 ),
			axis.title.x = element_text( family="bold_font", colour=weird_colours["ink"], size=12 ),
			axis.title.y = element_text( family="bold_font", angle=90, colour=weird_colours["ink"], size=12 ),
			axis.line = element_line( colour=weird_colours["ink"] ),
			panel.grid.major.x = element_blank(),
			panel.grid.major.y = element_blank(),
			panel.grid.minor.x = element_blank(),
			panel.grid.minor.y = element_blank()
			) 


# grimoire::decorate_plot() from <https://github.com/weirddatascience/grimoire>
parchment_plot <-
	decorate_plot( title="Tirant lo Blanc Chapter Changepoint Probability", 
					  	subtitle="http://www.weirddatascience.net | @WeirdDataSci", 
						footer="Data: http://einesdellengua.com/tirantloweb/tirantloblanch.html",
						plot=changepoint_plot, 
						bg_image="resources/img/parchment.jpg" )

save_plot("output/multinomial_changepoint_tirant_plot.pdf", 
							parchment_plot,
							base_width = 16,
							base_height = 9,
			           	base_aspect_ratio = 1.78 )
