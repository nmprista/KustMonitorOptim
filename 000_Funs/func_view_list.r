	view_list<-function(x){	
		#visualization of the structure of the final object res [wish list - simplify this]
		# see more in: https://stackoverflow.com/questions/32618369/is-there-a-way-to-view-a-list
		
		# x is a list
		
		devtools::install_github(c('jeroenooms/jsonlite', 'rstudio/shiny', 'ramnathv/htmlwidgets', 'timelyportfolio/listviewer'))
		library(listviewer)
		jsonedit( out )
		
		}