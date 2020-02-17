# ====================	
# faz_determine_best_scale
# ====================	

	# deterrmines the best scale (min, max) for different variables
	# Nuno Prista, SLU Aqua, Sweden	
	# 2018, Developed @ WKBIOPTIM1 and WKBIOPTIM2
	
	
	# 2018-09-10: created
	


		do_determine_best_scale<-function(x = ls_DT_compiled, variables = c("lenCls","age"), stats = c("cv","mwcv"), zero_is_lowest = FALSE)
			{

			out<-sapply(variables, function(x) NULL)
			
			for (variable1 in variables)
				{
					for (stat in stats)
					{
					y<-x[x$variable == variable1,][[stat]]
					z <- range(y, na.rm=T)
					if (zero_is_lowest) z <- c(0, max(z, na.rm=T))
					out[[variable1]][[stat]]<- z
					}
				}
			print(out)
			out		
			}