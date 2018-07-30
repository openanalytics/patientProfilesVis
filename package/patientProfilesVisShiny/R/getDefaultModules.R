#' Get default modules a specific data.
#' This is based on the ADaM dataset names.
#' @param data list of ADaM datasets
#' @return list of list of input parameter for modules
#' @author Laure Cougnaud
#' @export
getDefaultModules <- function(data){
	
	defaultModules <- list()
	defaultModules <- c(
		if("DM" %in% names(data)){
			params <- c("SEX", "AGE", "RACE", "COUNTRY", "ARM")
			params <- params[params %in% colnames(data$DM)]
			if(length(params) > 0)
				list(
					'Demography (default, text)' =
					list(
						dataName = "DM",
						paramValueVar  = params,
						type = "text",
						label = "defaultSDTMDMText",
						title = "Demographical information",
						subjectVar =  "USUBJID"
					)
				)
		},
		if("MH" %in% names(data) && 
			all(c("MHDECOD", "MHENRTPT") %in% colnames(data))){
			params <- c("SEX", "AGE", "RACE", "COUNTRY", "ARM")
			params <- params[params %in% colnames(data$DM)]
			if(length(params) > 0)
				list('Medical history (default, text)' = 
					c(
						list(
							dataName = "MH",
							paramNameVar = "MHDECOD",
							paramValueVar = "MHENRTPT",
							type = "text",
							label = "defaultSDTMMHText",
							title = "Medical history: status",
							subjectVar =  "USUBJID"
						),
						if("MHCAT" %in% colnames(data))	list(paramGroupVar = "MHCAT")
					)
				)
		},
		if("AE" %in% names(data) &&
			all(c("AETERM", "AESTDY", "AEENDY") %in% colnames(data$AE))){
			list('Adverse event (default, interval)' = 
				list(
					dataName = "AE",
					paramVar = "AETERM",
					timeStartVar = "AESTDY",
					timeEndVar = "AEENDY",
					type = "interval",
					label = "defaultSDTMAEInterval",
					title = "Adverse events",
					subjectVar =  "USUBJID"
				)
			)
			
		},
		if("LB" %in% names(data) &&
			all(c("LBTEST", "LBDY") %in% colnames(data$LB))){
			list('Laboratory data (default, event)' = 
				c(
					list(
						dataName = "LB",
						paramVar = "LBTEST",
						timeVar = "LBDY",
						colorVar = "LBNRIND",
						shapeVar = "LBNRIND",
						type = "event",
		#				shapePalette = c('LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24),
						title = "Laboratory test measurements",
						label = "defaultSDTMLBEvent",
						subjectVar =  "USUBJID"
					),
					if("LBNRIND" %in% colnames(data$LB))
						list(colorVar = "LBNRIND", shapeVar = "LBNRIND"),
					if("LBSCAT" %in% colnames(data$LB))
						list(paramGroupVar = "LBSCAT")
				)
			)
		},
		if("EX" %in% names(data) &&
			all(c("EXTRT", "EXSTDY", "EXENDY") %in% names(data$EX))){
			list('Exposure (default, interval)' = 
				c(
					list(
						dataName = "EX",
						paramVar = c("EXTRT",
							if("EXDOSE" %in% names(data$EX))	"EXDOSE", 
							if("EXDOSU" %in% names(data$EX))	"EXDOSU"
						),
						type = "interval",
						timeStartVar = "EXSTDY",
						timeEndVar = "EXENDY",
						label = "defaultSDTMEXInterval",
						title = "Treatment exposure",
						subjectVar =  "USUBJID"
					),
					if("EXDOSFRM" %in% names(data$EX))
						list(colorVar = "EXDOSFRM")
				)
			)
		}
	)
	
	return(defaultModules)
	
}