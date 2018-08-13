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
						data = "DM",
						paramValueVar  = params,
						type = "text",
						label = "defaultSDTMDMText",
						title = "Demographical information",
						subjectVar =  "USUBJID"
					)
				)
		},
		if("MH" %in% names(data) && 
			all(c("MHDECOD", "MHENRTPT") %in% colnames(data$MH))){
			params <- c("SEX", "AGE", "RACE", "COUNTRY", "ARM")
			params <- params[params %in% colnames(data$DM)]
			if(length(params) > 0)
				list('Medical history (default, text)' = 
					c(
						list(
							data = "MH",
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
			list('Adverse events (default, interval)' = 
				list(
					data = "AE",
					paramVar = "AETERM",
					timeStartVar = "AESTDY",
					timeEndVar = "AEENDY",
					type = "interval",
					label = "defaultSDTMAEInterval",
					title = "Adverse events",
					subjectVar =  "USUBJID"
				),
				if("AESEV" %in% colnames(data$AE))	list(colorVar = "AESEV")
			)
			
		},
		if("LB" %in% names(data) &&
			all(c("LBTEST", "LBDY") %in% colnames(data$LB))){
			list('Laboratory test measurements (default, event)' = 
				c(
					list(
						data = "LB",
						paramVar = "LBTEST",
						timeVar = "LBDY",
						colorVar = "LBNRIND",
						shapeVar = "LBNRIND",
						type = "event",
#						shapePalette = c('LOW' = 25, 'NORMAL' = 19, 'HIGH' = 24,),
						title = "Laboratory test measurements: reference range indicator",
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
		if("LB" %in% names(data) && all(c("LBTESTCD", "LBSTRESN", "LBDY") %in% colnames(data$LB))){
			list('Laboratory test measurements (default, line)' = 
				c(
					list(
						data = "LB",
						paramNameVar = "LBTESTCD",
						paramValueVar = "LBSTRESN",
						paramValueRangeVar = if(all(c("LBSTNRLO", "LBSTNRHI") %in% colnames(data$LB)))
							c("LBSTNRLO", "LBSTNRHI"),
						timeVar = "LBDY",
						type = "line",
						title = "Laboratory test measurements: actual value",
						label = "defaultSDTMLBLine",
						subjectVar =  "USUBJID"
					)
				)
			)
		},
		
		if("EX" %in% names(data) &&
			all(c("EXTRT", "EXSTDY", "EXENDY") %in% names(data$EX))){
			list('Treatment exposure (default, interval)' = 
				c(
					list(
						data = "EX",
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