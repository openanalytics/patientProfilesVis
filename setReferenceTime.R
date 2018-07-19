
setReferenceTime <- function(data, 
	timeVarReference = "RFSTDTC", timeDataset = "DM",
	subjectVar = "USUBJID"){

	timeUnit <- match.arg(timeUnit)
	
	# save label vars
	labelVars <- attributes(data)$labelVars
	
	if(!timeDataset %in% names(data)){
		stop(timeDataset, " not in available datasets in 'dataList'.")
	}else if(timeVarReference %in% colnames(data))
		stop(timeVarReference, " not in dataset: ", timeDataset, ".")
	
	with(data[[timeDataset]], tapply(get(timeVarReference), get(subjectVar), unique))
	
	lapply(names(data), function(topic){
		dataTopic <- data[[topic]]
		colsDate <- names(which(unlist(colwise(function(x) "POSIXct" %in% class(x))(dataTopic)[1, ])))
		names(colsDate) <- sub("DTC$", "DY", colsDate)
		colsDateNameInData <- names(colsDate)[names(colsDate) %in% colnames(dataTopic)]
		if(length(colsDateNameInData) > 0){
			message("Variables: ", toString(colsDateNameInData), 
				" already in dataset: ", topic, ", so they are not created.")
			colsDate <- colsDate[!names(colsDate) %in% colsDateNameInData]
		}
		if(length(colsDate) > 0){
			idxSubject <- match(dataTopic[, subjectVar], data[[timeDataset]][, subjectVar])
			
			data[, colsDate] <- lapply(colsDate, function(colDate)
				difftime(
					time1 = dataTopic[, colDate],
					time2 = data[[timeDataset]][idxSubject, timeVarReference],
					units = "days"
				)
			)
			
		}
			data[, colsDate] <- ddply(data, subjectVar, colwise(function(x)	data[, colsDate]
				
	})
	
}
