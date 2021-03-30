#' Parameters for all patient profiles visualization palette functions.
#' @param x Vector with elements used for palette.
#' If factor, the levels are used, otherwise the unique elements of the vector.
#' Missing values are automatically removed, excepted if
#' \code{includeNA} is set to TRUE.
#' @param includeNA Logical (TRUE by default), 
#' should NA elements be retained in the palette in case
#' \code{x} is specified?
#' @param n Integer of length 1, number of elements in palette.
#' @name patientProfilesVis-palette
NULL

#' Get a linetype palette palette of specified length
#' for patient profile visualization visualization.
#' 
#' Note that 7 unique symbols are available at maximum
#' (replicated if necessary).
#' @inheritParams patientProfilesVis-palette
#' @return character vector values with linetype
#' @author Laure Cougnaud
#' @examples
#' # extract longest linetype palette available
#' getLinetypePalettePatientProfile(n = 6)
#' # extract palette for a vector
#' getLinetypePalettePatientProfile(x = paste('treatment', 1:4))
#' # include missing
#' getLinetypePalettePatientProfile(x = c(NA_character_, "group1"), includeNA = TRUE)
#' @export
getLinetypePalettePatientProfile <- function(
	n = NULL, 
	x = NULL, 
	includeNA = TRUE){
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
				"should be specified.")
	
	if(!is.null(x)){
		x <- if(is.factor(x))	levels(x)	else unique(x)
		if(includeNA){
			x[is.na(x)] <- 'NA'
		}else{
			x <- x[!is.na(x)]
		}
	}
	
	if(is.null(n)) n <- length(x)
	
	palette <- c("solid", "dashed", "dotdash", "twodash", "dotted", "longdash") 
	
	palette <- rep(palette, length.out = n)
	
	if(!is.null(x)){
		
		# set palette names to x
		names(palette) <- x
		
	}else{
		palette <- unname(palette)
	}
	
	return(palette)
	
}

#' Get a shape palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' 
#' Note that 19 unique symbols are available at maximum
#' (replicated if necessary).
#' @param asText Logical (TRUE by default), should the palette
#' be expressed as integer (base R plot and ggplot2 compatible)
#' or in text format (e.g. required if combined with unicode symbols in ggplot2)?
#' @inheritParams patientProfilesVis-palette
#' @return vector of shape values
#' @author Laure Cougnaud
#' @examples
#' #' extract longest shape palette available
#' getShapePalettePatientProfiles(n = 19)
#' # extract palette for a vector
#' getShapePalettePatientProfiles(x = paste('treatment', 1:4))
#' # include missing
#' getShapePalettePatientProfiles(x = c(NA_character_, "group1"), includeNA = TRUE)
#' # get symbols as 'text' (e.g. to be combined with Unicode in ggplot2)
#' getShapePalettePatientProfiles(x = paste('treatment', 1:4), asText = TRUE)
#' @export
getShapePalettePatientProfile <- function(
	n = NULL, 
	x = NULL, 
	includeNA = TRUE,
	asText = TRUE){
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
				"should be specified.")
	
	if(!is.null(x)){
		x <- if(is.factor(x))	levels(x)	else unique(x)
		if(includeNA){
			x[is.na(x)] <- 'NA'
		}else{
			x <- x[!is.na(x)]
		}
	}
	
	if(is.null(n)) n <- length(x)
	
	# see: ggplot2:::translate_shape_string
	palette <- c(
		"square open"           = 0,
		"circle open"           = 1,
		"triangle open"         = 2,
		"plus"                  = 3,
		"cross"                 = 4, # (placebo)
		"diamond open"          = 5,
		"triangle down open"    = 6,
		"square cross"          = 7,
		"asterisk"              = 8,
		"diamond plus"          = 9,
		"circle plus"           = 10,
		"star"                  = 11,
		"square plus"           = 12,
		"circle cross"          = 13,
		"square triangle"       = 14,
#		"triangle square"       = 14,
		"square"                = 15, # (comparator)
#		"circle small"          = 16, # remove it: similar to 'circle'
		"triangle"              = 17,
		"diamond"               = 18,
		"circle"                = 19,
#		"bullet"                = 20, # remove it: similar to 'circle'
		# remove filled symbols, similar to open symbols when fill is not used
		"circle filled"         = 21,
		"square filled"         = 22,
		"diamond filled"        = 23,
		"triangle filled"       = 24,
		"triangle down filled"  = 25
	)
	
	# if required, include correct palette for placebo and comparator
	palette <- rep(palette, length.out = n)
	
	# shape as text (if specified)
	if(asText)	palette <- names(palette)
	
	if(!is.null(x)){
		
		# set palette names to x
		names(palette) <- x
		
	}else{
		
		# remove names
		palette <- unname(palette)
		
	}
	
	return(palette)
	
}

#' Get a color palette of specified length
#' for patient profile visualization visualization.
#' @inheritParams patientProfilesVis-palette
#' @return Character vector with colors,
#' named with the elements in \code{x} if \code{x} is specified.
#' @author Laure Cougnaud
#' @examples 
#' # extract longest palette available
#' getColorPalettePatientProfile(n = 11)
#' # extract palette for a vector
#' getColorPalettePatientProfile(x = paste('treatment', 1:4))
#' # color-blinded palette
#' getColorPalettePatientProfile(n = 10)
#' # no color for missing values:
#' getColorPalettePatientProfile(x = c(NA_character_, "group1"), includeNA = FALSE)
#' @importFrom viridisLite viridis
#' @export
getColorPalettePatientProfile <- function(
	n = NULL, 
	x = NULL, 
	includeNA = TRUE){
	
	if(is.null(x) & is.null(n))
		stop("A vector ('x') or number of colors ('n') ",
				"should be specified.")
	
	if(!is.null(x)){
		x <- if(is.factor(x))	levels(x)	else unique(x)
		if(includeNA){
			x[is.na(x)] <- 'NA'
		}else{
			x <- x[!is.na(x)]
		}
	}
	
	if(is.null(n)) n <- length(x)
	
	palette <- viridis(n)
	
	if(!is.null(x)){
		
		# set palette names to x
		names(palette) <- x
		
	}else{
		
		# remove names
		palette <- unname(palette)
		
	}
	
	return(palette)
	
}