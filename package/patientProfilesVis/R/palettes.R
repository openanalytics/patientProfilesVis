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

# Default palette for shapes for patient profiles
# see: ggplot2:::translate_shape_string
shapesPatientProfiles <- c(
	"circle filled", # 21
	"square filled", # 22
	"diamond filled", # 23
	"triangle filled", # 24
	"triangle down filled", # 25
	"square open", # 0
	"circle open", # 1
	"triangle open", # 2
	"plus", # 3
	"cross", # 4
	"diamond open" , #  5
	"triangle down open", #  6
	"square cross", # 7
	"asterisk", # 8
	"diamond plus", # 9
	"circle plus", # 10
	"star", # 11
	"square plus", # 12
	"circle cross", # 13
	"square triangle", # 14
#	"triangle square" = 14
	"square", # 15, # (comparator)
#	"circle small" = 16 # remove it: similar to 'circle'
	"triangle", # 17
	"diamond", # 18
	"circle" # 19
#	"bullet"                = 20 # remove it: similar to 'circle'
)

#' Get a shape palette of specified length,
#' either from a vector of names for the palette, or
#' from a specified length.
#' 
#' Note that 19 unique symbols are available at maximum
#' (replicated if necessary).
#' @inheritParams patientProfilesVis-palette
#' @return vector of shape values
#' @author Laure Cougnaud
#' @examples
#' #' extract longest shape palette available
#' getShapePalettePatientProfile(n = 19)
#' # extract palette for a vector
#' getShapePalettePatientProfile(x = paste('treatment', 1:4))
#' # include missing
#' getShapePalettePatientProfile(x = c(NA_character_, "group1"), includeNA = TRUE)
#' @export
getShapePalettePatientProfile <- function(
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
	
	palette <- getOption("patientProfilesVis.shapes")
	if(is.null(palette))
		palette <- shapesPatientProfiles
	palette <- if(is.function(palette)){
		palette(n)
	}else{
		rep(palette, length.out = n)
	}
	
	if(!is.null(x)){
		
		# set palette names to x
		names(palette) <- x
		
	}else{
		
		# remove names
		palette <- unname(palette)
		
	}
	
	return(palette)
	
}

# Default palette for colors for patient profiles
#' @importFrom viridisLite viridis
colorsPatientProfiles <- viridisLite::viridis

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
	
	palette <- getOption("patientProfilesVis.colors")
	if(is.null(palette))
		palette <- colorsPatientProfiles
	palette <- if(is.function(palette)){
		palette(n)
	}else{
		rep(palette, length.out = n)
	}
	
	if(!is.null(x)){
		
		# set palette names to x
		names(palette) <- x
		
	}else{
		
		# remove names
		palette <- unname(palette)
		
	}
	
	return(palette)
	
}