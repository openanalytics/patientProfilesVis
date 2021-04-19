#' Get a shape palette for patient profile
#' visualizations.
#' 
#' This is a simple wrapper around the 
#' \link[clinUtils]{getShapePalette},
#' with different defaults:
#' \itemize{
#' \item{inclusion of missing values by
#' default (\code{includeNA} set to \code{TRUE})
#' }
#' \item{the extraction of shapes as text
#' by default (\code{asText} set to \code{TRUE})
#' }
#' }
#' @inheritDotParams clinUtils::getShapePalette
#' @inherit clinUtils::getShapePalette return
#' @author Laure Cougnaud
#' @seealso \link[clinUtils]{getShapePalette}
#' @importFrom clinUtils getShapePalette
#' @export
getShapePalettePatientProfile <- function(
	...,
	includeNA = TRUE, asText = TRUE){

	palette <- getShapePalette(
		...,
		includeNA = includeNA,
		asText = asText,
		palette = getOption("patientProfilesVis.shapes")
	)
	
}

#' Get a color palette for patient profile
#' visualizations.
#' 
#' This is a simple wrapper around the 
#' \link[clinUtils]{getColorPalette},
#' with different defaults:
#' \itemize{
#' \item{inclusion of missing values
#' by default (\code{includeNA} set to \code{TRUE})
#' }
#' }
#' @inheritDotParams clinUtils::getColorPalette
#' @inherit clinUtils::getShapePalette return
#' @author Laure Cougnaud
#' @seealso \link[clinUtils]{getColorPalette}
#' @importFrom clinUtils getColorPalette
#' @export
getColorPalettePatientProfile <- function(..., includeNA = TRUE){
	
	palette <- getColorPalette(
		...,
		includeNA = includeNA,
		palette = getOption("patientProfilesVis.colors")
	)
	
	return(palette)
	
}