#' Parameters for all patient profiles visualization palette functions.
#' @param includeNA Logical (TRUE by default), 
#' should NA elements be retained in the palette in case
#' \code{x} is specified?
#' @name patientProfilesVis-palette
#' @return No return value, used for the documentation of 
#' the palette functions of the package.
NULL

#' Get a shape palette for patient profile
#' visualizations.
#' 
#' This is a simple wrapper around 
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
#' @param asText Logical (TRUE by default), should the palette
#' be expressed as integer (base R plot and ggplot2 compatible)
#' or in text format 
#' (e.g. required if combined with unicode symbols in ggplot2)?
#' @inheritParams patientProfilesVis-palette
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
#' This is a simple wrapper around 
#' \link[clinUtils]{getColorPalette},
#' with different defaults:
#' \itemize{
#' \item{inclusion of missing values
#' by default (\code{includeNA} set to \code{TRUE})
#' }
#' }
#' @inheritParams patientProfilesVis-palette
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