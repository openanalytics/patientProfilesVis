#' Get a Galapagos color palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgStyle]{getGLPGColorPalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgStyle]{getGLPGColorPalette}} function).
#' @param includeNA Logical (TRUE by default), should NA elements be retained in the palette.
#' @param ... Additional parameters for the 
#' \code{\link[glpgStyle]{getGLPGColorPalette}} function.
#' @inherit glpgStyle:::getGLPGColorPalette return
#' @author Laure Cougnaud
#' @importFrom glpgStyle getGLPGColorPalette
#' @export
getGLPGColorPalettePatientProfile <- function(...,
	includeNA = TRUE){

	palette <- getGLPGColorPalette(..., includeNA = includeNA)
	
	return(palette)
	
}

#' Get a Galapagos linetype palette palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgStyle]{getGLPGLinetypePalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgStyle]{getGLPGLinetypePalette}} function).
#' @param ... Additional parameters for the 
#' \code{\link[glpgStyle]{getGLPGLinetypePalette}} function.
#' @inheritParams getGLPGColorPalettePatientProfile
#' @inherit glpgStyle:::getGLPGLinetypePalette return
#' @author Laure Cougnaud
#' @importFrom glpgStyle getGLPGLinetypePalette
#' @export
getGLPGLinetypePalettePatientProfile <- function(...,
	includeNA = TRUE){
	
	palette <- getGLPGLinetypePalette(..., includeNA = includeNA)
	
	return(palette)
	
}

#' Get a Galapagos shape palette palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgStyle]{getGLPGShapePalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgStyle]{getGLPGShapePalette}} function).
#' @param ... Additional parameters for the 
#' \code{\link[glpgStyle]{getGLPGShapePalette}} function.
#' @inheritParams getGLPGColorPalettePatientProfile
#' @inherit glpgStyle:::getGLPGShapePalette return
#' @author Laure Cougnaud
#' @importFrom glpgStyle getGLPGShapePalette
#' @export
getGLPGShapePalettePatientProfile <- function(...,
	includeNA = TRUE){
	
	palette <- getGLPGShapePalette(..., includeNA = includeNA)
	
	return(palette)
	
}