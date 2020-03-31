#' Get a Galapagos color palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgUtilityFct]{getGLPGColorPalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgUtilityFct]{getGLPGColorPalette}} function).
#' @param includeNA Logical (TRUE by default), should NA elements be retained in the palette.
#' @param ... Additional parameters for the 
#' \code{\link[glpgUtilityFct]{getGLPGColorPalette}} function.
#' @inherit glpgUtilityFct:::getGLPGColorPalette return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getGLPGColorPalette
#' @export
getGLPGColorPalettePatientProfile <- function(...,
	includeNA = TRUE){

	palette <- getGLPGColorPalette(..., includeNA = includeNA)
	
	return(palette)
	
}

#' Get a Galapagos linetype palette palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgUtilityFct]{getGLPGLinetypePalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgUtilityFct]{getGLPGLinetypePalette}} function).
#' @param ... Additional parameters for the 
#' \code{\link[glpgUtilityFct]{getGLPGLinetypePalette}} function.
#' @inheritParams getGLPGColorPalettePatientProfile
#' @inherit glpgUtilityFct:::getGLPGLinetypePalette return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getGLPGLinetypePalette
#' @export
getGLPGLinetypePalettePatientProfile <- function(...,
	includeNA = TRUE){
	
	palette <- getGLPGLinetypePalette(..., includeNA = includeNA)
	
	return(palette)
	
}

#' Get a Galapagos shape palette palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgUtilityFct]{getGLPGShapePalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgUtilityFct]{getGLPGShapePalette}} function).
#' @param ... Additional parameters for the 
#' \code{\link[glpgUtilityFct]{getGLPGShapePalette}} function.
#' @inheritParams getGLPGColorPalettePatientProfile
#' @inherit glpgUtilityFct:::getGLPGShapePalette return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getGLPGShapePalette
#' @export
getGLPGShapePalettePatientProfile <- function(...,
	includeNA = TRUE){
	
	palette <- getGLPGShapePalette(..., includeNA = includeNA)
	
	return(palette)
	
}