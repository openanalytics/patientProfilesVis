#' Get a Galapagos color palette of specified length
#' for patient profile visualization visualization.
#' This is only a wrapper above \code{\link[glpgUtilityFct]{getGLPGColorPalette}},
#' including by default missing values (contrary to the default of the
#' code{\link[glpgUtilityFct]{getGLPGColorPalette}} function).
#' @param includeNA Logical (TRUE by default), should NA elements be retained in the
#' color palette.
#' @param ... Additional parameters for the 
#' \code{\link[glpgUtilityFct]{getGLPGColorPalette}} function.
#' @inheritParams glpgUtilityFct:::getGLPGColorPalette
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
#' @param includeNA Logical (TRUE by default), should NA elements be retained in the
#' color palette.
#' @param ... Additional parameters for the 
#' \code{\link[glpgUtilityFct]{getGLPGLinetypePalette}} function.
#' @inheritParams glpgUtilityFct:::getGLPGLinetypePalette
#' @inherit glpgUtilityFct:::getGLPGLinetypePalette return
#' @author Laure Cougnaud
#' @importFrom glpgUtilityFct getGLPGLinetypePalette
#' @export
getGLPGLinetypePalettePatientProfile <- function(...,
	includeNA = TRUE){
	
	palette <- getGLPGLinetypePalette(..., includeNA = includeNA)
	
	return(palette)
	
}