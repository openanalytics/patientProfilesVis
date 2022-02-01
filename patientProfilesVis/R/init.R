#' @importFrom clinUtils clinColors clinShapesText
.onAttach <- function(libname, pkgname) {
	options(patientProfilesVis.colors = clinUtils::clinColors)
	options(patientProfilesVis.shapes = clinUtils::clinShapesText)
}