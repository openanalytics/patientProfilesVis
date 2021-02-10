#' Arguments used across the functions of the patientProfilesVis package.
#' @param data Data.frame with data.
#' @param colorVar String, variable of \code{data} with color.
#' @param colorLab String, label for \code{colorVar}.
#' @param colorPalette Named vector with color palette.
#' The variable should be named with the corresponding element
#' in \code{colorVar}.
#' @param xLab String, label for the x-axis.
#' @param yLab String, label for the y-axis.
#' @param label String, label for the visualization.
#' This label is used when the visualization is 
#' included in the final patient profile report,
#' in case this visualization is empty (no data available)
#' for a specific subject.
#' @param paramVar Character vector with variable(s) of \code{data} 
#' with parameters. Variable content is displayed in the y-axis.
#' @param paramLab Named character vector, 
#' with label for the parameter variable(s) (\code{paramVar}).\cr
#' This is used to set the default title.
#' @name patientProfilesVis-common-args
NULL
