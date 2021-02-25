#' Arguments used across the functions of the patientProfilesVis package.
#' @param data Data.frame with data.
#' @param colorVar String, variable of \code{data} with color.
#' @param colorLab String, label for \code{colorVar}.
#' @param colorPalette Named vector with color palette.
#' The variable should be named with the corresponding element
#' in \code{colorVar}.
#' @param shapeVar String, variable of \code{data} for shape of the points.
#' By default, same as \code{colorVar}.
#' @param shapeLab String, label for \code{shapeVar}.
#' Set by default to \code{colorLab} if \code{colorVar}
#' but not \code{shapeVar} is not specified.
#' @param shapePalette Named character vector with shape palette
#' for \code{shapeVar}.
#' The variable should be named with the corresponding element
#' in \code{shapeVar}.
#' @param xLab String, label for the x-axis.
#' @param yLab String, label for the y-axis.
#' @param label String, label for the visualization.
#' This label is stored as attributes of the output
#' from the \code{subjectProfile[]Plot} function.\cr
#' This label is displayed in the final profile
#' report, in case no data is available for
#' for a specific patient, as: 'No [label] available.'
#' @param timeVar String, variable of \code{data} with time,
#' displayed in the x axis.\cr
#' Records with missing time are not displayed in the plot.
#' @param timeLab String, label for \code{timeVar}.
#' This is used in the message
#' indicating missing values for \code{timeVar},
#' and for the default label of the x-axis.
#' @param paramVar Character vector with variable(s) of \code{data} 
#' with parameters. Variable content is displayed in the y-axis.
#' @param paramLab Named character vector, 
#' with label for the parameter variable(s) (\code{paramVar}).\cr
#' This is used to set the default title.
#' @param timeLim (optional) Vector of length 2 with time limits (x-axis).
#' If not specified, these are extracted from the minimum \code{timeStartVar} 
#' and maximum \code{timeEndVar} per subject.\cr
#' The time limits are stored as attributes of the plots,
#' used to align the plots in the final report.
#' @param timeTrans transformation for the time variable, 
#' (see \code{trans} parameter in \code{\link[ggplot2]{scale_x_continuous}}, and
#' \code{\link[scales]{trans_new}}).
#' For example, produced by the \code{\link{getTimeTrans}} function.
#' @param timeExpand Vector of range expansion constants for the time axis
#' (see \code{expand} parameter in \code{\link[ggplot2]{scale_x_continuous}}).
#' @name patientProfilesVis-common-args
NULL
