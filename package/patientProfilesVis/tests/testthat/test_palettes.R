context("Get palettes")

library(viridisLite)

# Unit tests only included for the functionalities specified
# of the patient profiles vis package
# Complete suite of tests available in the clinUtils package

test_that("Missing values are included by default in the color palette", {
			
	xWithNA <- c(NA_character_, "group1")	
	expect_true('NA' %in% names(getColorPalettePatientProfile(x = xWithNA)))		
			
})

test_that("Missing values are correctly not included in the color palette when requested", {
			
	xWithNA <- c(NA_character_, "group1")	
	expect_false('NA' %in% names(getColorPalettePatientProfile(x = xWithNA, includeNA = FALSE)))	
			
})

test_that("Missing values are included by default in the shape palette", {
			
	xWithNA <- c(NA_character_, "group1")	
	expect_true('NA' %in% names(getShapePalettePatientProfile(x = xWithNA)))		
			
})

test_that("Missing values are correctly not included in the shape palette when requested", {
			
	xWithNA <- c(NA_character_, "group1")	
	expect_false('NA' %in% names(getShapePalettePatientProfile(x = xWithNA, includeNA = FALSE)))	
			
})

test_that("The shape palette is extracted as character by default", {
			
	x <- c("a", "b", "c", "d")
	expect_type(
		object = getShapePalettePatientProfile(x = x), 
		type = "character"
	)
			
})

test_that("The shape palette is extracted as integer when requested", {
			
	x <- c("a", "b", "c", "d")
	expect_type(
		object = getShapePalettePatientProfile(x = x, asText = FALSE), 
		type = "integer"
	)
			
})

test_that("The color palette is correctly set as a vector via global options", {
			
	colorsDefault <- getOption("patientProfilesVis.colors")
			
	# set colors to custom palette
	colorPalette <- c("gold", "blue", "green")
	options(patientProfilesVis.colors = colorPalette)
	
	expect_equal(
		getColorPalettePatientProfile(n = 10),
		rep(colorPalette, length.out = 10)
	)
	
	# reset to default
	options(patientProfilesVis.colors = colorsDefault)
	
})

test_that("The shape palette is correctly set as a vector via global options", {
			
	shapesDefault <- getOption("patientProfilesVis.shapes")
			
	# set colors to custom palette
	shapePalette <- c("square", "circle")
	options(patientProfilesVis.shapes = shapePalette)
			
	expect_equal(
		getShapePalettePatientProfile(n = 10),
		rep(shapePalette, length.out = 10)
	)
			
	# reset to default
	options(patientProfilesVis.shapes = shapesDefault)
			
})

test_that("The color palette is correctly set as a function via global options", {
			
	colorsDefault <- getOption("patientProfilesVis.colors")
		
	# set colors to custom palette
	colorPaletteFct <- rainbow
	options(patientProfilesVis.colors = colorPaletteFct)
			
	expect_equal(
		getColorPalettePatientProfile(n = 10),
		colorPaletteFct(10)
	)
			
	# reset to default
	options(patientProfilesVis.colors = colorsDefault)
			
})

test_that("The shape palette is correctly set as a function via global options", {
			
	shapesDefault <- getOption("patientProfilesVis.shapes")
			
	# set shapes to custom palette
	shapePaletteFct <- function(n)
		rep(c("cross", "circle"), length.out = n)
	options(patientProfilesVis.shapes = shapePaletteFct)
			
	expect_equal(
		getShapePalettePatientProfile(n = 10),
		shapePaletteFct(10)
	)
			
	# reset to default
	options(patientProfilesVis.shapes = shapesDefault)
		
})