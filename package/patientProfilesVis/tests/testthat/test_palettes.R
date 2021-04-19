context("Palettes")

library(viridisLite)

test_that("Missing values in palette is extracted as specified", {
      xWithNA <- c(NA_character_, "group1")
      expect_false('NA' %in% names(getColorPalettePatientProfile(x = xWithNA, includeNA = FALSE)))
      expect_true('NA' %in% names(getColorPalettePatientProfile(x = xWithNA)))
      expect_false('NA' %in% names(getShapePalettePatientProfile(x = xWithNA, includeNA = FALSE)))
      expect_true('NA' %in% names(getShapePalettePatientProfile(x = xWithNA)))
    })

test_that("Empty values in palette is retained", {
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getColorPalettePatientProfile(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getShapePalettePatientProfile(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
    })

test_that("Default palette is viridis", {
      
      expect_identical(
          getColorPalettePatientProfile(n = 1),
          viridis(n = 1)
      )     
      expect_identical(
          getColorPalettePatientProfile(n = 2),
          viridis(n = 2)
      )
      
    })

test_that("Color palette is successfully set as a vector via global options", {
			
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

test_that("Shape palette is successfully set as a vector via global options", {
			
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

test_that("Color palette is successfully set as a function via global options", {
			
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

test_that("Shape palette is successfully set as a function via global options", {
			
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

test_that("Unnamed palette", {
      
      expect_named(getColorPalettePatientProfile(n = 1), NULL)
      expect_named(getShapePalettePatientProfile(n = 1), NULL)
      
    })



