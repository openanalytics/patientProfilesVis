context("Palettes")

library(viridisLite)

test_that("Missing values in palette is extracted as specified", {
      xWithNA <- c(NA_character_, "group1")
      expect_false('NA' %in% names(getColorPalettePatientProfile(x = xWithNA)))
      expect_true('NA' %in% names(getColorPalettePatientProfile(x = xWithNA, includeNA = TRUE)))
      expect_false('NA' %in% names(getShapePalettePatientProfile(x = xWithNA)))
      expect_true('NA' %in% names(getShapePalettePatientProfile(x = xWithNA, includeNA = TRUE)))
    })

test_that("Empty values in palette is retained", {
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getColorPalettePatientProfile(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getShapePalettePatientProfile(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
      xWithEmpty <- c("", "group1")
      expect_silent(palette <- getGLPGLinetypePalette(x = xWithEmpty))
      expect_equal(names(palette), xWithEmpty)
    })

test_that("Default palette is viridis", {
      
      expect_identical(
          getColorPalettePatientProfile(n = 1, type = "viridis"),
          viridis(n = 1)
      )     
      expect_identical(
          getColorPalettePatientProfile(n = 2, type = "viridis"),
          viridis(n = 2)
      )
      
    })


test_that("Unnamed palette", {
      
      expect_named(getColorPalettePatientProfile(n = 1), NULL)
      expect_named(getShapePalettePatientProfile(n = 1), NULL)
      
    })



