context("time transformers for patient profiles")

test_that("an hyperbolic arc-sin transformer is extracted", {
			
	timeTrans <- getTimeTrans(type = "asinh")
	expect_is(timeTrans, "trans")
	expect_equal(timeTrans$name, "asinh")
	
	x <- c(-100, -10, 1, 0, 1, 10, 100)
	xTrans <- timeTrans$transform(x)
	expect_equal(xTrans, asinh(x)) # transformation
	expect_equal(timeTrans$inverse(xTrans), x) # reverse transformation
			
})

test_that("an hyperbolic arc-sin transformer for negative values is extracted", {
			
	timeTrans <- getTimeTrans(type = "asinh-neg")
	expect_is(timeTrans, "trans")
	expect_equal(timeTrans$name, "asinh_neg")
			
	x <- c(-100, -10, 1, 0, 1, 10, 100)
	xTrans <- timeTrans$transform(x)
	
	# transformation
	expect_equal(xTrans[x < 0], asinh(x[x < 0])) 
	expect_equal(xTrans[x >= 0], x[x >= 0])
	
	# reverse transformation is correct
	expect_equal(timeTrans$inverse(xTrans), x)
			
})

test_that("an hyperbolic arc-sin transformer with scale is extracted", {
			
	scale <- 10
	timeTrans <- getTimeTrans(type = "asinh", scale = scale)
			
	x <- c(-100, -10, 1, 0, 1, 10, 100)
	xTrans <- timeTrans$transform(x)
	expect_equal(xTrans, asinh(x/scale)) # transformation
	expect_equal(timeTrans$inverse(xTrans), x) # reverse transformation
			
})

test_that("the breaks are customized in the transformer", {
			
	formatFct <- formatC
	timeTrans <- getTimeTrans(type = "asinh", formatFct = formatFct)
	expect_equal(timeTrans$format, formatFct)
			
})
