context("Set a time transformation")

classTrans <- ifelse(packageVersion("scales") >= "1.3.0", "transform", "trans")

test_that("A hyperbolic arc-sin transformation is correctly extracted", {
			
	timeTrans <- getTimeTrans(type = "asinh")
	expect_s3_class(object = timeTrans, class = classTrans)
	expect_equal(timeTrans$name, "asinh")
	
	x <- c(-100, -10, 1, 0, 1, 10, 100)
	xTrans <- timeTrans$transform(x)
	expect_equal(xTrans, asinh(x)) # transformation
	expect_equal(timeTrans$inverse(xTrans), x) # reverse transformation
			
})

test_that("A hyperbolic arc-sin transformation handling negative values is correctly extracted", {
			
	timeTrans <- getTimeTrans(type = "asinh-neg")
	expect_s3_class(object = timeTrans, class = classTrans)
	expect_equal(timeTrans$name, "asinh_neg")
			
	x <- c(-100, -10, 1, 0, 1, 10, 100)
	xTrans <- timeTrans$transform(x)
	
	# transformation
	expect_equal(xTrans[x < 0], asinh(x[x < 0])) 
	expect_equal(xTrans[x >= 0], x[x >= 0])
	
	# reverse transformation is correct
	expect_equal(timeTrans$inverse(xTrans), x)
			
})

test_that("A hyperbolic arc-sin transformation with scaling factor is correctly extracted", {
			
	scale <- 10
	timeTrans <- getTimeTrans(type = "asinh", scale = scale)
			
	x <- c(-100, -10, 1, 0, 1, 10, 100)
	xTrans <- timeTrans$transform(x)
	expect_equal(xTrans, asinh(x/scale)) # transformation
	expect_equal(timeTrans$inverse(xTrans), x) # reverse transformation
			
})

test_that("The time axis breaks for the transformer is correctly formatted via a function", {
			
	formatFct <- formatC
	timeTrans <- getTimeTrans(type = "asinh", formatFct = formatFct)
	expect_equal(timeTrans$format, formatFct)
			
})
