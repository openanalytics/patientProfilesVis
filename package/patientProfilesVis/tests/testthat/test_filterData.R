context("Filter data")

test_that("The original data is retained when no filtering criteria are specified", {
			
	data <- data.frame(
		CAT = c("A", "A", "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
	expect_identical(
		filterData(data),
		data
	)			
			
})

test_that("A warning is generated is a filter variable is specified without a value", {
			
	data <- data.frame(
		CAT = c("A", "A", "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
	expect_warning(
		dataFiltered <- filterData(data, subsetVar = "CAT"),
		"variable.*not used.*no subset value"
	)	
	expect_identical(dataFiltered, data)
			
})

test_that("A warning is generated is a filter value is specified without a filter variable", {
			
	data <- data.frame(
		CAT = c("A", "A", "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
			
	expect_warning(
		dataFiltered <- filterData(data, subsetValue = "A"),
		"value.*not used.*no subset variable"
	)	
	expect_identical(dataFiltered, data)
			
})


test_that("The data is correctly filtered based on a filter variable and value", {
			
	data <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
			
	expect_identical(
		filterData(
			data = data, 
			subsetVar = "CAT", subsetValue = "A"
		),
		subset(data, CAT == "A")
	)	
	
})

test_that("A warning is generated when a filter variable is not available in the data", {
			
	data <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
	expect_warning(
		dataFiltered <- filterData(data, subsetVar = "CAT2"),
		"Subset variable.*not in the data"
	)
	
})

test_that("The data is correctly filtered based on a specified dataset", {
			
	data <- data.frame(
		CAT = c("A", "A", "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
			
	dataSubset <- data.frame(USUBJID = c("1", "2"))
			
	expect_identical(
		filterData(
			data = data, 
			subsetData = dataSubset
		),
		subset(data, USUBJID %in% c("1", "2"))
	)
			
})

test_that("The data is correctly filtered based on a specified dataset and custom subject variable", {
			
	data <- data.frame(
		CAT = c("A", "A", "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		`subject ID` = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE,
		check.names = FALSE
	)
			
	dataSubset <- data.frame(
		`subject ID` = c("1", "2"), 
		check.names = FALSE
	)
			
	expect_identical(
		filterData(
			data = data, 
			subsetData = dataSubset,
			subjectVar = "subject ID"
		),
		subset(data, `subject ID` %in% c("1", "2"))
	)
			
})

test_that("The data is correctly filtered based on a specified variable and value in a specified dataset", {
			
	data <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
	
	dataSubset <- data.frame(
		TRT = c("A", "B", "A", "B", "B"), 
		USUBJID = c("1", "2", "3", "4", "5")
	)
			
	expect_identical(
		filterData(
			data = data, 
			subsetData = dataSubset,
			subsetVar = "TRT", subsetValue = "A"
		),
		subset(data, USUBJID %in% c("1", "3"))
	)
			
})

test_that("The data is correctly filtered based on specified subjects", {
			
	data <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
			
	expect_identical(
		filterData(data, subjectSubset = "3"),
		subset(data, USUBJID == "3")
	)
			
})

test_that("The data is correctly filtered based on a random sample of subjects", {
			
	data <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
			
	expect_length(
		unique(filterData(data, subjectSample = "2")$USUBJID),
		2
	)
			
})

test_that("When a seed is specified, the random sample remains identical", {
			
	data <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
			
	seed <- 567
	set.seed(seed)
	subjectIDs <- sample(unique(data$USUBJID), 2)
	
	expect_setequal(
		filterData(data, subjectSample = "2", seed = seed)$USUBJID,
		subjectIDs
	)
	
})

