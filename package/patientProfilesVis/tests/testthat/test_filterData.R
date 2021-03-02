context("Filter data")

test_that("original data is retained if no filtering criterias are specified", {
			
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

test_that("warning if subject var is specified without subject value", {
			
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

test_that("warning if subject value is specified without subject variable", {
			
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


test_that("data is filtered based on subject variable and subject value", {
			
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

test_that("subset variable is not available", {
			
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

test_that("subjects are filtered based on a variable", {
			
	data1 <- data.frame(
		CAT = c("A", NA_character_, "B", "A"), 
		TERM = c("a1", "a2", "b", "a3"), 
		START = c("01/2020", "02/2020", "01/2019", "03/2021"),
		USUBJID = c("1", "1", "2", "3"),
		stringsAsFactors = FALSE
	)
	data2 <- data1
	colnames(data2)[which(colnames(data2) == "USUBJID")] <- "subject ID"
			
	expect_identical(
		subset(
			filterData(
				data = data1, 
				subsetVar = "CAT", subsetValue = "A",
				subjectVar = "USUBJID"
			),
			select = -USUBJID
		),
		subset(
			filterData(
				data = data2, 
				subsetVar = "CAT", subsetValue = "A",
				subjectVar = "subject ID"
			),
			select = -`subject ID`
		)
	)
			
})

test_that("data is filtered based on external dataset only", {
			
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


test_that("data is filtered based on external dataset, specified variable and value", {
			
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

test_that("data is filtered based on specified subjects", {
			
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

test_that("data is filtered based on random sample of subjects", {
			
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

test_that("seed is specified", {
			
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

