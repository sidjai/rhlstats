context("Server recon")

mockPage <- xml2::read_xml(system.file(package = "rhlstats",
	"extdata", "mockHlstatsServerPage.html"), as_html = TRUE)

mockServer <- "http://slaybox.site.nfoservers.com"


test_that("Recognize server and get nodes",{
	hlnode <- getHlServerPage(mockPage, "Stats", shOnlyNodes = TRUE)
	expect_true(grepl("xml_nodeset",class(hlnode)))
	expect_equal(length(hlnode), 1)

})

test_that("server gets different types of nodes",{
	hlnode <- getHlServerPage(mockPage, "Stats", type = "table", shOnlyNodes = TRUE)
	res <- rvest::html_table(
		rvest::html_node(hlnode, xpath = "td[1]/div[1]/table[1]"),
		header = TRUE)[[1]]

	expect_true(grepl("data.frame", class(res)))

	hlnode <- getHlServerPage(mockPage, "West", type = "table", shOnlyNodes = TRUE)
	res <- rvest::html_table(
		rvest::html_node(hlnode, xpath = "td[1]/div[1]/table[1]"),
		header = TRUE)[[1]]

	expect_true(grepl("data.frame", class(res)))

})

test_that("server gives right data.table",{
	hlnode <- getHlServerPage(mockPage, "Stats", type = "table")
	expect_true(grepl("data.frame", class(hlnode)))

})


test_that("Get top players", {
	hltable <- getHlTopPlayers(mockServer)
	expect_true(grepl("data.frame", class(hltable)))
	expect_equal(dim(hltable)[1], 50)
	hltable <- getHlTopPlayers(mockServer, 100)
	expect_equal(dim(hltable)[1], 100)

	for(col in 1:dim(hltable)[2]){
		expect_true(all(!is.na(hltable[,col])))
	}

	hlids <- getHlTopPlayers(mockServer, shJustIds = TRUE)
	expect_true(grepl("integer", class(hlids)))
})

test_that("Session does dead players right", {
	timeDat <- getSessionTimes(mockServer, 24859)
	expect_true(is.data.frame(timeDat))

})
