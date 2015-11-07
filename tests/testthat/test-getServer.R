context("Server recon")

statsServer <- "http://slaybox.site.nfoservers.com"

test_that("Recognize server and get nodes",{
	hlnode <- getHlServerPage(statsServer, "Stats", shOnlyNodes = TRUE)
	expect_true(grepl("xml_nodeset",class(hlnode)))
	expect_equal(length(hlnode), 1)

})

test_that("Recognize server gets different types of nodes",{
	hlnode <- getHlServerPage(statsServer, "Stats", type = "table", shOnlyNodes = TRUE)
	res <- rvest::html_table(
		rvest::html_node(hlnode, xpath = "td[1]/div[1]/table[1]"),
		header = TRUE)[[1]]

	expect_true(grepl("data.frame", class(res)))

	hlnode <- getHlServerPage(statsServer, "West", type = "table", shOnlyNodes = TRUE)
	res <- rvest::html_table(
		rvest::html_node(hlnode, xpath = "td[1]/div[1]/table[1]"),
		header = TRUE)[[1]]

	expect_true(grepl("data.frame", class(res)))

})

test_that("Recognize server gives right data.table",{
	hlnode <- getHlServerPage(statsServer, "Stats", type = "table")
	expect_true(grepl("data.frame", class(hlnode)))

})
