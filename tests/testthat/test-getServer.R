context("Server recon")

statsServer <- "http://slaybox.site.nfoservers.com"

test_that("Recognize server and get nodes",{
	hlnode <- getHlPage(statsServer, "Stats")
	expect_true(grepl("xml_nodeset",class(hlnode)))
	expect_equal(length(hlnode), 1)

})
