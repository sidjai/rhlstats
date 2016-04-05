context("Interacting with the player archive")

archLoc <- system.file(package = "rhlstats", "extdata", "critTop5Arch.sql")
load(system.file(package = "rhlstats", "extdata", "mockServerTable.RData"))

test_that("Archive can be queried w/o top people in the server",{

	res <- queryPlayerArchive(mockTab, archLoc)

	expect_equal(class(res), "list")

})


test_that("Archive can be queried w/ top people in the server",{
	addMockTab <- mockTab
	addMockTab[1,"playerid"] <- 3933
	res <- queryPlayerArchive(addMockTab, archLoc)

	expect_equal(class(res), "list")

})

test_that("Archive can be queried w/o any people in the server",{
	emptyMockTab <- mockTab[1,]
	emptyMockTab <- emptyMockTab[-1,]
	res <- queryPlayerArchive(emptyMockTab, archLoc)

	expect_equal(class(res), "list")

})

