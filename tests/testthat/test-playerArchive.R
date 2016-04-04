context("Interacting with the player archive")

archLoc <- "/home/computer/Documents/rcode/critArchive.sql"
load(system.file(package = "rhlstats",
	"extdata", "mockServerTable.RData"))
test_that("Archive can be queried",{

	res <- queryPlayerArchive(mockTab, archLoc)

	expect_equal(class(res), "list")

})

