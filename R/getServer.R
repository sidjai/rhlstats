getHlPage <- function(
	pathServer,
	boxName = "//S",
	type = c("header", "table")[1]
	){


	url <- paste0(pathServer, "/stats/hlstats.php")
	page <- xml2::read_html(url)
	xloc <- paste0(
		"//*[@id='accordion']/",
		"tr[contains(@class, 'game-table-row toggler')]")
	totTable <- rvest::html_nodes(page, xpath = xloc)
	avaBoxNames <- rvest::html_text(rvest::html_node(totTable, xpath = "td[1]"))
	boxNum <- grep(boxName, avaBoxNames)

	wantInd <- 1 + boxNum[1] * 2 + switch(type, header = 0, table = 1)

	goodXLoc <- sprintf(
		"//*[@id='accordion']/tr[%d]", wantInd)
	goodNodes <- rvest::html_nodes(page, xpath = goodXLoc)
	return(goodNodes)
}

getSeverInfo <- function(hlpage){


}

getServerTable <-function(hlpage){
	xloc <- '//*[@id="accordion"]/tbody/tr[contains(@class, "game-table-row toggler")]'
}
