getHlServerPage <- function(
	pathServer,
	boxName = "//S",
	type = c("header", "table")[1],
	shOnlyNodes = FALSE
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

	if(shOnlyNodes){
		return(goodNodes)
	} else {
		res <- switch(type,
			header = getSeverInfo(goodNodes),
			table = getServerTable(goodNodes))
		return(res)
	}

}

getSeverInfo <- function(hlpage){
	nodes <- rvest::html_nodes(hlpage,
		xpath = "td[contains(@class, 'game-table-cell')]")
	text <- html_text(nodes)
	text <- gsub("\\(join\\)", "", gsub("\n", "", text))

	return(text)
}

getServerTable <-function(hlpage){
	res <- rvest::html_table(
		rvest::html_node(hlpage, xpath = "td[1]/div[1]/table[1]"),
		header = TRUE)[[1]]
	return(res)
}
