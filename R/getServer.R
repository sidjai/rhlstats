getHlPage <- function(pathServer, boxName = "//S"){
	url <- paste0(pathServer, "/stats/hlstats.php")
	page <- xml2::read_html(url)
	xloc <- paste0(
		"//*[@id='accordion']/",
		"tr[contains(@class, 'game-table-row toggler')]")
	totTable <- rvest::html_nodes(page, xpath = xloc)
	avaBoxNames <- rvest::html_text(rvest::html_node(totTable, xpath = "td[1]"))
	boxNum <- grep(boxName, avaBoxNames)
	return(totTable[boxNum[1]])
}

getSeverInfo <- function(hlpage){


}

getServerTable <-function(hlpage){
	xloc <- '//*[@id="accordion"]/tbody/tr[contains(@class, "game-table-row toggler")]'
}
