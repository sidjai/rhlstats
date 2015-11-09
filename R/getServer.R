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

getHlTopPlayers <- function(
	pathServer,
	numPlayers = 50,
	rankingType = c("Total", "Week", "Month")[1],
	shJustIds = FALSE
	){

	rankInd <- switch(rankingType, Total = 0, Week = -1, Month = -2)

	playerList <- lapply(1:floor(numPlayers/50), function(pg){
		url <- sprintf(
			"%s/stats/hlstats.php?mode=players&game=tf&rank_type=%d&page=%d",
			pathServer,
			rankInd,
			pg)

		hlpage <- xml2::read_html(url)

		tablexloc <- "//div[2]/div[1]/div[2]/table"
		tableNode <- rvest::html_node(hlpage, xpath = tablexloc)
		tab <- rawTab <- rvest::html_table(
			tableNode,
			header = TRUE)


		idxloc <- "tr[not(contains(@class,'data-table-head'))]/td[2]/a"
		idNodes <- rvest::html_nodes(tableNode, xpath = idxloc)
		ids <- rvest::html_attr(idNodes, "href")

		ids <- regmatches(ids, regexpr("player=(\\d+)", ids))
		ids <- as.integer(gsub("player=", "", ids))

		if(shJustIds){
			names(ids) <- tab$Player
			return(ids)

		} else {
			tab$Rank <- ids
			colnames(tab)[1] <- "playerid"

			actxloc <- "tr[not(contains(@class,'data-table-head'))]/td[4]/img"
			actNodes <- rvest::html_nodes(tableNode, xpath = actxloc)
			act <- rvest::html_attr(actNodes, "style")
			act <- gsub("width|%|:|;", "", act)
			tab$Activity <- as.integer(act)

			tab[, "Connection Time"] <- parseHlTime(tab[, "Connection Time"])


			#Points has an extra character at the end that can't be regexed out
			colnames(tab)[3] <- "Points"
			tab[, "Points"] <- substr(tab[, "Points"], 1, nchar(tab[, "Points"]) - 1)

			easyStrSet <- c("Points", "Kills", "Deaths", "Headshots", "Accuracy")
			for (col in easyStrSet){
				goodVec <- as.integer(gsub("%|,", "", tab[,col]))
				if(col == "Accuracy"){
					tab[, col] <- as.double(goodVec)/100
				} else {
					tab[, col] <- as.integer(goodVec)
				}
			}

			return(tab)
		}
	})

	playerTable <- do.call(rbind, playerList)
	if(shJustIds) playerTable <- as.vector(playerTable)

	return(playerTable)



}

parseHlTime <- function(din){

	timeSplit <- strsplit(din, "d\\S")
	days <- vapply(timeSplit, function(sin){
		as.integer(sin[1])
	}, 1)

	hours <- vapply(timeSplit, function(sin){
		diffTT <- as.difftime(sin[2], format = " %H:%M:%Sh", units = "hours")
		as.numeric(diffTT)
	}, 1.1)

	return((days * 24) + hours)
}

getSessionTimes <- function(pathServer, playerId){

	url <- paste0(
		pathServer,
		"/stats/hlstats.php?mode=playersessions&player=",
		playerId)

	hlpage <- xml2::read_html(url)

	tablexloc <- "//div[2]/div[1]/div[2]/table"
	tableNode <- rvest::html_node(hlpage, xpath = tablexloc)
	tab <- rawTab <- rvest::html_table(
		tableNode,
		header = TRUE)






}
