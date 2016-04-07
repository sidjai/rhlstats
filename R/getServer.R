#' Driver to scrape the hlstats main pages
#'
#' @param pathServer The url to the main hlstats page
#' @param boxName The name of the area that you want to capture
#' @param type regex of the box that you want captured
#' @param shOnlyNodes Return the xml nodes or the tables
#'
#' @return Either the xml nodes or a string matrix of the table
#' @export
getHlServerPage <- function(
	pathServer,
	boxName = "\\S",
	type = c("header", "table")[1],
	shOnlyNodes = FALSE
	){

	url <- pathServer
	if(!any(grepl("xml_document", class(page)))){
		url <- paste0(pathServer, "/stats/hlstats.php")
		page <- xml2::read_html(url)
	} else {
		page <- pathServer
	}


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
			header = getServerInfo(goodNodes),
			table = getServerTable(goodNodes))
		return(res)
	}

}

getServerInfo <- function(hlpage){
	nodes <- rvest::html_nodes(hlpage,
		xpath = "td[contains(@class, 'game-table-cell')]")
	text <- rvest::html_text(nodes)
	text <- gsub("\\(join\\)", "", gsub("\n", "", text))

	return(text)
}

getServerTable <-function(hlpage){
	nd <- rvest::html_node(hlpage, xpath = "td[1]/div[1]/table[1]")
	res <- rvest::html_table(nd, header = TRUE)[[1]]
	idnode <- rvest::html_nodes(hlpage, xpath = "td[1]/div[1]/table[1]/tr")

	ids <- rep(NA, nrow(res))
	ids[grepl("\\d", res[,1])] <- getPlayerIds(idnode)

	res <- cbind(ids, res)
	colnames(res)[1] <- "playerid"
	res <- getRidOfBadChar(res)
	return(res)
}

#' Grab the top player lists for the first server
#'
#' @param pathServer The url to the main hlstats page
#' @param numPlayers The number of players to download(grabs in 50 player pages)
#' @param rankingType What type of ranking should the server output
#' @param shJustIds Give only the player ids of the players
#'
#' @return A dataframe of the final table or a vector of the player Ids
#' @export
getHlTopPlayers <- function(
	pathServer,
	numPlayers = 50,
	rankingType = c("Total", "Week", "Month")[1],
	shJustIds = FALSE
	){

	rankInd <- switch(rankingType, Total = 0, Week = -1, Month = -2)

	playerList <- lapply(1:ceiling(numPlayers/50), function(pg){
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

		simTableNode <- rvest::html_nodes(tableNode,
			xpath = "tr[not(contains(@class,'data-table-head'))]")

		ids <- getPlayerIds(simTableNode)

		if(shJustIds){
			names(ids) <- tab$Player
			return(ids)

		} else {
			tab$Rank <- ids
			colnames(tab)[1] <- "playerid"
			colnames(tab) <- gsub(" |:", ".", colnames(tab))

			actxloc <- "td[4]/img"
			actNodes <- rvest::html_nodes(simTableNode, xpath = actxloc)
			act <- rvest::html_attr(actNodes, "style")
			act <- gsub("width|%|:|;", "", act)
			tab$Activity <- as.integer(act)

			tab[, "Connection.Time"] <- parseHlTime(tab[, "Connection.Time"])

			tab <- getRidOfBadChar(tab)

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

#' Parse the data on each players per login stats for usage statistics
#'
#' @param pathServer The url to the main hlstats page
#' @param playerId The player's id
#' @param verbose Which columns to return as follows:
#' \describe{
#'   \item{all}{All the columns}
#'   \item{min}{Only returns the date, time and skill change}
#'   \item{onlyDate}{Only returns the date}
#' }
#'
#' @return data frame of the last sessions for the player
#' @export
getSessionTimes <- function(
	pathServer,
	playerId,
	verbose = c("all", "min", "onlyDate")[1]
	){

	url <- paste0(
		pathServer,
		"/stats/hlstats.php?mode=playersessions&player=",
		playerId)

	hlpage <- xml2::read_html(url)

	checkxloc <- "//div[contains(@class,'content')]/div[1]/div[1]"
	checkNode <- rvest::html_node(hlpage, xpath = checkxloc)

	if(length(rvest::html_attrs(checkNode)) == 1){
		tab <- as.data.frame(matrix(0,nrow = 0, ncol = 5))

	} else {

		tableNode <- rvest::html_node(checkNode, xpath = "table")
		tab <- rawTab <- rvest::html_table(
			tableNode,
			header = TRUE)

		colnames(tab) <- gsub(" |:", ".", colnames(tab))

		tab <- getRidOfBadChar(tab)
		tab <- getRidOfBadChar(tab, addBadChar = ",", outClass = "integer")

		tab$Time <- parseHlTime(tab$Time)
		tab$Date <- strptime(tab$Date, "%Y-%m-%d")
		tab$Skill.Change <- as.integer(tab$Skill.Change)

		colnames(tab) <- gsub(" |:", ".", colnames(tab))

		tab <- switch(verbose,
			onlyDate = tab[, "Date"],
			min = tab[, c("Date", "Skill.Change", "Time")],
			all = tab
		)
	}

	return(tab)

}

getPlayerIds <- function(tabNode){

	idNodes <- rvest::html_nodes(tabNode, xpath = "td[2]/a")
	ids <- rvest::html_attr(idNodes, "href")

	ids <- regmatches(ids, regexpr("player=(\\d+)", ids))
	ids <- as.integer(gsub("player=", "", ids))

	return(ids)


}

getAliases <- function(pathServer, playerId){
	url <- paste0(
		pathServer,
		"/stats/hlstats.php?mode=playerinfo&player=",
		playerId)

	hlpage <- xml2::read_html(url)

	baseNode <- rvest::html_node(hlpage,
		xpath = "//div[contains(@class,'content')]/div[1]/div[1]")

	check <- (9 == length(rvest::html_nodes(baseNode, xpath = "div")))

	out <- ""
	if(check){
		aTable <- rvest::html_table(
			rvest::html_node(baseNode, xpath = "div[9]/table"),
			header = TRUE)

		out <- aTable$Name
	}

	return(out)
}

getRidOfBadChar <- function(
	tab,
	addBadChar = NULL,
	outClass = c("character", "numeric", "integer")[1]
	){

	badCharRegEx <- paste(c(" ", addBadChar), collapse = "|")

	badHead <- grepl(badCharRegEx, colnames(tab))
	badTab <- vapply(tab, function(col){ any(grepl(badCharRegEx, col)) }, TRUE)
	for(ind in which(badHead | badTab)){
		colnames(tab)[ind] <- gsub(badCharRegEx, "", colnames(tab)[ind])
		goodChs <- gsub(badCharRegEx, "", tab[, ind])
		tab[, ind] <- switch(outClass,
			character = goodChs,
			numeric = as.numeric(goodChs),
			integer = as.integer(goodChs)
		)
	}
	return(tab)
}
