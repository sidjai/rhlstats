#' Title Builds the usage stats database
#'
#' Driver that scrapes the session data and then builds the models for tryhard
#'
#' @param pathServer The url to the main hlstats page
#' @param pathArch The path where the output sql database should be
#'
#' @return A logical if the sql was disconnected, side effect of writing the
#'   database
#' @export
buildPlayerArchive <- function(pathServer, pathArch){

	check <- length(getHlServerPage(pathServer, shOnlyNodes = TRUE)) > 0
	if(!check) stop(paste(
		"The server: ", pathServer, "is not a valid hlstat server"
		))

	if(file.exists(pathArch)){
		stop(paste(pathArch, "is already an sql database, use updatePlayerArchive"))
	}


	tab <- getHlTopPlayers(pathServer, 200)


	#Get session info for every player

	sessInfo <- lapply(tab[ ,"playerid"], function(x){
		sess <- getSessionTimes(pathServer, x, verbose = "min")
		sess <- cbind(rep(x, nrow(sess)), sess)
		sess$Date <- as.character(sess$Date)
		return(sess)
	})

	sessTab <- do.call("rbind", sessInfo)
	names(sessInfo) <- tab[ ,"playerid"]
	colnames(sessTab)[1] <- "playerid"


	#Add models to player archive

	tend <- t(vapply(tab[ ,"playerid"], function(pid){
		model <- calcPlayerTendencies(sessInfo[[paste(pid)]])
		return(c(model2str(model$life), model2str(model$try)))
	}, rep("wer", 2)))

	tab <- cbind(tab, tend)
	colnames(tab)[ncol(tab)-1] <- "Model.Lifetime"
	colnames(tab)[ncol(tab)] <- "Model.Tryhard"


	db <- RSQLite::dbConnect(RSQLite::SQLite(), pathArch)
	RSQLite::dbWriteTable(db, "players", tab)
	RSQLite::dbWriteTable(db, "session", sessTab)


	return(RSQLite::dbDisconnect(db))

}


updatePlayerArchive <- function(urlServer, pathArch){



	if(nzchar(pathSessArch)){
		#eval(parse(text = tab$modelStr))
		#update(model)
		#tab$modelStr <- model2str(model)
	}




}

#' Title Get the usage stats for a given table from the sql database
#'
#' @param currentTab Data.frame of the server page from getServerPage
#' @param pathArch The path where the output sql database should be
#'
#' @return list with predictions for tryhard and lifetime for all the players in
#'   the server
#' @export
queryPlayerArchive <- function(currentTab, pathArch){
	if(!file.exists(pathArch)){
		stop(paste(pathArch,
			"does not exist, please build an archive using buildPlayerArchive"))
	}
	notSpecSet <- nzchar(currentTab$`#`)

	db <- RSQLite::dbConnect(RSQLite::SQLite(), pathArch)
	sqlids <- paste("playerid=", currentTab[,"playerid"][notSpecSet], collapse = " OR ")
	sqlQuery <- paste("SELECT * from players WHERE", sqlids)
	topArch <- RSQLite::dbGetQuery(db, sqlQuery)

	lifetime <- vapply(topArch$Model.Lifetime, function(modtxt){
		eval(parse(text = modtxt))
		predict(model, as.integer(Sys.time()))
		}, 1)

	tryhard <- vapply(topArch$Model.Tryhard, function(modtxt){
		eval(parse(text = modtxt))
		predict(model, as.integer(Sys.time()))
		}, 1)

	RSQLite::dbDisconnect(db)
	out <- list(tryhard = tryhard, lifetime = lifetime)
	return(out)
}
