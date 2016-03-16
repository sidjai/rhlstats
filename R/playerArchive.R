buildPlayerArchive <- function(pathServer, pathArch, shSaveSess = FALSE){

	check <- length(getHlServerPage(pathServer, shOnlyNodes = TRUE)) > 0
	if(!check) stop(paste(
		"The server: ", pathServer, "is not a valid hlstat server"
		))




	tab <- getHlTopPlayers(pathServer, 200)

	sessInfo <- lapply(tab[ ,"playerid"], function(x){
		getSessionTimes(pathServer, x)
	})
	sessArch <- gsub("[.]sql", "sess.sql", pathArch)

	if(shSaveSess){
		sessDb <- RSQLite::dbConnect(RSQLite::SQLite(), sessArch)
		for( pind in 1:200){
			temp <- sessInfo[[pind]]
			temp$Date <- as.character(temp$Date)
			RSQLite::dbWriteTable(sessDb, paste0("p", tab[pind, "playerid"]),
				temp)
		}
		RSQLite::dbDisconnect(sessDb)
	}


	tend <- calcPlayerTendencies(sessInfo)

	if(file.exists(pathArch)){
		stop(paste(pathArch, "is already an sql database, use updatePlayerArchive"))
	}

	db <- RSQLite::dbConnect(RSQLite::SQLite(), pathArch)
	RSQLite::dbWriteTable(db, "players", tab)


	return(RSQLite::dbDisconnect(db))

}


updatePlayerArchive <- function(pathServer, pathArch){


}

queryPlayerArchive <- function(currentTab, pathArch){
	if(!file.exists(pathArch)){
		stop(paste(pathArch,
			"does not exist, please build an archive using buildPlayerArchive"))
	}

	db <- RSQLite::dbConnect(RSQLite::SQLite(), pathArch)
	sqlQuery <- ""
	topArch <- RSQLite::dbGetQuery(db, sqlQuery)

	topArch <- sqldb(pathArch)[currentTab$playerIds, ]
	tryhard <- cppCalcTryHard(
		currentTab$points,
		currentTab$`Current Skill`,
		topArch$tryhard)

	lifetime <-cppCalcLifeTime(
		currentTab$Time,
		tend$dailyTend,
		tend$timeTend)

	out <- list(tryhard = tryhard, lifetime = lifetime)
	return(out)
}

calcPlayerTendencies <- function(sessInfo){



	#colnames(tend) <- c("tryHardTend", "dailyTend", "uptimeTend")




}
