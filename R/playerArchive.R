buildPlayerArchive <- function(pathServer, pathArch){

	check <- is.character(getHlServerInfo(pathServer))
	if(!check) stop(paste(
		"The server: ", pathServer, "is not a valid hlstat server"
		))




	tab <- getHlTopPlayers(pathServer, 200)

	sessInfo <- lapply(tab[ ,"playerid"], function(x){
		getSessionTimes(pathServer, x)
	})

	#newTabs <- cppCalcPlayerTendencies(tab, sessInfo)
	#sqltab <- as.sql(tab)
	#finCheck <- sqldbsave(sqltab, path = pathArch)

}


updatePlayerArchive <- function(pathServer, pathArch){


}

queryPlayerArchive <- function(currentTab, pathArch){
	tend <- sqldb(pathArch)[currentTab$playerIds, ]
	tryhard <- tend$tryhard[1] * currentTab$Skill +
		tendtryHard[2] * currentTab$`+/-`

	lifetime <- tend$life[1] * parseHlTime(currentTab$Time)

	out <- list(tryhard = tryhard, lifetime = lifetime)
	return(out)
}
