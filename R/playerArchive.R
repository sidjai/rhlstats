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

queryPlayerArchive <- function(currentTab, pathArch){
	if(!file.exists(pathArch)){
		stop(paste(pathArch,
			"does not exist, please build an archive using buildPlayerArchive"))
	}
	notSpecSet <- nzchar(currentTab$`#`)

	db <- RSQLite::dbConnect(RSQLite::SQLite(), pathArch)
	sqlids <- paste("playerid=", currentTab$playerIds[notSpecSet], collapse = " OR ")
	sqlQuery <- paste("SELECT * from players WHERE", sqlids)
	topArch <- RSQLite::dbGetQuery(db, sqlQuery)

	lifetime <- vapply(topArch$Model.Lifetime, function(modtxt){
		eval(parse(text = modtxt))
		predict(model, as.integer(Sys.time()))
		})

	tryhard <- vapply(topArch$Model.Tryhard, function(modtxt){
		eval(parse(text = modtxt))
		predict(model, as.integer(Sys.time()))
		})

	RSQLite::dbDisconnect(db)
	out <- list(tryhard = tryhard, lifetime = lifetime)
	return(out)
}
