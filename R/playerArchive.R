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

	tend <- t(vapply(tab[ ,"playerid"], function(pid){
		model <- calcPlayerTendencies(sessInfo[[pid]])
		return(c(model2str(model$life), model2str(model$try)))
	}, rep("wer", 2)))

	tab <- cbind(tab, model2str(tend))
	colnames(tab) <- c(gsub(c(" ", ":"), ".", colnames(tab)),
		"Model.Lifetime", "Model.Tryhard")

	if(file.exists(pathArch)){
		stop(paste(pathArch, "is already an sql database, use updatePlayerArchive"))
	}

	db <- RSQLite::dbConnect(RSQLite::SQLite(), pathArch)
	RSQLite::dbWriteTable(db, "players", tab)


	return(RSQLite::dbDisconnect(db))

}


updatePlayerArchive <- function(pathServer, pathArch){

	#eval(parse(text = tab$modelStr))
	#update(model)
	#tab$modelStr <- model2str(model)


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

	lifetime <- vapply(topArch$Model.Lifetime, function(modtxt){
		eval(parse(text = modtxt))
		predict(model, as.integer(Sys.time()))
		})

	tryhard <- vapply(topArch$Model.Tryhard, function(modtxt){
		eval(parse(text = modtxt))
		predict(model, as.integer(Sys.time()))
		})


	out <- list(tryhard = tryhard, lifetime = lifetime)
	return(out)
}
