#' Title Calculate the models for the usage statistics
#'
#' @param sess data.frame of the events for a single player on the server gotten
#'   by using getSessionTimes
#'
#' @return a list with two models for tryhard and lifetime
#' @export
calcPlayerTendencies <- function(sess){

	model <- list()

	sess$Date <- as.POSIXct(sess[, "Date"])


	if(nrow(sess) == 1){
		#1 data point prediction gets a free padding row
		sess <- rbind(sess, ranWalkSessRow(sess))
	} else if(nrow(sess) == 0){
		#Dead players
		sess <- makeDefaultSess()

	}

	#Lifetime
	tser <- xts::xts(sess[, "Time"], sess[, "Date"])

	model$life <- tryCatch(arima(tser, order = c(1,0,0), method = "ML"),
		error = function(cond){
			if(grepl("essian", cond)){
				if(nrow(sess) == 2){
					sess <- rbind(sess, ranWalkSessRow(sess[2,]))

					tser <- xts::xts(sess$Time, sess$Date)
					arima(tser, order = c(1,0,0), method = "ML")
				} else if(grepl("non-finite finite", cond)){
					arima(tser, order = c(1,0,0), method = "CSS")

				}
			} else {

				message(paste(cond, "/n", sess))
			}
		}
	)

	#Tryhard
	avgDel <- mean(abs(sess[, "Skill.Change"]))
	tser <- xts::xts(sess[, "Skill.Change"] / avgDel, sess[, "Date"])

	model$try <- tryCatch(arima(tser, order = c(1,0,0), method = "ML"),
		error = function(cond){
			if(grepl("non-finite finite", cond)){
				arima(tser, order = c(1,0,0), method = "CSS")
			}
		}
	)

	return(model)
}

ranWalkSessRow <- function(sRow){

	sRow$Date <- sRow$Date - (1 * 60*60*24)
	sRow$Skill.Change <- sRow$Skill.Change + (30 * rnorm(1, 0))
	sRow$Time <- sRow$Time + (0.25 * rnorm(1, 0))

	return(sRow)
}

makeDefaultSess <- function(){

	#make fake Session data for last 3 days
	sess <- as.data.frame(matrix(c(100, 1), nrow = 1))
	sess <- cbind(as.POSIXct(Sys.Date()), sess)
	colnames(sess) <- c("Date", "Skill.Change", "Time")

	for(rind in 1:3){
		sess <- rbind(sess, ranWalkSessRow(sess[rind,]))
	}

	return(sess)


}

model2str <- function(model){
	modelStr <- gsub("[\r\n]","", capture.output(dump("model",file="")))
	modelStr <- paste0(modelStr, collapse = "")

	return(modelStr)
}
