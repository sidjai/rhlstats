calcPlayerTendencies <- function(sess){

	model <- list()

	sess$Date <- as.POSIXct(sess$Date)

	#1 data point prediction gets a free padding row
	if(nrow(sess) == 1){
		sess <- rbind(sess, sess)
		sess$Date[2] <- sess$Date[2] - (1 * 60*60*24)
		sess$Skill.Change[2] <- sess$Skill.Change[2] + 20
		sess$Time[2] <- sess$Time[2] + 0.25
	}

	#Lifetime
	tser <- xts::xts(sess$Time, sess$Date)

	model$life <- arima(tser, order = c(1,0,0), method = "ML")

	#Tryhard
	avgDel <- mean(abs(sess$Skill.Change))
	tser <- xts::xts(sess$Skill.Change / avgDel, sess$Date)

	model$try <- arima(tser, order = c(1,0,0), method = "ML")

	return(model)
}

model2str <- function(model){
	modelStr <- gsub("[\r\n]","", capture.output(dump("model",file="")))
	modelStr <- paste0(modelStr, collapse = "")

	return(modelStr)
}
