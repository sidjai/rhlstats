calcPlayerTendencies <- function(sess){

	model <- list()
	#Lifetime
	tser <- xts::xts(sess$Time, as.POSIXct(sess$Date))

	model$life <- arima(tser, order = c(1,0,0))

	#Tryhard
	avgDel <- mean(abs(sess$Skill.Change))
	tser <- xts::xts(sess$Skill.Change / avgDel, as.POSIXct(sess$Date))

	model$try <- arima(tser, order = c(1,0,0))

	return(model)
}

model2str <- function(model){
	modelStr <- gsub("[\r\n]","", capture.output(dump("model",file="")))
	modelStr <- paste0(modelStr, collapse = "")

	return(modelStr)
}
