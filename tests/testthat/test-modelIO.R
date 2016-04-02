context("Using Models")

testSess <-
	structure(list(Date = c("2016-03-06", "2016-03-01", "2016-02-29"
	), Skill.Change = c(132L, 1167L, 879L), Points = c(19598L, 19466L,
		18299L), Time = c(0.8425, 2.85444444444444, 1.84972222222222),
		Kills = c(44L, 179L, 91L), Deaths = c(28L, 82L, 52L), K.D = c(1.57,
			2.18, 1.75), HS = c(1L, 0L, 0L), HS.K = c(0.02, 0, 0), Suicides = c(0L,
				0L, 0L), TKs = c(0L, 0L, 0L), Kill.Strk = c(7L, 8L, 8L)), .Names = c("Date",
					"Skill.Change", "Points", "Time", "Kills", "Deaths", "K.D", "HS",
					"HS.K", "Suicides", "TKs", "Kill.Strk"), row.names = c(NA, 3L
					), class = "data.frame")

testBadSess <-
	structure(list(`rep(x, nrow(sess))` = integer(0), V1 = numeric(0),
		V2 = numeric(0), V3 = numeric(0), V4 = numeric(0), V5 = numeric(0),
		Date = character(0)), .Names = c("rep(x, nrow(sess))", "V1",
			"V2", "V3", "V4", "V5", "Date"), row.names = integer(0), class = "data.frame")

test_that("Model can be used directly",{
	models <- calcPlayerTendencies(testSess)
	res <- predict(models$life)

	expect_equal(names(res), c("pred", "se"))

})

test_that("Model can be converted to a string",{
	models <- calcPlayerTendencies(testSess)
	resStr <- model2str(models$life)
	eval(parse(text = resStr))

	expect_equal(model, models$life)

})

test_that("Model does things with a dead player", {
	models <- calcPlayerTendencies(testBadSess)
	res <- predict(models$life)

	expect_equal(names(res), c("pred", "se"))

})


