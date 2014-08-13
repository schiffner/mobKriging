context("kmModel")

test_that("kmModel", {
	require(DiceKriging)
	d <- 2L
	x <- seq(0, 1, length = 4L)
	design <- expand.grid(x1 = x, x2 = x)
	y <- apply(design, 1, branin)
	df <- data.frame(y = y, design)

	## data pre-processing
	mf <- dpp(kmModel, y ~ ., data = df) 

	## no trend (formula = ~ 1)
	set.seed(123)
	m1 <- fit(kmModel, mf)
	set.seed(123)
	m2 <- km(design = design, response = y)
	expect_equivalent(m1$m, m2)

	## linear trend (formula = ~ x1 + x2)
	set.seed(123)
	m1 <- fit(kmModel, mf, formula = ~ .)
	set.seed(123)
	m2 <- km(formula = ~ ., design = design, response = y)
	expect_equivalent(m1$m, m2)

	## predictions
	pred1 <- Predict(m1, type = "UK")
	pred2 <- predict(m2, newdata = design, type = "UK")
	expect_equal(pred1, pred2)
})
