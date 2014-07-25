context("mobKriging")

test_that("mobKriging", {
	require(DiceKriging)
	d = 2L
	x = seq(0, 1, length = 10L)
	design = expand.grid(x1 = x, x2 = x)
	y = apply(design, 1, branin)
	df = data.frame(y = y, design)
  
	capture.output({
		set.seed(123)
		m1 = mob(y ~ x1 + x2 | x1 + x2, data = df, model = kmModel)
		set.seed(123)
		m2 = mobKriging(y ~ x1 + x2 | x1 + x2, data = df)
	})
	expect_equivalent(m1, m2)
	# expect_equal(m1, m2)	## call, addargs

# n <- nodes(m1, unique(where(m1)))
# n[[1]]$model$addargs

# n <- nodes(m2, unique(where(m2)))
# n[[1]]$model$addargs

	capture.output({
		set.seed(123)
		m1 = mob(y ~ x1 + x2 | x1 + x2, data = df, model = kmModel, control = mob_control(objfun = deviance, minsplit = 30L, verbose = FALSE))
		set.seed(123)
		m2 = mobKriging(y ~ x1 + x2 | x1 + x2, data = df, mob.control = mob_control(objfun = deviance, minsplit = 30L, verbose = FALSE))
	})
	expect_equivalent(m1, m2)
	# expect_equal(m1, m2)	## call, addargs

	## linear trend (formula = ~ x1 + x2)
	capture.output({
		set.seed(123)
		m1 = mob(y ~ x1 + x2 | x1 + x2, data = df, model = kmModel, km.args = list(formula = ~ .), control = mob_control(objfun = deviance, minsplit = 30L, verbose = FALSE))
		set.seed(123)
		m2 = mobKriging(y ~ x1 + x2 | x1 + x2, data = df, km.args = list(formula = ~ .), mob.control = mob_control(objfun = deviance, minsplit = 30L, verbose = FALSE))
	})
	expect_equivalent(m1, m2)
	# expect_equal(m1, m2)	## call, addargs
	n <- nodes(m2, unique(where(m2)))
	expect_equal(colnames(n[[1]]$model$m@F), c("(Intercept)", "x1", "x2"))
})
