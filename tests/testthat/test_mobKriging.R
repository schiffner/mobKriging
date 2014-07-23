context("mobKriging")

test_that("mobKriging", {
  d = 2
  x = seq(0,1,length = 10)
  design = expand.grid(x1 = x, x2 = x)
  y = apply(design, 1, branin)
  df = data.frame(y = y, design)

  ## no trend (trend.formula = ~ 1)
  capture.output({
  fit = mob(y ~ x1 + x2 | x1 + x2, data = df, model = kmModel,
    control = mob_control(objfun = deviance, minsplit = 20, verbose = FALSE))
  })
  ## linear trend (trend.formula = ~ x1 + x2)
  capture.output({
  fit = mob(y ~ x1 + x2 | x1 + x2, data = df, model = kmModel, trend.formula = ~ .,
    control = mob_control(objfun = deviance, minsplit = 20, verbose = FALSE))
  })
})
