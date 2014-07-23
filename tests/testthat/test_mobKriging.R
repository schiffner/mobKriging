context("mobKriging")

test_that("mobKriging", {
  d = 2L
  x = seq(0, 1, length = 10L)
  design = expand.grid(x1 = x, x2 = x)
  y = apply(design, 1, branin)
  df = data.frame(y = y, design)

  capture.output({
  m = mobKriging(y ~ x1 + x2 | x1 + x2, data = df)
  })

  # capture.output({
  # m = mobKriging(y ~ x1 + x2 | x1 + x2, data = df,
    # mob.control = mob_control(objfun = deviance, minsplit = 20, verbose = FALSE))
  # })

  #FIXME: redo test
  ## linear trend (trend.formula = ~ x1 + x2)
  # capture.output({
  # fit = mobKriging(y ~ x1 + x2 | x1 + x2, data = df, trend.formula = ~ .,
    # control = mob_control(objfun = deviance, minsplit = 20, verbose = FALSE))
  # })
})
