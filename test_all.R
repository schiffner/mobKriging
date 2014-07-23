library(methods)
library(devtools)
library(testthat)
library(DiceKriging)
library(party)

if (interactive()) {
  load_all(".")
} else {
  library(mobKriging)
}
test_dir("tests/testthat")
