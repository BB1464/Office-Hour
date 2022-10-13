
# R Package Development Notes ---------------------------------------------

# create_package("~/path/to/regexcite")

# use_git()


# Write the first function ------------------------------------------------

(x <- "alfa,bravo,charlie,delta")

strsplit(x, split = ",")


## Function

## use_r("strsplit1")

strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}

## Load the function

load_all()

## Check if the function exist in the globalenv

exists("strsplit1", where = globalenv(), inherits = FALSE)


## Check for possible error

check()

## Edit Description

# Package: regexcite
# Title: Make Regular Expressions More Exciting
# Version: 0.0.0.9000
# Authors@R:
#   person("Jane", "Doe", , "jane@example.com", role = c("aut", "cre"))
# Description: Convenience functions to make some common tasks with string
# manipulation and regular expressions a bit easier.
# License: `use_mit_license()`, `use_gpl3_license()` or friends to pick a
# license
# Encoding: UTF-8
# Roxygen: list(markdown = TRUE)
# RoxygenNote: 7.1.2


## use_mit_license()

writeLines(readLines("LICENSE"))



## Document the function


#' Split a string
#'
#' @param x A character vector with one element.
#' @param split What to split on.
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' strsplit1(x, split = ",")
strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}



document()


?strsplit1

## NAMESPACE changes
export(strsplit1)


# check() again -----------------------------------------------

check()


install()

library(regexcite)


# use_testthat() -------------------------------------------------

use_testthat()

use_test("strsplit1")

## Add this to the test

test_that("strsplit1() splits a string", {
  expect_equal(strsplit1("a,b,c", split = ","), c("a", "b", "c"))
})


test()



# use_package() -------------------------------------------------

use_package("stringr")

### Modify the function

str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}


## Rename the file

rename_files("strsplit1", "str_split_one")

### Add the R oxygen skeleton for the documentation

#' Split a string
#'
#' @param string A character vector with, at most, one element.
#' @inheritParams stringr::str_split
#'
#' @return A character vector.
#' @export
#'
#' @examples
#' x <- "alfa,bravo,charlie,delta"
#' str_split_one(x, pattern = ",")
#' str_split_one(x, pattern = ",", n = 2)
#'
#' y <- "192.168.0.1"
#' str_split_one(y, pattern = stringr::fixed("."))
str_split_one <- function(string, pattern, n = Inf) {
  stopifnot(is.character(string), length(string) <= 1)
  if (length(string) == 1) {
    stringr::str_split(string = string, pattern = pattern, n = n)[[1]]
  } else {
    character()
  }
}


### Update the test
###

test_that("str_split_one() splits a string", {
  expect_equal(str_split_one("a,b,c", ","), c("a", "b", "c"))
})

test_that("str_split_one() errors if input length > 1", {
  expect_error(str_split_one(c("a,b","c,d"), ","))
})

test_that("str_split_one() exposes features of stringr::str_split()", {
  expect_equal(str_split_one("a,b,c", ",", n = 2), c("a", "b,c"))
  expect_equal(str_split_one("a.b", stringr::fixed(".")), c("a", "b"))
})



# Document ------------------------------------------------------

document()


### Load_all

load_all()

## Push it to github

use_github()

## use_readme_rmd()

use_readme_rmd()


### Build the readme file

build_readme()


### The end: check() and install()
###

check()

install()

