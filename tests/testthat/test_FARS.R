context("Test the map function of the FARS package")

library(dplyr)
library(maps)
library(FARS)


test_that("fars_map_state() works", {
     expect_silent(fars_map_state(13, 2014))           
     expect_silent(fars_map_state(6, 2013))           
})

test_that("fars_read() works", {
  expect_is(fars_read("accident_2013.csv.bz2"), "tbl_df")
  expect_error(fars_read("accident_2012.csv.bz2"))
})