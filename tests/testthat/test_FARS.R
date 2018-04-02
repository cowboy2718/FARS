context("Test the map function of the FARS package")

library(dplyr)
library(maps)
library(FARS)


test_that("fars_map_state() works correctly", {
     expect_silent(fars_map_state(13, 2014))           
     expect_silent(fars_map_state(6, 2013))           
})