## ---- echo = FALSE, include = FALSE--------------------------------------
library(FARS)
library(dplyr)
library(maps)

## ----Package Loading-----------------------------------------------------
library(FARS)
library(maps)

## ----eval=FALSE----------------------------------------------------------
#  list.files(system.file("extdata", package = "FARS"))

## ----fars_read function, eval=FALSE--------------------------------------
#  df<-fars_read("accident_2014.csv.bz2")

## ----make_filename function, eval=FALSE----------------------------------
#  df<-make_filename(2014)

## ----far_read_years function, eval=FALSE---------------------------------
#  df<-fars_read_years(2013:2015)

## ----fars_summarize_years function, eval=FALSE---------------------------
#  fars_summarize_years(2013:2015)

## ----fars_map_state function, eval=FALSE---------------------------------
#  fars_map_state(13,2014)

