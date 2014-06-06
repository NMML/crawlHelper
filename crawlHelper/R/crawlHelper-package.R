#' Miscellaneous functions for animal movement modeling with crawl
#' 
#' More about what it does (maybe more than one line) ~~ A concise (1-5 lines)
#' description of the package ~~
#' 
#' \tabular{ll}{ 
#' Package: \tab crawlHelper\cr 
#' Type: \tab Package\cr 
#' Version: \tab 1.2\cr 
#' Date: \tab 2014-06-06\cr 
#' License: \tab Unlimited\cr 
#' LazyLoad: \tab yes\cr 
#' }
#' 
#' @name crawlHelper-package
#' @aliases crawlHelper-package crawlHelper
#' @docType package
#' @author Devin S. Johnson
#' 
#' Maintainer: Devin S. Johnson <devin.johnson@@noaa.gov>
#' 
NULL

.onAttach <- function(library, pkgname)
{
  info <-utils::packageDescription(pkgname)
  package <- info$Package
  version <- info$Version
  date <- info$Date
  packageStartupMessage(
    paste(package, version, paste("(",date, ")", sep=""), "\n")
  )
  
}




