#' Run the Cardiovascular MFR shiny application
#' @return no return value
#' @importFrom shiny runApp
#' @export
runPoissontris <- function(){
	
	# (1) Copy the UI files & folders from "inst/ui" for local use
#	tmpDir <- tempdir()
#	setwd(tmpDir)
	library(shiny)
	uiDir <- system.file("ui", package = "poissontris")
  setwd(uiDir)
#	uiFiles <- list.files(path = uiDir, full.names = FALSE, recursive = TRUE)
#	
#	sapply(uiFiles, function(from) {
#				
#				to <- file.path(tmpDir, from)
#				toDir <- dirname(to)
#				
#				if (!dir.exists(toDir)) {
#					
#					dir.create(path = toDir, recursive = TRUE)
#					
#				}
#				
#				file.copy(from = file.path(uiDir, from), to = to, overwrite = TRUE)
#				
#			})  
	
	
	# (2) Run the application
	runApp()
	
}




