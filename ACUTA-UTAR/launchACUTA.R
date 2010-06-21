# R --slave --vanilla --args "in" "out" < launchACUTA.R

rm(list=ls())
inputsLocation <- commandArgs()[5]
outputsLocation <- commandArgs()[6]	

# inputsLocation <- "/Users/silver/Desktop/Rstat/in2"
# outputsLocation <- "/Users/silver/Desktop/Rstat/out"
# source("/Users/silver/Desktop/Rstat/libACUTAR.R")
# source("/Users/silver/Desktop/Rstat/ACUTAR/OLD/XMCDA/libxmcda.R")

library(RXMCDA)
source("libACUTAR.R")

err <- try({
	content <- scanInputsFolder( inputsLocation )
})
if (inherits(err, 'try-error'))
{ 
	content <- list( "validation"=FALSE, "LOG"="Fatal error while scanning inputfolder. Please check initial files")
}

err <- try({
	realContent <- combineContents ( content )
})
if (inherits(err, 'try-error'))
{ 
	realContent <- list( "validation"=FALSE, "LOG"="Fatal error while combining content. Please check initial files")
}

err <- try({
	realContent <- criteriaTrendAnalysis( realContent ) 
})
if (inherits(err, 'try-error'))
{ 
	realContent <- list( "validation"=FALSE, "LOG"="Fatal error while computing trend analysis. Please check performance table and alternatives or criteria")
}

err <- try({
	realContent <- computeSegments( realContent ) 
})
if (inherits(err, 'try-error'))
{ 
	realContent <- list( "validation"=FALSE, "LOG"="Fatal error while computing initial segments. Please check segments, criteria and directions.")
}

err <- try({
	solutions <- computeAcuta ( realContent )
})
if (inherits(err, 'try-error'))
{ 
	solutions <- list( "validation"=FALSE, "LOG"="Fatal error while computing ACUTA method. Please check data and initial files.")
}



exportSolutions ( solutions , outputsLocation )
exportLOG( solutions$LOG , outputsLocation )

