# R --slave --vanilla --args "in" "out" < UTASTAR-UTAR.R

rm(list=ls())
inputsLocation <- commandArgs()[5]
outputsLocation <- commandArgs()[6]	

library(RXMCDA)
library(UTAR)
#source("/Users/silver/RMCDA/Github/UTAR-lib.R")

err <- try({
			content <- scanInputsFolder( inputsLocation )
		})
if (inherits(err, 'try-error'))
{ 
	content <- list( "validation"=FALSE, "LOG"="Fatal error while scanning inputfolder. Please check initial files")
}
#print(content)

print("==================================")
print(" REAL CONTENT")
print("==================================")

err <- try({
			realContent <- combineAndRestrictContent ( content )
		})
if (inherits(err, 'try-error'))
{ 
	realContent <- list( "validation"=FALSE, "LOG"="Fatal error while combining content. Please check initial files")
}
#print(realContent)

print("==================================")
print(" UTASTAR SOLUTION")
print("==================================")

err <- try({
			UTASTARsolution <- computeUTASTAR ( realContent )
		})
if (inherits(err, 'try-error'))
{ 
	solutions <- list( "validation"=FALSE, "LOG"="Fatal error while computing ACUTA method. Please check data and initial files.")
}
#print(UTASTARsolution)

print("==================================")
print(" POST-OP SOLUTION ")
print("==================================")

err <- try({
			solution <- computePostOptimalityAnalysis ( UTASTARsolution )
		})
if (inherits(err, 'try-error'))
{ 
	solutions <- list( "validation"=FALSE, "LOG"="Fatal error while computing ACUTA method. Please check data and initial files.")
}

print(solution)

exportSituation ( solution , outputsLocation )

print("==================================")
print(" LOG : ")
print( solution$LOG )
print("==================================")

exportLog( solution$LOG , outputsLocation , error = !solution$validation )