inputsLocation <- commandArgs()[5]
outputsLocation <- commandArgs()[6]

source("/Users/silver/RMCDA/Github/UTAR-lib.R")
library(RXMCDA)

errTag = FALSE
if( errTag==FALSE )
{
	tmpErr <- try({
				listOfFiles <- list.files( inputsLocation )
				location <- listOfFiles[ listOfFiles == "alternatives.xml" ]
				if ( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "Failed to find alternatives.xml" , outputsLocation ) 
				}
				location <- paste( inputsLocation , location , sep="/" )
				xml <- xmlInternalTreeParse( location )
				if( checkXSD( xml ) != 1 )
				{
					errTag = TRUE
					exportLog( "alternatives.xml is not a valid XMCDA file" , outputsLocation ) 
				}
				alternativesIDs <- getAlternativesIDs ( xml )
			})
	if ( inherits(tmpErr, 'try-error') || alternativesIDs$status != "OK" )
	{
		if(errTag==FALSE){exportLog( "Failed to get data from alternatives.xml" , outputsLocation )}
		errTag = TRUE
	}
	alternativesIDs <- alternativesIDs[[1]]
}

if( errTag==FALSE )
{
	tmpErr <- try({
				location <- listOfFiles[ listOfFiles == "criteria.xml" ]
				if( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "failed to find criteria.xml" , outputsLocation ) 
				}
				location <- paste( inputsLocation , location , sep="/" )
				xml <- xmlInternalTreeParse( location )
				if( checkXSD( xml ) != 1 )
				{
					errTag = TRUE
					exportLog( "criteria.xml is not a valid XMCDA file" , outputsLocation )  
				}
				criteriaIDs <- getCriteriaIDs ( xml )
			})
	if ( inherits(tmpErr, 'try-error') || is.null( criteriaIDs$status != "OK" ) )
	{
		if(errTag == FALSE){exportLog( "Failed to get data from criteria.xml" , outputsLocation )}
		errTag = TRUE
	}
	criteriaIDs <- criteriaIDs[[1]]
}

if( errTag==FALSE )
{
	tmpErr <- try({
				listOfFiles <- list.files( inputsLocation )
				location <- listOfFiles[ listOfFiles == "performanceTable.xml" ]
				if( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "failed to find performanceTable.xml" , outputsLocation )  
				}
				location <- paste( inputsLocation , location , sep="/" )
				xml <- xmlInternalTreeParse( location )
				if( checkXSD( xml ) != 1 )
				{
					errTag = TRUE
					exportLog( "performanceTable.xml is not a valid XMCDA file" , outputsLocation ) 
				}
				PT <- getPerformanceTables( xml )[[1]]
			})
	if( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "Failed to get data from performanceTable.xml" , outputsLocation )}
		errTag = TRUE
	}
}

if( errTag==FALSE )
{
	tmpErr <- try({
				location <- listOfFiles[ listOfFiles == "valueFunctions.xml" ]
				if ( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "Failed to find valueFunctions.xml" , outputsLocation )  
				}
				location <- paste( inputsLocation , location , sep="/" )
				if( checkXSD( xmlInternalTreeParse( location ) ) != 1 )
				{
					errTag = TRUE
					exportLog( "valueFunctions.xml is not a valid XMCDA file" , outputsLocation )  
				}
				
				data_xml <- xmlInternalTreeParse( location )
				crit <- getCriteriaIDs(data_xml)[[1]]
				matrices <- getValueFunctions(data_xml)
				gmatrix <- matrices$gmatrix
				umatrix <- matrices$umatrix
				rownames(gmatrix) <- crit 
				rownames(umatrix) <- crit 
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "Failed to get data from valueFunctions.xml" , outputsLocation )}
		errTag = TRUE
	}
}

if( errTag==FALSE )
{
	tmpErr <- try({
				performanceTable <- PT
				co <- colnames(performanceTable)
				ro <- rownames(performanceTable)
				temporaryAlternatives <- c()
				for (l in 1:length(alternativesIDs) )
				{
					temp <- which( ro == alternativesIDs[l] )
					temporaryAlternatives <- c( temporaryAlternatives , temp )
				}
				temporaryCriteria <- c()
				for (l in 1:length(criteriaIDs) )
				{
					temp <- which( co == criteriaIDs[l] ) 
					temporaryCriteria <- c( temporaryCriteria , temp )
				}
				print(temporaryAlternatives)
				print(temporaryCriteria)
				print(matrices)
				PT <- performanceTable[ temporaryAlternatives , temporaryCriteria ]
				NPT <- computeNormalisedPerformanceTable( PT , gmatrix , umatrix )
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "Execution failure while manipulating performance tables and criteria/alternatives" , outputsLocation ) } 
		errTag = TRUE
	}
}

if( errTag==FALSE ){
	tmpErr<-try({
				z <- performanceTableToXML( NPT )
				saveXML( z, file = paste(outputsLocation,"normalizedPerformanceTable.xml",sep="/") )
				
			})	
	if (inherits(tmpErr, 'try-error')){
		message <- "Fatal error while exporting data"
		exportLog( message , outputsLocation , error=TRUE )
	} else {
		message <- "Execution successful"
		exportLog( message , outputsLocation , error=FALSE )
	}
}