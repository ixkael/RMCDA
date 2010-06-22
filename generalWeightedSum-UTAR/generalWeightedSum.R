#R --slave --vanilla --args "in" "out" < weightedSum.R

inputsLocation <- commandArgs()[5]
outputsLocation <- commandArgs()[6]


library(RXMCDA)
library(UTAR)

errTag = FALSE
if(errTag == FALSE){
	tmpErr <- try({
				listOfFiles <- list.files( inputsLocation )
				location <- listOfFiles[ listOfFiles == "alternatives.xml" ]
				if ( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "failed to find alternatives.xml" , outputsLocation )  
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
		if(errTag == FALSE){exportLog( "failed to get data from alternatives.xml" , outputsLocation ) }
		errTag = TRUE
	}
	alternativesIDs <- alternativesIDs[[1]]
}

if(errTag == FALSE){
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
	if ( inherits(tmpErr, 'try-error') ||  is.null( criteriaIDs$status != "OK" ) )
	{
		if(errTag == FALSE){exportLog( "failed to get data from criteria.xml" , outputsLocation )}
		errTag = TRUE
	}
	criteriaIDs <- criteriaIDs[[1]]
}

if(errTag == FALSE){
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
				performanceTable <- PT
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "failed to get data from performanceTable.xml" , outputsLocation )} 
		errTag = TRUE
	}
}

if(errTag == FALSE){
	tmpErr <- try({
				co <- colnames(performanceTable)
				ro <- rownames(performanceTable)
				temporaryAlternatives <- c()
				temporaryCriteria <- c()
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
				PT <- performanceTable[ temporaryAlternatives , temporaryCriteria ]
				critIDs <- colnames(PT)
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "error while manipulating criteria and alternatives" , outputsLocation)}
		errTag = TRUE
	}
}

if(errTag == FALSE){
	tmpErr <- try({
				location <- listOfFiles[ listOfFiles == "avg.xml" ]
				if( length( location ) == 0 ) 
				{
					avg <- FALSE
				}
				if(  length( location ) > 0 )
				{
					location <- paste( inputsLocation , location , sep="/" )
					xml <- xmlInternalTreeParse( location )
					if( checkXSD( xml ) != 1 )
					{
						errTag = TRUE
						exportLog( "avg.xml is not a valid XMCDA file" , outputsLocation )  
					}
					result <- getParameters( xml )	
					if( result$status != "OK")
					{
						avg <- FALSE
					}else{	
						avg <- as.logical(result[[1]])
						if( is.na(avg) )
						{
							avg <- FALSE
						}
					}
				}
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "error during average selection " , outputsLocation)}
		errTag = TRUE
	}
}

if(errTag == FALSE){
	tmpErr <- try({
				location <- listOfFiles[ listOfFiles == "norm.xml" ]
				if( length( location ) == 0 ) 
				{
					norm <- FALSE
				}
				if(  length( location ) > 0 )
				{
					location <- paste( inputsLocation , location , sep="/" )
					xml <- xmlInternalTreeParse( location )
					if( checkXSD( xml ) != 1 )
					{
						errTag = TRUE
						exportLog( "norm.xml is not a valid XMCDA file" , outputsLocation )  
					}
					result <- getParameters( xml )
					if( result$status != "OK")
					{
						norm <- FALSE
					}else{	
						norm <- as.logical(result[[1]])
						if( is.na(norm) )
							norm <- FALSE
					}
				}
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "error during normalisation process" , outputsLocation)}
		errTag = TRUE
	}
}

if(errTag == FALSE){
	tmpErr <- try({
				if( avg == TRUE )
				{
					weights <- rep( 1/ncol(PT) , ncol(PT) )
				} 
				if( avg == FALSE)
				{
					location <- listOfFiles[ listOfFiles == "weights.xml" ]
					if( length( location ) == 0 ) 
					{
						weights <- rep( 1 , ncol(PT) )
					}
					else 
					{
						location <- paste( inputsLocation , location , sep="/" )
						xml <- xmlInternalTreeParse( location )
						if( checkXSD( xml ) != 1 )
						{
							errTag = TRUE
							exportLog( "weights.xml is not a valid XMCDA file" , outputsLocation ) 
						}
						criteriaValues <- getCriteriaValues ( xml , critIDs )
						if ( is.null(criteriaValues[[1]]) || criteriaValues$status != "OK" )
						{
							errTag = TRUE
							exportLog( "weights file problem" , outputsLocation ) 
						}
						weights <- criteriaValues[[1]]
						if( nrow(weights) != length(critIDs) )
						{
							errTag = TRUE
							exportLog( "weights error - please provide weights of all previously specified criteria or restrict criteria" , outputsLocation ) 
						}
						rownames(weights) <- critIDs
						weights <- as.vector( weights[,2] )
						if( norm == TRUE )
						{
							weights <- weights / sum(weights)
						}
					}
				}
				
				
				sol <- rep(0,nrow(PT))
				for( i in 1:nrow(PT) )
				{
					sum <- 0
					for( j in 1:ncol(PT) )
					{
						sum <- sum + PT[i,j] * weights[j]
					}
					sol[i] <- sum
				}
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag == FALSE){exportLog( "error during weights manipulation" , outputsLocation)}
		errTag = TRUE
	}
}


if(errTag == FALSE){
	sol <- matrix(c(seq(from=1,to=nrow(PT),by=1),sol), nrow = nrow(PT) )
	
	tmpErr<-try({
				z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
				z <- xmlInternalTreeParse( saveXML(z) )
				putAlternativesValues( z , sol  , rownames(PT) )
				saveXML( z, file = paste(outputsLocation,"alternativesValues.xml",sep="/") )
			})	
	if (inherits(tmpErr, 'try-error')){
		message <- "Fatal error while exporting data"
		exportLog( message , outputsLocation , error=TRUE )
	} else {
		message <- "Execution successful"
		exportLog( message , outputsLocation  , error=FALSE , specialName = "messages.xml" )
	}
}