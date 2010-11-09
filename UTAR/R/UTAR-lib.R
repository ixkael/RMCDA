#############################################################################
#
# Copyright Boris Leistedt, 2010  
# Email : boris.leistedt@gmail.com
#		
# UTAR is a library for the R statistical software and specifically designed 
# to provide functions for MCDA UTA models, working with related webservices.
#
# This software is governed by the CeCILL license (v2) under French law
# and abiding by the rules of distribution of free software. You can
# use, modify and/ or redistribute the software under the terms of the
# CeCILL license as circulated by CEA, CNRS and INRIA at the following
# URL "http://www.cecill.info".
# 
# As a counterpart to the access to the source code and rights to copy,
# modify and redistribute granted by the license, users are provided only
# with a limited warranty and the software's author, the holder of the
# economic rights, and the successive licensors have only limited
# liability.
#		
# In this respect, the user's attention is drawn to the risks associated
# with loading, using, modifying and/or developing or reproducing the
# software by the user in light of its specific status of free software,
# that may mean that it is complicated to manipulate, and that also
# therefore means that it is reserved for developers and experienced
# professionals having in-depth computer knowledge. Users are therefore
# encouraged to load and test the software's suitability as regards their
# requirements in conditions enabling the security of their systems and/or
# data to be ensured and, more generally, to use and operate it in the
# same conditions as regards security.
#		
# The fact that you are presently reading this means that you have had
# knowledge of the CeCILL license and that you accept its terms.
#
##############################################################################  TO DO s
# - if(errTag == FALSE) {} everywhere
# - LOG = paste("new Log" , "oldLog" )
#
#

# getValueFunctions return matrices that contains the value functions read in an XML tree. McdaConcept is not yet used
getValueFunctions <- function ( tree,  mcdaConcept = NULL ) 
{	
	tmpErr <- try({
				if( length(tree)>0 ){
					xmcda <- getNodeSet(tree,"//criteria")
				}else{
					return(NULL)
				}
				if( length(xmcda)>0 ){
					criteria <- getNodeSet(xmcda[[1]],"criterion")
				}else{
					return(list("status"="error"))
				}
			})
	if (inherits(tmpErr, "try-error")) {
		return(list("status"="error"))
	}
	err1 <- NULL
	gmatrix <- NULL
	umatrix <- NULL
	if(length(criteria)>0){
		for( p in 1:length(criteria)){
			tmpErr <- try({
						criterion <- getNodeSet(criteria[[p]],"criterionFunction")
					})
			if (inherits(tmpErr, "try-error")) {
				
				return(list("status"="error"))
			}
			
			if(length(criterion)>0){
				for(i in 1:length(criterion)){
					gvec <- c()
					uvec <- c()
					points <- getNodeSet(criterion[[i]],"points")
					point <- getNodeSet(points[[1]],"point")
					if(length(point)>0){
						for (k in 1:length(point)){
							tmpErr <- try({
										g <- getNumericValue(getNodeSet(point[[k]],"abscissa"))[[1]]
										u <- getNumericValue(getNodeSet(point[[k]],"ordinate"))[[1]]
									})
							if (inherits(tmpErr, "try-error")) {
								err1 <- "At least one <point> contains no data."
							}
							gvec <- c(gvec,g)
							uvec <- c(uvec,u)
						}
						tmpErr <- try({
									uvec <- uvec[order(gvec)]
									gvec <- gvec[order(gvec)]
									if (is.null(gmatrix)){
										gmatrix <- matrix(gvec,ncol=length(gvec))
									}else{
										if(length(gvec) > ncol(gmatrix)){
											G <- matrix(NA,nrow=nrow(gmatrix),ncol=length(gvec))
											G[which(!is.nan(gmatrix))] <- gmatrix
											gmatrix <- rbind(G,gvec)
										}else{
											gmatrix <- rbind(gmatrix,c(gvec,rep(NA,ncol(gmatrix)-length(gvec))))
										}
									}
									if (is.null(umatrix)){
										umatrix <- matrix(uvec,ncol=length(uvec))
									}else{
										if(length(uvec) > ncol(umatrix)){
											U <- matrix(NA,nrow=nrow(umatrix),ncol=length(uvec))
											U[which(!is.nan(umatrix))] <- umatrix
											umatrix <- rbind(U,uvec)
										}else{
											umatrix <- rbind(umatrix,c(uvec,rep(NA,ncol(umatrix)-length(uvec))))
										}
									}
								})
						if (inherits(tmpErr, "try-error")) {
							err1 <- "Problem during construction of value functions while extracting"
						}
					}else{
						return(list("status"="error"))
					}
				}
			}else{
				return(list("status"="error"))
			}
		}
	}
	else{
		return(list("status"="error"))
	}
	
	names(gmatrix) <- NULL	
	names(umatrix) <- NULL	
	return(list("gmatrix"=gmatrix,"umatrix"=umatrix,"status"="OK"))
}

# exportLog creates an output file containing a message concerning the execution
exportLog <- function( message , location , error=TRUE , specialName = NULL) 
{
	if(is.null(message)){
		message = "no error"
	}
	
	z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
	z$addNode("methodMessages",close=FALSE)
		if( error == FALSE ){
			z$addNode("logMessage",close=FALSE)
			z$addNode("text",message)
			z$closeTag()
		}else{
			z$addNode("errorMessage",close=FALSE)
			z$addNode("text",message)
			z$closeTag()
		}
	z$closeTag()
		
	#z = newXMLDoc()
	#newXMLNode("xmcda:XMCDA", 
	#		attrs=c("xsi:schemaLocation" = "http://www.decision-deck.org/2009/XMCDA-2.0.0 http://www.decision-deck.org/xmcda/_downloads/XMCDA-2.0.0.xsd"),
	#		suppressNamespaceWarning=TRUE, 
	#		namespace = c("xsi" = "http://www.w3.org/2001/XMLSchema-instance", "xmcda" = "http://www.decision-deck.org/2009/XMCDA-2.0.0"), 
	#		parent=z)
	#if( error == FALSE ){
	#	status <- putLogMessage(z, message , name = "executionStatus")
	#}else{
	#	status <- putErrorMessage(z, message , name = "Error")
	#}
	if(is.null(specialName)){
		finalname = "message.xml"
	}else{
		finalname = specialName
	}
	if( is.null(location) ){
		outputLoc = finalname
	}else{
		outputLoc = paste(location,finalname,sep="/")
	}
	
	status <- saveXML(z, file= outputLoc  )
}

# exportSituation creates an output file contoining the solution

exportSituation  <- function( solution , outputsLocation ) 
{
	if( is.null(solution) || solution$validation != TRUE )
	{
		z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
		filename <- "valueFunctions.xml"
		whereF =  paste(outputsLocation,filename,sep="/")
		saveXML( z , file=whereF )
		return(NULL)
	}
	listOfOthersACUTA <- c()
	z <- saveUtilityFunctionUnderXML(solution)
	filename <- "valueFunctions.xml"
	whereF =  paste(outputsLocation,filename,sep="/")
	f <- tempfile()
	#saveXML(z, f, encoding = "UTF-8",indent=TRUE, file=whereF,prefix = '<?xml version="1.0"?>\n')
	saveXML( z , file=whereF )
	z <- NULL
	
}


# performanceTableToXML convert a performanceTable (matrix) to an appropriate XML-XMCDA
performanceTableToXML <- function( data )
{
	z <- xmlTree("xmcda:XMCDA",	
			namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",
					xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0") ,
			doc = newXMLDoc())
	altNames <- rownames(data)
	critNames <- colnames(data)
	z$addNode("performanceTable",close=FALSE )
	#,attrs=c("mcdaConcept"="performanceTable"))
	for( i in 1:length(altNames))
	{
		z$addNode( "alternativePerformances" , close=FALSE )
		z$addNode( "alternativeID" , altNames[i] )
		for( j in 1:length(critNames))
		{
			
			z$addNode( "performance" , close=FALSE )
			z$addNode( "criterionID" , critNames[j] )
			z$addNode( "value" , close=FALSE )
			z$addNode( "real" , data[i,j] )
			z$closeTag()
			z$closeTag()
		}
		z$closeTag()
	}
	z$closeTag()
	return( z )
}

# computeNormalisedPerformanceTable apply value functions (aggregation procedure) on performanceTable 
computeNormalisedPerformanceTable <- function(data,gmatrix,umatrix)
{
	A <- nrow(data)
	N <- ncol(data)
	datautility = NULL
	for (i in 1:A)
	{
		b <- rep( FALSE, length=N )
		vec <- rep(0, length=N)
		
		for (j in 1:N)
		{
			g <- data[i,j]
			if( g >= min(gmatrix[j,!is.na(gmatrix[j,])]) && g <= max(gmatrix[j,!is.na(gmatrix[j,])]) )
			{
				b[j] <- TRUE
				ginfind <- max(which(gmatrix[j,!is.na(gmatrix[j,])]<=g))
				gsupind <- min(which(gmatrix[j,!is.na(gmatrix[j,])]>=g))
				if (gsupind == ginfind){
					vec[j] <- umatrix[j,ginfind]
				}else{
					x <- c(gmatrix[j,ginfind],gmatrix[j,gsupind])
					y <- c(umatrix[j,ginfind],umatrix[j,gsupind])
					vec[j] <- approx(x,y,xout=g)$y
				}
			}
		}
		if( all(b) == TRUE )
		{
			if( is.null(datautility) )
			{
				datautility <- t(as.matrix(vec))
			}
			else
			{
				datautility <- rbind( datautility , vec )
			}
			rownames(datautility)[nrow(datautility)] <- rownames(data)[i]
		}
	}
	colnames(datautility) <- colnames(data)
	
	return(datautility)
}

# scanInputsFolder automaticly scans a folder and returns a list of
scanInputsFolder <- function( inputsLocation  )
{
	validation <- TRUE
	SOLUTION <- list( "validation" = validation )
	listOfFiles <- list.files( inputsLocation )
	
	listToLookAfter = c( "alternatives.xml" , "alternativesRanking.xml" ,
			"alternativesPreferences.xml" , "alternativesIndifferences.xml" ,
			"criteria.xml" , "delta.xml" , "segments.xml" ,
			"preferencesDirections.xml" , "performanceTable.xml" , 
			"method.xml" )
	for( i in 1:length(listToLookAfter) ){
		name = listToLookAfter[i]
		tmpErr <- try({
					aux = NULL
					if( name == "performanceTable" ){
						aux <- list( "criteriaIDs" = criteriaIDs , "alternativesIDs" = alternativesIDs )
					}
					if( name == "alternativesRanking.xml"
							||
							name == "alternativesPreferences.xml"
							||
							name == "alternativesIndifferences.xml"
							){
						aux <- alternativesIDs
					}
					if( name == "segments.xml" 
							||
							name == "preferencesDirections.xml" 
							){
						aux <- criteriaIDs
					}
					result <- findAndCheck( name , inputsLocation , listOfFiles , aux )
					if( is.null(result$content) && result$validation == TRUE ){
						if( name == "method.xml" ){
							result$content = FALSE
						}
					}					
				})
		if ( inherits(tmpErr, 'try-error') || result$validation == FALSE ) { 
			return( list( "validation" = FALSE , "LOG" = result$LOG ) )
		}else{
			if( !is.null( result$content ) ){
				newname = substr(name,1,nchar(name)-4)
				if(newname == "alternatives"){
					newname <- "alternativesIDs"
				}
				if(newname == "criteria"){
					newname <- "criteriaIDs"
				}
				SOLUTION[[newname]] = result$content[[1]]
				if( name == "alternatives.xml"){
					alternativesIDs <- result$content
				}
				if( name == "criteria.xml"){
					criteriaIDs <- result$content
				}
			}
		}	
	}
	
	return( c( SOLUTION , list("validation"=TRUE) ) )
}

# findAndCheck is analysing each file and creating LOG and content
findAndCheck <- function( filename , inputsLocation , listOfFiles , aux = NULL )
{
	print( paste( "Loading" , filename , sep=" " ) )
	location <- listOfFiles[ listOfFiles == filename ]
	validation <- FALSE
	
	if ( length( location ) == 0 ) 
	{
		print( paste( "Note:" , filename , "is missing" , sep=" " ) )
		LOG <- paste( filename , "is missing" , sep=" " )
		print( paste( "-->" , filename , "checked" , sep=" " ) )
		print( "                            " )
		if( filename == "alternatives.xml" || filename == "criteria.xml" || filename == "performanceTable.xml" || filename == "preferencesDirections.xml"){
			return( list( "validation" = FALSE , "content" = NULL , "LOG" = LOG ) )
		}else{
			return( list( "validation" = TRUE , "content" = NULL , "LOG" = LOG ) )
		}
	}
	else
	{
		location <- paste( inputsLocation , location , sep="/" )
		tmpErr <- try(
				{	xml <- xmlInternalTreeParse( location ) } )
		if (inherits(tmpErr, 'try-error'))
		{ return( list( "validation" = FALSE , "LOG" = "xml parsing error" ) ) }
		
		if( checkXSD( xml ) != 1 )
		{
			LOG <- paste( filename , "is not a valid XMCDA file" , sep=" " )
			print( paste( "Problem :" , filename , "is not a valid XMCDA file" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		result <- extractContent( xml , filename , aux )
		if( result$validation == FALSE ) { 
			return( list( "validation" = FALSE , "LOG" = result$LOG ) )
		}
		validation <- TRUE
		print( "Note: file found and structure ok" )
		print( "                            " )
		return( list( "validation" = validation , "content" = list(result$content) ) )
	} 
}

# extractContent is extracting the content of each XML tree
extractContent <- function( xml , filename , aux = NULL )
{
	validation = FALSE
	LOG <- "Extraction error : invalid filename"
	if( filename == "alternatives.xml" )
	{
		err <- try( {
					alternativesIDs <- getAlternativesIDs ( xml )
				} )
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "alternatives.xml extraction error" ) ) 
		}
		
		if ( is.null( alternativesIDs[[1]] ) || alternativesIDs$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- alternativesIDs[[1]]	
		validation <- TRUE
	}
	
	if( filename == "alternativesRanking.xml" )
	{
		alternativesIDs <- aux[[1]]
		err <- try( {
					alternativesRanking <- getAlternativesValues ( xml , alternativesIDs )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "alternativesRanking.xml extraction error" ) ) 
		}
		if ( is.null(alternativesRanking[[1]]) || alternativesRanking$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem - check alternativesIDs or content" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		alternativesRanking <- alternativesRanking[[1]]
		NalternativesRanking <- matrix(0, nrow=nrow(alternativesRanking),ncol=2)
		for (i in 1:nrow(alternativesRanking) )
		{
			ind <- alternativesRanking[i,1]
			NalternativesRanking[i,1] <- alternativesIDs[ind]
			NalternativesRanking[i,2] <- as.character(alternativesRanking[i,2])
		}
		content <- NalternativesRanking
		validation <- TRUE
	}
	
	if( filename == "alternativesPreferences.xml" )
	{
		alternativesIDs <- aux[[1]]
		err <- try( {
					alternativesPreferences <- getAlternativesComparisonsLabels( xml , alternativesIDs )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "alternativesPreferences.xml extraction error" ) ) 
		}
		if ( alternativesPreferences$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- alternativesPreferences[[1]]
		validation <- TRUE
	}
	
	if( filename == "alternativesIndifferences.xml" )
	{
		alternativesIDs <- aux[[1]]
		err <- try( {
					alternativesIndifferences <- getAlternativesComparisonsLabels( xml , alternativesIDs )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "alternativesIndifferences.xml extraction error" ) ) 
		}
		if ( alternativesIndifferences$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		alternativesIndifferences <- alternativesIndifferences[[1]]
		alternativesIndifferences <- alternativesIndifferences[,c(1,2,3)]
		if( is.vector(alternativesIndifferences) )
		{
			alternativesIndifferences <- t(as.matrix(alternativesIndifferences)) 
		}
		content <- alternativesIndifferences
		validation <- TRUE
	}
	
	if( filename == "criteria.xml" )
	{
		err <- try( {
					criteriaIDs <- getCriteriaIDs ( xml )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "criteria.xml extraction error" ) ) 
		}
		if ( is.null(criteriaIDs[[1]]) || is.null( criteriaIDs$status != "OK" ) )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- criteriaIDs[[1]]
		validation <- TRUE
	}
	
	if( filename == "delta.xml" )
	{
		err <- try( {
					delta <- getParameters ( xml )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "delta.xml extraction error" ) ) 
		}
		if ( is.null(delta[[1]]) || delta$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- delta[[1]]
		validation <- TRUE
	}
	
	if( filename == "method.xml" )
	{
		err <- try( {
					ac <- getParameters ( xml )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "method.xml extraction error" ) ) 
		}
		if ( is.null(ac[[1]]) || ac$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- ac[[1]]
		validation <- TRUE
	}
	
	
	
	if( filename == "performanceTable.xml" )
	{
		criteriaIDs <- aux$criteriaIDs[[1]]
		activeAlternativesIDs <- aux$alternativesIDs[[1]]
		
		err <- try( {
					performanceTable <- getPerformanceTables(xml,altIDs=activeAlternativesIDs,critIDs=criteriaIDs)
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "performanceTable.xml extraction error" ) ) 
		}
		if( is.null(performanceTable[[1]]) || performanceTable$status != "OK")
		{
			
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- performanceTable[[1]]
		validation <- TRUE
	}
	
	if( filename == "segments.xml" )
	{
		criteriaIDs <- aux[[1]]
		err <- try( {
					segments <- getCriteriaValues ( xml , criteriaIDs )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "segments.xml extraction error" ) ) 
		}
		if ( is.null(segments[[1]]) || segments$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		segments <- segments[[1]]
		content <- rbind( criteriaIDs[segments[,1]] , segments[,2] )
		validation <- TRUE
	}
	
	if( filename == "preferencesDirections.xml" )
	{
		err <- try({
					prefDirections <- getPreferencesDirections ( xml  )
				})
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "preferencesDirections.xml extraction error" ) ) 
		}
		if ( is.null(prefDirections[[1]]) || prefDirections$status != "OK"  )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		prefDirections <- prefDirections[[1]]
		content <- rbind( prefDirections[,1] , prefDirections[,2] )
		validation <- TRUE
	}
	
	return( list( "validation" = validation , "content" = content ) )  
}

# getPreferencesDirections extracts the directions of criteria from the XML tree
getPreferencesDirections <- function(tree)
{
	tmpErr <- try({
				criteriaValues <- getNodeSet(tree,"//criteriaValues")
			})
	if (inherits(tmpErr, "try-error")) {
		
		return(list("status"="error"))
	}
	prefDirections <- NULL
	if(length(criteriaValues)>0){
		tmpErr <- try({
					criterionValue <- getNodeSet(criteriaValues[[1]],"criterionValue")
				})
		if (inherits(tmpErr, "try-error")) {
			
			return(list("status"="error"))
		}
		if(length(criterionValue)>0){
			for(i in 1:length(criterionValue)){
				tmpErr <- try({
							id <- toString(xmlValue(getNodeSet(criterionValue[[i]],"criterionID")[[1]]))
							v <- toString(xmlValue(getNodeSet(getNodeSet(criterionValue[[i]],"value")[[1]],"label")[[1]]))
						})
				if (inherits(tmpErr, "try-error")) {
					
					return(list("status"="error"))
				}
				if(is.null(prefDirections)){
					prefDirections <- as.matrix(c(id,v))
				}else{
					prefDirections <- cbind(prefDirections,c(id,v))	
				}
			}
		}
	}
	return(list(t(prefDirections),"status"="OK"))
}

# combineContents take the information extracted from the inputs and computes the matrices and vectors that will be used 
combineAndRestrictContent <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	
	
	if( content$method != "mv" && content$method != "md" && content$method != "ac"){
		content$method <- "none"
	}
	
	content$initialPerformanceTable <- content$performanceTable
	
	content <- restrictAllFilesToCorrectAlternatives( content )
	#content <- checkAlternatives( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	content <- restrictAllFilesToCorrectCriteria( content )
	#content <- checkCriteria( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	content <- produceOrientedSegment( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	if( any( is.na(content$orientedSegment) )){
		return( list( "validation"=FALSE , "LOG"="Error - please check criteria segments and directions" ) )
	}
	content$validation <- TRUE
	
	return( "content" = content  )
}


# restrictToCorrectAlternatives computes corrections on IDs, PT and rankinks taking into account of effective alternatives
restrictAllFilesToCorrectAlternatives <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	err <- try({ 
				if( !is.null(content$alternativesIndifferences) ){
					content$alternativesIndifferences <- matrix(t(content$alternativesIndifferences[,c(1,2)]),ncol=2)
				}
			})
	if (inherits(err, "try-error")) {
		return(list("validation"=FALSE,"LOG"="error while restructuring indifferences"))
	}
	
	
	A <- c()
	B <- c()
	definitiveAlternatives <- c()
	
	err <- try({
				if( !is.null(content$alternativesRanking[,1]) && !is.na(content$alternativesRanking[,1]) && length(content$alternativesRanking[,1])>0 ){
					B <- c( B , content$alternativesRanking[,1] )
				}
				if( !is.null(content$alternativesPreferences) && !is.na(content$alternativesPreferences) && length(content$alternativesPreferences[,1])>0 ){
					B <- c( B , content$alternativesPreferences[,1] , content$alternativesPreferences[,2] )
				}
				if( !is.null(content$alternativesIndifferences) && !is.na(content$alternativesIndifferences) && length(content$alternativesIndifferences[,1])>0 ){
					B <- c( B , content$alternativesIndifferences[,1] , content$alternativesIndifferences[,2] )
				}
				if( !is.null(rownames(content$performanceTable)) && !is.na(rownames(content$performanceTable)) && nrow(content$performanceTable)>0 ){
					A <- c( A , rownames(content$performanceTable) )
				}
				if( !is.null(content$alternativesIDs) && !is.na(content$alternativesIDs) && length(content$alternativesIDs)>0 ){
					A <- c( A , content$alternativesIDs )
				}
				
				TOT <- c( A , B )
				for( i in 1:length(TOT) ){
					if( any( A == TOT[i] ) && any( B == TOT[i] ) && !any( definitiveAlternatives==TOT[i] ) ){
						definitiveAlternatives <- c( definitiveAlternatives , TOT[i] )
					}
				}
				content$alternativesIDs <- definitiveAlternatives
			})
	if (inherits(err, "try-error")) {
		return(list("validation"=FALSE,"LOG"="error while restructuring alternativesIDs "))
	}
	
	
	
	
	newPerfTable <- matrix(ncol=ncol(content$performanceTable),nrow=0)
	newAlternativesRanking <- matrix(ncol=2,nrow=0)
	newAltPref <- matrix(nrow=0,ncol=3)
	newAltIndiff <- matrix(nrow=0,ncol=2)
	
	err <- try({
				for( i in 1:length(content$alternativesIDs) ){
					a <- content$alternativesIDs[i]
					if( !is.null(content$alternativesRanking[,1]) && !is.na(content$alternativesRanking[,1]) && length(content$alternativesRanking[,1])>0 ){
						if( any( content$alternativesRanking[,1] == a ) && !any( newAlternativesRanking == a ) ){
							newAlternativesRanking <- rbind( newAlternativesRanking ,content$alternativesRanking[which(content$alternativesRanking[,1]==a),] )
						}
					}
					if( !is.null(content$alternativesPreferences) && !is.na(content$alternativesPreferences) && length(content$alternativesPreferences[,1])>0 ){
						if( any( content$alternativesPreferences[ , 1 ] == a ) ){
							selec <- which(  content$alternativesPreferences[ , 1 ] == a )
							if(length(selec)>0){
								for(j in 1:length(selec) ){
									if( any( content$alternativesIDs == content$alternativesPreferences[ selec[j] , 2 ]) ){
										if( !any( newAltPref[,1] == content$alternativesPreferences[ selec[j] , 1 ] )
												&&
												!any( newAltPref[which(newAltPref[,1] == content$alternativesPreferences[ selec[j] , 1 ]),2] == content$alternativesPreferences[ selec[j] , 2 ] )
												
												){
											newAltPref <- rbind( newAltPref , content$alternativesPreferences[ selec[j] ,] )
										}
									}
								}
							}
						}
					}
					if( !is.null(content$alternativesIndifferences) && !is.na(content$alternativesIndifferences) && length(content$alternativesIndifferences[,1])>0 ){
						if( any( content$alternativesIndifferences[ , 1 ] == a ) ){
							selec <- which(  content$alternativesIndifferences[ , 1 ] == a )
							if(length(selec)>0){
								for(j in 1:length(selec) ){
									if( any( content$alternativesIDs == content$alternativesIndifferences[ selec[j] , 2 ]) ){
										if( !any( newAltIndiff[,1] == content$alternativesIndifferences[ selec[j] , 1 ] )
												&&
												!any( newAltIndiff[which(newAltIndiff[,1] == content$alternativesIndifferences[ selec[j] , 1 ]),2] == content$alternativesIndifferences[ selec[j] , 2 ] )
												
												){
											newAltIndiff <- rbind( newAltIndiff , content$alternativesIndifferences[ selec[j] ,] )
										}
									}
								}
							}
						}
					}
					if( !is.null(rownames(content$performanceTable)) && !is.na(rownames(content$performanceTable)) && nrow(content$performanceTable)>0 ){
						if( any( rownames(content$performanceTable) == a ) && !any( rownames( newPerfTable ) == a ) ){
							newPerfTable <- rbind( newPerfTable , content$performanceTable[which(rownames(content$performanceTable)==a),])
							rownames( newPerfTable )[nrow(newPerfTable)] = a	
						}else{
							return(list("validation"=FALSE,"LOG"="Some alternatives you asked are not in performanceTable"))
						}
					}
				}
				content$alternativesRanking <- newAlternativesRanking
				content$performanceTable <- newPerfTable
				content$alternativesPreferences <- newAltPref
				content$alternativesIndifferences <- newAltIndiff
			})
	if (inherits(err, "try-error")) {
		return(list("validation"=FALSE,"LOG"="error while restructuring data"))
	}
	
	
	
	return( content  )
}

# restrictToCorrectCriteria
restrictAllFilesToCorrectCriteria <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	
	if( !is.null(content$segments) ){
		newSegments <- matrix(t(content$segments[2,]),nrow=1)
		colnames(newSegments) <- content$segments[1,]
		content$segments <- newSegments
		rm( newSegments )
	}
	
	newPrefDir <- matrix(t(content$preferencesDirections[2,]),nrow=1)
	colnames(newPrefDir) <- content$preferencesDirections[1,]
	content$preferencesDirections <- newPrefDir
	rm( newPrefDir )
	
	newCriteriaIDs <- c()
	newSegments <- matrix(nrow=1,ncol=0)
	newPrefDir <- matrix(nrow=1,ncol=0)
	newPerfTable <- matrix(ncol=0,nrow=nrow(content$performanceTable))
	
	
	if( !is.null(content$segments) && length(content$segments)>0 ){
		for( i in 1:ncol(content$segments) ){
			a <- colnames(content$segments)[i]
			if(
					( is.null(content$criteriaIDs) || any(content$criteriaIDs==a) )
					&&
					( is.null(content$preferencesDirections) || any(colnames(content$preferencesDirections)==a) )
					&&
					( is.null(content$performanceTable) || any(colnames(content$performanceTable)==a) )
					&&
					( !any(colnames(newSegments) == a) )
					){
				newSegments <- cbind( newSegments , content$segments[,which(colnames(content$segments)==a)[1]] )
				colnames(newSegments)[ncol(newSegments)] <- a
				rownames(newSegments) <- NULL
			}
		}
		content$segments <- newSegments
		rm(newSegments)
	}
	if( !is.null(content$preferencesDirections) && length(content$preferencesDirections)>0 ){
		for( i in 1:ncol(content$preferencesDirections) ){
			a <- colnames(content$preferencesDirections)[i]
			if(
					( is.null(content$criteriaIDs) || any(content$criteriaIDs==a) )
					&&
					( is.null(content$performanceTable) || any(colnames(content$performanceTable)==a) )
					&&
					( is.null(content$segments) || any(colnames(content$segments)==a) )
					&&
					( !any(colnames(newPrefDir) == a) )
					){
				newPrefDir <- cbind( newPrefDir , content$preferencesDirections[,which(colnames(content$preferencesDirections)==a)[1]] )
				colnames(newPrefDir)[ncol(newPrefDir)] <- a
				rownames(newPrefDir) <- NULL
			}
		}
		content$preferencesDirections <- newPrefDir
		rm(newPrefDir)
	}
	if( !is.null(content$performanceTable) && ncol(content$performanceTable)>0 ){
		for( i in 1:ncol(content$performanceTable) ){
			a <- colnames(content$performanceTable)[i]
			if(
					( is.null(content$criteriaIDs) || any(content$criteriaIDs==a) )
					&&
					( is.null(content$preferencesDirections) || any(colnames(content$preferencesDirections)==a) )
					&&
					( is.null(content$segments) || any(colnames(content$segments)==a) )
					){
				newPerfTable <- cbind( newPerfTable , content$performanceTable[,which(colnames(content$performanceTable)==a)[1]])
				colnames( newPerfTable )[ncol(newPerfTable)] = a
			}
		}
		content$performanceTable <- newPerfTable
		rm(newPerfTable)
	}
	if( !is.null(content$criteriaIDs) && length(content$criteriaIDs)>0 ){
		for( i in 1:length(content$criteriaIDs) ){
			a <- content$criteriaIDs[i]
			if(
					( is.null(content$preferencesDirections) || any(colnames(content$preferencesDirections)==a) )
					&&
					( is.null(content$performanceTable) || any(colnames(content$preferencesDirections)==a) )
					&&
					( is.null(content$segments) || any(colnames(content$segments)==a) )
					){
				newCriteriaIDs <- c( newCriteriaIDs , a )
			}
		}
		content$criteriaIDs <- newCriteriaIDs
		rm(newCriteriaIDs)
	}
	
	
	return( c(content, list("validation"=TRUE))  )
}



# produceOrientedSegment combines the information about criteria, directions and segments
produceOrientedSegment <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	err <- try({
				if( is.null( content$segments ) ) #||  is.null( content$preferencesDirections ) )
				{
					content$orientedSegment <- rep(1,length=length(content$criteriaIDs))
					names( content$orientedSegment ) <- content$criteriaIDs 
					orientedSegment <- c()
					crit <- c()
					for( i in 1:length(content$criteriaIDs) )
					{
						if(	any( colnames(content$preferencesDirections) == content$criteriaIDs[ i ] )	)
						{
							crit <- c( crit , content$criteriaIDs[ i ] )
							dir <- content$preferencesDirections[1, which(colnames(content$preferencesDirections) == content$criteriaIDs[ i ] ) ]
							if( dir == "min" )
							{
								g <- -1
							}
							if( dir == "max" )
							{
								g <- 1
							}
							orientedSegment <- c( orientedSegment , g )
							names( orientedSegment ) <- crit 
							content$orientedSegment <- orientedSegment
						}
					}
					return( content )
				}
			})
	if (inherits(err, "try-error")){
		return( list( "validation"=FALSE , "LOG"="fatal error while producing oriented segment 1" ) )
	}
	
	crit <- c()
	orientedSegment <- c()
	
	if( !is.null(content$segments) ){
		err <- try({
					for( i in 1:length(content$criteriaIDs) )
					{
						if(
								any( colnames(content$segments) == content$criteriaIDs[ i ] )
								&&
								any( colnames(content$preferencesDirections) == content$criteriaIDs[ i ] )
								)
						{
							crit <- c( crit , content$criteriaIDs[ i ] )
							g <- content$segments[1,which(colnames(content$segments) == content$criteriaIDs[ i ] ) ]
							dir <- content$preferencesDirections[1, which(colnames(content$preferencesDirections) == content$criteriaIDs[ i ] ) ]
							if( dir == "min" )
							{
								g <- - as.numeric(g)
							}
							if( dir != "max" && dir != "min" )
							{
								g <- NA
							}
							
						}
						else
						{
							g <- NA
						}
						if( max( content$performanceTable[, i] ) == min( content$performanceTable[, i] )  )
						{
							g <- 0
						}
						orientedSegment <- c( orientedSegment , g )
						names( orientedSegment ) <- crit 
					}
				})
	}
	if (inherits(err, "try-error")){
		return( list( "validation"=FALSE , "LOG"="fatal error while producing oriented segment 2" ) )
	}
	
	return( c( content , list( "orientedSegment" = orientedSegment) ) )
}

computeUTASTAR <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	
	if( is.null(content$segments) ){
		segmentation = FALSE
	}else{
		segmentation = TRUE
	}
	
	err <- try({
				result <- 
						computeUTASTARmatrices( 
								content$performanceTable , 
								content$alternativesRanking , 
								content$delta ,
								content$alternativesPreferences ,
								content$alternativesIndifferences ,
								content$orientedSegment ,
								segmentation 
						)
			})
	if (inherits(err, "try-error") || result$validation == FALSE ){
		return( list( "validation"=FALSE , "LOG"="error while computing constraint matrices for UTASTAR method" ) )
	}
	
	print(result)
	
	err <- try({
				xvec <- 
						solveUTASTAR(
								result$c ,
								result$A ,
								result$Aeq ,
								result$b ,
								result$beq ,
								length(content$orientedSegment)
						)
			})
	if (inherits(err, "try-error") ){
		return( list( "validation"=FALSE , "LOG"="error while solving LP-UTASTAR method" ) )
	}
	if( xvec$validation == FALSE ){
		return( list( "validation"=FALSE , "LOG"=xvec$LOG ) )
	}
	
	umatrix <- buildUmatrix(xvec$x,result$segs)
	#rownames(umatrix) = rownames(result$gmatrix)
	
	return( c(xvec, list("A"=result$A , "b"=result$b , "Aeq"=result$Aeq , "beq"=result$beq , "gmatrix"=result$gmatrix, "umatrix" = umatrix , "segs"=result$segs , "method"=content$method ) ) )
}


# computeUTASTARmatrices computes the constraints and lp problem for the UTASTAR disaggregation model
computeUTASTARmatrices <- function( performanceTable , ranking , delta , prefPairs , indiffPairs , prefDirections , segmentation = FALSE  )
{
	err <- try({
				if(nrow(ranking) == 0){
					ranking <- NULL
				}
				if( is.null(delta) ){
					delta <- 0.00001
				}
				# toutes les paires de preferences
				if( !is.null(prefPairs) ){
					totalPrefPairs = prefPairs
				}else{
					totalPrefPairs = matrix(ncol=3,nrow=0)
				}
				# toutes les paires d'indifferences
				if( !is.null(indiffPairs) ){
					totalIndiffPairs = indiffPairs
				}else{
					totalIndiffPairs =  matrix(ncol=2,nrow=0)
				}
				
				# on transforme le ranking en paires
				if( !is.null(ranking) ){
					ranking <- ranking[order(as.numeric(ranking[,2])),]
					M = nrow(ranking)
					for( i in 1:(M-1) ){
						alt1 <- ranking[i,1]
						r1 <- ranking[i,2]
						for( j in (i+1):(i+1)){ #:M ){  # ????? TODO 
							alt2 <- ranking[j,1]
							r2 <- ranking[j,2]
							if( r1 == r2 ){
								totalIndiffPairs <- rbind( totalIndiffPairs , c(alt1,alt2) )
							}else{
								totalPrefPairs <- rbind( totalPrefPairs , c(alt1 , alt2 , NA))
							}
						}
					}
				}
				if( nrow(totalPrefPairs)==0 ){
					totalPrefPairs = NULL
				}
				if( nrow(totalIndiffPairs)==0 ){
					totalIndiffPairs = NULL
				}
			})
	if (inherits(err, "try-error") ){
		return( list( "validation"=FALSE , "LOG"="error in preliminary computations of UTASTAR constraints" ) )
	}
	
	print("---totalPrefPairs---")
	print(totalPrefPairs)
	print("---totalIndiffPairs---")
	print(totalIndiffPairs)
	
	# début de la construction
	err <- try({
				# nbre de critères
				Ncrit = length( prefDirections )
				
				# nbre de paires
				if( !is.null(totalPrefPairs) ){
					Npp = nrow( totalPrefPairs )
				}else{
					Npp <- 0
				}
				if( !is.null(totalIndiffPairs) ){
					Nip = nrow( totalIndiffPairs )
				}else{
					Nip <- 0
				}
				# nbre d'alternatives
				Nalt <- countNbDiffAlternatives( totalPrefPairs , totalIndiffPairs )
				
				# segmentation des fonctions d'utilité
				R <- contructGmatrix( prefDirections , performanceTable , segmentation )
#########################			prefDirections <- R$prefDirections
				gmatrix <- R$gmatrix 
				S <- sum( abs( as.numeric(prefDirections) ) )
				
				
				V <- 2*Nalt + S + Ncrit
				
				A <- matrix(0,nrow=Npp+S,ncol=V)
				b <- matrix(0,nrow=Npp+S,ncol=1)
				
				Aeq = matrix(0,nrow=Nip+Ncrit+1,ncol=V)
				beq = matrix(0,nrow=Nip+Ncrit+1,ncol=1)
				
				
				if( !is.null(totalPrefPairs) ){
					for ( i in 1:Npp )
					{
						aindex <- which(rownames(performanceTable) == totalPrefPairs[i,1])
						bindex <- which(rownames(performanceTable) == totalPrefPairs[i,2])
						
						for( j in 1:Ncrit ){		
							crit <- names(prefDirections)[j]
							
							
							jPT <- which( colnames(performanceTable) == crit )
							g <- gmatrix[ which( rownames(gmatrix) == crit ) , ]
							
							
							ga <- performanceTable[aindex,jPT]
							gb <- performanceTable[bindex,jPT]
							
							gainfind <- max(which(g<=ga))
							gasupind <- min(which(g>=ga))
							gbinfind <- max(which(g<=gb))
							gbsupind <- min(which(g>=gb))
							
							gainf <- g[gainfind]
							gasup <- g[gasupind]
							gbinf <- g[gbinfind]
							gbsup <- g[gbsupind]
							
							
							if (j == 1){ x <- 0 }else{
								x <- sum(abs(as.numeric(prefDirections[1:(j-1)]))) +j-1
							}
							
							if (gainf == gasup) {
								A[i,x+gainfind] <- A[i,x+gainfind]+1
							}else{
								A[i,x+gainfind] <- A[i,x+gainfind]+ (gasup-ga)/(gasup-gainf)
								A[i,x+gasupind] <- A[i,x+gasupind]+ (ga-gainf)/(gasup-gainf)
							}
							
							if (gbinf == gbsup) {
								A[i,x+gbinfind] <- A[i,x+gbinfind]-1
							}else{
								A[i,x+gbinfind] <- A[i,x+gbinfind] - (gbsup-gb)/(gbsup-gbinf)
								A[i,x+gbsupind] <- A[i,x+gbsupind] - (gb-gbinf)/(gbsup-gbinf)
							}
					  	}
					
					
					
						x <- aindex-1+S+Ncrit
						A[i,x] <- -1
						A[i,x+Nalt] <- 1
					
						x <- aindex+S+Ncrit
						A[i,x] <- 1
						A[i,x+Nalt] <- -1
					
						if( is.na(totalPrefPairs[i,3]) )
						{
							b[i] = as.numeric( delta )
						}
						else
						{
							b[i] = as.numeric( totalPrefPairs[i,3] )
						}
					
					}
					
					for ( i in 1:Ncrit ){
						if (i==1){
							x <- 0
							y <- Npp
						}else{
							x <- sum(abs(as.numeric(prefDirections[1:(i-1)])))+i-1
							y <- sum(abs(as.numeric(prefDirections[1:(i-1)])))+Npp
						}
						
						s <- sign(as.numeric(prefDirections[i]))
						if( s != 0 ){
							for (j in (1:abs(as.numeric(prefDirections[i])))){
								A[y+j,x+j] <- -s
								A[y+j,x+j+1] <- s
							}
						}
					}
					
				}
				
				
				if( !is.null(totalIndiffPairs) ){
					
					for ( i in (1:Nip) )
					{
						aindex <- which(rownames(performanceTable) == totalIndiffPairs[i,1])
						bindex <- which(rownames(performanceTable) == totalIndiffPairs[i,2])
						
						y <- i
						
						for ( j in (1:Ncrit) )
						{
							
							crit <- names(prefDirections)[j]
							jPT <- which( colnames(performanceTable) == crit )
							g <- gmatrix[ which( rownames(gmatrix) == crit ) , ]
							
							ga <- performanceTable[aindex,jPT]
							gb <- performanceTable[bindex,jPT]
							
							gainfind <- max(which(g<=ga))
							gasupind <- min(which(g>=ga))
							gbinfind <- max(which(g<=gb))
							gbsupind <- min(which(g>=gb))
							
							gainf <- g[gainfind]
							gasup <- g[gasupind]
							gbinf <- g[gbinfind]
							gbsup <- g[gbsupind]
							
							
							
							if (j==1){
								x <- 0
							}else{
								x <- sum(abs(as.numeric(prefDirections[1:(j-1)])))+j-1
							}
							if (gainf == gasup) {
								Aeq[y,x+gainfind] <- Aeq[y,x+gainfind]+1
							} else {
								Aeq[y,x+gainfind] <- Aeq[y,x+gainfind]+ (gasup-ga)/(gasup-gainf)
								Aeq[y,x+gasupind] <- Aeq[y,x+gasupind]+ (ga-gainf)/(gasup-gainf)
							}
							
							if (gbinf == gbsup) {
								Aeq[y,x+gbinfind] <- Aeq[y,x+gbinfind]-1
							} else {
								Aeq[y,x+gbinfind] <- Aeq[y,x+gbinfind] - (gbsup-gb)/(gbsup-gbinf)
								Aeq[y,x+gbsupind] <- Aeq[y,x+gbsupind] - (gb-gbinf)/(gbsup-gbinf)
							}	
						}
						
						
						if (i==1){
							x <- S+Ncrit+aindex-1
						}else{
							x <- S+Ncrit+aindex-1
						}
						
						Aeq[y,x] <- -1
						Aeq[y,x+Nalt] <- 1
						
						x <- S+Ncrit+bindex-1
						Aeq[y,x] <- 1
						Aeq[y,x+Nalt] <- -1
					}	
					
				}
				
				for (i in 1:Ncrit){
					y=Nip+i
					if(i==1)
					{
						infe <- 1
					}
					else
					{
						infe <- sum(abs(as.numeric(prefDirections[1:(i-1)])))+i
					}
					supe <- sum(abs(as.numeric(prefDirections[1:i])))+i
					if (sign(as.numeric(prefDirections[i]))>0)
					{
						Aeq[y,infe]=1
						Aeq[Nip+Ncrit+1,supe]=1
					}
					else
					{
						Aeq[y,supe]=1
						Aeq[Nip+Ncrit+1,infe]=1
					}
				}
				beq[Nip+Ncrit+1]=1
				
				
				b <- as.vector(b)
				beq <- as.vector(beq)
				
				c <- matrix(1,nrow=1,ncol=V)
				c[1,1:(Ncrit+S)]=0
				c <- as.vector(c)
			})
	if (inherits(err, "try-error") ){
		return( list( "validation"=FALSE , "LOG"="error during computation of UTASTAR raw constraints" ) )
	}
	
	return( list( "A"=A , "b"=b , "Aeq"=Aeq , "beq"=beq , "c"=c , "segs"=prefDirections , "gmatrix"=gmatrix , "validation"=TRUE ) )
}


# countNbDiffAlternatives
countNbDiffAlternatives <- function( alternativesPreferences , alternativesIndifferences )
{
	err <- try({
				nams <- c()
				if( !is.null(alternativesPreferences) ){
					nams <- c( nams , alternativesPreferences[,1] , alternativesPreferences[,2] )
				}
				if( !is.null(alternativesIndifferences) ){
					nams <- c( nams , alternativesIndifferences[,1] , alternativesIndifferences[,2] )
				}
				
				nb <- nams[1]
				for ( i in 2:length(nams) )
				{
					if( all( nb != nams[i] ) )
					{
						nb <- c( nb , nams[i] )
					}
				}
			})
	if (inherits(err, "try-error") ){
		return( list( "validation"=FALSE , "LOG"="error counting alternatives" ) )
	}
	return( length(nb) )
	
}



# contructGmatrix
contructGmatrix <- function( prefDirections , performanceTable , segmentation ){
	
	err <- try({
				Glist <- list()
				if( segmentation == TRUE ){
					for( i in 1:ncol(performanceTable) ){
						PT <- performanceTable[,i]
						seg <- prefDirections[which(names(prefDirections)==colnames(performanceTable)[i])] 
						gmin <- min(PT)
						gmax <- max(PT)
						g <- seq(from=gmin,by=((gmax-gmin)/abs(as.numeric(seg))), to=gmax)
						prefDirections[which(names(prefDirections)==colnames(performanceTable)[i])] <- sign(as.numeric(seg))*length(g)
						Glist[[ colnames(performanceTable)[i] ]] <- g
				 }
				}else{
					for( i in 1:ncol(performanceTable) ){
						g <- c()
						PT <- performanceTable[,i]
						seg <- prefDirections[which(names(prefDirections)==colnames(performanceTable)[i])] 
						for( j in 1:length(PT) ){
							if( !any( g == PT[j] ) ){
								g <- c(g,PT[j])
							}
						}
						g <- g[order(g)]
						prefDirections[which(names(prefDirections)==colnames(performanceTable)[i])] <- sign(as.numeric(seg))*length(g)
						Glist[[ colnames(performanceTable)[i] ]] <- g
					}
				}
				
				L <- 0
				for( i in 1:length(Glist) ){
					if( length( Glist[[i]] ) > L ){
						L <- length(Glist[[i]])
					}
				}
				gmatrix = matrix(ncol=L,nrow=0)
				for( i in 1:length(Glist) ){
					if( length( Glist[[i]] ) < L ){
						gmatrix <- rbind(gmatrix,c( Glist[[i]] , rep(NA,L-length(Glist[[i]]))))
					}else{
						gmatrix <- rbind(gmatrix,Glist[[i]])
					}
					rownames(gmatrix)[nrow(gmatrix)] <- names(Glist)[i]
				}
				colnames(gmatrix)=NULL
			})
	if (inherits(err, "try-error") ){
		return( list( "validation"=FALSE , "LOG"="error during construction of Gmatrix" ) )
	}
	return( list( "prefDirections"=prefDirections , "gmatrix"=gmatrix ))
}

# solveUTASTARlp solves the lp problem for the UTASTAR disaggregation model
solveUTASTAR <- function( c , A , Aeq , b , beq , nC )
{
	# Linear Programming
	# err <- try({
	#			uy=c(rep(">=",length(b)),rep("==",length(beq)))
	#			b[which(b>0)] <- b[which(b>0)] + rep(10^(-4),length(b[which(b>0)]))
	#			SOL <- lp(direction = "min",objective.in=c,const.mat=rbind(A,Aeq),const.dir=uy,const.rhs=c(b,beq),presolve=1)
	#			x <- SOL$solution
	#		})
	
	
	# Quadratic Programming
	library(LowRankQP)
	nAeq <- cbind(Aeq,matrix(0,ncol=length(b),nrow=nrow(Aeq)))
	nV <- diag(rep(-1,nrow(A)))
	nA <- cbind(A,nV)
	Amat <- rbind(nAeq,nA)
	if( nrow(Aeq)-nC > 1 ){
		lll = rep(1, nrow(Aeq)-nC-1 )
		Amat = cbind(Amat,rep(0,nrow(Amat)))
		Amat[1:length(lll),ncol(Amat)] = lll
		dvec <- as.vector(c(c,1,rep(0,length(b))))
	}else{
		dvec <- as.vector(c(c,rep(0,length(b))))
		}
	Vmat <- matrix(0,nrow=ncol(Amat),ncol=ncol(Amat))
	bvec <- c(beq,b)
	uvec <- rep(1,ncol(Amat))
	err <- try({
				SOL <- LowRankQP(Vmat,dvec,Amat,bvec,uvec,method="LU",verbose=FALSE)
			})
	if (inherits(err, 'try-error'))
	{ return( list( "validation" = FALSE , "LOG" = "METHOD ERROR : FAILED TO CONVERGE" ) ) }
	
	x <- SOL[["alpha"]]
	
	if( all( is.na(x) ) )
	{ return( list( "validation" = FALSE , "LOG" = "Error - failed to converge, due to bad information. Please check your data, rescale the problem, or try with less constraints." ) ) }
	
	x <- as.vector(x)
	x <- x[1:ncol(A)]
	
	return(list("x"=x,"validation"=TRUE))	
}

# computePostOptimalityAnalysis
computePostOptimalityAnalysis <- function( sol ){
	LOG = sol$LOG
	if( sol$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=sol$LOG ) )
	}
	
	err <- try({
				LOG = NULL
				xAC = NULL
				x <-   sol$x
				A <- sol$A
				b <- sol$b
				Aeq <- sol$Aeq
				beq <- sol$beq
				segs <- sol$segs
				method <- sol$method
				N <- length( as.numeric(segs) )
				S <- sum(abs(as.numeric(segs)))
				sigmaoutput<-0
				status <- FALSE
			})
	if (inherits(err, 'try-error')){ 
		return( list( "validation" = FALSE , "LOG" = "Analytic Center Error during preliminary computations" ) ) 
	}
	
	if ((max(x[(S+N+1):length(x)]) > 0.00001 )){  
		# Contraintes non satisfaites
		LOG <- "One or more constraints are not satisfied ; UTA partial success"
		xAC <- x
	}else{
		# Contraintes satisfaites
		
		if( method == "ac" ){
			# Analytic Center
			err <- try({
						A <- -A
						b <- -b
						outAC <- analyticCenter( x[1:(S+N)] , A[,1:(S+N)] , b , Aeq[,1:(S+N)] )
					})
			if (inherits(err, 'try-error')){ 
				return( list( "validation" = FALSE , "LOG" = "Analytic Center Error" ) ) 
			}
			if( outAC$validation == TRUE ){
				xAC <- round(outAC$x,digits = 6)
				LOG = "Analytic Center method successfuly computed"
			}else{
				return( list("validation"=FALSE , "LOG"=outAC$LOG ) )
			}
		}
		
		if( method == "md" ){
			err <- try({
						outAC <- utamp( A[,1:(S+N)] , b , Aeq[,1:(S+N)] , beq , as.integer(segs) )
					})
			if (inherits(err, 'try-error')){ 
				return( list( "validation" = FALSE , "LOG" = "UTAMP - Max delta Error" ) ) 
			}
			if( outAC$validation == TRUE ){
				xAC <- round(outAC$x,digits = 6)
				LOG = "UTAMP - Max delta method successfuly computed"
			}else{
				return( list("validation"=FALSE , "LOG"=outAC$LOG ) )
			}
			
		}
		
		if( method == "mv" ){
			err <- try({
						outAC <- meanValue( A[,1:(S+N)] , b , Aeq[,1:(S+N)] , beq , as.integer(segs) )
						#outAC <- meanValue( A , b , Aeq , beq , as.integer(segs) )
					})
			if (inherits(err, 'try-error')){ 
				return( list( "validation" = FALSE , "LOG" = "Mean Value method error" ) ) 
			}
			if( outAC$validation == TRUE ){
				xAC <- round(outAC$x,digits = 6)
				LOG = "Mean Value method successfuly computed"
			}else{
				return( list("validation"=FALSE , "LOG"=outAC$LOG ) )
			}
			
		}
		
		if( method == "none" ){
			# Pas de méthode postOpt
			xAC <- x
			LOG = "Computation : Classical UTASTAR without Analytic Center"
		}
	}
	
	if ( is.null(xAC) || inherits(err, 'try-error')){ 
		return( list( "validation" = FALSE , "LOG" = "Computation error during post optimality analysis" ) ) 
	}
	
	umatrix <- buildUmatrix( xAC , segs )
	return( list( "validation"=TRUE , "LOG"=LOG , "x" = xAC , "umatrix"=umatrix , "gmatrix"=sol$gmatrix ) )
}

meanValue <- function(A,b,Aeq,beq,segs){
	library( lpSolve )
	library( linprog )
	
	err <- try({
				
				nCrit = length(segs)
				S = sum(abs(segs)) + nCrit
				nsig = ncol(A) - S
				xMV = rep( 0 , ncol(A) )
				
				print("c's")
				
				for( i in 1:nCrit ){
					c = rep( 0 , ncol(A) )
					
					if(i==1){
						infe <- 1
					}else{
						infe <- sum(abs(segs[1:(i-1)]))+i
					}
					supe <- sum(abs(segs[1:i]))+i
					if( segs[i] != 0 ){
						if (sign(segs[i])>0){
							c[supe] = 1
						}else{
							c[infe] = 1
						}
					}
					
					print(c)
					c_min = c
					c_max = -c
										
					err <- try({
								
								lp_min = lp( direction = "min" , 
											objective.in = c , 
											const.mat = rbind( A , Aeq ) ,
											const.dir = c( rep(">",nrow(A)) , rep("==",nrow(Aeq)) ) ,
											const.rhs = c( b , beq )  )
								lp_max =	lp( direction = "max" , 
											objective.in = c , 
											const.mat = rbind( A , Aeq ) ,
											const.dir = c( rep(">",nrow(A)) , rep("==",nrow(Aeq)) ) ,
											const.rhs = c( b , beq )  )
								
								#lp_min = solveUTASTAR(c_min,A,Aeq,b,beq)
								#lp_max = solveUTASTAR(c_max,A,Aeq,b,beq)
								
								#print(lp_min)
								#print(lp_max)
											
								#if( lp_min$validation == TRUE && lp_max$validation == TRUE ){
								#	x_min = lp_min$x
								#   x_max = lp_max$x
			
								if( lp_min$status == 0 && lp_max$status == 0 ){
									x_min = lp_min$solution
								    x_max = lp_max$solution
								}else{
									print("No feasible solution found")
									return( list( "validation" = FALSE , "LOG" = "Mean Value - error or no feasible solution. Try to rescale or to use another post-opt method" ) ) 
								}
								
							})
					if (inherits(err, 'try-error')){ 
						return( list( "validation" = FALSE , "LOG" = "Mean Value internal computation error" ) ) 
					}
					xMV = xMV + ( x_min + x_max ) / ( 2 * nCrit)
					
				}
	})
	if (inherits(err, 'try-error')){ 
		return( list( "validation" = FALSE , "LOG" = "Mean Value computation error" ) ) 
	}
	
	print( "--------------------------" )
	print( " X MEAN VALUE ")
	print( xMV )
	print( "--------------------------" )
	
	return( list("x"=xMV , "validation"=TRUE) )
}

utamp <- function(A,b,Aeq,beq,segs){
	
	library( lpSolve )
	library( linprog )
	
	
	err <- try({
				nCrit = length(segs)
				S = sum(abs(segs))
				Npp = nrow(A) - S
				c = c( rep(0,ncol(A)) , 1 ) 
				D = c( rep(-1,Npp) , rep(0,S) )
				A = cbind( A , as.matrix(D) )
				b = rep( 0 , length(b) )
				Aeq = cbind( Aeq , rep(0,nrow(Aeq)) )
				
				sol = lp( direction = "max" , 
						objective.in = c , 
						const.mat = rbind( A , Aeq ) ,
						const.dir = c( rep(">",nrow(A)) , rep("==",nrow(Aeq)) ) ,
						const.rhs = c( b , beq )  )
				
				if( sol$status == 0 ){
					x = sol$solution
				}else{
					print("No feasible solution found")
					return( list( "validation" = FALSE , "LOG" = "Mean Value - error or no feasible solution" ) ) 
				}
				
	})
	if (inherits(err, 'try-error')){ 
		return( list( "validation" = FALSE , "LOG" = "Utamp (max delta) computation error" ) ) 
	}
	
	return( list("x"=x , "validation"=TRUE) )
}

analyticCenter <- function(x,A,b,Aeq)
{
	if(!is.vector(x)){
		return (list("LOG"="x should be a vector","validation"=FALSE))
		}
	if(!is.matrix(A)){
		return (list("LOG"="A should be a matrix","validation"=FALSE))
		}
	if(!is.vector(b)){
		return (list("LOG"="b should be a vector","validation"=FALSE))
		}
	if(!is.matrix(Aeq)){
		return (list("LOG"="Aeq should be a matrix","validation"=FALSE))
		}
		
	err <- try({
	fac <- 0.95
	it <- 0
	oldnorm <- 0
	stepredcount <- 0
	flag <- 1
	
	library(MASS)
	if (is.null(x))
		x = matrix(1,nrow=ncol(A),ncol=1) 
	if (is.null(Aeq)){
		Z= diag(rep(1,ncol(A)))
	}else{
		Z <- nullspace(Aeq)
	}
	})
	if (inherits(err, 'try-error')){
		return (list("LOG"="Problem while computing nullspace","validation"=FALSE))
		}
	
	err <- try({
		out <- analyticCenterInitialization(x,A,b)
	})
	if (inherits(err, 'try-error')){
		return (list("LOG"="Problem while initialising analytic center","validation"=FALSE))
		}
		
	x <- out[[1]]
	flag <- out[[2]]
	oldnorm <- 0
	xini <- x
	if (flag>0){
		while (1){
			s <- b - A%*%x
			sbis = as.vector(s)
			
			e = matrix(1,nrow=nrow(s),ncol=1)
			iS <- diag(sbis^(-1))
			
			grad <- matrix(nrow=ncol(A),ncol=1)
			grad <- t(A) %*% iS %*% e
			Hess <- t(A) %*% iS %*% iS %*% A
			
			Zgrad <- t(Z)%*%grad
			gradnorm <- sqrt(sum(Zgrad^2))
			
			if (gradnorm < 1e-6)
				break
			if (abs(oldnorm-gradnorm)<1e-6)
				break
			oldnorm = gradnorm
			dz <- - pseudoinverse(t(Z) %*% Hess %*% Z) %*% Zgrad
			n <- Z %*% dz
			while (any((A %*% (x+n)) >= b)){
				n = n %*% fac
				stepredcount = stepredcount + 1
				if(stepredcount >200){
					print("step redcount error")
					break
				}
			}
			x <- x + n
			it = it + 1
			if (it>200) {
				flag=0
				break
			}
		}
	}
	return (list("x"=x,"validation"=TRUE))	
}

analyticCenterInitialization <- function(x,A,b)
{
	flag <- 1
	stepred <- 0.9
	it1 <- 0
	while (any(A %*% x >= b)){
		prode <- A %*% x
		indexoutside <- which(A %*% x >= b)
		indexinside <- which(A %*% x < b)
		val <- min(prode[indexoutside])[1]
		subindex <- which.min(prode[indexoutside]) 
		index <- indexoutside[subindex]
		dir <- -A[index,]
		it2 <-0
		while (any(A[indexinside,] %*% (x+dir) >= b[indexinside])){
			dir <- dir * stepred
			it2 <- it2+1
			if (it2 > 300){
				print("it2 block")
				it2B <- 1
				break
			}
		}
		it1 <- it1 + 1
		if ((it1 > 300) || (it2 > 300)){
			flag <- 0
			print("ERROR ACINI")
			break
		}
		x <- x + dir
	}
	return(list(x,flag))
}

pseudoinverse <- function (m, tol)
{
	msvd = fast.svd(m, tol)
	if (length(msvd$d) == 0) {
		return(array(0, dim(m)[2:1]))
	}
	else {
		return(msvd$v %*% (1/msvd$d * t(msvd$u)))
	}
}

nullspace <- function(Aeq)
{
	m <- nrow(Aeq)
	n <- ncol(Aeq)
	R <- svd(Aeq,nv=n)
	U <- R$u
	V <- R$v
	S <- R$d
	if (m>1){
		s <- diag(S)
	}else{
		if (m==1){
			s <- S[1]
		}else{
			s <- 0
		}
	}
	tol <- max(m,n) * max(s) * ( .Machine$double.eps )
	r <- sum(s > tol)
	Z <- V[,(r+1):n]
	return(Z)
}

psmall.svd <- function (m, tol) 
{
	B = crossprod(m)
	s = svd(B, nu = 0)
	if (missing(tol)) 
		tol = dim(B)[1] * max(s$d) * .Machine$double.eps
	Positive = s$d > tol
	d = sqrt(s$d[Positive])
	v = s$v[, Positive, drop = FALSE]
	u = m %*% v %*% diag(1/d, nrow = length(d))
	return(list(d = d, u = u, v = v))
}

fast.svd <- function (m, tol)
{
	n = dim(m)[1]
	p = dim(m)[2]
	EDGE.RATIO = 2
	if (n > EDGE.RATIO * p) {
		return(psmall.svd(m, tol))
	}
	else if (EDGE.RATIO * n < p) {
		return(nsmall.svd(m, tol))
	}
	else {
		return(positive.svd(m, tol))
	}
}

positive.svd <- function (m, tol) 
{
	s = svd(m)
	if (missing(tol)) 
		tol = max(dim(m)) * max(s$d) * .Machine$double.eps
	Positive = s$d > tol
	return(list(d = s$d[Positive], u = s$u[, Positive, drop = FALSE], 
					v = s$v[, Positive, drop = FALSE]))
}

buildUmatrix <- function(u,segs)
{
	N = length(segs)
	um	= matrix(nrow=N,ncol=max(abs(as.numeric(segs)))+1)
	for (i in 1:N){
		if (i==1){
			startindex <- 1
		}else{
			startindex <- sum(abs(as.numeric(segs[1:(i-1)]))+1)+1
		}
		
		um[i,1:(abs(as.numeric(segs[i]))+1)] <- u[startindex:(startindex+abs(as.numeric(segs[i])))]
	}
	rownames(um)=names(segs)
	return( round(um,digits=5) )
}

saveUtilityFunctionUnderXML <- function( sol )
{
#<xmcda:XMCDA xsi:schemaLocation="http://www.decision-deck.org/2009/XMCDA-2.0.0 file:../XMCDA-2.0.0.xsd"
	z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
#z$setNamespace("r")
	
	criteriaIDs <- rownames(sol$gmatrix)
	z$addNode("criteria",close=FALSE,attrs=c("mcdaConcept"="criteria"))
	if (TRUE){
		gmatrix  <- sol$gmatrix
		umatrix  <- sol$umatrix
		for (i in 1:nrow(gmatrix)){
			id = criteriaIDs[i]
			indGrow = which(rownames(gmatrix)==id)
			indUrow = which(rownames(umatrix)==id) 
			z$addNode("criterion",close=FALSE,attrs=c("id"=id))
			z$addNode("criterionFunction",close=FALSE)
			z$addNode("points",close=FALSE)
			for (j in 1:length(gmatrix[indGrow,])){
				if(!is.na(gmatrix[indGrow,j])){
					z$addNode("point",close=FALSE)
					z$addNode("abscissa",close=FALSE)
					z$addNode("real",gmatrix[indGrow,j])
					z$closeTag()
					z$addNode("ordinate",close=FALSE)
					z$addNode("real",umatrix[indUrow,j])
					z$closeTag()
					z$closeTag()
				}
			}
			z$closeTag()
			z$closeTag()
			z$closeTag()
		}
	}
	z$closeTag()
#cat(saveXML(z))
	return(z)
}