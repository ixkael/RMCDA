scanInputsFolder <- function( inputsLocation  )
{
	validation <- TRUE
	SOLUTION <- list( "validation" = validation )
	print( "--- LOADING FOLDER ---" )
	print( "                            " )
	listOfFiles <- list.files( inputsLocation )
	
	result <- findAndCheck( "alternatives.xml" , inputsLocation , listOfFiles )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
    if( !is.null( result$content ) )
    {
    	alternativesIDs <- result$content
    	SOLUTION <- c( SOLUTION , "alternativesIDs" = alternativesIDs )
    }	
    
    result <- findAndCheck( "alternativesRanking.xml" , inputsLocation , listOfFiles , aux=alternativesIDs )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
    if( !is.null( result$content ) )
    {
    	alternativesRanking <- result$content
    	SOLUTION <- c( SOLUTION , "alternativesRanking" = alternativesRanking )
    }
    
     result <- findAndCheck( "alternativesPreferences.xml" , inputsLocation , listOfFiles , aux=alternativesIDs )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
    if( !is.null( result$content ) )
    {
    	alternativesPreferences <- result$content
   		SOLUTION <- c( SOLUTION , "alternativesPreferences" = alternativesPreferences )
    }
    
    result <- findAndCheck( "alternativesIndifferences.xml" , inputsLocation , listOfFiles , aux=alternativesIDs )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
    if( !is.null( result$content ) )
    {
    	alternativesIndifferences <- result$content
   		SOLUTION <- c( SOLUTION , "alternativesIndifferences" = alternativesIndifferences )
    }
    
    result <- findAndCheck( "criteria.xml" , inputsLocation , listOfFiles )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
    criteriaIDs <- result$content
    SOLUTION <- c( SOLUTION , "criteriaIDs" = criteriaIDs )
    
    result <- findAndCheck( "delta.xml" , inputsLocation , listOfFiles )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
   	if( !is.null( result$content ) )
    {
    	delta <- result$content
    	SOLUTION <- c( SOLUTION , "delta" = delta )
    }
    
    result <- findAndCheck( "level.xml" , inputsLocation , listOfFiles )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
   	if( !is.null( result$content ) )
    {
    	level <- result$content
    	SOLUTION <- c( SOLUTION , "level" = level )
    }
    
    aux <- list( "criteriaIDs" = criteriaIDs , "alternativesIDs" = alternativesIDs )
    result <- findAndCheck( "performanceTable.xml" , inputsLocation , listOfFiles , aux )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
    if( !is.null( result$content ) )
    {
   		performanceTable <- result$content
    	SOLUTION <- c( SOLUTION , "performanceTable" = performanceTable )
    }
    
    result <- findAndCheck( "segments.xml" , inputsLocation , listOfFiles , aux=criteriaIDs )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
   	if( !is.null( result$content ) )
    {
    	segments <- result$content
    	SOLUTION <- c( SOLUTION , "segments" = segments )
    }
    
    result <- findAndCheck( "preferencesDirections.xml" , inputsLocation , listOfFiles , aux=criteriaIDs )
    if( result$validation == FALSE ) { 
    	return( list( "validation" = FALSE , "LOG" = result$LOG ) )
    	}
   	if( !is.null( result$content ) )
    {
    	preferencesDirections <- result$content
    	SOLUTION <- c( SOLUTION , "preferencesDirections" = preferencesDirections )
    }
    
    
    print( "--- FOLDER CORRECLY LOADED ---" )

    return( SOLUTION )
 }

#########################################################################

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
		return( list( "validation" = TRUE , "content" = NULL , "LOG" = LOG ) )
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

#########################################################################

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
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
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
		if ( is.null(alternativesPreferences[[1]]) ||  alternativesPreferences$status != "OK" )
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
		if ( is.null(alternativesIndifferences[[1]]) || alternativesIndifferences$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		alternativesIndifferences <- alternativesIndifferences[[1]]
		alternativesIndifferences <- alternativesIndifferences[,c(1,2)]
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
	
	if( filename == "level.xml" )
	{
		err <- try( {
				   level <- getParameters ( xml )
				   })
		if ( inherits( err , 'try-error' ) )
		{ 
			return( list( "validation" = FALSE , "LOG" = "level.xml extraction error" ) ) 
		}
		if ( is.null(level[[1]]) || level$status != "OK" )
		{
			LOG <- paste( filename , "XMCDA file problem" , sep=" " )
			print( paste( "Problem :" , filename , "XMCDA file problem" , sep=" " ) )
			return( list( "validation" = validation , "LOG" = LOG ) )  
		}
		content <- level[[1]]
		validation <- TRUE
	}
	
	if( filename == "performanceTable.xml" )
	{
		criteriaIDs <- aux$criteriaIDs[[1]]
		activeAlternativesIDs <- aux$alternativesIDs[[1]]

		err <- try( {
				   performanceTable <- getPerformanceTables(xml,altIDs=activeAlternativesIDs,critIDs=criteriaIDs)
				   print("performanceTable")
				   print(performanceTable)
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

#########################################################################

getPreferencesDirections <- function(tree)
{
	criteriaValues <- getNodeSet(tree,"//criteriaValues")
	prefDirections <- NULL
	if(length(criteriaValues)>0){
		criterionValue <- getNodeSet(criteriaValues[[1]],"criterionValue")
		if(length(criterionValue)>0){
			for(i in 1:length(criterionValue)){
				id <- toString(xmlValue(getNodeSet(criterionValue[[i]],"criterionID")[[1]]))
				v <- toString(xmlValue(getNodeSet(getNodeSet(criterionValue[[i]],"value")[[1]],"label")[[1]]))
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

#########################################################################

modifyAlternatives <- function( content )
{
	if( content$PREORDER == FALSE )
	{
		definitiveAlternatives <- c()
		for( i in 1:length( content$alternativesIDs ) )
		{
			flag <- FALSE
			
			a <- content$alternativesIDs[ i ]
			if(
				any( content$alternativesRanking[,1] == a ) 
				&&
				any( rownames(content$performanceTable) == a )
				){ flag <- TRUE }
			
			if(flag == TRUE)
			{
				definitiveAlternatives <- c( definitiveAlternatives , a )
			}
		}
		definitiveRanking <- matrix( ncol=2 , nrow=0 )
		for( i in 1:nrow( content$alternativesRanking ) )
		{
			flag <- FALSE
			a <- content$alternativesRanking[ i , 1 ]
			if( any( content$alternativesIDs == a ) ) 
			{
				flag <- TRUE
			}
			if(flag == TRUE)
			{
				definitiveRanking <- rbind( definitiveRanking , content$alternativesRanking[i,] )
			}
		}
		content$alternativesRanking <- definitiveRanking
		content$alternativesIDs <- definitiveAlternatives
		print("* alternatives and ranking are OK")
	}
	
	
	if( content$PREORDER == TRUE ) 
	{
		content$alternativesPreferences[ which(is.na(content$alternativesPreferences[,3])) , 3 ] = 0.001
		if( any( as.numeric( content$alternativesPreferences[,3] ) >=1 ) )
		{
			content$validation <- FALSE
			content$LOG <- "alternativesPreferences.xml error : deltas must be positive and inferior to 1"
			return(content)
		}
		altPref <- matrix( ncol=3 , nrow=0 )
		for( i in 1:nrow( content$alternativesPreferences ) )
		{
			flag <- FALSE
			a <- content$alternativesPreferences[ i , 1 ]
			b <- content$alternativesPreferences[ i , 2 ]
			if(
				any( content$alternativesIDs == a )
				&&
				any( content$alternativesIDs == b )
				&&
				any( rownames(content$performanceTable) == a )
				&&
				any( rownames(content$performanceTable) == b )
				)
			{
				flag <- TRUE
			}	
			if(flag == TRUE)
			{
				altPref <- rbind( altPref , content$alternativesPreferences[i,] )
			}
		}
		
		for( i in 1:nrow(altPref) )
		{
			if( as.numeric(altPref[i,3]) < 0.0000001 ){
				temp <- altPref[i,1]
				altPref[i,1] <- altPref[i,2]
				altPref[i,2] <- temp
				altPref[i,3] <- -as.numeric(altPref[i,3])
			}
		}
		print(altPref)
		
		
		if( !is.null(  content$alternativesIndifferences ) )
		{
		   altIndiff <- matrix( ncol=2 , nrow=0 )
		   if( !is.null(content$alternativesIndifferences) )
		   {
				for( i in 1:nrow( content$alternativesIndifferences ) )
				{
					flag <- FALSE
					a <- content$alternativesIndifferences[ i , 1 ]
					b <- content$alternativesIndifferences[ i , 2 ]
					if(
					   any( content$alternativesIDs == a )
					   &&
					   any( content$alternativesIDs == b )
					   &&
					   any( rownames(content$performanceTable) == a )
					   &&
					   any( rownames(content$performanceTable) == b )
					   )
					{ 
						flag <- TRUE	
					}	
					if(flag == TRUE)
					{
						altIndiff <- rbind( altIndiff , content$alternativesIndifferences[i,] )
					}
				}
		   }
		}
		
		definitiveAlternatives <- c()
		for( i in 1:length( content$alternativesIDs ) )
		{
			flag <- FALSE
			
			a <- content$alternativesIDs[ i ]
			if(
				any( content$alternativesPreferences[,1] == a ) 
				||
				any( content$alternativesIndifferences[,1] == a ) 
				||
				any( content$alternativesPreferences[,2] == a ) 
				||
				any( content$alternativesIndifferences[,2] == a ) 
				)
		    { flag <- TRUE }
			
			if(flag == TRUE)
			{
				definitiveAlternatives <- c( definitiveAlternatives , a )
			}
		}
		content$alternativesIDs <- definitiveAlternatives
		content$alternativesPreferences <- altPref
		if( !is.null(  content$alternativesIndifferences ) )
		{ content$alternativesIndifferences <- altIndiff }
		print("* alternatives and preferences/indifferences in preorder are OK")
	}
	
	print("* alternatives analysis : done")
	return( content  )
}

modifyPerfTable <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	
	alt <- content$alternativesIDs
	crit <- content$criteriaIDs
	PT <- content$performanceTable
	
	na <- c()
	nc <- c()
	for( i in 1:length(alt) )
	{
		na <- c( na , which( rownames(PT) == alt[i] ) )
	}
	for( i in 1:length(crit) )
	{
		nc <- c( nc , which( colnames(PT) == crit[i] ) )
	}
	
	PT <- PT [ na , nc ]
	
	content$performanceTable <- PT
	print("* performanceTable analysis : done")
	return( content )
}

produceOrientedSegment <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	
	if( is.null( content$segments ) ||  is.null( content$preferencesDirections ) )
	{
		orientedSegment <- rep(NA,length=length(content$criteriaIDs))
		names( orientedSegment ) <- content$criteriaIDs 
		return( c( content , list( "orientedSegment" = orientedSegment) ) )
	}
	
	crit <- c()
	orientedSegment <- c()
	
	for( i in 1:length(content$criteriaIDs) )
	{
		if(
		   any( content$segments[1,] == content$criteriaIDs[ i ] )
		   &&
		   any( content$preferencesDirections[1,] == content$criteriaIDs[ i ] )
		   )
		{
			crit <- c( crit , content$criteriaIDs[ i ] )
			g <- content$segments[2, which(content$segments[1,] == content$criteriaIDs[ i ] ) ]
			dir <- content$preferencesDirections[2, which(content$preferencesDirections[1,] == content$criteriaIDs[ i ] ) ]
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
	
	
	print("* OrientedSegment correctly produced")
	return( c( content , list( "orientedSegment" = orientedSegment) ) )
	
}

#########################################################################

modifyCriteria <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	
	crit <- content$criteriaIDs
	segCrit <- content$segments
	dirCrit <- content$preferencesDirections
	PT <- content$performanceTable
	

	newCriteriaIDs <- c()
	for( i in 1:length(crit) )
	{
		if(
		   any( segCrit[1,] == crit[i] )
		   &&
		   any( dirCrit[1,] == crit[i] )
		   )
		{
			newCriteriaIDs <- c( newCriteriaIDs , crit[i] )
		}
	}
    content$initialCriteriaIDs <- newCriteriaIDs
	
	newCriteriaIDs <- c()
	for( i in 1:length(crit) )
	{
		if(
		   ( max( PT[,which(colnames(PT)==crit[i])] )
			!=
			min( PT[,which(colnames(PT)==crit[i])] ) )
		   &&
		   any( segCrit[1,] == crit[i] )
		   &&
		   any( dirCrit[1,] == crit[i] )
		   )
		{
			newCriteriaIDs <- c( newCriteriaIDs , crit[i] )
		}
	}
    content$criteriaIDs <- newCriteriaIDs
	
	if( !is.null(segCrit) )
	{
		newSegCrit <- matrix(nrow=2,ncol=0)
		for( i in 1:ncol(segCrit) )
		{
			if(
			   any( content$criteriaIDs == segCrit[1,i] )
			   &&
			   any( dirCrit[1,] == segCrit[1,i] )
			   )
			{
				newSegCrit <- cbind( newSegCrit , segCrit[,i] )
			}
		}
	content$segments <- newSegCrit
	}
	
	if( !is.null(dirCrit) )
		{
		newDirCrit <- matrix(nrow=2,ncol=0)
		for( i in 1:ncol(dirCrit) )
		{
			if(
			   any( content$criteriaIDs == dirCrit[1,i] )
			   &&
			   any( newSegCrit[1,] == dirCrit[1,i] )
			   )
			{
				newDirCrit <- cbind( newDirCrit , dirCrit[,i] )
			}
		}
		content$preferencesDirections <- newDirCrit
	}
	
	print("* criteria analysis : done")
	return( content )
}

rejectUnusableCriteria <- function( content )
{
	
	return(content)
}

#########################################################################

combineContents <- function( content )
{
	if( content$validation == FALSE )
	{
		return( list( "validation"=FALSE , "LOG"=content$LOG ) )
	}
	print( "                            " )
	print( "---PROCEEDING VERIFICATIONS AND COMBINING CONTENTS---" )
	situation <- whichSituation( content )
	if( situation$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=situation$LOG ) ) }
	
	content <-  c( content , "PREORDER"=situation$PREORDER )
	
	content <- modifyAlternatives( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	content <- modifyPerfTable( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	content$initialPerformanceTable <- content$performanceTable
	
	rejectUnusableCriteria( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	content <- modifyCriteria( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	content <- modifyPerfTable( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	content <- produceOrientedSegment( content )
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	
	print("---CONTENT CHECKED---")
	print( "                            " )
	content$validation <- TRUE
	
	print(content)
	return( "content" = content  )
}

#########################################################################

whichSituation <- function( content )
{
	if( content$validation == FALSE ) { return( list( "validation"=FALSE , "LOG"=content$LOG ) ) }
	
	PREORDER <- NA
	
	if( 
		is.null( content[["alternativesIDs"]] )
		||
		is.null( content[["criteriaIDs"]] )
		||
		is.null( content[["performanceTable"]] )
		) 
	{
		LOG <- "alternatives, criteria and performanceTable must be present!" 
		return( list( "validation"=FALSE , "LOG"=LOG ) )
	}
	
	if(	is.null( content[["alternativesRanking"]] ) ) 
	{	
		if( 
		!is.null( content[["alternativesPreferences"]] )
		||
		!is.null( content[["alternativesIndifferences"]] )
		)
		{
			PREORDER <- TRUE
		}
	}
	
	if(
		!is.null( content[["alternativesRanking"]] )
		&&
		is.null( content[["alternativesPreferences"]] )
		&&
		is.null( content[["alternativesIndifferences"]] )
		)
	{
		PREORDER <- FALSE
	}
	
	if(
		!is.null( content[["alternativesRanking"]] )
		&&
		  (
		   !is.null( content[["alternativesPreferences"]] )
		   ||
		   !is.null( content[["alternativesIndifferences"]] )
		  )
		)
	{
		LOG <- "Contradiction : please provide a preorder OR a ranking, not both" 
		return( list( "validation"=FALSE , "LOG"=LOG ) )
	}
	
	if( is.na(PREORDER) )
	{
		LOG <- "Program error in -whichSituation-"
		return( list( "validation"=FALSE , "LOG"=LOG ) )
	}
	
	print("* situation analysis : done")
	return( list( "validation"=TRUE, "PREORDER"=PREORDER ) )	
}

#########################################################################

criteriaTrendAnalysis <- function(realContent)
{
	data <- realContent$performanceTable
	if( realContent$validation == FALSE || is.null(data) )
	{
		return( list("validation"=FALSE,"LOG"=realContent$LOG) )
	}
	if( !is.matrix(data) )
	{
		return( list("validation"=FALSE,"LOG"="check performancetable and alternatives or criteria : no valid initial conditions") )
	}
	print("--- PROCEEDING TREND ANALYSIS---")
	nA <- nrow(data)
	nbCriteria <- ncol(data)
	growthResults <- t(as.matrix(rep(0,nbCriteria)))
	growthResults <- rbind(growthResults,rep(0,nbCriteria))
	x <- 1:nA
	id <- dimnames(data)[[2]]
	for (i in 1:nbCriteria)
	{
		growthResults[1,i] <- id[i]
		growthResults[2,i] <- criterionAnalysis(data[,id[i]])
	}
	out <- t(as.matrix(as.numeric(growthResults[2,])))
	dimnames(out)[[2]] <- growthResults[1,]
	print("DONE")
	
	return( c(realContent, list( "trend"=out) ) )
}

criterionAnalysis <- function(T)
{
	growth <- 0
	data <- as.vector(T)
	nA <- length(T)
	weigths <- rep(0,nA)
	for (k in 1:nA)
	{
		weights <- createWeights(nA,k)
		for (l in (-nA):nA)
		{
			if (((k+l) >0) && ((k+l)<=nA))
			{
				growth <- growth + sign(l)*sign(T[k]-T[k+l])* weights[k+l]
				if (T[k] == T[k+l])
				growth <- growth + sign(l)* weights[k+l]
			}
		}
	}
	growth <- growth / nA 
	return(growth)
}

createWeights <- function(nA,k)
{
	weights <- rep(0,nA)
	for (l in 1:nA)
	{
		sol <- 1/l
		if (k-l >0)
		weights[k-l] <- sol
		if (k+l <= nA)
		weights[k+l] <- sol		
	}
	weights <- weights / sum(weights)
	return(weights)
}


#########################################################################

computeSegments <- function( realContent )
{
	print( "                            " )
	print("---COMPUTING DEFINITIVE SEGMENTS---")
	weights <- realContent$trend
	segs <- rbind( names(realContent$orientedSegment) , realContent$orientedSegment )
	if( realContent$validation == FALSE || is.null(weights) )
	{
		return( list("validation"=FALSE,"LOG"=realContent$LOG) )
	}
	segbis <- segs[,which(!is.na(segs[1,]))]
	indices <- dimnames(weights)[[2]]

	segs <- rep(NA,length(indices))
	for (i in 1:length(indices)){
		if( any(segbis[1,]==indices[i]) )
		{
			segs[i] <- segbis[2,which(segbis[1,]==indices[i])]
			names( segs )[ i ] <- indices[i]
		}
	}
	if(is.null(segs))
		segs <- rep(NA,length(weights))
	if(all(!is.na(segs)))
	{
		print("DONE")
		return( c(realContent , list( "SEGS" = t( as.matrix(segs )))))
	}
	num <- which(is.na(segs))
	ncrit <- length(num)
	bsegs <- ordonnateSegs(weights[num],ncrit)
	SEGS <- matrix(0,ncol=length(segs),nrow=nrow(bsegs))
	for (l in 1:length(segs))
	{
		if(any(num == l) == TRUE)
		{
			SEGS[,l] <- as.numeric(bsegs[,which(num == l)])
		}
		else
		{
			SEGS[,l] <- as.numeric(as.matrix(rep(segs[l],nrow(SEGS))))
		}
	}
	colnames(SEGS) <- indices
	realContent <- c(realContent , list( "SEGS" = SEGS ) )

	print( "DONE")
	return( realContent )
}

integerAsBinary <- function( int , nbit )
{
	answer <- rep(0,nbit)
	if (int == 0)
		return(answer)
	if (int %/% 2 * 2 != int)
	{
		answer[1] = 1
	}
	while (int > 1)
	{
		for (i in (nbit-1):1)
		{
			if (int %/% 2^i > 0)
			{
				answer[i+1] <- 1
				int <- int - 2^i
				break
			}
		}	
	}	
	return(answer)
}

ordonnateSegs <- function( weights , ncrit )
{
	if (ncrit == 1)
	{
		if (weights > 0)
			return(matrix(c(1,-1),nrow=2))
		if (weights <= 0)
			return(matrix(c(-1,1),nrow=2))
	}
	segs = matrix(0,ncol=ncrit,nrow=2^ncrit)
	index <-order(abs(weights),decreasing=TRUE)
	for (i in 1:(2^ncrit))
	{
		segs[i,] <- integerAsBinary(i-1,ncrit)
	}
	for (i in 1:ncrit)
	{
		if (i==1)
		{
			underclasses <- list(seq(from=1,to=2^ncrit,by=1))
			segs <- segs[order(segs[,index[i]],decreasing = ((sign(weights[index[i]]) >0)==TRUE)),]
		}
		else
		{
			underclasses <- findUnderclasses(i,segs,index, weights)
		}
		out <- NULL
		for (j in underclasses)
		{
			seg <- segs[j,]
			seg <- seg[order(seg[,index[i]],decreasing = ((sign(weights[index[i]]) >0)==TRUE) ),]
			if(is.null(out))
			{
				out <- seg
			}
			else
			{
				out <- rbind(out,seg)
			}
		}
		segs <- out	
	}
	segs[segs==0]=-1
	return(segs)
}

findUnderclasses <- function(i,segs,index, weights)
{
	if (i==2)
	{
		a1 <- which(segs[,index[i-1]]==1)
		a2 <- which(segs[,index[i-1]]==0)
		if (sign(weights[index[i-1]]) == 1)
			return(list(a1,a2))
		if (sign(weights[index[i-1]]) == -1)
			return(list(a2,a1))
	}
	else
	{
		classes <- findUnderclasses(i-1,segs,index, weights)
		out <- NULL
		for (j in classes)
		{
			a1 <- j[which(segs[j,index[i-1]]==1)]
			a2 <- j[which(segs[j,index[i-1]]==0)]
			if (sign(weights[index[i-1]]) == 1)
				ext <- list(a1,a2)
			if (sign(weights[index[i-1]]) == -1)
				ext <- list(a2,a1)
			if (is.null(out))
			{
				out <- ext
			}
			else
			{
				out <- c(out,ext)	
			}
		}
		return(out)
	}
}

#########################################################################

computeAcuta <- function( dataContent )
{
	print( "                            " )
	print("---COMPUTING ACUTA METHOD---")
	if( dataContent$validation == FALSE )
	{
		return( list("validation"=FALSE,"LOG"=realContent$LOG) )
	}
	PREORDER <- dataContent$PREORDER
	trend <- dataContent$trend
	SEGS <- dataContent$SEGS
	
	delta <- dataContent$delta
	if ( is.null(delta) )
		delta <- 1e-3
	level <- dataContent$level
	
	performanceTable <- dataContent$performanceTable
	alternativesIDs <- dataContent$alternativesIDs
	criteriaIDs <- dataContent$criteriaIDs
	
	if( PREORDER == TRUE )
	{
		print("* preorder is true")
		alternativesPreferences <- dataContent$alternativesPreferences
		alternativesIndifferences <- dataContent$alternativesIndifferences
		prefset <- NULL
	}
	else
	{
		print("* ranking is true")
		alternativesPreferences <- NULL
		alternativesIndifferences <- NULL
		prefset <- dataContent$alternativesRanking	
	}
	
	solutions <- NULL
	
	if( is.null(level) || level==0 )
	{
			SEGS <- t(as.matrix(SEGS[1,]))
	}
	else
	{
		if( nrow(SEGS)>=2 )
		{
			if( 2^level > nrow(SEGS) )
				level <- 1
			SEGS <- SEGS[1:(2^level),]
		}
	}

	solutions <- list()

	for (i in 1:nrow(SEGS))
	{
		err <- try( {
				   solACUTA <- ACUTA(alternativesIDs,performanceTable,delta,as.numeric(SEGS[i,]),prefset, alternativesPreferences, alternativesIndifferences)
				   solACUTA$criteriaIDs <- criteriaIDs
				   rownames(solACUTA$gmatrix) <- criteriaIDs
				   rownames(solACUTA$umatrix) <- criteriaIDs
				   solACUTA <- addUnusedValidCriteria( solACUTA , dataContent )
				   solutions <- c( solutions , list(solACUTA) )
				   })
		if (inherits(err, 'try-error'))
		{ return( list( "validation" = FALSE , "LOG" = "ACUTA FATAL ERROR" ) ) }
		
		print("-----------------------------")
		print("G-MATRIX")
		print(solACUTA$gmatrix)
		print("-----------------------------")
		print("U-MATRIX")
		print(solACUTA$umatrix)
		print("-----------------------------")
		
		if(solACUTA$validation == FALSE)
		{return( list( "validation" = FALSE , "LOG" = solACUTA$LOG ) ) }
		
	}
	print("--- ACUTA METHOD FULL SUCCESS---")
	if( is.null(solACUTA$LOG) )
	{
		LOG <-  "--- ACUTA METHOD FULL SUCCESS---"
	}else
	{
		LOG <- solACUTA$LOG
	}
	
	return( list("solutions"=solutions,"validation"=TRUE,"LOG"=LOG) )
}

#########################################################################

addUnusedValidCriteria <- function( solACUTA , content )
{
	crit <- content$initialCriteriaIDs
	PT <- content$initialPerformanceTable
	
	
	for( i in 1:length(crit) )
	{
		if( !any(rownames(solACUTA$gmatrix)==crit[i]) )
		{
			n <- matrix( NA , nrow=1, ncol =ncol(solACUTA$gmatrix) )
			n[1] <- max( PT[ , which( colnames(PT)==crit[i] ) ] )
			rownames(n)[1] <- crit[i]
			solACUTA$gmatrix <- rbind( solACUTA$gmatrix , n )
			rm(n)
			
			m <- matrix( NA , nrow=1, ncol =ncol(solACUTA$umatrix) )
			m[1] <- 0
			rownames(m)[1] <- crit[i]
			solACUTA$umatrix <- rbind( solACUTA$umatrix , m )
			rm(m)
		} 
	}
	return( solACUTA )
}

adaptPrecision <- function( delta )
{

	precision <- -4
	dr <- 0
	while (round(delta,digits=-precision)==0 || (abs(round(delta,digits=-precision)-delta)/delta)>0.0005){
		precision <- precision - 1
	}
	return(list("precision"=precision-1))
}

#########################################################################

createUTAPairConstraints <- function(alternativesPreferences,alternativesIndifferences=NULL,segs,data, delta)
{


	I <- nrow(alternativesPreferences)
	N <- length(segs)
	S <- sum(abs(segs))
	nA <- countNbAlternatives( alternativesPreferences , alternativesIndifferences  )

	V <- 2*nA + S + N
	A <- matrix(0,nrow=I+S,ncol=V)
	b <- matrix(0,nrow=I+S,ncol=1)

	for ( i in 1:I )
	{
		aindex <- as.numeric(alternativesPreferences[i,1])
		bindex <- as.numeric(alternativesPreferences[i,2])
		
		
		
		for (j in (1:N))
		{
			
			if( as.numeric(segs[j]) == 0 )
			{
				
			}
			else
			{
				gmin <- min(data[,j])
				gmax <- max(data[,j])
			
				g <- seq(from=gmin,by=((gmax-gmin)/abs(segs[j])), to=gmax)
			
				ga <- data[aindex,j]
				gb <- data[bindex,j]
				
				gainfind <- max(which(g<=ga))
				gasupind <- min(which(g>=ga))
				gbinfind <- max(which(g<=gb))
				gbsupind <- min(which(g>=gb))
			
				gainf <- g[gainfind]
				gasup <- g[gasupind]
				gbinf <- g[gbinfind]
				gbsup <- g[gbsupind]
			
				if (j == 1){ x <- 0 }else{
					x <- sum(abs(segs[1:(j-1)])) +j-1
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
			
		}

	
		x <- aindex-1+S+N
		A[i,x] <- -1
		A[i,x+nA] <- 1
		
		x <- aindex+S+N
		A[i,x] <- 1
		A[i,x+nA] <- -1
		
		if( is.na(alternativesPreferences[i,3]) )
		{
			b[i] = as.numeric( delta )
		}
		else
		{
			b[i] = as.numeric( alternativesPreferences[i,3] )
		}
	}
	
	for (i in 1:N){
		if (i==1){
			x <- 0
			y <- I
		}else{
			x <- sum(abs(segs[1:(i-1)]))+i-1
			y <- sum(abs(segs[1:(i-1)]))+I
		}

		if( as.numeric(segs[i]) == 0 )
		{
			
		}
		else
		{
			print( " OK OK OK OK " ) 
			s <- sign(as.numeric(segs[i]))
			if( s != 0 ){
				for (j in (1:abs(segs[i]))){
					A[y+j,x+j] <- -s
					A[y+j,x+j+1] <- s
				}
			}
		}
	}
	
	
	if( is.null(alternativesIndifferences) )
	{
		E <- 0
		Aeq = matrix(0,nrow=E+N+1,ncol=V)
		beq = matrix(0,nrow=E+N+1,ncol=1)
	}
	
	if( !is.null(alternativesIndifferences) )
	{
	   E <- nrow(alternativesIndifferences)
	   Aeq = matrix(0,nrow=E+N+1,ncol=V)
	   beq = matrix(0,nrow=E+N+1,ncol=1)
	
	
	   for ( i in (1:E) )
	   {
			aindex <- as.numeric(alternativesIndifferences[i,1])
			bindex <- as.numeric(alternativesIndifferences[i,2])
		   y <- i
		   
			for ( l in (1:N) )
			{
				
				gmin <- min(data[,l])
				gmax <- max(data[,l])
				g <- seq(from=gmin,by=(gmax-gmin)/abs(segs[l]),to=gmax)
			
				ga <- data[aindex,l]
				gb <- data[bindex,l]
						
				gainfind <- max(which(g<=ga))
				gasupind <- min(which(g>=ga))
				gbinfind <- max(which(g<=gb))
				gbsupind <- min(which(g>=gb))
					
				gainf <- g[gainfind]
				gasup <- g[gasupind]
				gbinf <- g[gbinfind]
				gbsup <- g[gbsupind]
					
				
				if (l==1){
					x <- 0
				}else{
					x <- sum(abs(segs[1:(l-1)]))+l-1
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
				x <- S+N+aindex-1
			}else{
				x <- S+N+aindex-1
			}
					
			Aeq[y,x] <- -1
		   Aeq[y,x+nA] <- 1
					
			x <- S+N+bindex-1
			Aeq[y,x] <- 1
		   Aeq[y,x+nA] <- -1
	   }	
	}		
	
	
	for (i in 1:N){
		y=E+i
		if(i==1)
		{
			infe <- 1
		}
		else
		{
			infe <- sum(abs(segs[1:(i-1)]))+i
		}
		supe <- sum(abs(segs[1:i]))+i
		if (sign(segs[i])>0)
		{
			Aeq[y,infe]=1
			Aeq[E+N+1,supe]=1
		}
		else
		{
			Aeq[y,supe]=1
			Aeq[E+N+1,infe]=1
		}
	}
	beq[E+N+1]=1
	
	c <- matrix(1,nrow=1,ncol=V)
	c[1,1:(N+S)]=0
	
	b <- as.vector(b)
	beq <- as.vector(beq)
	c <- as.vector(c)
	print("* constraints correctly computed")
	return(list(A,b,Aeq,beq,c,"validation"=TRUE))
}

countNbAlternatives <- function( alternativesPreferences , alternativesIndifferences )
{
	nams <- c( alternativesPreferences[,1] , alternativesPreferences[,2] , alternativesIndifferences[,1] , alternativesIndifferences[,2] )
	nb <- nams[1]
	for ( i in 2:length(nams) )
	{
		if( all( nb != nams[i] ) )
		{
			nb <- c( nb , nams[i] )
		}
	}
	return( length(nb) )
	
}

createUTAconstraints <- function(delta,segs,data,prefset)
{
	index <- matrix(c(prefset[order(as.numeric(prefset[,2])),1],prefset[order(as.numeric(prefset[,2])),2]),ncol=2)
	nA <- nrow(index)
	N <- length(segs)
	S <- sum(abs(segs))
	I <- as.numeric(max(as.numeric(index[,2])))
	nI = rep(0,I)
	for (i in (1:I)){
		nI[i] <- sum((index[,2]==i))
	}
	nIindex <- cumsum(c(1,nI[1:(I-1)]))
	nE <- nI*(nI-1)/2
	E <- sum(nE)
	V <- 2*nA + S + N
	A <- matrix(0,nrow=I-1+S,ncol=V)
	b <- matrix(0,nrow=I-1+S,ncol=1)

	for (i in (1:(I-1))){
		aindex <- as.numeric(index[nIindex[i],1])
		bindex <- as.numeric(index[nIindex[i+1],1])
		for (j in (1:N)){
			gmin <- min(data[,j])
			gmax <- max(data[,j])
			
			if( gmin == gmax )
			{
				
			}
			else
			{
			
				g <- seq(from=gmin,by=((gmax-gmin)/abs(segs[j])), to=gmax)
			
				ga <- data[aindex,j]
				gb <- data[bindex,j]
			
				gainfind <- max(which(g<=ga))
				gasupind <- min(which(g>=ga))
				gbinfind <- max(which(g<=gb))
				gbsupind <- min(which(g>=gb))
			
				gainf <- g[gainfind]
				gasup <- g[gasupind]
				gbinf <- g[gbinfind]
				gbsup <- g[gbsupind]
			
				if (j == 1){
					x <- 0
				}else{
					x <- sum(abs(segs[1:(j-1)])) +j-1
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
		}
		
		if (i==1){
			x=S+N+1
		}else{
			x <- sum(nI[1:(i-1)])+S+N+1
		}
		A[i,x] <- -1
		A[i,x+nA] <- 1
		
		x <- sum(nI[1:i])+S+N+1
		A[i,x] <- 1
		A[i,x+nA] <- -1
		b[i] = delta 
	}
	
	for (i in 1:N){
		if (i==1){
			x <- 0
			y <- I-1
		}else{
			x <- sum(abs(segs[1:(i-1)]))+i-1
			y <- sum(abs(segs[1:(i-1)]))+I-1
		}
		if( as.numeric( segs[i] ) != 0 )
		{
			s <- sign(segs[i])
			for (j in (1:abs(segs[i]))){
				A[y+j,x+j] <- -s
				A[y+j,x+j+1] <- s
			}
		}
	}
	
	Aeq = matrix(0,nrow=E+N+1,ncol=V)
	beq = matrix(0,nrow=E+N+1,ncol=1)
	
	for (i in (1:I)){
		if (nI[i]>1){
			for (j in (1:(nI[i]-1))){
				for (k in ((j+1):nI[i])){
					aindex <- as.numeric(index[nIindex[i]+j-1,1])
					bindex <- as.numeric(index[nIindex[i]+k-1,1])
					
					if (i==1){
						y <- j+k-2
					}else{
						y <- sum(nE[1:(i-1)])+j+k-2
					}
					
					for (l in (1:N)){
						
						
						if( segs[l] == 0 )
						{
							
						}
						else
						{
							
						
						gmin <- min(data[,l])
						gmax <- max(data[,l])
						g <- seq(from=gmin,by=(gmax-gmin)/abs(segs[l]),to=gmax)
						
						ga <- data[aindex,l]
						gb <- data[bindex,l]
						
						gainfind <- max(which(g<=ga))
						gasupind <- min(which(g>=ga))
						gbinfind <- max(which(g<=gb))
						gbsupind <- min(which(g>=gb))
						
						gainf <- g[gainfind]
						gasup <- g[gasupind]
						gbinf <- g[gbinfind]
						gbsup <- g[gbsupind]
						
						if (l==1){
							x <- 0
						}else{
							x <- sum(abs(segs[1:(l-1)]))+l-1
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
						
					}
					
					if (i==1){
						x <- S+N+j
					}else{
						x <- S+N+sum(nI[1:(i-1)])+j
					}
					
					Aeq[y,x] <- -1
					Aeq[y,x+nA] <- 1
					
					x <- x+k-j
					Aeq[y,x] <- 1
					Aeq[y,x+nA] <- -1
				}
			}
		}
	}
	
	
	for (i in 1:N){
		y=E+i
		if(i==1){
			infe <- 1
		}else{
			infe <- sum(abs(segs[1:(i-1)]))+i
		}
		supe <- sum(abs(segs[1:i]))+i
		if( segs[i] != 0 ){
			if (sign(segs[i])>0){
				Aeq[y,infe]=1
				Aeq[E+N+1,supe]=1
			}else{
				Aeq[y,supe]=1
				Aeq[E+N+1,infe]=1
			}
		}
	}
	beq[E+N+1]=1
	
	c <- matrix(1,nrow=1,ncol=V)
	c[1,1:(N+S)]=0
	
	b <- as.vector(b)
	beq <- as.vector(beq)
	c <- as.vector(c)
	print("* constraints correctly computed")
	return(list(A,b,Aeq,beq,c,"validation"=TRUE))
}

#########################################################################

ACUTA <- function( alternativesIDs , performanceTable , delta , segs , prefset , alternativesPreferences, alternativesIndifferences ) 
{

	precision <- adaptPrecision(delta)$precision
	nbAlternatives <- length(alternativesIDs)
	criteriaIDs <- names(prefset)
	nbCriteria <- length(criteriaIDs)
	data <- performanceTable
	N <- length(segs)
	S <- sum(abs(segs))
	
	if ( !is.null(alternativesPreferences) )
	{
		for( i in 1:nrow(alternativesPreferences) )
		{
			alternativesPreferences[i,1] <- which(rownames(performanceTable)==alternativesPreferences[i,1])[1]
			alternativesPreferences[i,2] <- which(rownames(performanceTable)==alternativesPreferences[i,2])[1]
		}
		if( !is.null( alternativesIndifferences ) )
		{
		   for( i in 1:nrow(alternativesIndifferences) )
		   {
				alternativesIndifferences[i,1] <- which(rownames(performanceTable)==alternativesIndifferences[i,1])
				alternativesIndifferences[i,2] <- which(rownames(performanceTable)==alternativesIndifferences[i,2])
		   }
		}
		I <-  nrow(alternativesPreferences)
#err <- try({
				   UTA.constraints <- createUTAPairConstraints(alternativesPreferences,alternativesIndifferences,segs, performanceTable,delta)
#		   })
#if (inherits(err, 'try-error'))
#{ return( list( "validation" = FALSE , "LOG" = "ACUTA CONSTRAINTS FATAL ERROR" ) ) }
		
		if(UTA.constraints$validation == FALSE)
		{ return( list( "validation" = FALSE , "LOG" = UTA.constraints$LOG ) ) }
		
		A <- UTA.constraints[[1]]
		b <- UTA.constraints[[2]]
		Aeq <- UTA.constraints[[3]]
		beq <- UTA.constraints[[4]]
		c <- UTA.constraints[[5]]
	}
	else
	{
		for( i in 1:nrow(prefset) )
		{
			prefset[i,1] <- which(rownames(performanceTable)==prefset[i,1])
		}
		index <- matrix(c(prefset[order(as.numeric(prefset[,2])),1],prefset[order(as.numeric(prefset[,2])),2]),ncol=2)
		nA <- nrow(index)
		I <- as.numeric(max(as.numeric(index[,2])))
		nI = rep(0,I)
		for (i in (1:I)){
			nI[i] <- sum((index[,2]==i))
		}
		nIindex <- cumsum(c(1,nI[1:(I-1)]))
		nE <- nI*(nI-1)/2
		E <- sum(nE)
		V <- 2*nA + S + N	
		

		
		err <- try({
				   UTA.constraints <- createUTAconstraints(delta,segs,data,prefset)
				   })
		if (inherits(err, 'try-error'))
		{ return( list( "validation" = FALSE , "LOG" = "ACUTA CONSTRAINTS FATAL ERROR" ) ) }
		
		if(UTA.constraints$validation == FALSE)
		{ return( list( "validation" = FALSE , "LOG" = UTA.constraints$LOG ) ) }
		
		A <- UTA.constraints[[1]]
		b <- UTA.constraints[[2]]
		Aeq <- UTA.constraints[[3]]
		beq <- UTA.constraints[[4]]
		c <- UTA.constraints[[5]]
	}
	
#######	
#print(UTA.constraints)
#######
	
	err <- try({
			   UTA.solution <- solveUTASTAR(c,A,Aeq,b,beq,N)
			   })
	if (inherits(err, 'try-error'))
	{ return( list( "validation" = FALSE , "LOG" = "UTA RESOLUTION FATAL ERROR" ) ) }
	
	if(UTA.solution$validation == FALSE)
	{ return( list( "validation" = FALSE , "LOG" = UTA.solution$LOG ) ) }
	
	UTA.x <- UTA.solution$x

	u <- UTA.x[1:(S+N)]
	
	err <- try({
			   UTA.sol <- buildmatrices(data,u,segs)
			   })
	if (inherits(err, 'try-error'))
	{ return( list( "validation" = FALSE , "LOG" = "Error in building UTA end matrices" ) ) }
	
	print("* UTA method converged")
	UTA.gmatrix <- UTA.sol[[1]]
	UTA.umatrix <- UTA.sol[[2]]
	UTA.sol[[2]] <- round(UTA.sol[[2]],digits = 14)     
	UTA.x <- round(UTA.x,digits = 14)
	
#	print(UTA.sol)
	
	flag = 1 
	if (all(is.nan(UTA.x))){
		flag = 0
		return(list("validation"=FALSE,"LOG"="Result - No feasible solution"))
	}
	
	
	err <- try({
			   AC.solution <- analyticCenter(UTA.x,-A,b,Aeq)
			   })
	if (inherits(err, 'try-error'))
	{ return( list( "validation" = FALSE , "LOG" = "Analytic center FATAL ERROR" ) ) }
	
	if(AC.solution$validation == FALSE)
	{ return( list( "validation" = FALSE , "LOG" = AC.solution$LOG ) ) }
	
	LOG <- AC.solution$LOG
	
	
	AC.x <- AC.solution$x

	status <- AC.solution$status
	if (flag>0){
		u <- AC.x[1:(S+N)]
		
		print("AC method converged")
		print("UTA.X")
		print(as.vector(UTA.x))
		print("AC.x")
		print(as.vector(AC.x))
		
	}else{
			u <- UTA.x[1:(S+N)]
			print("flag < 0")
			LOG <- "AC method failed : taking UTA solution"
			print("* AC method failed : taking UTA solution")		}
	
	err <- try({
			   AC.sol <- buildmatrices(data,u,segs)
			   })
	if (inherits(err, 'try-error'))
	{ return( list( "validation" = FALSE , "LOG" = "Error in building AC end matrices" ) ) }

	
	AC.gmatrix <- AC.sol[[1]]
	AC.umatrix <- AC.sol[[2]]
	err <- try({
			   datautility <- computeutilities(data,AC.gmatrix,AC.umatrix)
			   })
	if (inherits(err, 'try-error'))
	{ return( list( "validation" = FALSE , "LOG" = "Error in computing utilities" ) ) }
	
	
	
	utotal = rep(0,nrow(datautility))
	for (i in 1:nrow(datautility)){
		utotal[i] <- sum(datautility[i,])
	}	
	names(utotal) <- rownames( data ) 
	print("-----------------------------")
	print("UTILITIES : ")
	print(utotal)
	print("-----------------------------")
	
	
	weights <- rep(0,length(segs))
	for (i in 1:N){
		if (sign(segs[i]) < 0){
			weights[i] <- AC.umatrix[i,1]
		}else{
			weights[i] <- AC.umatrix[i,abs(segs[i])+1]
		}
	}
#names(utotal)=alternativesIDs[index[,1]]
	names(weights)=criteriaIDs
	
	return(list("validation"=TRUE,"LOG"=LOG,"segs"=segs,"gmatrix"=AC.gmatrix,"umatrix"=AC.umatrix,"utilities"=utotal,"weights"=weights,"status"=status,"criteriaIDs"= criteriaIDs,"LOG"=LOG,"delta"=delta))
}

#########################################################################

UTA.resolution <- function(A,b,Aeq,beq,c,nC,precision)
{
	
#uy=c(rep(">=",length(b)),rep("==",length(beq)))
#b[which(b>0)] <- b[which(b>0)] + rep(10^(precision-6),length(b[which(b>0)]))
#SOL <- lp(direction = "min",objective.in=c,const.mat=rbind(A,Aeq),const.dir=uy,const.rhs=c(b,beq),presolve=1)
#x <- SOL$solution
#print(x)

#####
	print("* using quadratic programming resolution")
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
	{ return( list( "validation" = FALSE , "LOG" = "ERROR : FAILED TO CONVERGE" ) ) }
	
	x <- SOL[["alpha"]]
	x <- as.vector(x)
	x <- x[1:ncol(A)]
#####
	
	
#if (all(is.nan(x)))

	return(list("x"=x,"validation"=TRUE))
}

#########################################################################

UTA.analytic.center <- function(x,flag,A,b,Aeq,beq,precision,delta,data,segs,I)
{
	A <- -A
	b <- -b
	N <- length(segs)
	S <- sum(abs(segs))
	sigmaoutput<-0
	status <- FALSE
	if ((max(x[(S+N+1):length(x)]) > 10^(precision-1))){  
	print("One or more constraints are not satisfied")
	LOG <- "One or more constraints are not satisfied ; UTA partial success"

	}else{
		print("* PostUTA - Constraints are satisfied")
		print("* Analytic center method launched")
		out <- analyticccenter(A[,1:(S+N)],b,Aeq[,1:(S+N)],x[1:(S+N)],delta,precision)
		print(out$x)
		xpostopt <- out$x
		flag <- out$flag
		if (flag > 0){
			x <- xpostopt
			status <- TRUE
			LOG <- "Analytic center method succeed"
			print("* Analytic center method succeed")
		}else{
			LOG <- "Analytic center problem : UTASTAR solution is provided"
			}
	}
	x <- round(x,digits = 6)
	return(list("x"=x,"flag"=flag,"status"=status,"validation"=TRUE,"LOG"=LOG))
}

analyticccenter <- function(A,b,Aeq=NULL,x=NULL,delta,precision)
{
	print("* COMPUTATION OF ANALYTIC CENTER---")
	fac <- 0.95
	it <- 0
	oldnorm <- 0
	stepredcount <- 0
	flag <- 1
	
	library(MASS)
	if (is.null(x))
	x = matrix(1,nrow=ncol(A),ncol=1) 
	if (is.null(Aeq)){
		Z= diag(rep(1,nxol(A)))
	}else{
		Z <- nullspace(Aeq)
	}
	
	out <- analyticcenterinitialization(A,b,x,delta,precision)
	print("* Analytic center properly initialized")
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
	return (list("x"=x,"flag"=flag))	
}

analyticcenterinitialization <- function(A,b,x,delta,precision)
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

buildmatrices <- function(data,u,segs)
{
	N = length(segs)
	gm = matrix(nrow=N,ncol=max(abs(segs))+1)
	um	= matrix(nrow=N,ncol=max(abs(segs))+1)
	for (i in 1:N){
		gmin <- min(data[,i])
		gmax <- max(data[,i])  
		gm[i,1:(abs(segs[i])+1)] <- seq(from=gmin,to=gmax,by=(gmax-gmin)/abs(segs[i]))
		if (i==1){
			startindex <- 1
		}else{
			startindex <- sum(abs(segs[1:(i-1)])+1)+1
		}
		um[i,1:(abs(segs[i])+1)] <- u[startindex:(startindex+abs(segs[i]))] ######tranpose????
	}
	return (list(gm,um))
}

#########################################################################

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

#########################################################################

computeutilities <- function(data,gmatrix,umatrix)
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
				if( max(gmatrix[j,!is.na(gmatrix[j,])]) - min(gmatrix[j,!is.na(gmatrix[j,])]) < 0.001 )
				{
					vec[j] <- mean(	gmatrix[j,!is.na(gmatrix[j,])] )
					b[j] <- TRUE
				}
				else
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
	

	print("----------------------------------------------")
	print("PERFORMANCE TABLE")
	print(data)
	print("----------------------------------------------")
	print("FINAL UTILITIES")
	print(datautility)
	print("----------------------------------------------")
	
	return(datautility)
}

#########################################################################

exportSolutions  <- function( solutions , outputsLocation ) 
{
	if( is.null(solutions) )
	{
		z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
		filename <- "valueFunctions.xml"
		whereF =  paste(outputsLocation,filename,sep="/")
		saveXML( z , file=whereF )
		filename <- "otherValueFunctions.xml"
		whereF =  paste(outputsLocation,filename,sep="/")
		#saveXML( z , file=whereF )
		return(NULL)
	}
	N <- length(solutions$solutions)
	if( N == 0 )
	{
		z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
		filename <- "valueFunctions.xml"
		whereF =  paste(outputsLocation,filename,sep="/")
		saveXML( z , file=whereF )
		filename <- "otherValueFunctions.xml"
		whereF =  paste(outputsLocation,filename,sep="/")
		#saveXML( z , file=whereF )
		return(NULL)
	}
	print("                                ")
	print("-- EXPORTATION OF SOLUTIONS---")
	for (i in 1:N)
	{
		if( solutions$solutions[[i]]$validation == FALSE )
		{ break ; }
		sol <- solutions$solutions[[i]]
		segs <- sol$segs
		#filename <- construct.filenames(segs,i)
		#sol <- c(sol,list("filename"=filename))
		if (i==1)
		{
			listOfOthersACUTA <- c()
			z <- saveDataUnderXML(sol)
			filename <- "valueFunctions.xml"
		}
		if (N > 1)
		{
			if (i > 1 && i <= N){
				listOfOthersACUTA <- c(listOfOthersACUTA,list(sol))
			}
		} 
		else
		{
			listOfOthersACUTA <- NULL
		}
		whereF =  paste(outputsLocation,filename,sep="/")
	    if(i == 1 ) #|| i == N)
		{
			f <- tempfile()
			#saveXML(z, f, encoding = "UTF-8",indent=TRUE, file=whereF,prefix = '<?xml version="1.0"?>\n')
			saveXML( z , file=whereF )
			z <- NULL
		}
		if(i == N ) #&& N>1  && !is.null(listOfOthersACUTA))
		{
			f <- tempfile()
			#print(listOfOthersACUTA)
			z <- save.otherPossibilities.underXML(listOfOthersACUTA)
			filename <- "otherValueFunctions.xml"
			whereF =  paste(outputsLocation,filename,sep="/")
			#saveXML(z, f, encoding = "UTF-8",indent=TRUE, file=whereF,prefix = '<?xml version="1.0"?>\n')
			saveXML( z , file=whereF )
		}
	}
	print("---DONE---")
}

#########################################################################

saveDataUnderXML <- function( sol )
{

	z = newXMLNode("xmcda:XMCDA", namespace = c(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))

	criteriaNames <- sol$criteriaNames
	criteriaIDs <- sol$criteriaIDs
	criteria = newXMLNode("criteria",attrs=c("mcdaConcept"="criteria","name"=sol$filename))
		
		gmatrix  <- sol$gmatrix
		umatrix  <- sol$umatrix
		
		for (i in 1:nrow(gmatrix)){
			
			criterion  = newXMLNode("criterion",attrs=c("id"=criteriaIDs[i]), parent=criteria)
			criterionFunction = newXMLNode("criterionFunction",parent=criterion)
			points = newXMLNode("points", parent=criterionFunction)
			
			for (j in 1:length(gmatrix[i,])){
				if(!is.na(gmatrix[i,j])){
					point = newXMLNode("point", parent=points)
					abscissa = newXMLNode("abscissa", parent=point)
					real = newXMLNode("real",gmatrix[i,j], parent=abscissa)
					ordinate = newXMLNode("ordinate", parent=point)
					real = newXMLNode("real",umatrix[i,j], parent=ordinate)
				}
			}
		}
	
	addChildren(z, criteria)
	
	return(z)
}

#########################################################################

save.otherPossibilities.underXML <- function(ACUTASOLS)
{
	
	z = newXMLNode("xmcda:XMCDA", namespace = c(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
	
	if( is.null(ACUTASOLS) )
	{
		return(z)
	}

	for (i in 1:length(ACUTASOLS)){
		
		sol <- ACUTASOLS[[i]]
		criteriaNames <- sol$criteriaNames
		criteriaIDs <- sol$criteriaIDs
		
		criteria = newXMLNode("criteria",attrs=c("mcdaConcept"="criteria","name"=sol$filename))
		
		gmatrix  <- sol$gmatrix
		umatrix  <- sol$umatrix
		
		for (i in 1:nrow(gmatrix)){
			
			criterion  = newXMLNode("criterion",attrs=c("id"=criteriaIDs[i]), parent=criteria)
			criterionFunction = newXMLNode("criterionFunction",parent=criterion)
			points = newXMLNode("points", parent=criterionFunction)
			
			for (j in 1:length(gmatrix[i,])){
				if(!is.na(gmatrix[i,j])){
					point = newXMLNode("point", parent=points)
					abscissa = newXMLNode("abscissa", parent=point)
					real = newXMLNode("real",gmatrix[i,j], parent=abscissa)
					ordinate = newXMLNode("ordinate", parent=point)
					real = newXMLNode("real",umatrix[i,j], parent=ordinate)
				}
			}
		}
	}
	
	addChildren(z, criteria)
	
	
#cat(saveXML(z))
	return(z)
}

#########################################################################

exportLOG <- function(LOG,where,status)
{
	if( is.null( LOG )) {
	   if(status=="FAILED"){
			logMessage <- "ACUTA method Failed"	
	   } else {
			logMessage <- "ACUTA method succeed"
	   }	
	}
	else{
	   logMessage <- LOG
	   }
	
	
	#z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
	#z$addNode("methodMessages",close=FALSE,attrs=c("mcdaConcept"="methodMessages"))
	#z$addNode("logMessage",close=FALSE)
	#z$addNode("text",logMessage)
	#z$closeTag()
	#z$closeTag()
	
	text = newXMLNode( "text", logMessage )
	logMessage = newXMLNode( "logMessage" )
	methodMessages = newXMLNode( "methodMessages", attrs = c("mcdaConcept"="methodMessages") )
	z = newXMLNode("xmcda:XMCDA", namespace = c(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
	
	addChildren(logMessage, text)
	addChildren(methodMessages, logMessage)
	addChildren(z, methodMessages)	
	
	saveXML( z , file=paste(where,"message.xml",sep="/") )
}

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################

#########################################################################
