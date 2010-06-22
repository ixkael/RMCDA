
inputsLocation <- commandArgs()[5]
outputsLocation <- commandArgs()[6]

#source("/Users/silver/RMCDA/Github/UTAR-lib.R")
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
				location <- listOfFiles[ listOfFiles == "alternativesValues1.xml" ]
				if( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "failed to find alternativesValues1.xml" , outputsLocation ) 
				}
				location <- paste( inputsLocation , location , sep="/" )
				xml <- xmlInternalTreeParse( location )
				if( checkXSD( xml ) != 1 )
				{
					errTag = TRUE
					exportLog( "alternativesValues1.xml is not a valid XMCDA file" , outputsLocation ) 
				}
				alternativesValues1 <- getAlternativesValues ( xml , alternativesIDs )
			})
	if ( inherits(tmpErr, 'try-error') || alternativesValues1$status != "OK" )
	{
		if(errTag==FALSE){exportLog( "failed to get data from alternativesValues1.xml" , outputsLocation )}
		errTag = TRUE
	}
	alternativesValues1 <- alternativesValues1[[1]]
}

if(errTag == FALSE){
	tmpErr <- try({
				listOfFiles <- list.files( inputsLocation )
				location <- listOfFiles[ listOfFiles == "alternativesValues2.xml" ]
				if( length( location ) == 0 ) 
				{
					errTag = TRUE
					exportLog( "failed to find alternativesValues1.xml" , outputsLocation )
				}
				location <- paste( inputsLocation , location , sep="/" )
				xml <- xmlInternalTreeParse( location )
				if( checkXSD( xml ) != 1 )
				{
					exportLog( "alternativesValues2.xml is not a valid XMCDA file" , outputsLocation ) 
				}
				alternativesValues2 <- getAlternativesValues ( xml , alternativesIDs )
			})
	if ( inherits(tmpErr, 'try-error') || alternativesValues2$status != "OK" )
	{
		if(errTag==FALSE){exportLog( "failed to get data from alternativesValues2.xml" , outputsLocation )}
		errTag = TRUE
	}
	alternativesValues2 <- alternativesValues2[[1]]
}

if(errTag == FALSE){
	tmpErr <- try({
				n <- NULL
				for( i in 1:nrow(alternativesValues1) )
				{
					if( any( alternativesValues1[i,1] == alternativesValues2[,1] ) )
					{
						if( is.null(n) )
						{
							n <- alternativesValues1[i,] 
						}
						else
						{
							n <- rbind( n , alternativesValues1[i,] )
						}
					}
				}
				colnames(n) = NULL
				rownames(n) = NULL
				alternativesValues1 <- n
				
				n <- NULL
				for( i in 1:nrow(alternativesValues2) )
				{
					if( any( alternativesValues2[i,1] == alternativesValues1[,1] ) )
					{
						if( is.null(n) )
						{
							n <- alternativesValues2[i,] 
						}
						else
						{
							n <- rbind( n , alternativesValues2[i,] )
						}
					}
				}
				colnames(n) = NULL
				rownames(n) = NULL
				alternativesValues2 <- n
				
				a1 <- alternativesValues1[ order( alternativesValues1[,2]) , 1]
				a2 <- alternativesValues2[ order( alternativesValues2[,2] ) , 1 ]
				
				v1 <- alternativesValues1[ order( alternativesValues1[,2] ) , 2 ]
				o <- alternativesValues1[ order( alternativesValues1[,2] ) , 1 ]
#v2 <- alternativesValues2[ order( alternativesValues2[,1] ) , 2 ]
				v2 <- c()
				for( i in 1:length(a1) )
				{
					v2 <- c( v2 , alternativesValues2[ which( alternativesValues2[,1] == a1[i] ) , 2 ] )
				}
				
			})
	if ( inherits(tmpErr, 'try-error') ){
		if(errTag==FALSE){exportLog( "technical error during manipulation of alternativesValues",outputsLocation)}
		errTag = TRUE
	}
}

if(errTag == FALSE){
	tmpErr<-try(
			{
				#setwd(outputsLocation)
				pdf("out/out.pdf", width=length(alternativesIDs) + 2 , height = length(alternativesIDs) + 2)
				plot(v2~v1 , type="b", cex=2 , cex.axis=2.1 , xlim=c(min(v1)-mean(v1)/3 , max(v1)+mean(v1)/3 ) , ylim=c(min(v2)-mean(v2)/3 , max(v2)+mean(v2)/3 )) 
				text( v1,v2,pos=3,offset=1.3,cex=2,labels = alternativesIDs[o] )
				title(xlab="alternativesValues1", ylab="alternativesValues2" , cex.lab=1.5)
				dev.off()
				system("convert out/out.pdf out/out.png")
			system("base64 -w0 out.png > out.base64")
#				system("openssl enc -base64 -in out/out.png -out out/out.base64 ")
				tmp <-readLines(file("out/out.base64","rt"))
				system("rm out/out.base64 out/out.png out/out.pdf")
				closeAllConnections()
			})
	if (inherits(tmpErr, 'try-error')){
		if(errTag==FALSE){exportLog( "error while creating image output" , outputsLocation )}
		errTag = TRUE
	}
}

if(errTag == FALSE){
	tmpErr <- try({
				f <- tempfile()
				z <- xmlTree("xmcda:XMCDA",namespaces=list(xsi="http://www.w3.org/2001/XMLSchema-instance",xmcda="http://www.decision-deck.org/2009/XMCDA-2.0.0"))
#				z <- xmlInternalTreeParse( saveXML(z, f, encoding = "UTF-8",indent=TRUE,prefix = '<?xml version="1.0"?>\n') )
#				putAlternativesPlot(z, tmp, alternativesIDs)
				z$addNode("alternativeValue",close=FALSE)
				z$addNode("alternativesSet",close=FALSE)
				for( i in 1:length(alternativesIDs)){
					z$addNode( "element" , close=FALSE )
					z$addNode( "alternativeID" , alternativesIDs[i] )
					z$closeTag()
				}
				z$closeTag()
				z$addNode( "value" , close=FALSE )
				tmp = paste( unlist( strsplit(toString(tmp),split=", ") ) )
				tmp = paste( tmp , collapse = "")
				z$addNode( "image", tmp )
				z$closeTag()
				z$closeTag()
				saveXML( z, file = paste(outputsLocation,"alternativesValuesPlotComparison.xml",sep="/") )
			})	
	if (inherits(tmpErr, 'try-error')){
		message <- "Fatal error while exporting data"
		exportLog( message , outputsLocation , error=TRUE )
	} else {
		message <- "Execution successful"
		exportLog( message , outputsLocation , error=FALSE )
	}
}