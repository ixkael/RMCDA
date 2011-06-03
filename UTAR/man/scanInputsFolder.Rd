\name{scanInputsFolder}

\alias{scanInputsFolder}

\title{Detects input files and analyses it}

\description{Scans input directory, detects and analyse inputs of given name ("alternatives.xml" , "alternativesRanking.xml" , "alternativesPreferences.xml" , "alternativesIndifferences.xml" , "criteria.xml" , "delta.xml" , "segments.xml" , "preferencesDirections.xml" , "performanceTable.xml" , "method.xml").}

\usage{ 
	out <- scanInputsFolder( inputsLocation ) 
	}

\arguments{
	\item{inputsLocation}{Path (directory) containing all XMCDA input files}
	}

\value{
	List containing the extracted content in appropriate format
	}

\examples{}