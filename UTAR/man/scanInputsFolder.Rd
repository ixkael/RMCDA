\name{scanInputsFolder}

\alias{scanInputsFolder}

\title{Detects input files and analyse it}

\description{Scans input directory, detects and analyse inputs of given name ("alternatives.xml" , "alternativesRanking.xml" , "alternativesPreferences.xml" , "alternativesIndifferences.xml" , "criteria.xml" , "delta.xml" , "segments.xml" , "preferencesDirections.xml" , "performanceTable.xml" , "method.xml").}

\usage{ content <- scanInputsFolder( inputsLocation ) }

\arguments{
	\item{directoryLocation}{Directory containing input files}
	}

\value{
	List containing the extracted content in appropriate format.
	}

\examples{}