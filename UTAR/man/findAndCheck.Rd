\name{findAndCheck}

\alias{findAndCheck}

\title{Finds and checks \code{XML} inputs and content}

\description{Finds and checks \code{XML} inputs and content in the inputsDirectory.}

\usage{
	out <- findAndCheck( name , inputsLocation , listOfFiles , aux )
	}

\arguments{
	\item{filename}{filename}
	\item{inputsLocation}{inputsLocation}
	\item{listOfFiles}{listOfFiles}
	\item{aux}{aux}
	}

\value{
	A list structured as follows:
	\item{result}{The appropriate result}
	\item{validation}{Validation tag (True if execution was successful)}
	\item{log}{If necessary, in case of error}
	}

\examples{}