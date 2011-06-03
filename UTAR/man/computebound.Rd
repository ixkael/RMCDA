\name{computebound}

\alias{computebound}

\title{Computes the bounds of a variable}

\description{Computes the bounds of one variable of the model.}

\usage{
	out <- computebound( M, i, l )
	}

\arguments{
	\item{M}{A model properly initialized and enriched with the other functions of UTAR}
	\item{i}{Criterion subscript}
	\item{l}{Linear segment subscript} 
	}

\value{
	The difference of the bounds, i.e. |Maxvalue-Minvalue|.
	}

\examples{}