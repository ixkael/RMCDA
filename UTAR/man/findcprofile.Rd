\name{findcprofile}

\alias{findcprofile}

\title{Finds c profile}

\description{Constructs the c profile of an alternative. Dual method of \code{invertprofile}.}

\usage{
	calt <- findcprofile( gmatrix, alt, segs=NULL )
	}

\arguments{
	\item{alt}{Evaluation profile of alternative alt}
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{segs}{Vector : number of segments in each value function}
	}

\value{The c profile (vector)}

\examples{}