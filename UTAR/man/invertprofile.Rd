\name{invertprofile}

\alias{invertprofile}

\title{Inverts c profile}

\description{Inverts the c profile (in the utilities space) and gets the equivalent alternative. Dual method of \code{findcprofile}.}

\usage{
	alt <- invertprofile( gmatrix, calt, segs=NULL )
	}

\arguments{
	\item{calt}{The c profile (coefficients in the utilities space, for example obtained with \code{findcprofile})}
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{segs}{Vector : number of segments in each value function}
	}

\value{An alternative, i.e. a vector-evaluation profile.}

\examples{}