\name{checkEasyPair}

\alias{checkEasyPair}

\title{Checks easy pair}

\description{Checks an easy pair (x,y) by performing various tests on its structure. y is assumed to be generated from x.}

\usage{
	out <- checkEasyPair( verbose, x, y, cx, cy, gmatrix, u, crits, poss1, poss2 )
	}

\arguments{
	\item{verbose}{Boolean}
	\item{x}{The evaluation profile of the first alternative}
	\item{y}{The evaluation profile of the second alternative}
	\item{cx}{The c profile (coefficients in the utilities space, obtained with \code{findcprofile}) of x}
	\item{cy}{The c profile (coefficients in the utilities space, obtained with \code{findcprofile}) of y}
	\item{gmatrix}{A matrix containing the abscissa of the value functions, with the proper number of breakpoints and criteria}
	\item{u}{A valid point (value function) in the utilities space, through which the easy pair is supposed to pass}
	\item{crits}{The two critaria in which x and y differ}
	\item{poss1}{Vector containing the two locations (segments numbers) for x in the two specified criteria}
	\item{poss2}{Vector containing the two locations (segments numbers) for y in the two specified criteria}
	}

\value{The checked value for alternative y}

\examples{}