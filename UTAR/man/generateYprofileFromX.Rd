\name{generateYprofileFromX}

\alias{generateYprofileFromX}

\title{Generates Y profile from X and U}

\description{Generates a profile cy for alternative Y from the profile of X.}

\usage{
	out <- generateYprofileFromX( segs, crits, poss1, poss2, cx, u)
	}

\arguments{
	\item{segs}{Vector : number of segments in each value function}
	\item{cx}{The c profile (coefficients in the utilities space, obtained with \code{findcprofile}) of x}
	\item{u}{A valid point (value function) in the utilities space, through which the easy pair is supposed to pass}
	\item{crits}{The two critaria in which x and y differ}
	\item{poss1}{Vector containing the two locations (segments numbers) for x in the two specified criteria}
	\item{poss2}{Vector containing the two locations (segments numbers) for y in the two specified criteria}
	}

\value{
	A vector of coefficients c(y) in the utilities space.
	}

\examples{}