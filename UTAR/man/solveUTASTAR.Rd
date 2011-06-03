\name{solveUTASTAR}

\alias{solveUTASTAR}

\title{Solves UTASTAR}

\description{
	Solves the UTASTAR linear programming system and constructs a set of compatible value functions, having a non-particular position in the constraints polyhedron (interior point method).
	}

\usage{
	out <- solveUTASTAR( c, A, Aeq, b, beq, nC )
	}

\arguments{
	\item{c}{Vector : objective function}
	\item{A}{Matrix : inequality constraints matrix}
	\item{b}{Vector : inequality constraints right-member}
	\item{Aeq}{Matrix : equality constraints matrix}
	\item{beq}{Vector : inequality constraints right-member}
	\item{nC}{Number of dimensions (criteria) in the problem}
	}
	
\value{
	A list structured as follows:
	\item{x}{The solution (Vectorized value function)}
	\item{validation}{Success or failure flage}
	\item{LOG}{Log information}
	}

\examples{}