\name{solveUTASTAR}

\alias{solveUTASTAR}

\title{Solves UTASTAR}

\description{
	Solves the UTASTAR linear programming system and constructs a set of compatible value functions, having a non-particular position in the constraints polyhedron (interior point method).
	}

\usage{
	solveUTASTAR( c, A, Aeq, b, beq, nC )
	}

\arguments{
	\item{c}{Vector : objective function}
	\item{A}{Matrix : inequality matrix}
	\item{b}{Vector : inequality right-member}
	\item{Aeq}{Matrix : equality matrix}
	\item{beq}{Vector : inequality right-member}
	\item{nC}{Number of dimensions (criteria) in the problem}
	}
	
\value{
	\item{x}{The solution (vectorized value function)}
	\item{validation}{Success or failure tag}
	\item{LOG}{Log information}
	}

\examples{}