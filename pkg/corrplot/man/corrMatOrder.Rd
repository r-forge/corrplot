\name{corrMatOrder}
\Rdversion{1.1}
\alias{corrMatOrder}

\title{
Reorder a correlation matrix.
}
\description{
Draw rectangle(s) around the chart of corrrlation matrix based on the number 
of each cluster's members.
}
\usage{
corrMatOrder(corr, order=c("AOE", "FPC", "hclust", "alphabet"),
	hclust.method = c("complete", "ward", "single", "average",
	"mcquitty", "median", "centroid"))
}

\arguments{
  \item{corr}{
correlation matrix to reorder.
}

  \item{order}{
character, the ordering method of the correlation matrix. 
	\itemize{
		\item \code{"original"} for original order.
		\item \code{"AOE"} for the angular order of the eigenvectors. See Michael Friendly (2002) for details.
		\item \code{"PCA"} for the first principal component order.
		\item \code{"hclust"} for hierarchical clustering order.
		\item \code{"alphabet"} for alphabetical order.
	}
}
  \item{hclust.method}{
character, the agglomeration method to be used when \code{order} is \code{hclust}. This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.
}

}

\value{
returns  the order (a integer vector).
}

\seealso{
Package \code{seriation} offered more methods to reorder matrices, such as 
ARSA, BBURCG, BBWRCG, MDS, TSP, Chen and so forth.
}


\author{
Taiyun Wei
}

\examples{

M <- cor(mtcars)
(order.AOE <- corrMatOrder(M, order="AOE"))
(order.FPC <- corrMatOrder(M, order="FPC"))
(order.hc <- corrMatOrder(M, order="hclust"))
(order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="ward"))
M.AOE <- M[order.AOE,order.AOE ]
M.FPC <- M[order.FPC,order.FPC ]
M.hc  <- M[order.hc, order.hc ]
M.hc2 <- M[order.hc2,order.hc2]



par(ask=TRUE)
corrplot(M)
corrplot(M.AOE)
corrplot(M.FPC)
corrplot(M.hc)

corrplot(M.hc)
corrHclust.rect(corr=M.hc, k=2)

corrplot(M.hc)
corrHclust.rect(corr=M.hc, k=3)

corrplot(M.hc2)
corrHclust.rect(M.hc2, k=2, method="ward")

}

\keyword{hplot}% __ONLY ONE__ keyword per line
