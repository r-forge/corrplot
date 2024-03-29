\name{corrRect}
\Rdversion{1.1}
\alias{corrRect}
\alias{corrRect.hclust}

\title{
Draw rectangle(s) on the correlation matrix graph.
}
\description{
Draw rectangle(s) around the chart of corrrlation matrix. 

\code{corrRect} needs the number(parameter \code{clus}) of each cluster's members, while \code{corrRect.hclust}  can get the members in each cluster based on hierarchical clustering (\code{\link{hclust}}).
}
\usage{
corrRect(clus,  col = "black", lwd = 2)

corrRect.hclust(corr, k=2, col = "black", lwd = 2,
	method = c("complete", "ward", "single", "average","mcquitty", "median", "centroid"))
}

\arguments{
  \item{clus}{
Vector, the number of each cluster's members.
}

  \item{corr}{
Correlation matrix for function \code{corrRect.hclust}. It use \code{1-corr} as dist in  hierarchical clustering (\code{\link{hclust}}).
}

  \item{k}{
Integer, the number of rectangles drawn on the graph according to the hierarchical cluster,
for function \code{corrRect.hclust}.
}
  \item{col}{
Color of rectangles.  
}
  \item{lwd}{
Line width of rectangles.
}

  \item{method}{
Character, the agglomeration method to be used for hierarchical clustering (\code{\link{hclust}}). This should be (an unambiguous abbreviation of) one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.
}
}


\author{
Taiyun Wei
}

\examples{

data(mtcars)
M <- cor(mtcars)
corrplot(M, method="circle", order = "FPC")
corrRect(c(5,6))

(order.hc <- corrMatOrder(M, order="hclust"))
(order.hc2 <- corrMatOrder(M, order="hclust", hclust.method="ward"))
M.hc  <- M[order.hc, order.hc ]
M.hc2 <- M[order.hc2,order.hc2]

par(ask=TRUE)
##same as: corrplot(M, order="hclust", addrect=2)
corrplot(M.hc)
corrRect.hclust(corr=M.hc, k=2)

##same as: corrplot(M, order="hclust", addrect=3)
corrplot(M.hc)
corrRect.hclust(corr=M.hc, k=3)

##same as: corrplot(M, order="hclust", hclust.method="ward", addrect=2)
corrplot(M.hc2)
corrRect.hclust(M.hc2, k=2, method="ward")

##same as: corrplot(M, order="hclust", hclust.method="ward", addrect=3)
corrplot(M.hc2)
corrRect.hclust(M.hc2, k=3, method="ward")

##same as: corrplot(M, order="hclust", hclust.method="ward", addrect=4)
corrplot(M.hc2)
corrRect.hclust(M.hc2, k=4, method="ward")

}

\keyword{hplot}% __ONLY ONE__ keyword per line
