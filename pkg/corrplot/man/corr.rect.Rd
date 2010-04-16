\name{corr.rect}
\Rdversion{1.1}
\alias{corr.rect}

\title{
Draw rectangle(s) around the corrrlation matrix graph.
}
\description{
Draw rectangle(s) around the chart of corrrlation matrix based on the number 
of each cluster's members.
}
\usage{
corr.rect(clus, rect.col = "black", lwd = 2)
}

\arguments{
  \item{clus}{
vector, the number of each cluster's members.
}
  \item{rect.col}{
color of rectangles.  
}
  \item{lwd}{
line width of rectangles.
}
}
\author{
Taiyun Wei
}

\examples{
\dontrun{
data(mtcars)
corr <- cor(mtcars)
corrplot(corr, method="circle", order = "PCA")
corr.rect(c(6,5))

}
}

\keyword{hplot}% __ONLY ONE__ keyword per line
