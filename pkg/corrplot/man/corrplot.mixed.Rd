\name{corrplot.mixed}
\Rdversion{1.1}
\alias{corrplot.mixed}

\title{
Using mixed methods to visualize a correlation matrix.
}
\description{
Using mixed methods to visualize a correlation matrix.
}
\usage{
corrplot.mixed(corr, lower = "number", upper = "circle",
	addtextlabel = c("d","lt","no"), diag = c("n","l","u"), 
	bg = "white", addgrid.col = "gray",...)
}

\arguments{
  \item{corr}{
vector, the number of each cluster's members.
}
  \item{lower}{
character, the visualization method for the lower triangular correlation matrix.
}
  \item{upper}{
character, the visualization method for the upper triangular correlation matrix.
}
  \item{addtextlabel}{
character, \code{"lt"} or \code{"d"}, giving position of 
text labels, \code{"lt"} means left and top,  \code{"d"} means diagonal. 
If \code{"no"}, do not add textlabel.
}
  \item{diag}{
character, for specifying the glyph on the principal diagonal. It is one of
\code{"n"} (default, not draw anything), \code{"l"} (draw the glyphs of 
lower triangular) or \code{"u"} (draw the glyphs of upper triangular). 
}
  \item{bg}{
  the background color.
}

\item{addgrid.col}{
the color of grid, if \code{NULL}, don't add grid.
}

  \item{...}{
additional arguments for corrplot's wrappers
}
}
\author{
Taiyun Wei
}

\examples{
M <- cor(mtcars)
ord <- corrMatOrder(M, order="AOE")
M2 <- M[ord,ord]
par(ask=TRUE)
corrplot.mixed(M2)
corrplot.mixed(M2, lower="ellipse", upper="circle")
corrplot.mixed(M2, lower="square", upper="circle")
corrplot.mixed(M2, lower="shade", upper="circle")
corrplot.mixed(M2, addtextlabel="lt")
corrplot.mixed(M2, addtextlabel="lt", diag="u")
corrplot.mixed(M2, addtextlabel="lt", diag="l")
corrplot.mixed(M2, addtextlabel="no")

}

\keyword{hplot}% __ONLY ONE__ keyword per line
