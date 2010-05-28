\name{colorlegend}
\Rdversion{1.1}
\alias{colorlegend}

\title{
Draw color legend.
}
\description{
Draw color legend.
}
\usage{
colorlegend(colbar, labels, at=NULL,  xlim=c(0,1), ylim=c(0,1), 
	vertical=TRUE, ratio.colbar = 0.4, lim.segment = NULL, 
	align=c("c","l","r"), addlabels=TRUE, ...)
}

\arguments{
  \item{colbar}{
vector, col.
}


  \item{labels}{
vector, numeric or character to specify the text to be written.
}


  \item{at}{
numeric vector, in [-1,1], where to put labels. Default value is writting labels aside
colorbar ly.
}
  \item{xlim}{
see in \code{\link{plot}}.  
}
  \item{ylim}{
see in \code{\link{plot}}.  
}

  \item{vertical}{
logical scalar, whether the colorlegend is vertical or horizon.  
}


  \item{ratio.colbar}{
the width ratio of colorbar to colorlegend.  
}

  \item{lim.segment}{
the width ratio of segments to colorlegend. 
}

  \item{align}{
the width ratio of segments to colorlegend. 
}

  \item{addlabels}{
logical, whether add text label.
}

  \item{\dots}{
additional arguments, passed to \code{\link{plot}}.  
}

}
\author{
Taiyun Wei
}

\examples{
\dontrun{
par(mar=rep(0,4))
plot(0,xlim=c(0,6),ylim=c(-0.5,1.2),type="n")
colorlegend(rainbow(100), 0:9)
colorlegend(heat.colors(100),LETTERS[1:12], xlim=c(1,2))
colorlegend(terrain.colors(100),0:9, ratio.colbar=0.6, 
		lim.segment=c(0,0.6),xlim=c(2,3), ,align="l")
colorlegend(topo.colors(100), 0:9, lim.segment=c(0,0.6), xlim=c(3,4),align="l", offset=0)
colorlegend(cm.colors(100),1:5, xlim=c(4,5))
colorlegend(sample(rainbow(12)), labels=LETTERS[1:12], at=seq(0.05, 0.95, len=12),
		xlim=c(5,6),align="r")
colorlegend(colbar=grey(1:100/100),1:10, col="red",
		xlim=c(0,6), ylim=c(-0.5,-0.1), vertical=FALSE, align="l")
colorlegend(sample(rainbow(12)), labels=LETTERS[1:12], at=seq(0.05, 0.95, len=12),
		xlim=c(0,6), ylim=c(1.1,1.2), vertical=FALSE)
}
}

\keyword{hplot}% __ONLY ONE__ keyword per line