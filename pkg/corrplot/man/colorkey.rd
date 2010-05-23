\name{colorkey}
\Rdversion{1.1}
\alias{colorkey}

\title{
Draw colorkey.
}
\description{
Draw colorkey legend.
}
\usage{
colorkey(num_col, xlim=c(0,1), ylim=c(0,1), 
vertical=TRUE, precise=FALSE,
label_len=11, cex=1, col.text="black",  offset=0.5, 
width.colorkey.ratio = 0.4, length.segment.ratio = 0.1,
add=FALSE, axes = FALSE,...)
}

\arguments{
  \item{num_col}{
data.frame, the first column is number, the second is corresponding color.
}
  \item{xlim}{
see in \code{\link{plot}}.  
}
  \item{ylim}{
see in \code{\link{plot}}.  
}

  \item{vertical}{
logical scalar, whether the colorkey is vertical or horizon.  
}

  \item{precise}{
logical scalar, whether draw the number in precise.  
}

  \item{label_len}{
the number of number-text in colorkey.  
}

  \item{cex}{
cex of number-text in colorkey.  
}

  \item{col.text}{
color of number-text in colorkey. 
}

  \item{width.colorkey.ratio}{
the width ratio of colorbar to colorkey.  
}

  \item{length.segment.ratio}{
the width ratio of segments to colorkey. 
}

  \item{add}{
logical scalar, whether to add the plot to the current device, or delete the device's current contents first. 
}

  \item{axes}{
see in \code{\link{plot}}.  
}

  \item{offset}{
for number-label in colorkey, see in \code{\link{plot}}. 
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
plot(0,xlim=c(0,5),ylim=c(-0.5,1.2),type="n")
colorkey(data.frame(seq(0,1,len=100),rainbow(100)))
colorkey(data.frame(seq(0,1,len=100),heat.colors(100)),xlim=c(1,2))
colorkey(data.frame(seq(0,1,len=100),terrain.colors(100)),xlim=c(2,3))
colorkey(data.frame(seq(0,1,len=100),topo.colors(100)),xlim=c(3,4))
colorkey(data.frame(seq(0,1,len=100),cm.colors(100)),xlim=c(4,5))
colorkey(data.frame(seq(0,1,len=100),grey(1:100/100)),
		xlim=c(0,5), ylim=c(-0.5,-0.1), vertical=FALSE)
colorkey(data.frame(LETTERS[1:12],sample(rainbow(12))),precise=T,
		xlim=c(0,5), ylim=c(1,1.2), vertical=FALSE)

}
}

\keyword{hplot}% __ONLY ONE__ keyword per line