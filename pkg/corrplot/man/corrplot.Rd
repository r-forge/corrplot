\name{corrplot}
\Rdversion{1.1}
\alias{corrplot}
\alias{corrplot.circle}
\alias{corrplot.square}
\alias{corrplot.ellipse}
\alias{corrplot.number}
\alias{corrplot.shade}
\alias{corrplot.pie}
\alias{corrplot.color}

\title{
A visualization of a correlation matrix.
}
\description{
A graphical display of a correlation matrix, confidence interval. 
The details of graph are paid great attention to.

\code{corrplot.circle}, \code{corrplot.square}, \code{corrplot.ellipse},
\code{corrplot.number},
\code{corrplot.pie}, \code{corrplot.shade}, and \code{corrplot.color} are
convenience wrappers of \code{corrplot} that take the same arguments as the
corresponding option it replaces such as circle, ellipse etc.

}
\usage{
corrplot(corr,
		method = c("circle", "square", "ellipse", "number", "shade", "color", "pie"),
		type = c("full", "lower", "upper"),	col = NULL, bg = "white", title = "",
		diag = TRUE, outline = FALSE, mar = c(0,0,0,0),
		addgrid.col = "gray", addnum.col= NULL,

		order = c("original", "PCA", "hclust", "alphabet"),
		hclust.method = c("complete", "ward", "single", "average",
							"mcquitty", "median", "centroid"),
		addrect = NULL, rect.col="black", rect.lwd = 2,

		addtextlabel = c("lt","ld","td","d", "no"), tl.cex = 1,
		tl.col = "red", tl.offset = 0.4,

		addcolorlabel = c("right", "bottom","no"), cl.range=c( "-1to1","min2max"),
		cl.length=11, cl.cex =0.8, cl.ratio = 0.15, cl.align.text="c",cl.offset=0.5,

		addshade = c("negtive", "positive", "all"),
		shade.lwd = 1, shade.col = "white",

		p.mat = NULL, evi.level = 0.05,
		insig = c("pch","blank", "no"),
		pch = 4, pch.col = "black", pch.cex = 3,

		plotConf = c("no","square", "circle", "rect"),
		lowconf.mat = NULL, uppconf.mat = NULL)
corrplot.circle(corr, ...)
corrplot.square(corr, ...)
corrplot.ellipse(corr,...)
corrplot.number(corr, ...)
corrplot.shade(corr, ...)
corrplot.pie(corr, ...)
corrplot.color(corr, ...)
}
\arguments{
  \item{corr}{
the correlation matrix, must be square if \code{order} is not \code{"original"} (default).
}
  \item{method}{
the visualization method of correlation matrix to be used. Currently, it supports seven methods, named \code{"circle"}, \code{"square"}, \code{"ellipse"}, \code{"number"}, \code{"pie"}, \code{"shade"} and \code{"color"}. 

The areas of circles or squares show the absolute value of corresponding correlation. Method
\code{"pie"} and \code{"shade"} came from Michael Friendly's job (with some adjustment 
about the shade added on), and \code{"ellipse"} came from D.J. Murdoch and E.D. Chow's job, see in section references.
}
  \item{type}{
plot "full" matrix or just "upper" or "lower" triangular part of it.
}

  \item{col}{
vector, the color of glyphs.
}

  \item{bg}{
the background color.
}

  \item{title}{
title of the graph.
}


  \item{diag}{
logical, whether display the correlation on the principal diagonal.
}

  \item{outline}{
logical, whether plot outline of circles, square and ellipse.
}

  \item{mar}{
see \code{\link{par}}.


}


  \item{addgrid.col}{
the color of grid, if \code{NULL}, don't add grid.
}


  \item{addnum.col}{
col of numerical correlations, if \code{NULL}, don't add numerical correlations. Note: the figures are expressed in terms of a percentage for spacesaving.
}

  \item{order}{
the ordering method of the correlation matrix.
	\itemize{
		\item \code{"original"} for orginal order.
		\item \code{"alphabet"} for alphabetical order.
		\item \code{"PCA"} for principal component analysis.
		\item \code{"hclust"} for hierarchical clustering.
	}
}

  \item{hclust.method}{
the agglomeration method to be used when \code{order} is \code{\link{hclust}}. This should be one of \code{"ward"}, \code{"single"}, \code{"complete"}, \code{"average"}, \code{"mcquitty"}, \code{"median"} or \code{"centroid"}.

}

  \item{addrect}{
integer, the numer of rectangles drawed on the graph according to the hierarchical cluster,
only valid when \code{order} is \code{hclust}. If its value is \code{NULL} (default), then
don't add rectangles.
}

  \item{rect.col}{
color for rectangle border(s), only valid when \code{addrect} is not \code{NULL}.
}


  \item{rect.lwd}{
line width for borders for rectangle border(s), only valid
when \code{addrect} is not \code{NULL}.


}



  \item{addtextlabel}{
position of text labels, \code{"lt"} means left and top, \code{"ld"} means left and diagonal(near), \code{"td"} means top and diagonal(near), \code{"d"} means diag. If \code{"no"},
do not add textlabel.
}

  \item{tl.cex}{
numeric, for the text label (variable names).
}

  \item{tl.col}{
the color of text label.
}

  \item{tl.offset}{
for text label, see \code{\link{text}}.


}



  \item{addcolorlabel}{
where to draw colorlabel, \code{"right"} for right and \code{"bottom"} for bottom.
If \code{"no"}, don't draw colorlabel.
}
  \item{cl.range}{
style of colorlabel, must be \code{"-1to1"} or \code{"min2max"}.
}

\item{cl.length}{
the number of number-text in colorlabel, passed to \code{\link{colorlegend}}.
}

\item{cl.cex}{
cex of number-label in colorlabel,  passed to \code{\link{colorlegend}}.
}

\item{cl.ratio}{
to justify the width of colorlabel, 0.1~0.2 is suggested.
}

\item{cl.align.text}{
for number-label in colorlabel, \code{"l"} means left, \code{"c"} means center,
and \code{"r"} means right.
}

\item{cl.offset}{
for number-label in colorlabel, see \code{\link{text}}.


}



  \item{addshade}{
shade method, only valid when \code{method} is \code{shade}. If \code{"negative"}, then the negtive correlation will be shaded; if \code{"positive"}, then the positive correlation will be shaded; If \code{"all"}, then all correlation will be shaded. Note: the angle of shade line is different, 45 degrees for positive and 135 degrees for negtive.
}

  \item{shade.lwd}{
the line width of shade.
}
  \item{shade.col}{
the color of shade line.


}



  \item{p.mat}{
matrix of p-value, if \code{NULL}, arguments \code{evi.level}, \code{insig}, 
\code{pch}, \code{pch.col}, \code{pch.cex} is invalid.
}

  \item{evi.level}{
evidence level,  if the p-value in \code{p-mat} is bigger than \code{evi.level}, then
the corresponding correlation is regarded as insignificant.
}

  \item{insig}{
the method to deal with the insignificant correlation, if \code{"blank"}, wipe away the corresponding glyphs; if \code{"pch"}, add characters(see \code{pch} for details) on the
corresponding glyphs; if \code{"no"}, don't take any measures.
}
  \item{pch}{
add character on the glyphs of insignificant correlation 
(only valid when \code{insig} is \code{pch}).
}
  \item{pch.col}{
the color of pch (only valid when \code{insig} is \code{pch}).
}
  \item{pch.cex}{
the cex of pch (only valid when \code{insig} is \code{pch}).


}


  \item{plotConf}{
method to plot confidence interval, if \code{"no"}, don't plot confidence interval; 
if \code{"square"}, plot two square whose areas is ; if \code{"circle"}, ; if\code{"rect"},.
}
  \item{lowconf.mat}{
matrix of the lower bound of confidence interval.
}
  \item{uppconf.mat}{
matrix of the upper bound of confidence interval.


}


  \item{\dots}{
additional arguments for \code{corrplot}'s wrappers 
(\code{corrplot.circle}, \code{corrplot.square}, etc.), passed to \code{corrplot}.
}

}

\value{
(Invisibly) returns a single permutation vector.
}

\details{
.....
}


\references{
Michael Friendly (2002).
\emph{Corrgrams: Exploratory displays for correlation matrices}.
The American Statistician, 56, 316--324.


D.J. Murdoch, E.D. Chow (1996).
\emph{A graphical display of large correlation matrices}.
The American Statistician, 50, 178--180.
}

\author{
 Taiyun Wei
}

\note{
	\code{Cairo} and \code{cairoDevice} package is strongly recommended to produce
	high-quality PNG, JPEG, TIFF bitmap files, especially for that \code{method}
	is \code{circle}, \code{ellipse} .
}

\seealso{
The \code{plotcorr} function in the \code{ellipse} package and \code{corrgram} function in the \code{corrgram} package has some similarities.

Package \code{seriation} offered more methods to reorder matrices, such as 
ARSA, BBURCG, BBWRCG, MDS, TSP, Chen and so forth.
}

\examples{
\dontrun{

data(mtcars)
corr <- cor(mtcars)
par(ask = TRUE)

##
corrplot(corr, method="number", col="black", addcolorlabel="no")
corrplot(corr, method="number")
corrplot(corr)
corrplot(corr, order="hclust")
corrplot(corr, order ="PCA")
corrplot(corr, method="square", order = "PCA")
corrplot(corr, method="ellipse", order = "PCA")
corrplot(corr, method="color", order = "PCA")
corrplot(corr, method="shade", order = "PCA")
corrplot(corr, method="pie", order = "PCA")



## order is hclust and draw rectangles on graph
corrplot(corr, order="hclust", addrect = 2)
corrplot(corr, order="hclust", addrect = 3, rect.col = "red")
corrplot(corr, order="hclust", addrect = 4, rect.col = "blue")
corrplot(corr, order="hclust", hclust.method="ward", addrect = 4)



## about text-labels and type
corrplot(corr, order="PCA", addtextlabel="d")
corrplot(corr, order="PCA", diag=FALSE, addtextlabel="d")
corrplot(corr, order="PCA", diag=FALSE, addtextlabel="d",cl.range="min2max")
corrplot(corr, order="PCA", type="upper", addtextlabel="td")
corrplot(corr, order="PCA", type="upper", diag=FALSE, addtextlabel="td")
corrplot(corr, order="PCA", type="lower", addcolorlabel="b", addtextlabel="ld")
corrplot(corr, order="PCA", type="lower", addcolorlabel="b", diag=FALSE, addtextlabel="ld")



#### about color-legend, corr-color, and background-color
corrplot(corr, order="PCA", cl.ratio=0.1, cl.align="r", cl.offset=0)
corrplot(corr, order="PCA", cl.ratio=0.15,cl.length=9)
corrplot(corr, order="PCA", cl.ratio=0.2, cl.align="l")
corrplot(corr, order="PCA", addcolorlabel="b")
corrplot(corr, order="PCA", addcolorlabel="b",cl.range="min2max",addt="d")
corrplot(corr, order="PCA", addcolorlabel="no")
col1 <- rev(heat.colors(100))
col2 <- colorRampPalette(c("red", "white", "blue"))
col3 <- colorRampPalette(rev(c("#00007F", "blue", "#007FFF", "cyan",
                     "white", "yellow", "#FF7F00", "red", "#7F0000")))
col4 <- grey(10:1/10)
wb <- c("white","black")
corrplot(corr, order="PCA", col=col1)
corrplot(corr, order="PCA", col=col2(100))
corrplot(corr, order="PCA", col=col3(100))
corrplot(corr, order="PCA", col=col4)

corrplot(corr, col = wb, order="PCA", outline=TRUE, addcolorlabel="no")
## like Chinese wiqi, suit for either on screen or white-black print.
corrplot(corr, col = wb, bg="gold2",  order="PCA", addcolorlabel="no")



## addshade, addgrid and addnum
corrplot.shade(corr, order="PCA", addshade = "negtive", addgrid.col=NULL)
corrplot.shade(corr, order="PCA", addshade = "positive", addgrid.col=NULL)
corrplot.shade(corr, order="PCA", addshade = "all", addgrid.col=NULL)
corrplot.color(corr, order="PCA", addgrid.col=NULL)
corrplot.color(corr, order="PCA", addgrid.col="white")
corrplot.color(corr, order="PCA", addgrid.col="white", addnum.col="grey10")


##the input matrix is not square
corrplot(corr[1:8,])
corrplot(corr[,1:8])



cor.mtest <- function(mat, conf.level = 0.95){
	mat <- as.matrix(mat)
	n <- ncol(mat)
	p.mat <- lowconf.mat <- uppconf.mat <- matrix(NA, n, n)
	diag(p.mat) <- 0
	diag(lowconf.mat) <- diag(uppconf.mat) <- 1
	for(i in 1:(n-1)){
		for(j in (i+1):n){
			tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
			p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
			lowconf.mat[i,j] <- lowconf.mat[j,i] <- tmp$conf.int[1]
			uppconf.mat[i,j] <- uppconf.mat[j,i] <- tmp$conf.int[2]
		}
	}
	return(list(p.mat, lowconf.mat, uppconf.mat))
}

res <- cor.mtest(mtcars,0.99)

##
corrplot(corr, p.mat = res[[1]], evi.level=0.2)
corrplot(corr, p.mat = res[[1]], evi.level=0.05)
corrplot(corr, p.mat = res[[1]], evi.level=0.01)
corrplot(corr, p.mat = res[[1]], insig = "blank")
corrplot(corr, p.mat = res[[1]], order="hclust", insig = "blank", addrect=3)
corrplot(corr, p.mat = res[[1]], order="hclust", insig = "pch", addrect=3)

##
corrplot(corr,low=res[[2]], upp=res[[3]],
	plotC="circle", addg="grey20",addc="no")
corrplot(corr, p.mat = res[[1]],low=res[[2]], upp=res[[3]],
	plotC="circle", addg="grey20",addc="no")
	
corrplot(corr, low=res[[2]], upp=res[[3]], 
	col=c("white","black"),bg="gold2",
	plotConf="circle",addc="no",pch.col="red")
corrplot(corr, p.mat = res[[1]], low=res[[2]], upp=res[[3]], 
	col=c("white","black"),bg="gold2",
	plotConf="circle",addc="no",pch.col="red")
	
corrplot(corr, low=res[[2]], upp=res[[3]],
	plotConf="square",addg="grey20",addc="no",pch.col=1)
corrplot(corr, p.mat = res[[1]],low=res[[2]], upp=res[[3]],
	plotC="square", addg="grey20",addc="no")

corrplot(corr, low=res[[2]], upp=res[[3]],
	plotConf="rect", order="PCA",addg="grey20",addc="no")
corrplot(corr, p.mat = res[[1]],low=res[[2]], upp=res[[3]], pch.col="red",
	plotC="rect", order="PCA",addg="grey20",addc="no")

}
}

\keyword{hplot}% __ONLY ONE__ keyword per line
