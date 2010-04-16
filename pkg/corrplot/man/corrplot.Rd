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
\alias{corrplot.mtest}

\title{
A visualization of a correlation matrix.
}
\description{
A graphical display of a correlation matrix, test for correlation, and other visualization methods about association and correlation. The details of graph are paid great attention to.

\code{corrplot.circle}, \code{corrplot.square}, \code{corrplot.ellipse}, \code{corrplot.number},  
\code{corrplot.pie}, \code{corrplot.shade}, and \code{corrplot.color} are convenience wrappers of 
\code{corrplot} that take the same arguments as the corresponding option it replaces such 
as circle, ellipse etc. The \code{corrplot} function is more flexible than the wrappers.

\code{corrplot.mtest} function is also a wrapper of \code{corrplot}, however it can calculate correlation (input parameter \code{corr}), p-value (input parameter \code{p.mat}) and  confidence interval (input parameter \code{low.mat} and \code{upp.mat}) from original data.frame or matrix (input parameter \code{dat}) automatically, while \code{corrplot} function need more input parameters because it does not support input parameter \code{dat}. Consequently, \code{corrplot.mtest} function is strongly recommended if you want to visualize the result of test for correlation.
}
\usage{
corrplot(corr, method = c("circle", "square", "ellipse", "number", 
				"pie", "shade", "color"),
    type = c("full", "lower", "upper"), 
    order = c("original", "alphabet", "PCA", "hclust"),
    hclust.method = c("complete", "ward", "single", "average", 
    	               "mcquitty", "median", "centroid"),
	rect.hc = NA, rect.col="black", rect.lwd = 2,
	
    col = colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", 
	    "#FDDBC7", "#F7F7F7", "#D1E5F0", "#92C5DE", 
	    "#4393C3", "#2166AC", "#053061"))(200),
             
    outline = FALSE, cex = 1, title = "", bg = "white",
    addcolorkey = TRUE, colorkey=c("min2max", "-1to1"),
    cex.col.num = 0.8, mar = c(0,0,2,0),
    			
    addtextlabel = TRUE, pos.text = c("sides","diag"), col.text = "red",
    			
    shade.method = c("negtive", "positive", "all"),
    lwd.shade = 1, col.shade = "white", 
    			
    			
    addgrid = TRUE, col.grid = "gray", diag = TRUE,   
    addnum = FALSE,  col.num = NULL, 
    			
    corr.mtest = FALSE, p.mat = NULL, conf.level = 0.95,
    non_corr.method = c("pch","blank"),
    pch = 4, col.pch = "red",  cex.pch = 1,
    plotConf = FALSE, low.mat = NULL, upp.mat = NULL)
		
corrplot.circle(corr, ...)
corrplot.ellipse(corr,...)
corrplot.number(corr, ...)
corrplot.pie(corr, ...)
corrplot.shade(corr, ...)
corrplot.square(corr, ...)
corrplot.shade(corr, ...)
corrplot.mtest(dat = NULL, corr = NULL, p.mat = NULL, conf.level = 0.95,
	plotConf = FALSE, low.mat = NULL, upp.mat = NULL, 
    non_corr.method = c("pch","blank"), method = c("circle","square"),
    pch = 4, col.pch = "black",...)
}
\arguments{
  \item{corr}{
the correlation matrix, must be square if \code{order} is not \code{"original"} (default).
}
  \item{method}{
the visualization method of correlation matrix to be used. Currently, it support seven methods, named \code{"circle"}, \code{"square"}, \code{"ellipse"}, \code{"number"}, \code{"pie"}, \code{"shade"} and \code{"color"}  for function \code{corrplot}, and \code{"circle"}, \code{"square"} for function \code{corrplot.mtest}.
}
  \item{type}{
plot "full" matrix or just "upper" or "lower" triangular part of it.
}

  \item{order}{
the ordering method of the correlation matrix, need square matrix.
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

  \item{rect.hc}{
integer, the numer of rectangles drawed on the graph according to the hierarchical cluster, 
only valid when \code{order} is \code{hclust}. 
}

  \item{rect.col}{
color for rectangle border(s), only valid when \code{rect.hc} is not \code{NA}.
}


  \item{rect.lwd}{
line width for borders for rectangle border(s), only valid when \code{rect.hc} is not \code{NA}.
}

  \item{col}{
vector(any length), the color of glyphs.
}
  \item{outline}{
logical, whether plot outline of circles, square and ellipse or not.
}
  \item{cex}{
numeric, for the text label (variable names).
}
  \item{title}{
title of the graph.
}

  \item{bg}{
the background color.
}

  \item{addcolorkey}{
whether add colorkey or not.
}
  \item{colorkey}{
style of colorkey, must be \code{"-1to1"} or \code{"min2max"}.
}
  \item{cex.col.num}{
cex of the number in colorkey. 
}
  \item{mar}{
see \code{\link{par}}.
}
  \item{addtextlabel}{
logical, whether add text label or not.
}
  \item{pos.text}{
position of text labels, if \code{"sides"}, all the text labels will be plot on sides; if \code{"diag"} (only valid when \code{type} is \code{upper} or \code{lower}), a set of text labels will be plot near the principal diagonal.
}
  \item{col.text}{
the color of text label.
}
  \item{shade.method}{
shade method, only valid when \code{method} is \code{shade}. If \code{negative}, then the negtive correlation will be shaded; if \code{positive}, then the positive correlation will be shaded; If \code{all}, then all correlation will be shaded. Note: the angle of shade line is different, 45 degrees for positive and 135 degrees for negtive.
}
  \item{lwd.shade}{
the line width of shade.
}
  \item{col.shade}{
the color of shade line.
}

  \item{addgrid}{
logical, whether add grid or not.
}
  \item{col.grid}{
the color of grid.
}
  \item{diag}{
logical, whether display the correlation of the principal diagonal.
}

  \item{addnum}{
logical, whether to plot numerical correlations on glyphs. Note: the figures are expressed in terms of a percentage for spacesaving.
}
  \item{col.num}{
col of numerical correlations.
}
  \item{corr.mtest}{
logical, whether to visualize the result of test for correlation. Note: need input parameters \code{dat} (only supported by function \code{corrplot.mtest}) or \code{p.mat} if \code{TRUE}.
}
  \item{p.mat}{
matrix of p-value, function \code{corrplot.mtest} can also calculate it from input parameter \code{dat}.
}
  \item{conf.level}{
confidence level for the returned confidence interval (valid in function \code{corrplot.mtest()}) and calculating the level of significance.
}
  \item{non_corr.method}{
the method to deal with the correlation whose p-value is less than level of significance (1-\code{conf.level}), if \code{"blank"}, wipe away the corresponding glyphs; if \code{"pch"}, add character on the corresponding glyphs.
}
  \item{pch}{
plotting character for the correlation whose p-value is less than level of significance (only valid when \code{non_corr.method} is \code{pch}).
}
  \item{col.pch}{
the color of pch (only valid when \code{non_corr.method} is \code{pch}).
}
  \item{cex.pch}{
the cex of pch (only valid when \code{non_corr.method} is \code{pch}).
}
  \item{plotConf}{
logical, whether to plot confidence interval or not. Note: need input parameters \code{dat} (only supported by function \code{corrplot.mtest}) or \code{low.mat} and \code{upp.mat} if \code{TRUE}.
}
  \item{low.mat}{
matrix of the lower bound of confidence interval, function \code{corrplot.mtest} can also calculate it from input parameter \code{dat} automatically.
}
  \item{upp.mat}{
matrix of the upper bound of confidence interval, function \code{corrplot.mtest} can also calculate it from input parameter \code{dat} automatically.
}

  \item{dat}{
original data.frame or matrix for function \code{corrplot.mtest},  \code{corrplot.mtest} can calculate correlation , p-value and confidence interval automatically from \code{dat}.
}

  \item{\dots}{
extra parameters.
}
}

\value{
(Invisibly) returns a single permutation vector.
}

\details{
\code{method} shoud be \code{circle} or \code{square} if draw confidence 
interval (\code{plotConf = TRUE}).
\code{Cairo} and \code{cairoDevice} package is strongly recommended to produce high-quality PNG, 
JPEG, TIFF bitmap files, especially for that \code{method} is \code{circle}, 
\code{ellipse} or \code{pie}.   
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
Using circles or squares with different areas and colors to display a correlation matrix, test for correlation's p-values and confidence intervals is Taiyun Wei's pioneering work.
}

\seealso{
The \code{plotcorr} function in the \code{ellipse} package and \code{corrgram} function in the \code{corrgram} package has some similarities.
}

\examples{
\dontrun{

data(mtcars)
corr <- cor(mtcars)
par(ask = TRUE)

corrplot(corr, method="circle", order = "PCA")
##same as corrplot.circle(corr, order = "PCA")

corrplot(corr, method="square", order="hclust", type="upper", diag=FALSE, pos.text="diag")
##same as corrplot.square(corr, order="hclust", type="upper", diag=FALSE, pos.text="diag")

corrplot(corr, order="hclust", rect.hc = 2)
corrplot(corr, order="hclust", rect.hc = 3, rect.col = "red")
corrplot(corr, order="hclust", rect.hc = 4, rect.col = "yellow")


corrplot(corr, order="hclust", hclust.method="ward", rect.hc = 3) ## same as:
corrplot(corr, order="hclust", hclust.method="ward")
corr.rect(c(5,2,4))

corrplot.shade(corr, order="PCA", shade.method = "negtive", col.grid="white")
corrplot.shade(corr, order="PCA", shade.method = "positive", col.grid="white")
corrplot.shade(corr, order="PCA", shade.method = "all", col.grid="white")

corrplot.ellipse(corr, order="PCA", colorkey = "-1to1", type="lower", pos.text="diag")

corrplot.number(corr, order="PCA")

corrplot.pie(corr, order="hclust", hclust.method="median")

corrplot.color(corr, order="alphabet", col.grid="white")


## like Chinese wiqi
corrplot(corr, bg="gold2", col = c("white","black"), order="PCA", outline=TRUE, addcolorkey=FALSE)

## note the parameter outline
corrplot(corr, col = c("white","black"), order="PCA",  addcolorkey=FALSE, outline=TRUE)

## visualize the result of test for correlation
corrplot.mtest(dat=mtcars, order="hclust", rect.hc=3)
corrplot.mtest(dat=mtcars, plotC=TRUE)
corrplot.mtest(dat=mtcars, method="square", non_corr.method = "blank", addgrid=FALSE)
corrplot.mtest(dat=mtcars, plotC=TRUE, method="square", addgrid=FALSE)

##the input matrix is not square
corrplot(corr[1:8,], method="shade")
corrplot(corr[1:8,], method="number",  type="upper",diag=FALSE, addgrid=TRUE, pos.text="diag")
corrplot(corr[,1:8], method="number",  type="upper",diag=FALSE, addgrid=TRUE, pos.text="diag")
}
}

\keyword{hplot}% __ONLY ONE__ keyword per line
