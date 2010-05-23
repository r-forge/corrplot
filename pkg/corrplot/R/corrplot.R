#' Represents Correlation circles
#'
#' @author Taiyun Wei
#' @param corr, Correlation matrix to represent
#' @param col, vector the fill color of circles from -1 to 1
#' @param bg, background color of graph
#' @param outline,
#' @param cex, numeric, for the variable names
#' @param title, title of the graph
#' @param order, the mothod of reorder the variables
#' @param method,
#' @param addgrid,
#' @param col.grid,
#' @param diag,
#' @param addnum,
#' @param col.num,
#' @param lwd.shade,
#' @param col.shade,
#' @param ... extra parameters, currenlty ignored
#  last modified by Taiyun 2010-5-23 0:20:11
corrplot <- function(corr, method = c("circle", "square", "ellipse", "number", 
                                "pie", "shade", "color"),
		type = c("full", "lower", "upper"), 
		order = c("original", "alphabet", "PCA", "hclust"),
		hclust.method = c("complete", "ward", "single", "average", 
                       "mcquitty", "median", "centroid"), 
		rect.hc = NA, rect.col="black", rect.lwd = 2,

		col = colorRampPalette(c("#67001F", "#B2182B", "#D6604D", "#F4A582", 
				"#FDDBC7", "#FFFFFF", "#D1E5F0", "#92C5DE", 
				"#4393C3", "#2166AC", "#053061"))(200),
             
		outline = FALSE, cex = 1, title = "", bg = "white",
		addcolorkey = c("right", "bottom","no"), colorkey=c( "min2max","-1to1"),
		colorkey.label_len=11, colorkey.cex =1, ckRatio = 0.15, mar = c(0,0,0,0),
                        
		addtextlabel = TRUE, pos.text = c("lt","ld","td","d"), 
		col.text = "red", offset = 0.4, family.text = NULL,
                        
		shade.method = c("negtive", "positive", "all"),
		lwd.shade = 1, col.shade = "white", 
                        
                        
		addgrid = TRUE, col.grid = "gray", diag = TRUE,   
		addnum = FALSE,  col.num = NULL, 
                        
		corr.mtest = FALSE, p.mat = NULL, conf.level = 0.95,
		non_corr.method = c("pch","blank"),
		pch = 4, col.pch = "red",  cex.pch = 1,
		plotConf = FALSE, low.mat = NULL, upp.mat = NULL){
    
    if (is.null(corr)) 
        return(invisible())

    if (!is.matrix(corr) || min(corr) < -1 - .Machine$double.eps
         || max(corr) > 1 + .Machine$double.eps)
        stop("Need a correlation matrix!")
    n <- nrow(corr)
    m <- ncol(corr)
    min.nm <- min(n,m)
    
    order <- match.arg(order)
    hclust.method <- match.arg(hclust.method)
    myorder <- function(corr, order, ...){
        if((!n==m)&(!order=="original")){
        	stop("The matrix must be squre if reorder variables!")
        }

        ## reorder the variables using principal component analysis
        if (order == "PCA") {
          x.eigen <- eigen(corr)$vectors[, 1:2]
          e1 <- x.eigen[, 1]
          e2 <- x.eigen[, 2]
          alpha <- ifelse(e1 > 0, atan(e2/e1), atan(e2/e1) + pi)
          ord <- order(alpha)
        }
        
        ## reorder the variables in alphabet ordering
        if(order =="alphabet"){
        	ord <- sort(rownames(corr))
        }
        
        ## reorder the variables using hclhust
        if(order == "hclust"){
        	ord <- order.dendrogram(as.dendrogram(hclust(as.dist(1-corr), 
					method = hclust.method, ...)))
        }   
        
        return(ord)
    }
	
	ord <- NA
    if(!order=="original"){
        ord <- myorder(corr, order=order)
        corr <- corr[ord,ord]
    }
    ## set up variable names
    if (is.null(rownames(corr))) 
        rownames(corr) <- 1:n
    if (is.null(colnames(corr))) 
        colnames(corr) <- 1:m
 
    ## assign fill color
    assign.color <- function(col, corrVector){
      nc <- length(col)
      nm <- length(corrVector)
      if(nc==1)
          syms_bg <- rep(col, nm)
      else{
          ff <- seq(-1,1, length=nc+1) 
          bg2 = rep(0, nm)
           for (i in 1:(nm)){
              bg2[i] <- rank(c(ff[2:nc], corrVector[i]), 
                              ties.method = "first")[nc]
          }
          syms_bg <- col[bg2]
      }
      return(syms_bg)
    }
    
    method <- match.arg(method)
    type <- match.arg(type)
      
    getMy.dat <- function(mat){
      x <- matrix(1:n*m, n, m)
      tmp <- mat
      if(type=="upper")  tmp[row(x)>col(x)] <- Inf
      if(type=="lower")  tmp[row(x)<col(x)] <- Inf
      if(type=="full")   tmp <- tmp
      if(!diag)          diag(tmp) <- Inf
      
      myDat <- tmp[is.finite(tmp)]        
      ind  <- which(is.finite(tmp),arr.ind = TRUE)
      myPos <- ind
      myPos[,1] <-  ind[,2]
      myPos[,2] <- -ind[,1] + 1 + n
      return(list(myPos, myDat))
    }
    
    mypos  <- getMy.dat(corr)[[1]]
    n2 <- max(mypos[,2]); n1 <- min(mypos[,2])
    nn <- n2 -n1
    myrownames <- as.character(rownames(corr)[(n+1-n2):(n+1-n1)])
    m2 <- max(mypos[,1]); m1 <- min(mypos[,1])
    mm <- m2 -m1
    mycolnames <- as.character(colnames(corr)[m1:m2])
    mycorr <- getMy.dat(corr)[[2]]
    len.mycorr <- length(mycorr)
    col.fill <- assign.color(col, mycorr)
    
    if(outline)
    	col.border <- "black"
    if(!outline)
    	col.border <- col.fill 
    	
    ## calculate label-text width approximately
    par(mar = mar, bg = "white")
    plot.new()
    plot.window(c(m1-0.5, m2+0.5), c(n1-0.5, n2+0.5), asp = 1)
    xlabwidth <- max(strwidth(myrownames, cex = cex))
    ylabwidth <- max(strwidth(mycolnames, cex = cex))
    S1 <- nn*mm   
	
	pos.text <- match.arg(pos.text)  
	if(!addtextlabel|pos.text=="d") xlabwidth <- xlabwidth <- 0
    ## set up an empty plot with the appropriate dimensions
	
	addcolorkey <- match.arg(addcolorkey)
	colorkey <- match.arg(colorkey)
    if(addcolorkey=="no"){
      plot.window(c(-xlabwidth + m1 - 0.5, m2 + 0.5), 
                  c(n1 - 0.5, n2 + 0.5 + ylabwidth),
                asp = 1, xlab="", ylab="")
      S2 <- (mm + xlabwidth)*(mm + ylabwidth)##area of figure
    }
    if(addcolorkey=="right"){
      plot.window(c(-xlabwidth + m1 - 0.5, m2 + 0.5 + mm*ckRatio), 
                  c(n1 - 0.5, n2 + 0.5 + ylabwidth),
                  asp = 1, xlab="", ylab="")
      S2 <- (mm + xlabwidth+ mm*ckRatio)*(nn + ylabwidth)
    }
    if(addcolorkey=="bottom"){
      plot.window(c(-xlabwidth + m1 - 0.5, m2 + 0.5), 
                  c(n1 - 0.5 - nn*ckRatio, n2 + 0.5 + ylabwidth),
                  asp = 1, xlab="", ylab="")
      S2 <- (mm + xlabwidth+ mm*ckRatio)*(nn + ylabwidth)
    }    
    ## background color
    symbols(mypos, add = TRUE, inches = FALSE, 
            squares = rep(1, len.mycorr), bg = bg, fg = bg)
    	
    ## circle
    if(method=="circle"){
    	symbols(mypos, add = TRUE,  inches = FALSE, bg = col.fill, 
         circles = 0.9*abs(mycorr)^0.5/2, fg = col.border)
    }
    
    ## ellipse
    if(method=="ellipse"){
    	ell.dat <- function(rho, length = 100){
    		k <- seq(0, 2*pi, length=length)
    		x <- cos(k + acos(rho)/2)/2
    		y <- cos(k - acos(rho)/2)/2
    		return(rbind(x,y))
    	}
    	myEll.dat <- lapply(mycorr, ell.dat)
    	for(i in 1:len.mycorr){
    		polygon(t(myEll.dat[[i]]*0.85+mypos[i,]), 
    		        border = col.border[i], col = col.fill[i])
    	}
    	
    }
    
    ## number
    if(method=="number"){
    	 text(mypos[,1], mypos[,2], font = 2, round(100 * mycorr), col = col.fill)
    }
      
    ## pie
    if(method=="pie"){
    	symbols(mypos, add = TRUE, inches = FALSE,  
    	        circles = rep(0.5, len.mycorr)*0.85)
      pie.dat <- function(theta, length = 100){
      	k <- seq(pi/2, pi/2 - theta, length = 0.5*length*abs(theta)/pi)
    		x <- c(0, cos(k)/2, 0)
    		y <- c(0, sin(k)/2, 0)
    		return(rbind(x,y))
    	}
    	
    	myPie.dat <- lapply(mycorr*2*pi, pie.dat)
    	for(i in 1:len.mycorr){
    		polygon(t(myPie.dat[[i]]*0.85+mypos[i,]), 
    		        border = "black", col = col.fill[i])
    	}
    		
    }
    	
    ## shade
    if(method=="shade"){
    	shade.method <- match.arg(shade.method)
    	symbols(mypos, add = TRUE, inches = FALSE, squares = rep(1, len.mycorr), 
				bg = col.fill, fg = "white")
        shade.dat <- function(w){
			x <- w[1];  y <- w[2];  rho <- w[3] 
			x1 <- x - 0.5
			x2 <- x + 0.5
			y1 <- y - 0.5
			y2 <- y + 0.5
			dat <- NA
      	
			if(shade.method=="positive"||shade.method=="all"){
				if(rho >0)
					dat <- cbind(c(x1, x1, x), c(y, y1, y1),
      		               c(x, x2, x2), c(y2, y2 ,y))
			}
			if(shade.method=="negtive"||shade.method=="all"){
				if(rho <0)
					dat <- cbind(c(x1, x1, x), c(y, y2, y2),
      		               c(x, x2, x2), c(y1, y1 ,y))
			}
			
      	return(t(dat))
      }
    
	pos_corr <- rbind(cbind(mypos, mycorr))
	pos_corr2 <- split(pos_corr, 1:nrow(pos_corr))
    myShade.dat <- matrix(na.omit(unlist(lapply(pos_corr2,  shade.dat))),byrow=TRUE, ncol=4) 
	segments(myShade.dat[,1], myShade.dat[,2], 
    			myShade.dat[,3], myShade.dat[,4], 
    			col = col.shade, lwd = lwd.shade)
    } 
    
    ##square
    if(method=="square"){
    	symbols(mypos, add = TRUE, inches = FALSE, 
         squares = abs(mycorr)^0.5, bg = col.fill, fg = col.border)
    }
    
    ##  color
    if(method=="color"){
    	symbols(mypos, add = TRUE, inches = FALSE, 
              squares = rep(1, len.mycorr), bg = col.fill, fg = col.fill)
    }
	
       
    if(addcolorkey!="no"){
		##Max <- ifelse(diag==TRUE, 1, Max=max(mycorr))
		if(colorkey=="min2max") corrVec <- seq(min(mycorr), max(mycorr), length=200)
		if(colorkey=="-1to1") corrVec <- seq(-1, 1, length=200)
		labelcolor <- assign.color(col, corrVec)
		num_col <- data.frame(corrVec, labelcolor)
		if(addcolorkey=="right"){
			vertical <- TRUE
			xlim <- c(m2 + 0.5 + mm*0.02, m2 + 0.5 + mm*ckRatio)
			ylim <- c(n1-0.5, n2+0.5)
		}
		
		if(addcolorkey=="bottom"){
			vertical <- FALSE
			xlim <- c(m1-0.5, m2+0.5)
			ylim <- c(n1 - 0.5 - nn*ckRatio, n1 - 0.5- nn*0.02)
		}
		colorkey(num_col=num_col, xlim=xlim, ylim=ylim, vertical=vertical, 
				label_len=colorkey.label_len, cex=colorkey.cex, col.text="black")
	}
    
    ## add variable names and title
    if(addtextlabel){	  
        cex2 <- cex * S1/S2
        ylabwidth2 <- strwidth(myrownames, cex = cex2)
        xlabwidth2 <- strwidth(mycolnames, cex = cex2) 
        
        pos.xlabel <- cbind(m1:m2, n2 + 0.5 + xlabwidth2/2)
        pos.ylabel <- cbind(m1 - 0.5, n2:n1)
        

        if(pos.text=="td")
            pos.ylabel <- cbind(m1:(m1+nn)-0.5, n2:n1)     	
        if(pos.text=="ld")
            pos.xlabel <- cbind(m1:m2, n2:(n2-mm) + 0.5 + xlabwidth2/2)
        if(pos.text=="d"){
			if(type!="full") stop("type should be \"full\" if pos.text is \"d\"")
			pos.ylabel <- cbind(m1:(m1+nn)-0.5, n2:n1)
			pos.ylabel <- pos.ylabel[1:min(n,m),]
			text(pos.ylabel[,1]+0.5, pos.ylabel[,2], mycolnames[1:min(n,m)],
                 col = col.text, cex = cex * S1/S2, family=family.text)
		} else {
			text(pos.xlabel[,1], pos.xlabel[,2], mycolnames, srt = 90, family=family.text,
                 col = col.text, cex = cex * S1/S2, pos=3, offset=offset)
			text(pos.ylabel[,1], pos.ylabel[,2], myrownames, family=family.text,
                 col = col.text, cex = cex * S1/S2, pos=2, offset=offset)
		}
    }
    
    title(title)
    
    if(corr.mtest){
    	if(is.null(p.mat)) stop("Need p.mat!")
    	if(!order=="original")
    	    p.mat <- p.mat[ord, ord]
      pos.pNew  <- getMy.dat(p.mat)[[1]]
    	pNew      <- getMy.dat(p.mat)[[2]]
    	non_corr.method <- match.arg(non_corr.method)
      
      ind.p <- which(pNew > (1 - conf.level))
    	if(non_corr.method=="pch"){
        	points(pos.pNew[,1][ind.p], pos.pNew[,2][ind.p],
        	       pch = pch, col = col.pch, cex = 3, lwd=2)
      }
      if(non_corr.method=="blank"){
      	  symbols(pos.pNew[,1][ind.p], pos.pNew[,2][ind.p], inches = FALSE,
      	          squares = rep(1, length(pos.pNew[,1][ind.p])), 
      	          fg = NA, bg = "white", add = TRUE)
      }
      if(plotConf){
      	  if(is.null(low.mat)||is.null(upp.mat)) 
      	      stop("Need low.mat and upp.mat!")
      	  if(!order=="original"){
              low.mat <- low.mat[ord,ord]
              upp.mat <- upp.mat[ord,ord]
          }
          pos.lowNew  <- getMy.dat(low.mat)[[1]]
    	    lowNew      <- getMy.dat(low.mat)[[2]]
          pos.uppNew  <- getMy.dat(upp.mat)[[1]]
    	    uppNew      <- getMy.dat(upp.mat)[[2]]
    	    if(!(method=="circle"||method=="square"))
    	       stop("method shoud be circle or square if draw confidence interval!")
    	    ind2 <- which(pNew <= (1 - conf.level))
    	    if(method=="circle"){
    	       symbols(pos.uppNew[,1][ind2], pos.uppNew[,2][ind2],
    	               add = TRUE,  inches = FALSE, bg = col.fill[ind2], 
                     circles = abs(uppNew[ind2])^0.5/2, fg = col.fill[ind2])
    	       symbols(pos.lowNew[,1][ind2], pos.lowNew[,2][ind2], 
    	               add = TRUE, inches = FALSE, bg = "white", 
                     circles = abs(lowNew[ind2])^0.5/2, fg = col.fill[ind2])
          }
          if(method=="square"){
    	       symbols(pos.uppNew[,1][ind2], pos.uppNew[,2][ind2], 
    	               add = TRUE, inches = FALSE, bg = col.fill[ind2], 
    	               fg = col.fill[ind2],
                     squares = abs(uppNew[ind2])^0.5)
    	       symbols(pos.lowNew[,1][ind2], pos.lowNew[,2][ind2],  
    	               add = TRUE, inches = FALSE, bg = "white", 
    	               fg = col.fill[ind2], 
                     squares = abs(lowNew[ind2])^0.5)             
          }
      }
    }

    ## add numbers
    if(addnum&(!method == "number")){
    	  text(mypos[,1], mypos[,2], round(100 * mycorr), col = col.num)
    }
    ## add grid
    if(addgrid){
    	  symbols(mypos, add=TRUE, inches = FALSE,  bg = NA,
    	     squares = rep(1, len.mycorr), fg = col.grid)
    }
	if(type=="full") rect(m1-0.5, n1-0.5, m2+0.5, n2+0.5, border=col.grid)
	##  draws rectangles 
	if(!is.na(rect.hc)){
		tree <- hclust(as.dist(1-corr), method = hclust.method)
		hc <- cutree(tree, k = rect.hc)
		clustab <- table(hc)[unique(hc[tree$order])]
		cu <- c(0, cumsum(clustab))
		mat <- cbind(cu[-(rect.hc + 1)] + 0.5, n - cu[-(rect.hc + 1)] + 0.5, 
		              cu[-1] + 0.5, n - cu[-1] + 0.5)
		rect(mat[,1], mat[,2], mat[,3], mat[,4], border = rect.col, lwd = rect.lwd)
	}
	
	invisible(ord)
} ## end
