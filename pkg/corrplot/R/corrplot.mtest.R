corrplot.mtest <-
function(dat = NULL, corr = NULL, p.mat = NULL, conf.level = 0.95,
		 plotConf = FALSE, low.mat = NULL, upp.mat = NULL, 
    	 non_corr.method = c("pch","blank"), method = c("circle","square"),
    	 pch = 4, col.pch = "black",...){
				
	if(is.null(corr)&is.null(dat)&is.null(p.mat))
		stop("need both corr and p.mat!")
	if(plotConf&is.null(low.mat)&is.null(upp.mat)&is.null(dat))
		stop("need low.mat and upp.mat!")
		
    cor.mtest <- function(mat, conf.level = NULL){
	  mat <- as.matrix(mat)
	  n <- ncol(mat)
	  p.mat <- low.mat <- upp.mat <- matrix(NA, n, n)    
	  diag(p.mat) <- 0
	  diag(low.mat) <- diag(upp.mat) <- 1
	  for(i in 1:(n-1)){
		  for(j in (i+1):n){
			  tmp <- cor.test(mat[,i], mat[,j], conf.level = conf.level)
			  p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
			  low.mat[i,j] <- low.mat[j,i] <- tmp$conf.int[1]
			  upp.mat[i,j] <- upp.mat[j,i] <- tmp$conf.int[2]
		  }
	  }
	  return(list(p.mat, low.mat, upp.mat))
    }
	
	if(!is.null(dat)){
		if(!is.null(c(corr, p.mat, low.mat, upp.mat)))
			warning("dat is specialized, so corr, p.mat, low.mat, upp.mat will be omitted!\n")
		mcor <- cor.mtest(mat = dat,  conf.level = conf.level)
		corr <- cor(dat, use = "pair")
		p.mat <- mcor[[1]]
		low.mat <- mcor[[2]]
		upp.mat <- mcor[[3]]
	}
	
	method <- match.arg(method)
	non_corr.method <- match.arg(non_corr.method)
	
	corrplot(corr = corr, p.mat = p.mat, low.mat = low.mat, upp.mat = upp.mat,  corr.mtest = TRUE,
			plotConf = plotConf, method = method, non_corr.method = non_corr.method,
			pch = pch, col.pch = col.pch, ...)
} ## end

