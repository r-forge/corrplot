cor.mtest <-
function(mat, conf.level = 0.95, method = "pearson", ...){
	mat <- as.matrix(mat)
	n <- ncol(mat)
	p.mat <- low.mat <- upp.mat <- matrix(NA, n, n)    
	diag(p.mat) <- 0
	diag(low.mat) <- diag(upp.mat) <- 1
	for(i in 1:(n-1)){
		for(j in (i+1):n){
			tmp <- cor.test(mat[,i], mat[,j], )
			p.mat[i,j] <- p.mat[j,i] <- tmp$p.value
			low.mat[i,j] <- low.mat[j,i] <- tmp$conf.int[1]
			upp.mat[i,j] <- upp.mat[j,i] <- tmp$conf.int[2]
		}
	}
	return(list(p.mat, low.mat, upp.mat))
}

