    
colorkey <- 
function(num_col, xlim=c(0,1), ylim=c(0,1), 
vertical=TRUE, precise=FALSE,
label_len=11, cex=1, col.text="black",  offset=0.5, 
width.colorkey.ratio = 0.4, length.segment.ratio = 0.1,
add=TRUE, axes = FALSE,...)
{ ### begin colorkey	

if(!add)
plot(0, 0, type = "n",xlim = xlim, ylim = ylim, axes = axes,...)

num_col[,2] <- as.character(num_col[,2])	
xgap <- diff(xlim)
ygap <- diff(ylim)
len  <- length(num_col[,1])
rat1 <- width.colorkey.ratio 
rat2 <- width.colorkey.ratio + length.segment.ratio

if(is.numeric(num_col[,1])){
label <- seq(min(num_col[,1]), max(num_col[,1]), length = label_len)
label <- round(label, 2)
} else {
label <- as.character(num_col[,1])
label_len <- length(label)
}


if(vertical){
yyy <- seq(ylim[1], ylim[2], length=len+1)
rect(rep(xlim[1], len),  yyy[1:len],
	 rep(xlim[1] +xgap*rat1, len), yyy[-1], col = num_col[,2], 
	 border = num_col[,2])
rect(xlim[1], ylim[1], xlim[1] +xgap*rat1, ylim[2], border="black")

if(precise) 
	pos.ylabel <- seq(ylim[1]+ygap/label_len/2, ylim[2]-ygap/label_len/2, length=label_len)
if(!precise) 
	pos.ylabel <- seq(ylim[1], ylim[2], length=label_len)
pos.xlabel <- rep(xlim[1] +xgap*rat2, length(pos.ylabel))
segments(rep(xlim[1] +xgap*rat1, label_len), pos.ylabel,
    	 rep(xlim[1] +xgap*rat2, label_len), pos.ylabel)
text(pos.xlabel, pos.ylabel, label, cex = cex, 
	 pos = 4, col=col.text, offset=offset)
}

if(!vertical){

xxx <- seq(xlim[1], xlim[2], length=len+1)
rect(xxx[1:len], rep(ylim[2] - rat1*ygap, len),
	 xxx[-1], rep(ylim[2], len), col = num_col[,2], 
	 border = num_col[,2])
rect(xlim[1], ylim[2] - rat1*ygap, xlim[2], ylim[2], border="black")


if(!precise)
pos.xlabel <- seq(xlim[1], xlim[2], length=label_len)

if(precise)
pos.xlabel <- seq(xlim[1]+xgap/label_len/2, xlim[2]-xgap/label_len/2, length=label_len)

pos.ylabel <- rep(ylim[2] - ygap*rat2, length(pos.xlabel))
segments(pos.xlabel , ylim[2] - ygap*rat1,
    	 pos.xlabel , ylim[2] - ygap*rat2)
text(pos.xlabel, pos.ylabel, label, cex = cex, 
	 pos = 1, col=col.text, offset=offset)
}	
} ## end colorkey


