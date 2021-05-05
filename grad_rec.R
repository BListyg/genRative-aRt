library(plotrix)

# par(mfrow = c(4,1))

plot(0:21,type = 'n',xaxt = 'n', yaxt = 'n', frame.plot = F, xlab = NA, ylab = NA)

grad_rec <- function(xl, xr, yb, yt, j = 'red'){
  
  gradient.rect(xleft = xl, ybottom = yb, xright = xr, ytop = yt , 
                col = smoothColors(j,100,'white'),
                gradient = 'y',border = NA)
  
  for(i in 1:10){
    gradient.rect(xleft = xl + i, ybottom = yb + i, xright = xr+ i, ytop = yt , 
                  col = smoothColors(j,100,'white'),
                  gradient = 'y',border = NA)
  }
  
  for(i in xr:yt){
    gradient.rect(xleft = i, ybottom = yb, xright = i + 1, ytop = i,
                  col = smoothColors("white",100,j),
                  gradient = 'y',border = NA)
  }
  
}

col_seq <- seq(0,10,2)

for(i in col_seq){
  
  c_ = smoothColors("white",length(col_seq),'turquoise')
  
  grad_rec(xl = 0+i, xr = 1+i, yb = 0+i, yt = 10+i, j = c_[which(i == col_seq)])
  
}
