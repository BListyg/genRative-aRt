# Tidal Wave

plot(0:10, type = 'n',xaxt = 'n',yaxt='n',ylab=NA,xlab=NA,frame.plot = F)

d <- expand.grid(c(1:10),c(1:10))

mapply(FUN = function(x,y){draw.arc(x, 
         y, 
         1:10/sample(c(-10,sample(c(10,11,12),1)),1),
         deg2 = 1:10*sample(c(-10,10),1), 
         col = "blue",lwd = sample(c(0.5,1,2),1),lty='dotted')
}, x = d$Var1, y= d$Var2)
