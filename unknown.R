library(dplyr)
library(igraph)
library(reshape2)
library(plotrix)

g3 <- c(
  '#CC99C9',
  '#9EC1CF',
  '#9EE09E',
  '#FDFD97',
  '#FEB144',
  '#FF6663'
)

lightning <- function(){
  
  y <- function(x, n){x + n}
  
  d <- expand.grid(X = c(-20:20), N = c(-20:20))
  
  points <- cbind(d, y = mapply(FUN = y, x = d$X, n = d$N)) %>% 
    data.frame() %>% 
    split(.$N) %>% 
    lapply(., FUN = function(x) x[sample(x = c(1:nrow(x)), size = 2, replace = F),]) %>% 
    lapply(., FUN = function(x) select(x, -N)) 
  
  points[[1]] %>% plot(ylim = c(-50,50), xlim = c(-50,50), type = 'l', col = sample(x = g3, size = 1), xaxt = 'n', yaxt = 'n', frame.plot = F, xlab = NA, ylab = NA, lwd =sample(c(1,2),1))
  
  for(i in 2:length(points)){
    points <- cbind(d, y = mapply(FUN = y, x = d$X, n = d$N)) %>% 
      data.frame() %>% 
      split(.$N) %>% 
      lapply(., FUN = function(x) select(x, -N)) %>% 
      lapply(., FUN = function(x) x[sample(x = c(1:nrow(x)), size = 2, replace = F),]) 
    
    lines(points[[i]], type = 'l', col = sample(x = g3, size = 1), lwd =sample(c(1,2),1))
  }
}
wave <- function(){t <- seq(1,10,.25)

plot(
  t,
  sample(c(3,5,7),1)*sin(t)+10,
  type = sample(x = 'l',1),pch = 0,
  col = sample(x = g3, size = 1), 
  xaxt = 'n', yaxt = 'n', 
  frame.plot = F, xlab = NA, 
  ylab = NA, lwd =sample(c(0.5,1,2,3),1),
  ylim = c(-20,20),
  xlim = c(0,11))

for(i in seq(1,10,.5)){
  points(
    t,
    sample(c(3,5,7),1)*sin(t) - 1.5*i,
    type = sample(x = 'l',1),pch = 0,
    col = sample(x = g3, size = 1), 
    xaxt = 'n', yaxt = 'n', 
    xlab = NA, 
    ylab = NA, lwd =sample(c(0.5,1,2,3),1))
}

for(i in seq(1,10,.5)){
  lines(
    t,
    sample(c(3,5,7),1)*sin(t) + 1.5*i,
    type = sample(x = 'l',1),pch = 0,
    col = sample(x = g3, size = 1), 
    xaxt = 'n', yaxt = 'n', 
    xlab = NA, 
    ylab = NA, lwd =sample(c(0.5,1,2,3),1))
}

}
planets <- function(){library(igraph)
  
  g <- igraph::erdos.renyi.game(n = sample(seq(30,80,2),1), p.or.m = 0.01) 
  
  V(g)$size <- sample(x = c(1:49),
                      size = length(V(g)),
                      replace = T)
  
  V(g)$color <- sample(x = g3, size = length(V(g)),replace = T)
  
  plot(g, edge.color = NA, vertex.label = NA, layout = layout_with_fr, vertex.shape = 'circle',
       vertex.frame.color=NA)
  
}
grid <- function(){library(igraph)
  
  g <- igraph::erdos.renyi.game(n = sample(seq(40,80,2),1), p.or.m = 0.01) 
  
  V(g)$size <- sample(x = c(1:100),
                      size = length(V(g)),
                      replace = T)
  
  V(g)$color <- sample(x = g3, size = length(V(g)),replace = T)
  
  plot(g, 
       edge.color = NA, vertex.label = NA, 
       layout = layout_with_fr, vertex.shape = 'rectangle',
       vertex.frame.color='black')

}
circles <- function(){
  
  plot(1:3,type="n",frame.plot = F,xaxt = 'n',yaxt = 'n',ylab=NA,xlab=NA)
  
  # d <- expand.grid(seq(0,5,1),seq(0,10,1))
  
  s_d <- expand.grid(2,2) 
  
  draw.arc(s_d$Var1,
           s_d$Var2,
           radius = 1:10/-25,
           deg1 = 1:10*-25,
           col = sample(g3,10,replace = T),
           lwd = sample(c(1,2,3),1))
  
  draw.arc(s_d$Var1 + .5,
           s_d$Var2,
           radius = 1:10/25,
           deg1 = 1:10*-25,
           col = sample(g3,10,replace = T),
           lwd = sample(c(1,2,3),1))
  
}

plot_order <- latinsquare(len = 5) %>% c

generative_plot <- function(x){
  if(x == 1){
    wave()
  }
  else if(x == 2){
    planets()
  }
  else if(x ==3){
    grid()
  }
  else if(x == 4){
    lightning()
  }
  else if(x == 5){
    circles()
  }
}

tiff("~/Desktop/test.tiff", units="in", width=10, height=10, res=1000)

par(bg = 'beige',mai = c(0.01, 0.01, 0.01, 0.01), mfrow = c(5,5))

sapply(plot_order, generative_plot)

dev.off()

