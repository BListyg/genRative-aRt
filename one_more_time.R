library(dplyr)
library(igraph)
library(reshape2)
library(plotrix)

g3 <- c(
  '#ed7a31',
  '#FFC511',
  '#31ed94',
  '#6ac773',
  '#14d4ae')

lightning <- function(){
  
  y <- function(x, n){1.5*x + n}
  
  d <- expand.grid(X = seq(-30,30,1), N = seq(-30,30,1))
  
  points <- cbind(d, y = mapply(FUN = y, x = d$X, n = d$N)) %>% 
    data.frame() %>% 
    split(.$N) %>% 
    lapply(., FUN = function(x) x[sample(x = c(1:nrow(x)), size = 2, replace = F),]) %>% 
    lapply(., FUN = function(x) select(x, -N)) 
  
  points[[1]] %>% plot(ylim = c(-50,50), xlim = c(-50,50), type = 'l', col = sample(x = g3, size = 1), xaxt = 'n', yaxt = 'n', frame.plot = F, xlab = NA, ylab = NA, lwd =sample(c(3,4,5,6,7,8),1),lend='butt')
  
  for(i in 2:length(points)){
    points <- cbind(d, y = mapply(FUN = y, x = d$X, n = d$N)) %>% 
      data.frame() %>% 
      split(.$N) %>% 
      lapply(., FUN = function(x) select(x, -N)) %>% 
      lapply(., FUN = function(x) x[sample(x = c(1:nrow(x)), size = 2, replace = F),]) 
    
    lines(points[[i]], type = 'l', col = sample(x = g3, size = 1), lwd =sample(c(3,4,5,6,7,8),1),lend='butt')
  }
}
wave <- function(){t <- seq(-17,17,.1)

plot(
  t,
  sample(c(3,5,7),1)*sin(t),
  type = sample(x = 'l',1),pch = 0,
  col = sample(x = g3, size = 1), 
  xaxt = 'n', yaxt = 'n', 
  frame.plot = F, xlab = NA, 
  ylab = NA, lwd =sample(c(0.5,1,2,3,4,5,8),1),
  ylim = c(-20,20),
  xlim = c(0,11),lend='butt')

for(i in seq(1,10,.5)){
  points(
    t,
    sample(c(3,5,7),1)*sin(t) - .5*i,
    type = sample(x = 'l',1),pch = 0,
    col = sample(x = g3, size = 1), 
    xaxt = 'n', yaxt = 'n', 
    xlab = NA, 
    ylab = NA, lwd =sample(c(0.5,1,2,3,4,5,8),1),lend='butt')
}

for(i in seq(1,10,.5)){
  lines(
    t,
    sample(c(3,5,7),1)*sin(t) + .5*i,
    type = sample(x = 'l',1),pch = 0,
    col = sample(x = g3, size = 1), 
    xaxt = 'n', yaxt = 'n', 
    xlab = NA, 
    ylab = NA, lwd =sample(c(0.5,1,2,3,4,5,8),1),lend='butt')
}

}
planets <- function(){library(igraph)
  
  g <- igraph::erdos.renyi.game(n = sample(seq(60,80,2),1), p.or.m = 0.01) 
  
  V(g)$size <- sample(x = c(1:49),
                      size = length(V(g)),
                      replace = T)
  
  V(g)$color <- sample(x = g3, size = length(V(g)),replace = T)
  
  plot(g, edge.color = NA, vertex.label = NA, layout = layout_with_fr, vertex.shape = 'circle',
       vertex.frame.color=sample(x = g3, size = length(V(g)),replace = T))
  
}
grid <- function(){library(igraph)
  
  g <- igraph::erdos.renyi.game(n = sample(seq(40,100,2),1), p.or.m = 0.01) 
  
  V(g)$size <- sample(x = c(1:80),
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
  
  draw.arc(s_d$Var1 - .5,
           s_d$Var2,
           radius = 1:10/25,
           deg1 = 1:10*-25,
           col = sample(g3,10,replace = T),
           lwd = sample(c(3,4,5),1))
  
  draw.arc(s_d$Var1,
           s_d$Var2,
           radius = 1:10/-25,
           deg1 = 1:10*-25,
           col = sample(g3,10,replace = T),
           lwd = sample(c(3,4,5),1))
  
  draw.arc(s_d$Var1 + .5,
           s_d$Var2,
           radius = 1:10/25,
           deg1 = 1:10*-25,
           col = sample(g3,10,replace = T),
           lwd = sample(c(3,4,5),1))
  
}
wavy <- function(){
  
  g<-erdos.renyi.game(n = 100,p.or.m = c(1/100),type = 'gnp')
  
  E(g)$color <- sample(g3,length(E(g)),replace = T)
  
  plot(g,vertex.size=page.rank(g)$vector*10,vertex.label=NA,layout=layout_as_tree,edge.arrow.size=0,arrow.color=NA,vertex.color=NA,arrow.curved=0,edge.width=2,edge.curved=sample(c(0.25,0.5),1),vertex.shape = 'none')
  
}

latinsquare <- function(len, reps=1, seed=NA, returnstrings=FALSE) {
  
  # Save the old random seed and use the new one, if present
  if (!is.na(seed)) {
    if (exists(".Random.seed"))  { saved.seed <- .Random.seed }
    else                         { saved.seed <- NA }
    set.seed(seed)
  }
  
  # This matrix will contain all the individual squares
  allsq <- matrix(nrow=reps*len, ncol=len)
  
  # Store a string id of each square if requested
  if (returnstrings) {  squareid <- vector(mode = "character", length = reps) }
  
  # Get a random element from a vector (the built-in sample function annoyingly
  #   has different behavior if there's only one element in x)
  sample1 <- function(x) {
    if (length(x)==1) { return(x) }
    else              { return(sample(x,1)) }
  }
  
  # Generate each of n individual squares
  for (n in 1:reps) {
    
    # Generate an empty square
    sq <- matrix(nrow=len, ncol=len) 
    
    # If we fill the square sequentially from top left, some latin squares
    # are more probable than others.  So we have to do it random order,
    # all over the square.
    # The rough procedure is:
    # - randomly select a cell that is currently NA (call it the target cell)
    # - find all the NA cells sharing the same row or column as the target
    # - fill the target cell
    # - fill the other cells sharing the row/col
    # - If it ever is impossible to fill a cell because all the numbers
    #    are already used, then quit and start over with a new square.
    # In short, it picks a random empty cell, fills it, then fills in the 
    # other empty cells in the "cross" in random order. If we went totally randomly
    # (without the cross), the failure rate is much higher.
    while (any(is.na(sq))) {
      
      # Pick a random cell which is currently NA
      k <- sample1(which(is.na(sq)))
      
      i <- (k-1) %% len +1       # Get the row num
      j <- floor((k-1) / len) +1 # Get the col num
      
      # Find the other NA cells in the "cross" centered at i,j
      sqrow <- sq[i,]
      sqcol <- sq[,j]
      
      # A matrix of coordinates of all the NA cells in the cross
      openCell <-rbind( cbind(which(is.na(sqcol)), j),
                        cbind(i, which(is.na(sqrow))))
      # Randomize fill order
      openCell <- openCell[sample(nrow(openCell)),]
      
      # Put center cell at top of list, so that it gets filled first
      openCell <- rbind(c(i,j), openCell)
      # There will now be three entries for the center cell, so remove duplicated entries
      # Need to make sure it's a matrix -- otherwise, if there's just 
      # one row, it turns into a vector, which causes problems
      openCell <- matrix(openCell[!duplicated(openCell),], ncol=2)
      
      # Fill in the center of the cross, then the other open spaces in the cross
      for (c in 1:nrow(openCell)) {
        # The current cell to fill
        ci <- openCell[c,1]
        cj <- openCell[c,2]
        # Get the numbers that are unused in the "cross" centered on i,j
        freeNum <- which(!(1:len %in% c(sq[ci,], sq[,cj])))
        
        # Fill in this location on the square
        if (length(freeNum)>0) { sq[ci,cj] <- sample1(freeNum) }
        else  {
          # Failed attempt - no available numbers
          # Re-generate empty square
          sq <- matrix(nrow=len, ncol=len)
          
          # Break out of loop
          break;
        }
      }
    }
    
    # Store the individual square into the matrix containing all squares
    allsqrows <- ((n-1)*len) + 1:len
    allsq[allsqrows,] <- sq
    
    # Store a string representation of the square if requested. Each unique
    # square has a unique string.
    if (returnstrings) { squareid[n] <- paste(sq, collapse="") }
    
  }
  
  # Restore the old random seed, if present
  if (!is.na(seed) && !is.na(saved.seed)) { .Random.seed <- saved.seed }
  
  if (returnstrings) { return(squareid) }
  else               { return(allsq) }
}

plot_order <- c(1:6) # latinsquare(len = 6) %>% c

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
  else if(x == 6){
    wavy()
  }
}

tiff("lightning.tiff", units="in", width=16, height=9, res=200)
par(bg = 'blanchedalmond')
lightning()
dev.off()

tiff("planets.tiff", units="in", width=16, height=9, res=200)
par(bg = 'blanchedalmond')
planets()
dev.off()

tiff("wave.tiff", units="in", width=16, height=9, res=200)
par(bg = 'blanchedalmond')
wave()
dev.off()

tiff("grid.tiff", units="in", width=16, height=9, res=200)
par(bg = 'blanchedalmond')
grid()
dev.off()

tiff("circles.tiff", units="in", width=16, height=9, res=200)
par(bg = 'blanchedalmond')
circles()
dev.off()

tiff("all.tiff", units="in", width=16, height=9, res=200)
par(bg = 'blanchedalmond',mfrow = c(1,5))
lightning()
planets()
wave()
grid()
circles()
dev.off()


