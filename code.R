library(igraph)

g<-erdos.renyi.game(n = 100,p.or.m = c(1/100),type = 'gnp',directed = T)

plot(g,vertex.size=page.rank(g)$vector*1000,vertex.label=NA,edge.arrow.size=0,arrow.color=NA,edge.color=NA,vertex.color='white',layout=layout_in_circle)

par(new=TRUE)

g<-erdos.renyi.game(n = 50,p.or.m = c(1/50),type = 'gnp',directed = T)

plot(g,vertex.size=page.rank(g)$vector*1000,vertex.label=NA,edge.arrow.size=0,arrow.color=NA,edge.color=NA,vertex.color='lightblue',layout=layout_in_circle)

###

g<-erdos.renyi.game(n = 30,p.or.m = c(1/30),type = 'gnp',directed = T)

plot(g,vertex.size=0.001,vertex.label=NA,layout=layout_on_grid,edge.arrow.size=0,arrow.color=NA,edge.color='black',vertex.color=NA,arrow.curved=0,edge.width=2)

###

g<-erdos.renyi.game(n = 15,p.or.m = c(1/15),type = 'gnp',directed = T,loops = T)

plot(g,vertex.size=page.rank(g)$vector*10,vertex.label=NA,layout=layout_as_tree,edge.arrow.size=0,arrow.color=NA,edge.color='black',vertex.color=NA,arrow.curved=0,edge.width=1,edge.curved=0.5)
