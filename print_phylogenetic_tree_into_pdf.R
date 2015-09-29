
# reflink
# http://blog.phytools.org/2015/03/splitting-tree-over-mutiple-plotting.html

setwd("/Users/lisongwang/Desktop/")
library(phytools)
tree <- read.tree("ets.tre")


# function for split pages
split.plotTree<-function(tree,splits=NULL,file=NULL,...){
  ef<-0.037037037037
  if(!is.null(file)) pdf(file,width=8.5,height=11)
  if(is.null(splits)) splits<-(floor(0.5*Ntip(tree))+0.5)/Ntip(tree)
  S<-matrix(c(0,splits,splits,1+1/Ntip(tree)),length(splits)+1,2)
  S<-cbind(S[,1]+ef*(S[,2]-S[,1]),S[,2]-ef*(S[,2]-S[,1]))
  for(i in nrow(S):1){
    if(is.null(file)&&i<nrow(S)) par(ask=TRUE)
    plotTree(tree,ylim=Ntip(tree)*S[i,],...)
    }
  if(!is.null(file)) oo<-dev.off()
  }


splits<-c(0.255,0.505,0.755)

# print tree on screen
# split.plotTree(tree,splits,ftype="i",mar=rep(1.1,4),fsize=0.5,lwd=1)

#print tree on pdf
split.plotTree(tree,splits,ftype="i",mar=rep(1,4),file="split.plotTree.pdf",lwd=1)
