#directory
   setwd("D:/payanname/master prog R")

#load Dataset
 X<-read.csv("D:/payanname/data smart city/citypulse_traffic_raw_data
              _surrey_feb_jun_2014/traffic_feb_june/trafficData158415.csv",
              header=TRUE)
 x<- X[1:1000 , -c(1,4,6,8,9)]

#fcm clustering
 sdata <-scale(x)

#Initialize the prototype matrix using K-means ++ algorithm
v <- inaparc ::kmpp(sdata , k=3)$v

#Initialize the memberships degrees matrix
 u <- inaparc :: imembrand(nrow(sdata), k=3)$u
 library(ppclust)
 res <- fcm(data.frame(sdata), centers=v, memberships=u, m=2)

#plot clustering
 library(scatterplot3d)
 s3d <- scatterplot3d(res$u, color=res$cluster , type="h", angle =55,
                        scale.y=0.7, pch=16, main="Pertinence")
 plot(sdata , col=res$cluster)
 plotcluster(res , cp=1, trans=TRUE)
 res.fcm2 <- ppclust2(res , "kmeans")
 factoextra ::fviz_cluster(res.fcm2 , data = sdata ,
                              ellipse.type = "convex",
                              palette = "jco",
                              repel = FALSE)
#Bayesian Network
 library(bnlearn)
 library(Rgraphviz)
 cl<-res$cluster
 x<-cbind(x,cl)

#BN for first cluster
 D1<-subset(x[,1:4],cl==1)
 D1data <-lapply(D1 ,as.factor)
 bn_df1 <- data.frame(D1data)
 hres1 <- hc(bn_df1) # Score -based algorithms:
 bnhc1 <-bn.fit(hres1 ,bn_df1 , method="bayes")
 bnhc1
 graphviz.plot(bnhc1)
 acyclic(bnhc1)

#Model validation for data cluster1
 bn.cv(bn_df1 , bn = "hc", algorithm.args = list(score = "bic"))

#BN for second cluster
 D2<-subset(x[,1:4],cl==2)
 D2data <-lapply(D2 ,as.factor)
 bn_df2 <- data.frame(D2data)
 hres2 <- hc(bn_df2) # Score -based algorithms:
 bnhc2 <-bn.fit(hres1 ,bn_df2 ,method="bayes")
 bnhc2
 graphviz.plot(bnhc2)
 acyclic(bnhc2)

#Model validation for data cluster2
 bn.cv(bn_df2 , bn = "hc", algorithm.args = list(score = "bic"))

#BN for third cluster
 D3<-subset(x[,1:4],cl==3)
 D3data <-lapply(D3 ,as.factor)
 bn_df3 <- data.frame(D3data)
 hres3 <- hc(bn_df3) # Score -based algorithms:
 bnhc3 <-bn.fit(hres1 ,bn_df3 ,method="bayes")
 bnhc3
 graphviz.plot(bnhc3)
 acyclic(bnhc3)

#Model validation for data cluster3
 bn.cv(bn_df3 , bn = "hc", algorithm.args = list(score = "bic"))

#BMA
 netlist <-list(bnhc1 ,bnhc2 ,bnhc3)
 arcs = custom.strength(netlist , nodes = names(bn_df1),cpdag = FALSE)
 meanmodel <-averaged.network(arcs)
 BMA <-modelstring(meanmodel)
 BMA
 modlBMA <-paste(BMA)
 hresBMA <-hc(data.frame(sdata))
 plot(hresBMA)
 bnBMA <-bn.fit(hresBMA ,data.frame(sdata))
 bnBMA
 graphviz.plot(bnBMA)

#Model validation for dataset
 bn.cv(data.frame(lapply(x,as.factor)), bn = "hc", algorithm.args =
           list(score = "bic"))