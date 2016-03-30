#lazy update queue
lue<-function(evq){
  
  
}
#Direct Updata Queue
due<-function(evq){
  
  
}
departure<-function(effectPt,ptMap,eventQueue,slide,count){
  newPtMap<-ptMap
  newEventQueue=eventQueue
  
  expiredPt<-effectPt[1:slide,ncol(effectPt)]
  newEffectPt<-effectPt[-(1:slide),]
  newPtMap[as.character(expiredPt)]=NULL
  
  
  return(newEffectPt=newEffectPt,newPtMap=newPtMap,
         newEventQueue=newEventQueue)
}
arrival<-function(pt,time,param){  
  neig<-rangeQuery(pt,param)
  
  
}

rangeQuery<-function(pt,effectPt,param){
  k<-param$k
  R<-param$R
  tknn<-get.knnx(effectPt,pt,k,algo="kd_tree")  
  newPtInfo$preNeig<-tknn$nn.index[tknn$nn.dist<=R]
  tid<-effectPt[newPtInfo$preNeig,labelBit+1]
  return(tid)
}