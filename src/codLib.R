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
arrUpdata<-function(ptId,outlier,ptMap,outlierParam){
  mvToInlier<-'N'
  ev<-list()
  ptInfo<-values(ptMap,as.character(ptId))  
  names(ptInfo)<-c('preNeig','nsucc','time','expired','id')
  ptInfo$nsucc<-ptInfo$nsucc+1
  nn<-ptInfo$nsucc+length(ptInfo$preNeig)
  
  if ((sum(outlier==ptInfo$id)!=0)&&(nn==outlierParam$k)){
    mvToInlier<-'Y'
  }
  if (length(ptInfo$preNeig)!=0){
    res<-values(ptMap,as.character(ptInfo$preNeig))  
    res1<-lapply(res,function(x)names(x)<-c('preNeig','time',
                                      'expired','id') x)
    res2<-lapply(res1,function(x) x$time )
    minid<-which.min(unlist(res2))    
    ev<-list(time=res1[[minid]]$time,id=ptInfo$id)    
  }
  
  return(ptInfo=ptInfo,mvToInlier=mvToInlier,
         insertEventQue=ev)
}
rangeQuery<-function(pt,effectPt,param){
  k<-param$k
  R<-param$R
  tknn<-get.knnx(effectPt,pt,k,algo="kd_tree")  
  newPtInfo$preNeig<-tknn$nn.index[tknn$nn.dist<=R]
  tid<-effectPt[newPtInfo$preNeig,labelBit+1]
  return(tid)
}