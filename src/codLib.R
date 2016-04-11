getEvtQue<-function(evq){
  temp<-lapply(evq,function(x) x["time"])
  minId<-which.min(temp)  
  return(minId)
}
#lazy update queue
lue<-function(expiredPt,evq,ptMap,outlier,curTime){
  #browser()
  newOutlier<-outlier
  newEvq<-evq 
  #browser()
  if (length(newEvq)>0){
    evtid<-getEvtQue(newEvq)    
    #browser()
    while (newEvq[[evtid]]["time"]<=curTime){
      ptInfo<-t(values(ptMap,as.character(newEvq[[evtid]]["id"])))
      ptInfo<-ptInfo[1,]
      names(ptInfo)<-c('preNeig','nsucc','time','expired','id')
      newEvq[[evtid]]=NULL
      ptInfo$preNeig[ptInfo$preNeig==expiredPt]=NULL
      nsz<-length(ptInfo$preNeig)+ptInfo$nsucc
      if(nsz<outlier$k){
        newOutlier$idList<-c(newOutlier$idList,ptInfo$id)
      } else {
        res2<-sapply(ptInfo$preNeig,function(x,y) t(values(y,as.character(x)))[1,]$time,ptMap)
        newEvq<-c(newEvq,list(c(time=min(res2),id=ptInfo$id))  )
        
      }
      if (length(newEvq)==0){
        break
      } else {
        evtid<-getEvtQue(newEvq) 
      }
             
    }
  }
   
  return(list(evq=newEvq,outlier=newOutlier))
}
#Direct Updata Queue
due<-function(evq){
  
  
}

findExpired<-function(ptInfo,curWindow){
  id<-0
  if (ptInfo$time<curWindow$start){
    id<-ptInfo$id
  } else {
    id<-0
  }
  return(id)  
}
departure<-function(effectPt,ptMap,eventQueue,curWindow,outlier,curTime){
  newPtMap<-ptMap
  #browser()
  newEffectPt<-effectPt
  newEventQueue=eventQueue
  newOutlier<-outlier
  ptInfoMx<-t(values(ptMap))
  expired<-apply(ptInfoMx,1,findExpired,curWindow)
  #browser()  
  expId<-expired[expired!=0]
  if (length(expId)>0){
    newPtMap[as.character(expId)]<-NULL
    
    newEffectPt<-newEffectPt[newEffectPt[,ncol(newEffectPt)]!=expId,]  
    updateLue<-lue(expId,newEventQueue,ptMap,outlier,curTime)
    newEventQueue<-updateLue$evq
    newOutlier<-updateLue$outlier
  }  
  return(list(newEffectPt=newEffectPt,newPtMap=newPtMap,
         newEventQueue=newEventQueue,newOutlier=newOutlier))
}
arrival<-function(ptId,outlier,ptMap,evq){
  #browser()
  mvToInlier<-'N'
  evt<-list()  
  
  ptInfo<-t(values(ptMap,as.character(ptId)))  
  names(ptInfo)<-c('preNeig','nsucc','time','expired','id')
  ptInfo$nsucc<-ptInfo$nsucc+1
  nn<-ptInfo$nsucc+length(ptInfo$preNeig)  
  if (sum(outlier$idList==ptInfo$id)!=0 &&(nn==outlier$k)){
    mvToInlier<-'Y'
    if (length(ptInfo$preNeig)!=0){
      res<-t(values(ptMap,as.character(ptInfo$preNeig)))  
      #browser()
      res1<-apply(res,1,function(x) x[4])   
      
      evt<-list(c(time=min(unlist(res1)),id=ptInfo$id))    
    }
  } 
  
  return(list(ptInfo=ptInfo,mvToInlier=mvToInlier,
         evt=evt))
}
rangeQuery<-function(effectPt,pt,param){
  k<-param$k
  R<-param$R
  #browser()
  tknn<-get.knnx(effectPt,pt,k,algo="kd_tree")    
  return(tknn)
}