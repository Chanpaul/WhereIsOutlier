cod<-function(streamInfo,Window,Outlier){
 #streamInfo is a list, e.g., 
  #list(name=file_name,N=numOfInstances,
  #dim=dim,nclusters=nclusters,labelBit=labelBit);
  #browser()
  file<-streamInfo$name;
  N<-streamInfo$N;
  featureBit<-streamInfo$dim$bit;
  featureType<-streamInfo$dim$type
  nclusters<-streamInfo$nclusters;
  labelBit<-streamInfo$labelBit;
  window<-Window
  outlier<-Outlier  
  R=outlier$R;
  k=outlier$k;
  slide<-window$slide;
  width<-window$width 
  
  ptMap<-hash()      #index data points in the current window;
  eventQue=list()    #event list
  #browser();
  tempStream<-DSD_ReadCSV(file,sep=",");
  myCount<-0;
  
  effectPt<-c();
  while(myCount<N){
    myCount<-myCount+1;
    curTime<-myCount;     
    cat(sprintf("%d\n",myCount))          
            
    #browser()
    newPt<-get_points(tempStream,1,outofpoint="stop")
    browser()
    newPt<-c(unlist(newPt),curTime)
    newPtInfo<-list(preNeig=c(),nsucc=0,time=curTime,
                    expired=curTime+window$width,id=curTime) 
    effectPt<-rbind(effectPt,matrix(newPt,byrow=TRUE,nrow=1))
    
    nr<-nrow(effectPt)    
    #param1<-list(k=k,R=R)
    if (nr<k){
      effectPt<-rbind(effectPt,matrix(rep(newPt,k-nr),byrow=TRUE,nrow=k-nr))
      
      neig<-rangeQuery(effectPt[,featureBit],matrix(newPt[featureBit],byrow=TRUE,nrow=1),outlier)
      
      newPtInfo$preNeig<-neig$nn.index[neig$nn.dist<=R]
      tid<-effectPt[newPtInfo$preNeig,ncol(effectPt)]
      
      effectPt<-effectPt[-(nr+1):-k,]
      newPtInfo$preNeig<-neig$nn.index[tid!=curTime]
      
    } else {      
      if ((curTime-window$start)>width){ 
        #browser()
        window$start<-window$start+slide
        depUpdate<-departure(effectPt,ptMap,eventQue,window,outlier,curTime)   
        effectPt<-depUpdate$newEffectPt
        ptMap<-depUpdate$newPtMap
        eventQue<-depUpdate$newEventQueue
        outlier<-depUpdate$newOutlier
      }
      
      #*****************updata upon arrival*******************************
      neig<-rangeQuery(effectPt[,featureBit],matrix(newPt[featureBit],byrow=TRUE,nrow=1),outlier)
      newPtInfo$preNeig<-neig$nn.index[neig$nn.dist<=R]
      tid<-effectPt[newPtInfo$preNeig,ncol(effectPt)]
      newPtInfo$preNeig<-tid[tid!=newPtInfo$id]
      if (length(newPtInfo$preNeig)!=0){        
        arrUpdate<-lapply(newPtInfo$preNeig,arrival,outlier,ptMap)         
        for (i in 1:length(newPtInfo$preNeig) ){
          #browser()
          ptMap[as.character(newPtInfo$preNeig[i])]=arrUpdate[[i]]$ptInfo  
          if(arrUpdate[[i]]$mvToInlier=='Y'){            
            #outlier$idList[outlier$idList==newPtInfo$preNeig[i]]=NULL            
            outlier$idList[outlier$idList==arrUpdate[[i]]$ptInfo$id]=NULL            
          }
          if (length(arrUpdate[[i]]$ev)>0){
            eventQue<-c(eventQue,arrUpdate[[i]]$evt)
          }        
        }    
        
        if(length(newPtInfo$preNeig)<k){
          outlier$idList<-c(outlier$idList,newPtInfo$id)
        }else {
          neigPtInfo<-t(values(ptMap,as.character(newPtInfo$preNeig)))          
          temp<-apply(neigPtInfo,1, function(x) x[4])
          eventQue<-c(eventQue,list(c(min(unlist(temp)),newPtInfo$id)))
        } 
      }
    }    
    ptMap[as.character(newPtInfo$id)]<-newPtInfo    
  } 
  
}