cod<-function(streamInfo,param){
 #streamInfo is a list, e.g., 
  #list(name=file_name,N=numOfInstances,
  #dim=dim,nclusters=nclusters,labelBit=labelBit);
  file<-streamInfo$name;
  N<-streamInfo$N;
  dim<-streamInfo$dim;
  nclusters<-streamInfo$nclusters;
  labelBit<-streamInfo$labelBit;
  window<-param$window
  outlierParam<-param$outlierParam
  R=outlierParam$R;
  k=outlierParam$k;
  slide<-window$slide;
  width<-window$width 
  
  outlier<-list()    #record outliers;
  ptMap<-hash()      #index data points in the current window;
  eventQue=list()    #event list
  browser();
  tempStream<-DSD_ReadCSV(file);
  count<-0;
  effectPt<-c();
  while(count<(N+1){
    count<-count+1;
    curTime=count;
    newPt<-get_points(tempStream,1,outofpoint="stop")
    newPt<-c(newPt,curTime)
    newPtInfo<-list(preNeig=c(),nsucc=0,time=curTime) 
    nr<-nrow(effectPt)
    param1<-list(k=k,R=R)
    if (nr<k)&& length(effectPt)!=0){
      effectPt<-rbind(effectPt,matrix(rep(newPt,k-nr),byrow=TRUE,nrow=k-nr))
      
      neig<-rangeQuery(newPt,effectPt,param1)
      effectPt<-effectPt[-nr:-k,]
      newPtInfo$preNeig<-neig[neig!=curTime]
      
    } else {      
      if ((curTime-window$start)>width){
        window$start<-window$start+slide
        expired<-c(effectPt[,ncol(effectPt)]<window$start)
        expired<-effectPt[expired,ncol(effectPt)]
        effectPt<-effectPt[-expired,]
        lapply(expired,departure,ptMap)    
        
      }
      updata<-arrival(pt,effectPt,eventQue,ptMap,curTime,outlierParam)
    }
    effectPt<-rbind(effectPt,newPt)
    ptMap[as.character(count)]<-newPtInfo    
  } 
  
}