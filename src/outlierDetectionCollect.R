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
  browser();
  tempStream<-DSD_ReadCSV(file);
  count<-0;
  effectPt<-c();
  while(count<(N+1){
    count<-count+1;
    newPt<-get_points(tempStream,slide,outofpoint="stop")
    newPt<-c(newPt,count)
    newPtInfo<-list(preNeig=c(),nsucc=0,time=count) 
    nr<-nrow(effectPt)
    param1<-list(k=k,R=R)
    if (nr<k)&& length(effectPt)!=0){
      effectPt<-rbind(effectPt,matrix(rep(newPt,k-nr),byrow=TRUE,nrow=k-nr))
      
      neig<-rangeQuery(newPt,effectPt,param1)
      effectPt<-effectPt[-nr:-k,]
      newPtInfo$preNeig<-neig[neig!=count]
      
    } else {      
      if ((count-window$start)>width){
        window$start<-window$start+slide
        expired<-c(effectPt[,ncol(effectPt)]<window$start)
        expired<-effectPt[expired,ncol(effectPt)]
        effectPt<-effectPt[-expired,]
        lapply(expired,departure,ptMap)       
        
      }
      neig<-rangeQuery(newPt,effectPt,param1)
      arrUpdata<-lapply(neig,arrival,count)
    }
    effectPt<-rbind(effectPt,newPt)
    ptMap[as.character(count)]<-newPtInfo
    
  }
  
  
}