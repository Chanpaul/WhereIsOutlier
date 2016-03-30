setDataset<-function(dataInfo){
  
  file_hash<-hash();
  #0 for continuous data attribute, 1 for nominal data attribute,
  #2 for ordinal data attribute
  browser()
  #**************Forest Cover Dataset******************
  flag<-"ForestCover";
  file_name="D://Wangjin//UmassMed//Code//Dataset//Forest Cover//covtype.data"
  numOfInstances=581012;
  dim=c(rep(0,10),rep(1,45));
  nclusters=7;
  labelBit=55;
  browser()
  file_hash[flag]=list(name=file_name,N=numOfInstances,
                       dim=dim,nclusters=nclusters,
                       labelBit=labelBit);
  #*****************************************************
  return(file_hash[dataInfo])
} 

