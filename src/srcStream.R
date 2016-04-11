setDataset<-function(dataInfo){
  
  file_hash<-hash();
  #0 for continuous data attribute, 1 for nominal data attribute,
  #2 for ordinal data attribute
  #browser()
  #**************Forest Cover Dataset******************
  flag<-"ForestCover";
  file_name="D://Wangjin//UmassMed//Code//Dataset//Forest Cover//covtype.data"
  numOfInstances=581012;
  dim=list(type=c(rep(0,10),rep(1,44)),bit=c(1:54));
  nclusters=7;
  labelBit=55;
  #browser()
  file_hash[flag]=list(name=file_name,N=numOfInstances,
                       dim=dim,nclusters=nclusters,
                       labelBit=labelBit);
  #*********************Cocaine********************************
  flag<-"Cocaine"
  file_name<-"D://Wangjin//UmassMed//Code//Dataset//cocaine//patient1.csv"
  numOfInstances=42;
  attr_nm<-c("Z_axis_Mean", "Y_axis_Mean", "X_axis_Mean",
             "Celsius_Mean","EDA_Mean")
  dim=list(type=c(rep(0,10)),bit=c(1:54));
  nclusters=2;
  labelBit="None";
  #browser()
  
  file_hash[flag]=list(name=file_name,N=numOfInstances,
                       dim=dim,nclusters=nclusters,
                       labelBit=labelBit,attrName=attr_nm);
  
  
  
  return(file_hash[dataInfo])
} 

