library(hash)
file_hash<-hash();
#0 for continuous data attribute, 1 for nominal data attribute,
#2 for ordinal data attribute

#**************Forest Cover Dataset******************
flag<-"ForestCover";
file_name="D:\\Wangjin\\UmassMed\\Code\\Dataset\\Forest Cover\\covtype.data"
numOfInstances=581012;
dim=c(rep(0,10),rep(1,45));
nclusters=7;
labelBit=55;
file_hash[flag]=c(name=file_name,N=numOfInstances,...
                  dim=dim,nclusters=nclusters...
                  labelBit=labelBit);
#*****************************************************
