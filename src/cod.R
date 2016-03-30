library(stream)   #stream package
library(pracma)
library(hash)
source(srcDir)
projDir<-"D://Wangjin//UmassMed//Code//RCode//WhereIsOutlier";
#projDir<-regexprep(projDir,"\","//"");
browser();
setwd(projDir)    #working directory;
srcDir<-paste(projDir,"//src//srcStream.R",sep="");
dataInfo<-c("ForestCover");
srcData<-setDataset(dataInfo);
srcDataList<-as.list(srcData)
result<-lapply(srcDataList,);
