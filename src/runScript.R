library(stream)   #stream package
library(pracma)
library(hash)
library(FNN)

projDir<-"D://Wangjin//UmassMed//Code//RCode//WhereIsOutlier";
#projDir<-regexprep(projDir,"\","//"");
setwd(projDir)    #working directory;
srcFile<-list("//src//srcStream.R","//src//outlierDetectionCollect.R",
              "//src//codLib.R")
srcFile<-lapply(srcFile,function(x,y) paste(y,x,sep=""),y=projDir);
lapply(srcFile,source)  #include multiple files;
browser()
dataInfo<-c("ForestCover");
srcData<-setDataset(dataInfo);
srcDataList<-as.list(srcData)
methodSet<-list("cod");
window<-list(start=1,width=200000,slide=1)
outlierParam<-list(R=42,k=6)
simParam<-list(window=window,outlierParam=outlierParam)
result<-lapply(srcDataList,eval(as.name(methodSet[[1]])),simParam);
