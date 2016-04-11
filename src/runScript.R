library(stream)   #stream package
library(pracma)
library(hash)
library(FNN)
#library(rscala)

projDir<-"D://Wangjin//UmassMed//Code//RCode//WhereIsOutlier";
#projDir<-regexprep(projDir,"\","//"");
setwd(projDir)    #working directory;
srcFile<-list("//src//srcStream.R","//src//outlierDetectionCollect.R",
              "//src//codLib.R")
srcFile<-lapply(srcFile,function(x,y) paste(y,x,sep=""),y=projDir);
lapply(srcFile,source)  #include multiple files;
#browser()
dataInfo<-c("Cocaine")           #ForestCover");
srcData<-setDataset(dataInfo);
srcDataList<-as.list(srcData)
#browser()
methodSet<-list("cod");
window<-list(start=1,width=10,slide=1)   #200000
outlier<-list(R=42,k=6,idList=c())
result<-lapply(srcDataList,eval(as.name(methodSet[[1]])),window,outlier);
