library(dplyr)
library(ggplot2)
library(gridExtra)
library(rJava)
library(sas7bdat.parso)
library(gridExtra)
library(pracma)
#library()
errorBar<-function(dataFile,attrPrefix,attrSufix,indexPattern){
  #browser()
  sasDat<-read.sas7bdat.parso(dataFile) 
  #browser()
  cols<-colnames(sasDat)
  indexCol<-grep(indexPattern,cols)
  indexName<-cols[indexCol]
  #browser()
  errorFig<-lapply(attrPrefix,
                   function(x,y,z) ggplot(z[ ,c(x,paste(y,"_Mean",sep=""),
                                                paste(y,"_Min",sep=""),
                                                paste(y,"_Max",sep=""))],
                                            aes_string(x=x,y=paste(y,"_Mean",sep=""),
                                                       ymin=paste(y,"_Min",sep=""),
                                                       ymax=paste(y,"_Max",sep="")))
                   +geom_point(position="jitter",col=2,pch=16,cex=1)
                   +geom_errorbar()
                   +scale_x_continuous( breaks=1:nrow(z),minor_breaks=NULL)
                   +theme(axis.text.x=element_text(size=8),axis.text.y=element_text(size=8))
                   +theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16)) 
                   +theme(axis.ticks.x = element_line(size = 1)),#+xlab("Days")+ylab("")
                   x=indexName,z=sasDat)
  # fig<-lapply(attrName,
  #             function(x,y,z) qplot(x,z[,y],main=y,xlab="days",ylab=y),
  #             x=index,z=interest) 
  ml <- marrangeGrob(errorFig, nrow=3, ncol=2)
  
  tIdEnd<-as.vector(gregexpr("\\.",dataFile)[[1]])[1]
  tIdStart<-as.vector(gregexpr("//[a-zA-Z0-9]+\\.",dataFile)[[1]])[1]
  #browser()
  if (tIdEnd[1]!=-1 && tIdStart!=-1){
    tDir<-getwd()
    tName<-substring(dataFile,tIdStart[1],tIdEnd[1])
    tFileName<-paste("../result//errorBar",tName,"png",sep="")
    #tFileName<-paste(tDir,"//..//",tName,"",".png",sep="")
    ggsave(tFileName,ml,height=9,width=12,dpi=72)
  }
  
  #grid.arrange()
  
}

plotPerPatient<-function(dataFile,attrpattern,indexPattern){
  #browser()
  sasDat<-read.sas7bdat.parso(dataFile) 
  newPattern<-paste(attrpattern,"|",indexPattern,sep="")
  #index<-sasDat%>%select(matches(indexPattern,ignore.case=FALSE))
  interest<-sasDat%>%select(matches(newPattern,ignore.case=FALSE))
  #browser()
  cols<-colnames(interest)
  indexCol<-grep(indexPattern,cols)
  indexName<-cols[indexCol]
  attrName<-cols[-indexCol]
  #browser()
  meanFig<-lapply(attrName,
              function(x,y,z) ggplot(z[ ,c(x,y)],
                                     aes_string(x=x,y=y))+geom_point(),
              x=indexName,z=interest)
  # fig<-lapply(attrName,
  #             function(x,y,z) qplot(x,z[,y],main=y,xlab="days",ylab=y),
  #             x=index,z=interest) 
  ml <- marrangeGrob(fig, nrow=3, ncol=2)
  
  tIdEnd<-as.vector(gregexpr("\\.",dataFile)[[1]])[1]
  tIdStart<-as.vector(gregexpr("//[a-zA-Z0-9]+\\.",dataFile)[[1]])[1]
  #browser()
  if (tIdEnd[1]!=-1 && tIdStart!=-1){
    tDir<-getwd()
    tName<-substring(dataFile,tIdStart[1],tIdEnd[1])
    tFileName<-paste("../result",tName,"png",sep="")
    #tFileName<-paste(tDir,"//..//",tName,"",".png",sep="")
    ggsave(tFileName,ml,height=9,width=12,dpi=72)
  }
  
  #grid.arrange()
  
}
setwd("D://Wangjin//UmassMed//Code//RCode//WhereIsOutlier//src")
fileDir<-"D://Wangjin//UmassMed//Code//Dataset//cocaine//sasdata"
fileName<-sapply(1:15,function(i) sprintf("patient%d.sas7bdat",i))
fileName<-paste(fileDir,"//",fileName,sep="")
attrPattern<-"_Mean\\b"
indexPattern<-"\\bdays\\b"
minPattern<-"_Min\\b"
maxPattern<-"_Max\\b"
attrPrefix<-c("X_axis","Y_axis","Z_axis","Celsius","EDA")
attrSufix<-c("_Mean\\b","_Min\\b","_Max\\b")

#sapply(fileName,plotPerPatient,attrPattern,indexPattern)   

sapply(fileName,errorBar,attrPrefix,attrSufix,indexPattern) 

#dat<-read.csv("D://Wangjin//UmassMed//Code//Dataset//cocaine//patient1.csv",
#              sep=",")




# f1<-qplot(dat[,6],dat[,11])
# f2<-qplot(dat[,6],dat[,12])
# 
# #multiplot(f1,f2,cols=2)
# grid.arrange(f1,f2,f3,ncol=3)