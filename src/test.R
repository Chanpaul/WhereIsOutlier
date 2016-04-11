library(rscala)
library(hash)

s <- scalaInterpreter()
trymap<-intpDef(s,'t1:String,t2:Int','Map(t1->t2)')
trymap('w',11)