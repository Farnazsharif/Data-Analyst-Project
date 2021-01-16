### Installing the necessary packages and library
##### If there is any error later that bla bla function is not.. then install package

#install.packages("psychTools")
#install.packages("psych")

########  
library("psychTools")
library("psych")
library(reshape)

### Read data from package "psychTools"
### Link to package ....   https://cran.r-project.org/web/packages/psychTools/psychTools.pdf
data(bfi)
force(bfi)

### Make list to put the data from diff analysis
v<-list(bfi)
### Questions A1...
v$items<- dput(names(bfi))
head(bfi[,v$items])

#### FINDING THE MISSING data
bfi$missing<-apply( bfi[,v$items],1,function(x) sum(is.na(x)))

#### Number of missing data
table(bfi$missing)
bfin<-bfi[bfi$missing==0,]

#### Finding who gave unique answer
bfin$unique<-apply( bfin[,1:25],1,function(x) length(unique(x)))
table(bfin$unique)##### 4 cases just answered unique score
range(bfin[,v$items])
### Droping the cases who does responsed 1
bfin<-bfin[bfin$unique!=1,]####2232

### Spiliting data fine to train and test randomly 

bfin$sample<-sample(c("train","test"), nrow(bfin),replace = TRUE)
table(bfin$sample)# test=1117, train=1115 

split_bfin<-split(bfin, bfin$sample)
train<-split_bfin$train
test<-split_bfin$test

#### Get the summary of data
psych::describe(bfin)
psych::describe(train)
ftab<-data.frame(mean=sapply(train[,v$items], mean))


####put(names(trainf))
psych::describe(train[,c("A1", "A2", "A3", "A4", "A5", "C1", "C2", "C3", "C4", "C5", 
                         "E1", "E2", "E3", "E4", "E5", "N1", "N2", "N3", "N4", "N5", "O1", 
                         "O2", "O3", "O4", "O5", "gender")])
table(train$missing)
v$train<-train

###big 5 personality####defined by psycologist
v$big5<-c("Agreeableness", "Conscientiousness", "Extraversion", "Neuroticism", "Openness"
)

### create the bfi.keys 
keys.list <- list(agree=c("-A1","A2","A3","A4","A5"),
                  conscientious=c("C1","C2","C3","-C4","-C5"),extraversion=c("-E1","-E2","E3","E4","E5"),
                  neuroticism=c("N1","N2","N3","N4","N5"), openness = c("O1","-O2","O3","O4","-O5")) 
keys <- make.keys(psychTools::bfi,keys.list) 
keys<-(keys[1:25,])

####clustring the data to 5 cluster
scores <- psych::scoreItems(keys,train,min=1,max=6) #specify the minimum and maximum values
scores
length(scores$scores)

### Find correlations
cor<-round(scores$cor,2)
cor.plot(cor,cex=0.8,main="Heat map of inter-trait correlations within the study",cex.main=2)
fit1<-scores$scores

### Spilt personality by facet rows and gender by histogram bins
fit1<-data.frame(fit1)
quartz(width=10,height=6)
par(mfrow=c(3,2))

for(i in 1:5){ hist(fit1[,i],xlab="Response scale",sub="1:Very Inaccurate 2:Moderately Inaccurate 3:Slightly Inaccurate 4:Slightly Accurate 5:Moderately Accurate 6:Very Accurate"
                    ,main=c(names(fit1[i])),cex.lab=1.5, cex.axis=1,cex.main=2,cex.sub=.78)
}
