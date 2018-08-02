#Construct mention network from Russian Troll Tweets Dataset (via fivethirtyeight.com
#Author: Chris Marcum @csmarcum <christopher.steven.marcum@gmail.com>
#Date: 1 August 2018
#Last modified: 2 August 2018

#Script assumes that you have downloaded the data
# from https://github.com/fivethirtyeight/russian-troll-tweets

library(stringr)

#Read in all the files and process
fnames<-as.list(dir(pattern=".csv"))
f1<-do.call("rbind",lapply(fnames,read.csv,stringsAsFactors=FALSE))

f1[,2]<-tolower(f1[,2])
f1[,3]<-tolower(f1[,3])
f1$publish_date<-strptime(f1$publish_date,"%m/%d/%Y %H:%M")

gc()

#Divide data
f1.split<-split(f1,f=f1$author)

#generate mention list 
f1.neighbs<-lapply(f1.split,function(x) unlist(str_match_all(x[,3],"(?<=@)[^\\s:]+")))

#Generate unique senders,receivers
senders<-(names(f1.neighbs))
receivers<-(unique(c(unlist(f1.neighbs))))
trollnet<-intersect(senders,receivers)

gc()

#Less conservative all senders and all edges rule network
f1.neighbs.tn<-lapply(f1.neighbs,function(x) x[which(x%in%senders)])
f1.nt.len<-sapply(f1.neighbs.tn,length)
troll.isolates<-names(f1.nt.len)[which(f1.nt.len==0)]
f1.neighbs.tn<-f1.neighbs.tn[which(f1.nt.len>0)]
f1.nt.len<-f1.nt.len[which(f1.nt.len>0)]

trolls.senders<-rep(names(f1.nt.len),f1.nt.len)
trolls.receivers<-unlist(f1.neighbs.tn)
names(trolls.receivers)<-NULL

trollnet.mat<-matrix(0,nrow=length(union(trolls.senders,trolls.receivers)),ncol=length(union(trolls.senders,trolls.receivers)),dimnames=list((union(trolls.senders,trolls.receivers)),(union(trolls.senders,trolls.receivers))))
for(i in 1:length(trolls.senders)){
trollnet.mat[trolls.senders[i],trolls.receivers[i]]<-trollnet.mat[trolls.senders[i],trolls.receivers[i]]+1
}

#Build network and add attributes
trolls<-as.network(trollnet.mat)
set.edge.value(trolls,"tweets",trollnet.mat)
trolls.counts<-table(f1[,2])
set.vertex.attribute(trolls,"activity",as.numeric(trolls.counts[match(c(trolls%v%"vertex.names"),names(trolls.counts))]))

accounttype<-by(f1$account_type,f1[,2],function(x) paste(sort(unique(x)),sep="-"))
set.vertex.attribute(trolls,"accounttype",c(accounttype[match(c(trolls%v%"vertex.names"),names(accounttype))]))

accountcategory<-by(f1$account_category,f1[,2],function(x) paste(sort(unique(x)),sep="-"))
set.vertex.attribute(trolls,"accountcategory",c(accountcategory[match(c(trolls%v%"vertex.names"),names(accountcategory))]))

region<-by(f1$region,f1[,2],function(x) paste(sort(unique(x)),sep="-"))
set.vertex.attribute(trolls,"region",c(region[match(c(trolls%v%"vertex.names"),names(region))]))

language<-by(f1$language,f1[,2],function(x) paste(sort(unique(x)),sep="-"))
set.vertex.attribute(trolls,"language",c(language[match(c(trolls%v%"vertex.names"),names(language))]))

maxfollowing<-by(f1$following,f1[,2],max)
minfollowing<-by(f1$following,f1[,2],min)

set.vertex.attribute(trolls,"maxfollowing",as.numeric(maxfollowing[match(c(trolls%v%"vertex.names"),names(maxfollowing))]))
set.vertex.attribute(trolls,"minfollowing",as.numeric(minfollowing[match(c(trolls%v%"vertex.names"),names(minfollowing))]))

maxfollowers<-by(f1$followers,f1[,2],max)
minfollowers<-by(f1$followers,f1[,2],min)

set.vertex.attribute(trolls,"maxfollowers",as.numeric(maxfollowers[match(c(trolls%v%"vertex.names"),names(maxfollowers))]))
set.vertex.attribute(trolls,"minfollowers",as.numeric(minfollowers[match(c(trolls%v%"vertex.names"),names(minfollowers))]))

maxpostdate<-by(f1$publish_date,f1[,2],function(x) as.character(max(x)))
minpostdate<-by(f1$publish_date,f1[,2],function(x) as.character(min(x)))

set.vertex.attribute(trolls,"maxpostdate",as.character(maxpostdate[match(c(trolls%v%"vertex.names"),names(maxpostdate))]))
set.vertex.attribute(trolls,"minpostdate",as.character(minpostdate[match(c(trolls%v%"vertex.names"),names(minpostdate))]))

save(trolls,file="trolls.Rdata")

set.seed(20818)

png("RTRT.png",width=3000,height=2500,res=100,pointsize=24)
par(mar=c(3,1,3,1),xpd=TRUE)
gplot(trolls,vertex.col=as.color(trolls%v%"accountcategory"),displayisolates=FALSE,vertex.cex=abs(log(trolls%v%"maxfollowers"+2,base=10)-1),edge.col=rgb(0,0,0,.5),main="Russian Troll to Russian Troll Twitter Mention Network (n=1245) \n fivethirtyeight.com Data 08/01/18 \ by @csmarcum")
legend("bottom",legend=unique(trolls%v%"accountcategory"),pch=19,col=unique(as.color(trolls%v%"accountcategory")),title="Account Type",horiz=TRUE)
dev.off()

stop("Code below is outdated")
###First attempt was to strict but retaining code here for transparency.
#Conservative intersection rule network
f1.neighbs.tn<-f1.neighbs[trollnet]
names(f1.neighbs.tn)<-(names(f1.neighbs.tn))
f1.neighbs.tn<-f1.neighbs.tn[which(lapply(f1.neighbs.tn,length)>0)]

f1.neighbs.tn<-lapply(f1.neighbs.tn,function(x) intersect(x,trollnet))

trollnet.mat<-matrix(0,nrow=length(f1.neighbs.tn),ncol=length(f1.neighbs.tn),dimnames=list(trollnet,trollnet))

for(i in 1:length(f1.neighbs.tn)){
 ind<-names(f1.neighbs.tn)[i]
 trollnet.mat[ind,f1.neighbs.tn[[i]]]<-trollnet.mat[ind,f1.neighbs.tn[[i]]]+1
}

library(network)
set.seed(8118)
png("RTRTnl.png",width=3000,height=2500,res=100,pointsize=24)
par(mar=c(1,1,3,1),xpd=TRUE)
gplot(trollnet.mat,displayisolates=FALSE,vertex.col="cornflowerblue",main="Russian Troll to Russian Troll Twitter Mention Network (n=331) \n fivethirtyeight.com Data 08/01/18 \ by @csmarcum")
dev.off()

set.seed(8118)
png("RTRTl.png",width=3000,height=2500,res=100,pointsize=24)
par(mar=c(1,1,3,1),xpd=TRUE)
gplot(trollnet.mat,displayisolates=FALSE,vertex.col="cornflowerblue",main="Russian Troll to Russian Troll Twitter Mention Network (n=331) \n fivethirtyeight.com Data 08/01/18 \ by @csmarcum",displaylabels=TRUE)
dev.off()

