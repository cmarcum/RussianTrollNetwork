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

stop("The next section eats memory for breakfast.")
###Dynamic network and total network
#Added 9/8/2018
#First temporal order
f1<-f1[order(f1$publish_date),]
f1$tdiff<-diff(c(f1$publish_date[1],f1$publish_date))
f1$ctime<-cumsum(as.numeric(f1$tdiff))

#Next, repeat pull all mentions. Ignore formatting issues.
f100<-str_match_all(f1[,3],"(?<=@)[^\\s:]+")
gc()

#How big is this?
object.size(f100)/1e6

#Generate total edge list (no temporal offset or adjustment for non-simultaneity yet)
#Can be multithreaded to improve speed. Otherwise, this is slower than binary lookup [O(log_2(n))]
# on a i7 chip on account of nesting of vectorized functions. The resulting f100 object has complexity in excess
# of 2e6*f(x) X ncol(f1),  where f(x) is the average the number of mentions per tweet.
#We want to retain the null events (tweets without mentions) for completeness in the relational event framework.
#As the resulting object will be enormous in memory, it's not feasible to retain in situ at the moment.
# Solution: split task into two objects, one with mentions and one without and recombine later.

#Total expected single dimension length:
tdl<-sum(sapply(f100,function(x) ifelse(length(x)==0,1,length(x))))

#Total expected complexity
tdl*(nrow(f1)+1)

#index the null events
f100.nulls<-which(sapply(f100,length)==0)

#index the mention events
f100.ments<-which(sapply(f100,length)>0)

#subset the mentions
f100<-f100[f100.ments]

#subset expected single dimension length:
tdl2<-sum(sapply(f100,function(x) ifelse(length(x)==0,1,length(x))))

#subset expected complexity
tdl2*(nrow(f1)+1)

#Fraction of tweets with ANY mentions (~15%):
length(f100)/nrow(f1)

for(i in 1:length(f100)){
  f100[[i]]<-as.data.frame(do.call("rbind",apply(f100[[i]],1,function(x) as.data.frame(list(rec=as.character(x),f1[f100.ments[i],])))))
  f100[[i]]$rec<-as.character(f100[[i]]$rec)
}

#Several options for recombining the final dataset
# Here, I'm writing out the file file and reading it
# back in but if you have a lot of throughout, use do.call()
# or otherwise, rbind on a loop
f100.el<-do.call("rbind",f100)

#Vectorized write-out version:
#f100.el<-f100[[1]]
#lapply(f100,write.table, file="f100.el.csv",row.names=FALSE,sep=",",col.names=FALSE,append=TRUE)

#Loop version for those that really need to know about every iteration
#write.table(f100.el,file="f100.el.csv",row.names=FALSE,sep=",")
#for(i in 2:length(f100)){
#   write.table(f100[[i]],file="f100.el.csv",row.names=FALSE,sep=",",col.names=FALSE,append=TRUE)
#   if(i%%100==0) print(paste("iteration",i))
#}

#Now dump the originals and read back in the new edgelist
rm(f1,f100)
gc()

#fns<-colnames(f100.el)
#f100.el<-read.csv("f100.el.csv",head=FALSE,stringsAsFactors=FALSE)
#colnames(f100.el)<-fns

#Construct edgelist from author, and rec (could optionally paste in timestamps and re-order if relational event framework is desired)
RT.el<-as.data.frame(list(sender=as.factor(f100.el$author),receiver=as.factor(f100.el$rec),val=1))

#Reduce dimensionality to union rule:
dupes<-which(duplicated(RT.el))
RT.el<-RT.el[-dupes,]

#Total mention volume per receiver
b1.tab<-table(f100.el$rec)

barplot(sort(b1.tab,decreasing=TRUE)[1:100],col=ifelse(names(sort(b1.tab,decreasing=TRUE)[1:100])%in%f100.el$author,"green","gray"),las=2,ylab="freq")

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

