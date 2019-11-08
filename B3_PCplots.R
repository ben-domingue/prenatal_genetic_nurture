## gen bri=0
## replace bri=1 if m_pc_all_1<-.002 & m_pc_all_1>-.005 & m_pc_all_2>.004 ///
## 			& m_pc_all_2<.011 & eth0eth9gp==1
## gen pak=0
## replace pak=1 if m_pc_all_1>-.002 | m_pc_all_1<-.005 | m_pc_all_2<.004 ///
## 			| m_pc_all_2>.011 & eth0eth9gp==7

library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x


x[,c("bri_report","pak_report","m_pc_all_1","m_pc_all_2")]->tmp

##dump to local machine for figure
dim(tmp)
tmp[tmp$bri_report==1 | tmp$pak_report==1,]->x
dim(x)
ifelse(x$bri_report==1,"red","blue")->col
ifelse(x$bri_report==1,2,1)->cex
ifelse(x$bri_report==1,1,19)->pch
par(mgp=c(2,1,0))
plot(x[,3],x[,4],xlab="PC 1",ylab="PC 2",col=col,cex=cex,pch=pch)
tmp[!(tmp$bri_report==1 | tmp$pak_report==1),]->y
points(y[,3],y[,4],col="gray",cex=.4)
legend("bottomright",bty="n",fill=c("red","blue","gray"),c("British","Pakistani","Other"),title="Self-reported ancestry")
##
xv<-c(-.002,-.005)
yv<-c(.004,.011)
cc<-col2rgb("red")
polygon(c(xv,rev(xv)),c(yv[1],yv[1],yv[2],yv[2]),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))

tmp[tmp$pak_report==1,]->p
apply(p,2,mean,na.rm=TRUE)->M
apply(p,2,sd,na.rm=TRUE)->S
##
xv<-c(M[3]-1.5*S[3],M[3]+1.5*S[3])
yv<-c(M[4]-1.5*S[4],M[4]+1.5*S[4])
cc<-col2rgb("blue")
polygon(c(xv,rev(xv)),c(yv[1],yv[1],yv[2],yv[2]),col=rgb(cc[1],cc[2],cc[3],max=255,alpha=75))



