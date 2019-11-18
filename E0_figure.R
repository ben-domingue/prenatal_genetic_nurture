load("/mnt/phs/group/bdomingu/data_clean_bd.Rdata")

paste(paste("m_pc_bri_",1:10,sep=""),collapse="+")->covars
#paste(paste("c_pc_bri_",1:10,sep=""),collapse="+")->covars2
#paste(covars,"+",covars2,sep="")->covars

##################################################################################
##residualize
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
out<-list()
'm_pgs_ea'->yv
paste(yv,"~c_pgs_ea+age+",covars,sep="")->f1
as.formula(f1)->f1
df[,c("motherid",all.vars(f1))]->tmp
tmp[rowSums(is.na(tmp))==0,]->tmp
lm(f1,tmp)->m1
std(m1$resid) -> tmp$ea2
tmp[,c("motherid","ea2")]->tmp
merge(df,tmp[!duplicated(tmp$motherid),],all.x=TRUE)->x

par(mgp=c(2,1,0),mfrow=c(1,2),mar=c(3,3,1,1),oma=rep(.5,4))
nms<-c(eyfsp="EYFSP",ks1="Key Stage 1")
for (yv in c("eyfsp","ks1")) {
    paste(yv,"~m_pgs_ea+c_pgs_ea+",covars,sep="")->f1
    as.formula(f1)->f1
    x[,c("motherid","ea2",all.vars(f1))]->tmp
    tmp[rowSums(is.na(tmp))==0,]->tmp
    std(tmp$m_pgs_ea)->tmp$m_pgs_ea
    std(tmp$c_pgs_ea)->tmp$c_pgs_ea
    std(tmp[[yv]])->tmp[[yv]]
    ##
    nrow(tmp)->NN
    ##
    paste(yv,"~ea2")->fm
    lm(fm,tmp)->m
    quantile(tmp$ea2,c(.025,.975),na.rm=TRUE)->qu
    data.frame(ea2=seq(qu[1],qu[2],length.out=1000))->zz
    predict(m,zz)->z
    cbind(zz,z)->pred
    plot(NULL,xlab="Maternal PGS",ylab=nms[yv],xlim=c(-3,3),ylim=c(-.7,.7))
    #mtext(side=3,line=0.25,nm)
    tmp[,c("ea2",yv)]->z
    #ecdf(z[,1])->f2
    #f2(z[,1])->qu
    z[order(z[,1]),]->z
    rep(1:(1+ceiling(nrow(z)/25)),each=25)[1:nrow(z)]->gr
    #cut(qu,seq(0,1,length.out=50))->gr
    split(z,gr)->yy
    lapply(yy,colMeans,na.rm=TRUE)->xx
    do.call("rbind",xx)->xx
    data.frame(xx)->xx
    points(xx[,1],xx[,2],pch=19,col="blue",cex=.5)
    lines(pred,lwd=2,lty=2,col="red")
    mean(tmp[[yv]],na.rm=TRUE)->M
    abline(h=M,col="gray")
    mean(tmp$ea2,na.rm=TRUE)->M
    abline(v=M,col="gray")
    paste("r=",round(cor(tmp$ea2,tmp[[yv]],use='p'),2))->rho
    paste("N=",NN,sep="")->NN
    legend("topleft",bty="n",c(rho,NN))
}

    


## fun2<-function(nm,L,ylim,outcome.var,ylab) {
##     L[[nm]]->x
##     paste(outcome.var,"~ea2")->fm
##     lm(fm,x)->m
##     quantile(x$ea2,c(.025,.975),na.rm=TRUE)->qu
##     data.frame(ea2=seq(qu[1],qu[2],length.out=1000))->zz
##     predict(m,zz)->z
##     cbind(zz,z)->pred
##     plot(NULL,xlab="PGS",ylab=ylab,xlim=c(-3,3),ylim=ylim)
##     mtext(side=3,line=0.25,nm)
##     x[,c("ea2",outcome.var)]->z
##     #ecdf(z[,1])->f2
##     #f2(z[,1])->qu
##     z[order(z[,1]),]->z
##     rep(1:(1+ceiling(nrow(z)/25)),each=25)[1:nrow(z)]->gr
##     #cut(qu,seq(0,1,length.out=50))->gr
##     split(z,gr)->yy
##     lapply(yy,colMeans,na.rm=TRUE)->xx
##     do.call("rbind",xx)->xx
##     data.frame(xx)->xx
##     points(xx[,1],xx[,2],pch=19,col="blue",cex=.5)
##     lines(pred,lwd=2,lty=2,col="red")
##     mean(x[[outcome.var]],na.rm=TRUE)->M
##     abline(h=M,col="gray")
##     mean(x$ea2,na.rm=TRUE)->M
##     abline(v=M,col="gray")
##     legend("topleft",bty="n",legend=paste("r=",round(cor(x$ea2,x[[outcome.var]],use='p'),2)),cex=1.4)
## }
