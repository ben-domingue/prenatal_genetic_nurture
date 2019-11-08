pf<-function(beta,N,niter=1000) {
    library(MASS)
    es<-pv<-numeric()
    std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
    for (i in 1:niter) {
        x<-mvrnorm(N,c(0,0),matrix(c(1,.5,.5,1),2,2))
        x[,1]->mp
        x[,2]->kp
        y<-kp+beta*mp+rnorm(length(mp))
        std(y)->y
        std(mp)->mp
        lm(y~mp+kp)->m
        summary(m)$coef->S
        S[2,1]->es[i]
        S[2,4]->pv[i]
    }
    #beta/(1+1+.5+1)->es    
    cbind(es,pv)
}
proc<-function(beta.list,N) {
    out<-list()
    for (beta in beta.list) {
        pf(beta,N=N)->foo
        mean(foo[,1])->foo[,1]
        foo->out[[as.character(beta)]]
    }
    data.frame(do.call("rbind",out))->z
    ifelse(z[,2]<.05,1,0)->z[,2]
    by(z[,2],z[,1],mean)->y
    y
}
Nl<-c(1611,1267)
y<-list()
for (n in Nl) {
    proc(seq(0,.15,by=.01),N=n)->y[[as.character(n)]]
}

colorRampPalette(c("blue", "red"))( length(y) )->cols
par(mgp=c(2,1,0))
plot(NULL,xlim=c(0,.1),ylim=c(0,1),xlab="effect size",ylab="power",bty="n")
for (i in 1:length(y)) lines(as.numeric(names(y[[i]])),y[[i]],col=cols[i],pch=19,type="b")
mtext(side=3,line=0,"Power to detect association of maternal PGS (net of child PGS)")
legend("bottomright",bty="n",title="Sample Size",legend=Nl,fill=cols)
abline(h=.8,lty=3)
