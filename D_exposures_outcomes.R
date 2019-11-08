## are ses.pc & health.pc associated with ks1 and eyfsp?


load("/mnt/phs/group/bdomingu/data_clean_bd.Rdata")
robust.se <- function(model, cluster=df$family){
    require(sandwich)
    require(lmtest)
    M <- length(unique(cluster))
    N <- length(cluster)
    K <- model$rank
    dfc <- (M/(M - 1)) * ((N - 1)/(N - K))
    uj <- apply(estfun(model), 2, function(x) tapply(x, cluster, sum));
        rcse.cov <- dfc * sandwich(model, meat = crossprod(uj)/N)
    rcse.se <- coeftest(model, rcse.cov)
    return(list(rcse.cov, rcse.se)[[2]])
}

paste(paste("m_pc_bri_",1:10,sep=""),collapse="+")->covars
#paste(paste("c_pc_bri_",1:10,sep=""),collapse="+")->covars2
#paste(covars,"+",covars2,sep="")->covars

##table 3
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
out<-list()
for (yv in c("eyfsp","ks1")) {
    paste(yv,"~ses.pc+health.pc+",covars,sep="")->f1
    as.formula(f1)->f1
    df[,c("motherid",all.vars(f1))]->tmp
    tmp[rowSums(is.na(tmp))==0,]->tmp
    std(tmp$ses.pc)->tmp$ses.pc
    std(tmp$health.pc)->tmp$health.pc
    std(tmp[[yv]])->tmp[[yv]]
    lm(update.formula(f1,".~.-health.pc"),tmp)->m1
    robust.se(m1,tmp$motherid)->m1
    lm(update.formula(f1,".~.-ses.pc"),tmp)->m2
    robust.se(m2,tmp$motherid)->m2
    list(m1,m2)->out[[yv]]
}

tab2<-list()
for (i in 1:length(out)) {
    out[[i]]->z
    ##
    l<-list()
    makeci<-function(cil,cih) paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")
    for (j in 1:2) { #mom pgs then kid pgs
        z[[j]]->y
        y[2,1]->bm
        y[2,2]->se
        bm-1.96*se -> clm
        bm+1.96*se -> chm
        c(bm,makeci(clm,chm))->l[[j]]
    }
    ##
    do.call("rbind",l)->tab2[[i]]
}
do.call("rbind",tab2)->tab2

