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

##################################################################################
##table 2
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
out<-list()
for (yv in c("eyfsp","ks1")) {
    paste(yv,"~m_pgs_ea+c_pgs_ea+age+",covars,sep="")->f1
    as.formula(f1)->f1
    df[,c("motherid",all.vars(f1))]->tmp
    tmp[rowSums(is.na(tmp))==0,]->tmp
    std(tmp$m_pgs_ea)->tmp$m_pgs_ea
    std(tmp$c_pgs_ea)->tmp$c_pgs_ea
    std(tmp[[yv]])->tmp[[yv]]
    lm(f1,tmp)->m1
    print(summary(m1))
    print(length(m1$resid))
    robust.se(m1,tmp$motherid)->m1
    ecdf(tmp[[yv]])->ff
    ff(tmp[[yv]])->tmp[[yv]]
    lm(f1,tmp)->m2
    robust.se(m2,tmp$motherid)->m2
    list(m1,m2)->out[[yv]]
}

tab2<-list()
for (i in 1:length(out)) {
    out[[i]]->y
    ##
    l<-list()
    makeci<-function(cil,cih) paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")
    for (j in 1:2) { #mom pgs then kid pgs
        y[[1]][j+1,1]->bm
        y[[1]][j+1,2]->se
        bm-1.96*se -> clm
        bm+1.96*se -> chm
        y[[2]][j+1,1]->bk
        y[[2]][j+1,2]->se
        bk-1.96*se -> clk
        bk+1.96*se -> chk
        c(bm,makeci(clm,chm),bk,makeci(clk,chk))->l[[j]]
    }
    ##
    do.call("rbind",l)->tab2[[i]]
}
do.call("rbind",tab2)->tab2


## ##################################################################################
## ##table 3
## std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
## out<-list()
## for (yv in c("eyfsp","ks1")) {
##     paste(yv,"~m_pgs_ea+c_pgs_ea+",covars,sep="")->f1
##     as.formula(f1)->f1
##     update.formula(f1,".~.+health.pc+ses.pc")->f2
##     df[,c("motherid",all.vars(f2))]->tmp
##     tmp[rowSums(is.na(tmp))==0,]->tmp
##     std(tmp$m_pgs_ea)->tmp$m_pgs_ea
##     std(tmp$c_pgs_ea)->tmp$c_pgs_ea
##     std(tmp[[yv]])->tmp[[yv]]
##     print(nrow(tmp))
##     lm(f1,tmp)->m1
##     robust.se(m1,tmp$motherid)->m1
##     lm(update.formula(f1,".~.+health.pc"),tmp)->m1a
##     robust.se(m1a,tmp$motherid)->m1a
##     lm(update.formula(f1,".~.+ses.pc"),tmp)->m1b
##     robust.se(m1b,tmp$motherid)->m1b
##     lm(f2,tmp)->m2
##     print(length(m2$resid))
##     robust.se(m2,tmp$motherid)->m2
##     list(m1,m1a,m1b,m2)->out[[yv]]
## }

## proc<-function(tab) {
##     grepl("pc_bri",rownames(tab))->i1
##     grepl("Intercept",rownames(tab))->i2
##     tab[!(i1 | i2),]->tab
##     tab[,1]->b
##     tab[,2]->se
##     b-1.96*se -> cl
##     b+1.96*se -> ch
##     makeci<-function(cil,cih) paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")
##     makeci(cl,ch)->ci
##     cbind(b,ci)
## }
## lapply(out[[1]],proc)->t1
## lapply(out[[2]],proc)->t2













