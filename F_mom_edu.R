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
if (covars!="") paste("+",covars,sep="")->covars
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
## N<-out<-list()
## for (yv in c("health.pc","ses.pc")) {
##     paste(yv,"~m_pgs_ea+factor(edu)",covars,sep="")->f1
##     as.formula(f1)->f1
##     df[,c("motherid",all.vars(f1))]->tmp
##     tmp[rowSums(is.na(tmp))==0,]->tmp
##     std(tmp$m_pgs_ea)->tmp$m_pgs_ea
##     #std(tmp$c_pgs_ea)->tmp$c_pgs_ea
##     std(tmp[[yv]])->tmp[[yv]]
##     lm(f1,tmp)->m1
##     robust.se(m1,tmp$motherid)->m1
##     list(m1)->out[[yv]]
##     nrow(tmp)->N[[yv]] #get Ns
## }

out<-list()
for (yv in c("eyfsp","ks1")) {
    paste(yv,"~m_pgs_ea+c_pgs_ea+factor(edu)+",covars,sep="")->f1
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
    m1->out[[yv]]
}

##source ~/Dropbox/coding_experiments/Rfunctions/make_lm_table.R
N<-tab2<-list()
makeci<-function(cil,cih) paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")
for (i in 1:length(out)) {
    out[[i]]->y
    grep("m_pgs_ea",rownames(y))->ii
    y[ii,1]->bm
    y[ii,2]->se
    bm-1.96*se -> clm
    bm+1.96*se -> chm
    grep("c_pgs_ea",rownames(y))->ii
    y[ii,1]->bk
    y[ii,2]->se
    bk-1.96*se -> clk
    bk+1.96*se -> chk
    c(bm,makeci(clm,chm),bk,makeci(clk,chk))->tab2[[i]]
}
do.call("rbind",tab2)->tab2
