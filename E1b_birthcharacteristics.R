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


##need to get these vars
## label var eclgestday "Gestational Age (days)" 
## label var eclapgar1m "APGAR score at 1 min" 
## label var eclapgar5m "APGAR score at 5 min" 
## label var apgar "APGAR Score Mean"
## label var eclbirthwt "Birthweight (g)"
## label var eclsgaukwho "Small for gestational age"
## label var ecllgaukwho "Large for gestational age" 
library(pcaMethods)
library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x
vars<-c(eclgestday="Gestional Age (days)",
        apgar="APGAR Score",
        eclbirthwt="Birthweight (g)",
        eclsgaukwho="Small for gestional age"
        )
x[,c("childid",names(vars))]->x
merge(df,x)->df

paste(paste("m_pc_bri_",1:10,sep=""),collapse="+")->covars
if (covars!="") paste("+",covars,sep="")->covars
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
N<-out<-list()
for (yv in names(vars)) {
    paste(yv,"~m_pgs_ea+c_pgs_ea+age",covars,sep="")->f1
    as.formula(f1)->f1
    df[,c("motherid",all.vars(f1))]->tmp
    tmp[rowSums(is.na(tmp))==0,]->tmp
    std(tmp$m_pgs_ea)->tmp$m_pgs_ea
    std(tmp$c_pgs_ea)->tmp$c_pgs_ea
    std(tmp[[yv]])->tmp[[yv]]
    lm(f1,tmp)->m1
    robust.se(m1,tmp$motherid)->m1
    #lm(update.formula(f1,".~.-m_pgs_ea"),tmp)->m2
    #robust.se(m2,tmp$motherid)->m2
    #list(m1,m2)->out[[yv]]
    m1->out[[yv]]
    nrow(tmp)->N[[yv]] #get Ns
}

##source ~/Dropbox/coding_experiments/Rfunctions/make_lm_table.R
tab2<-list()
makeci<-function(cil,cih) paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")
for (i in 1:length(out)) {
    out[[i]]->y
    y[2,1]->bm
    y[2,2]->se
    bm-1.96*se -> clm
    bm+1.96*se -> chm
    y[3,1]->bk
    y[3,2]->se
    bk-1.96*se -> clk
    bk+1.96*se -> chk
    c(bm,makeci(clm,chm),bk,makeci(clk,chk),N[[i]])->tab2[[i]]
}
do.call("rbind",tab2)->tab2
vars->rownames(tab2)
