##big comparison
library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x

ifelse(x$edu=="1 + 0 levels/CSEs/GCEs(any grades)",1,NA)->x$edu2
ifelse(x$edu=="5 + 0 levels, 5+ CSEs (grade 1) 5 + GCSEs, School Certificate",2,x$edu2)->x$edu2
ifelse(x$edu=="1 + A levels/AS levels",3,x$edu2)->x$edu2
ifelse(x$edu=="2 + A levels, 4 + AS levels, Higher School certificate",4,x$edu2)->x$edu2
x$edu2 -> x$edu


##just a quick gander at education
load("/mnt/phs/group/bdomingu/data_clean_bd.Rdata")
x[,c("motherid","edu0mumede")]->tmp
tmp[!duplicated(tmp$motherid),]->tmp
merge(df,tmp)->tmp
table(tmp$edu0mumede,tmp$edu)
##

fnames<-character()
for (nm in c("edu","job0fthwrk")) {
    unique(x[[nm]])->nms
    sort(nms[!is.na(nms)])->nms
    for (i in nms) {
        paste(nm,i,sep='')->nm2
        ifelse(x[[nm]]==i,1,0)->x[[nm2]]
        c(fnames,nm2)->fnames
    }
}
fnames->names(fnames)

vars<-c(
    eyfsp="eyfsp",
    ks1="ks1",
    fnames,#edu="mom edu",
    ## edu1='edu1',
    ## edu2='edu2',
    ## edu3='edu3',
    ## edu4='edu4',
    age="mom age",
    bmi="mom bmi",
    hlth="mental health",
    vit="vitamins",
    smk="smoking",
    cig="cigarettes",
    alc="alcohol",
    caff_day="caffeine",
    drg="drug use",
    solo="single",
    job="employed",
    mlv="job leave",
    mdi="deprivation",
    #maternal_strain="strain",
    sleep_probs="sleep",
    sub_fin_diff="$ difficulty",
    benefits_received="gvmt benefits"
)


x[,c(names(vars),"c_pgs_ea")]->x1
x1[x$bri_report==1,]->x2
x1[x$bri==1 & !is.na(x$m_pgs_ea),]->x3
x3[!is.na(x3$ks1) & !is.na(x3$c_pgs_ea),]->x4
list(x1,x2,x3,x4)->y
lapply(y,nrow)

f<-function(x) {
    x[,names(vars)]->x
    colMeans(x,na.rm=TRUE)->M
    apply(x,2,sd,na.rm=TRUE)->S
    f<-function(x) sum(!is.na(x))
    apply(x,2,f)->N
    cbind(M,S,N)
}
lapply(y,f)->tab

diff<-function(x,y) {
    x[,1]-y[,1] -> del
    sqrt(x[,2]/sqrt(x[,3])^2+y[,2]/sqrt(y[,3])^2) ->se
    del/se
}
diff(tab[[3]],tab[[2]])->ts
2*pnorm(abs(ts),lower=FALSE)->pv1
diff(tab[[4]],tab[[3]])->ts
2*pnorm(abs(ts),lower=FALSE)->pv2

do.call("cbind",tab)->tab
cbind(tab[,1:9],pv1,tab[,10:12],pv2)->tab
rownames(tab)->rn
vars[rn]->rownames(tab)
write.csv(tab,'')



