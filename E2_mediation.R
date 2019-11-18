load("/mnt/phs/group/bdomingu/data_clean_bd.Rdata")
##dump("df","")
library(mediation)
paste(paste("m_pc_bri_",1:10,sep=""),collapse="+")->covars


##################################################################################
##table 3
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
out<-list()
set.seed(30301010)
c("ses.pc","health.pc")->xvals
for (yv in c("eyfsp","ks1")) {
    for (xv in xvals) {
        xvals[xvals!=xv]->oth.xv
        #paste(xv,"~m_pgs_ea+c_pgs_ea+age+",oth.xv,"+",covars,sep="")->f1
        paste(xv,"~m_pgs_ea+c_pgs_ea+age+",covars,sep="")->f1
        as.formula(f1)->f1
        paste(yv,"~",xv,"+m_pgs_ea+c_pgs_ea+age+",covars)->f2
        as.formula(f2)->f2
        unique(c(all.vars(f1),all.vars(f2)))->vars
        df[,c("motherid",vars)]->tmp
        tmp[rowSums(is.na(tmp))==0,]->tmp
        std(tmp$m_pgs_ea)->tmp$m_pgs_ea
        std(tmp$c_pgs_ea)->tmp$c_pgs_ea
        std(tmp[[yv]])->tmp[[yv]]
        lm(f1,tmp)->m1
        lm(f2,tmp)->m2
        ##
        mediate(m1,m2,treat="m_pgs_ea",mediator=xv,nsims=5000)->mm
        mm->out[[paste(yv,xv,sep="--")]]
    }
}

lapply(out,summary)


#Outcome	Mediator	Total Effect (Maternal PGS on outcome)	% Mediated	CI	N
strsplit(names(out),"--")->txt
makeci<-function(ci) {ci[1] -> cil; ci[2]->cih; paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")}
tab<-list()
for (i in 1:length(out)) {
    out[[i]]->x
    txt[[i]][1]->outcome
    txt[[i]][2]->med
    x$tau.coef -> tot.est
    x$tau.ci -> tot.ci
    x$n0->prop.est
    x$n0.ci -> prop.ci
    x$nobs -> n
    c(outcome,med,tot.est,makeci(tot.ci),prop.est,makeci(prop.ci),n)->tab[[i]]
}
write.csv(do.call("rbind",tab))

##all outcomes
##################################################################################
c("bmi","hlth","smk","cig","alc","caff_day","drg","vit","sleep_probs")->v1
c("edu","solo","job","mlv","benefits_received","sub_fin_diff","mdi")->v2
c(v1,v2)->med

std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
out<-list()
set.seed(30301010)
for (yv in c("eyfsp","ks1")) {
    for (xv in med) {
        paste(xv,"~m_pgs_ea+c_pgs_ea+age+",covars,sep="")->f1
        as.formula(f1)->f1
        paste(yv,"~",xv,"+m_pgs_ea+c_pgs_ea+age+",xv,"+",covars)->f2
        as.formula(f2)->f2
        unique(c(all.vars(f1),all.vars(f2)))->vars
        df[,c("motherid",vars)]->tmp
        tmp[rowSums(is.na(tmp))==0,]->tmp
        std(tmp$m_pgs_ea)->tmp$m_pgs_ea
        std(tmp$c_pgs_ea)->tmp$c_pgs_ea
        std(tmp[[yv]])->tmp[[yv]]
        lm(f1,tmp)->m1
        lm(f2,tmp)->m2
        ##
        mediate(m1,m2,treat="m_pgs_ea",mediator=xv,nsims=1000)->mm
        mm->out[[paste(yv,xv,sep="--")]]
    }
}

#lapply(out,summary)

vars<-c(health.pc="health.pc",
        bmi="BMI",
        hlth="mental health",
        vit="vitamins",
        smk="smoking",
        cig="cigarettes",
        alc="alcohol",
        caff_day="caffeine",
        drg="drug use",
        sleep_probs="sleep",
        ses.pc="ses.pc",
        edu="mom edu",
        solo="single",
        job="employed",
        mlv="job leave",
        mdi="deprivation",
        #maternal_strain="strain",
        sub_fin_diff="$ difficulty",
        benefits_received="gvmt benefits"
        )


#Outcome	Mediator	Total Effect (Maternal PGS on outcome)	% Mediated	CI	N
strsplit(names(out),"--")->txt
makeci<-function(ci) {ci[1] -> cil; ci[2]->cih; paste(format(round(cil,3),nsmall=3),format(round(cih,3),nsmall=3),sep=", ")}
tab<-list()
for (i in 1:length(out)) {
    out[[i]]->x
    txt[[i]][1]->outcome
    txt[[i]][2]->med
    x$tau.coef -> tot.est
    x$tau.ci -> tot.ci
    x$n0->prop.est
    x$n0.ci -> prop.ci
    x$nobs -> n
    c(vars[med],tot.est,makeci(tot.ci),prop.est,makeci(prop.ci),n)->tab[[i]]
}
write.csv(do.call("rbind",tab))


