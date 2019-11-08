library(mediation)
load("/mnt/phs/group/bdomingu/data_clean_bd.Rdata")
paste(paste("m_pc_bri_",1:10,sep=""),collapse="+")->covars

##################################################################################
##table 3
std<-function(x) (x-mean(x,na.rm=TRUE))/sd(x,na.rm=TRUE)
out<-list()
for (yv in c("eyfsp","ks1")) {
    for (xv in c("ses.pc","health.pc")) {
        paste(xv,"~m_pgs_ea+c_pgs_ea+",covars,sep="")->f1
        as.formula(f1)->f1
        paste(yv,"~",xv,"+m_pgs_ea+c_pgs_ea+",xv,"+",covars)->f2
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
        mediate(m1,m2,treat="m_pgs_ea",mediator=xv)->mm
        mm->out[[paste(yv,xv) ]]
    }
}

lapply(out,summary)
