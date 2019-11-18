load("/mnt/phs/group/bdomingu/data_clean_bd.Rdata")


######################################################################
##who are we missing outcomes on
dim(df)
is.na(df$eyfsp)->t1
is.na(df$ks1)->t2
by(df$edu,t1,mean,na.rm=TRUE)
by(df$edu,t2,mean,na.rm=TRUE)

library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x
x[,c("childid","edcont_academicyear")]->tmp
merge(df,tmp,all.x=TRUE)->df
table(is.na(df$eyfsp),df$edcont_academicyear,useNA="always")
table(is.na(df$ks1))
table(is.na(df$eyfsp))
table(is.na(df$ks1),df$edcont_academicyear,useNA="always")
table(is.na(df$eyfsp),is.na(df$ks1))
table(is.na(df$c_edu_pgs))

by(df$m_pgs_ea,is.na(df$c_pgs_ea),summary)
t.test(df$m_pgs_ea[is.na(df$c_pgs_ea)],df$m_pgs_ea[!is.na(df$c_pgs_ea)])
t.test(df$m_pgs_ea[is.na(df$eyfsp)],df$m_pgs_ea[!is.na(df$eyfsp)])
t.test(df$m_pgs_ea[is.na(df$ks1)],df$m_pgs_ea[!is.na(df$ks1)])

df[is.na(df$eyfsp),]->tmp
tmp[,c("motherid","childid")]->tmp

######################################################################
##histograms
vars<-c(
        eyfsp="eyfsp",
        ks1="ks1",
        health.pc="health.pc",
    ses.pc="ses.pc")
dim(df)
df[,vars]->tmp
dump("tmp","")

par(mgp=c(2,1,0),mfrow=c(2,2),mar=c(3,3,1,1),oma=rep(1,4))
for (nm in names(vars)) hist(tmp[[nm ]],xlab=vars[nm],ylab="",breaks=40,main="",sub="",col="blue")

######################################################################
##corelation plot
## vars<-c(m_pgs_ea="mom pgs",
##         c_pgs_ea="kid pgs",
##         edu="mom edu",
##         eyfsp="eyfsp",
##         ks1="ks1",
##         health.pc="health.pc",
##         ses.pc="ses.pc",
##         age="mom age",
##         bmi="mom bmi",
##         hlth="mental health",
##         vit="vitamins",
##         smk="smoking",
##         cig="cigarettes",
##         alc="alcohol",
##         caff_day="caffeine",
##         drg="drug use",
##         solo="single",
##         job="employed",
##         mlv="job leave",
##         mdi="deprivation",
##         #maternal_strain="strain",
##         sleep_probs="sleep",
##         sub_fin_diff="$ difficulty",
##         benefits_received="gvmt benefits"
##         )
vars<-c(m_pgs_ea="Maternal PGS",
        c_pgs_ea="Child PGS",
        age="Maternal Age",
        eyfsp="EYFSP",
        ks1="Key Stage 1",
        health.pc="Health Composite.",
        bmi="BMI",
        hlth="Mental Health",
        vit="Vitamin use",
        smk="Indirect Smoke Exposure",
        cig="Cigarette use",
        alc="Alcohol Consumption",
        caff_day="Caffeine use",
        drg="Drug use",
        sleep_probs="Sleep Problems",
        ses.pc="SES Composite.",
        edu="Maternal Education",
        solo="Single",
        job="Employed",
        mlv="Maternal Leave",
        mdi="Neighborhood Deprivation",
        sub_fin_diff="Financial Difficulty",
        benefits_received="Receipt of governmental benefits"
        )
df[,names(vars)]->tmp
C<-cor(tmp,use='p')
rownames(C)->nms
vars[nms]->rownames(C)->colnames(C)

dump("C","")

library(corrplot)
par(mar=c(1,10,15,1))
corrplot.mixed(C ,number.cex=.7,diag='l',tl.pos="lt",lower.col = "black",cex.lab=.5)





