## label var m_pgs_ea "Maternal Education PGS"
## label var c_pgs_ea "Child Education PGS"
## label var bmi "Body Mass Index"
## label var hlth "Health Factor Score"
## label var vit "Vitamin Use"
## label var smk "Smoke Exposure"
## label var cig "Cigarette Use"
## label var alc "Alcohol Use"
## label var drg "Drug Use"
## label var solo "Single"
## label var job "Employed"
## label var mlv "Maternity Leave"
## label var mdi "Neighborhood Deprivation"
## label var sleep_probs "Sleep Problems" 
## label var behind_bills "Behind on Bills" 
## label var sub_fin_diff "Perceived Financial Difficulty" 
## label var benefits_received "Benefits Received"
## label var maternal_strain "Maternal Stress"
## label var caff_day "Caffeine Mg/Day"
## label var c_caff_day "Caffeine Mg/Day"
## label var eclgestday "Gestational Age (days)" 
## label var eclapgar1m "APGAR score at 1 min" 
## label var eclapgar5m "APGAR score at 5 min" 
## label var apgar "APGAR Score Mean"
## label var eclbirthwt "Birthweight (g)"
## label var eclsgaukwho "Small for gestational age"
## label var ecllgaukwho "Large for gestational age" 
## label var eyfsp "EYFSP Child Development"
## label var ks1 "Key Stage Assessment"
## label var phonics_mark1 "Phonics Assessment"
## label var age "Maternal Age"


library(pcaMethods)
library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x
table(x$bri,useNA="always")
x[x$bri==1,]->x
##checking Ns
lm(eyfsp~m_pgs_ea+c_pgs_ea,x)->m
length(m$resid)
lm(ks1~m_pgs_ea+c_pgs_ea,x)->m
length(m$resid)
##
vars<-c("motherid","childid","m_pgs_ea","c_pgs_ea","age","bmi","hlth","vit","smk","cig","alc","caff_day","drg","solo","job","mlv","mdi",#"maternal_strain",
        "sleep_probs","sub_fin_diff","benefits_received","apgar","eclgestday","eclbirthwt","eyfsp","ks1","edu",
        paste("m_pc_bri_",1:10,sep="")
        #paste("c_pc_ea_",1:10,sep="")
        )
x[,vars]->df
#rm("x")
df[!is.na(df$m_pgs_ea),]->df
##df[!is.na(df$c_pgs_ea),]->df
#df[!is.na(df$ks1) | !is.na(df$eyfsp),]->df


##mom edu, edu0mumeuk,  https://borninbradford.nhs.uk/wp-content/uploads/Baseline_questionnaire_Full_Dict.pdf
ifelse(df$edu=="1 + 0 levels/CSEs/GCEs(any grades)",1,NA)->df$edu2
ifelse(df$edu=="5 + 0 levels, 5+ CSEs (grade 1) 5 + GCSEs, School Certificate",2,df$edu2)->df$edu2
ifelse(df$edu=="1 + A levels/AS levels",3,df$edu2)->df$edu2
ifelse(df$edu=="2 + A levels, 4 + AS levels, Higher School certificate",4,df$edu2)->df$edu2
df$edu2 -> df$edu
x[,c("motherid","edu0mumede")]->tmp
merge(df,tmp)->x2
table(x2$edu0mumede,x2$edu) ##looking good!

fa<-function(z) { #first column needs to be id
    rowSums(!is.na(z[,-1]))->rs
    z[rs>0,]->z
    library(pcaMethods)
    for (i in 2:ncol(z)) {
        (z[,i]-mean(z[,i],na.rm=TRUE))/sd(z[,i],na.rm=TRUE)->z[,i]
    }
    result<-pca(z[,-1],methoda="bpca",nPcs=1,center=FALSE)
    result@loadings->zz
    print(zz[order(zz[,1]),1])
    data.frame(motherid=z$motherid,pc1=result@scores[,1])->hold
    print(stem(hold[,2]))
    hold
}
##Body Mass IndexHealth Factor ScoreIndirect smoke exposureCigarette useAlcohol consumptionCaffeine consumptionDrug useVitamin useSleep
df[,c("motherid","bmi","hlth","smk","cig","alc","caff_day","drg","vit","sleep_probs")]->tmp
tmp[!duplicated(tmp$motherid),]->tmp
fa(tmp)->tmp
tmp[,c("motherid","pc1")]->tmp
names(tmp)[2]<-"health.pc"
dim(df)
merge(df,tmp,all.x=TRUE)->df
dim(df)
##Maternal cohabitationEmployment  Maternal leave Governmental benefitsPerceived financial difficultyIndex of Multiple Deprivation
df[,c("motherid", "edu","solo","job","mlv","benefits_received","sub_fin_diff","mdi")]->tmp
tmp[!duplicated(tmp$motherid),]->tmp
fa(tmp)->tmp
tmp[,c("motherid","pc1")]->tmp
names(tmp)[2]<-"ses.pc"
merge(df,tmp,all.x=TRUE)->df
##Maternal cohabitationEmployment  Maternal leave Governmental benefitsPerceived financial difficultyIndex of Multiple Deprivation
df[,c("motherid", "solo","job","mlv","benefits_received","sub_fin_diff","mdi")]->tmp
tmp[!duplicated(tmp$motherid),]->tmp
fa(tmp)->tmp
tmp[,c("motherid","pc1")]->tmp
names(tmp)[2]<-"sesNOedu.pc"
merge(df,tmp,all.x=TRUE)->df

#checking Ns
lm(eyfsp~m_pgs_ea+c_pgs_ea,df)->m
length(m$resid)
lm(ks1~m_pgs_ea+c_pgs_ea,df)->m
length(m$resid)

save(df,file="/mnt/phs/group/bdomingu/data_clean_bd.Rdata")

##for emma
#write.csv(df,file="data_clean_bd.csv",quote=FALSE,row.names=FALSE)


## load("data_clean_bd.Rdata")
## df[,c("bmi","hlth","smk","cig","alc","caff_day","drg","vit","sleep_probs")]->tmp
## rowSums(tmp)->df$h
## df[,c( "solo","job","mlv","benefits_received","sub_fin_diff","mdi")]->tmp
## rowSums(tmp)->df$s
#
## lm("ks1~m_pgs_ea+c_pgs_ea",df)->m1
## lm("ks1~m_pgs_ea+c_pgs_ea+s+h",df)->m2
## length(m1$resid)
## length(m2$resid)
## lm("eyfsp~m_pgs_ea+c_pgs_ea",df)->m1
## lm("eyfsp~m_pgs_ea+c_pgs_ea+s+h",df)->m2
## length(m1$resid)
## length(m2$resid)


