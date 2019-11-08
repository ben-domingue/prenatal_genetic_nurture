##get ids
library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x
x[,c("motherid","pak_report","m_pc_all_1","m_pc_all_2")]->tmp

tmp[tmp$pak_report==1,]->p
apply(p,2,mean,na.rm=TRUE)->M
apply(p,2,sd,na.rm=TRUE)->S
##
val<- 1.5
xv<-c(M[3]-val*S[3],M[3]+val*S[3])
yv<-c(M[4]-val*S[4],M[4]+val*S[4])
p[,3]>xv[1] & p[,3]<xv[2] ->t1
p[,4]>yv[1] & p[,4]<yv[2] ->t2
p[t1 & t2,]->pnew
pnew$motherid -> pakistan.id



###################################

library(pcaMethods)
library(readstata13)
read.dta13("/mnt/phs/group/bdomingu/data_clean.dta")->x
x[x$motherid %in% pakistan.id,]->x
##
vars<-c("motherid","childid","m_pgs_ea","c_pgs_ea","age","bmi","hlth","vit","smk","cig","alc","caff_day","drg","solo","job","mlv","mdi",#"maternal_strain",
        "sleep_probs","sub_fin_diff","benefits_received","apgar","eclgestday","eclbirthwt","eyfsp","ks1","edu",
        paste("m_pc_all_",1:10,sep="")
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

save(df,file="/mnt/phs/group/bdomingu/data_eastasian_bd.Rdata")



