trte=readRDS("trte.rds")

all_size<-c(trte$sizet0,trte$sizet1)

quantile(all_size,c(0.33,0.66),na.rm=T)

library("viridis")
palette(rainbow(12))
plot(sizet1~sizet0,data=trte,bg=trte$Location,pch=21,cex=1.3)
abline(0,1)

plot(log(sizet1)~log(sizet0),data=trte,bg=trte$Location,pch=21,cex=1.3)
abline(0,1)

#note there is some shrinkage
gro=trte$sizet1-trte$sizet0
hist(gro)

library("rstanarm")

trte$s1=log(trte$sizet0)
trte$s2=log(trte$sizet1)

gro_dat<-na.omit(trte)

mind<-stan_glmer(s2~s1+(1|Location)+(1|plantID),data=gro_dat)

msite<-stan_glmer(s2~s1+(1|Location),data=gro_dat) #msite is better model


msite_s<-stan_glmer(s2~s1+(s1|Location),data=gro_dat) #msite is better model


plot(jitter(survival)~s1,bg=Location,pch=21,cex=1.3,data=trte)

msur<-stan_glmer(survival~s1+(1|Location),data=trte)

#best
msur_s<-stan_glmer(survival~s1+(s1|Location),data=trte)

msur_2<-stan_glmer(survival~s1+I(s1^2)+(s1|Location),data=trte)

loo(msur_s)
loo(msur_2)