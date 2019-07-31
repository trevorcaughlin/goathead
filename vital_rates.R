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

#mind<-stan_glmer(s2~s1+(1|Location)+(1|plantID),data=gro_dat)

msite<-stan_glmer(s2~s1+(1|Location),data=gro_dat) #msite is better model


#msite_s<-stan_glmer(s2~s1+(s1|Location),data=gro_dat) #msite is better model


plot(jitter(survival)~s1,bg=Location,pch=21,cex=1.3,data=trte)

#msur<-stan_glmer(survival~s1+(1|Location),data=trte)

#best
msur_s<-stan_glmer(survival~s1+(s1|Location),data=trte)

#msur_2<-stan_glmer(survival~s1+I(s1^2)+(s1|Location),data=trte)

loo(msur_s)
loo(msur_2)

fruity=trte[-which(is.na(trte$fruitt1)==T),]


mfru<-stan_glmer.nb(fruitt1~s1+(1|Location),data=fruity)

#mfru2<-stan_glmer.nb(fruitt1~s1+(s1|Location),data=fruity)
loo(mfru)
loo(mfru2)

levels(fruity$Location)[7]
levels(fruity$Location)[2]

hist(cbind(trte$s1,trte$s2))

size_bins=quantile(cbind(trte$s1,trte$s2),c(0.33,0.66,0.99),na.rm=T)

#midpoint1: 
m1=size_bins[1]/2

#midpoint2:
m2=(size_bins[1]+(size_bins[2]-size_bins[1]))/2

#midpoint3:
m3=(size_bins[2]+(size_bins[3]-size_bins[2]))/2

#appropriate to use for fruit, maybe not for others
f_overland_small=posterior_predict(mfru,newdata=data.frame(s1=m1,Location=levels(trte$Location)[7]))
f_overland_medium=posterior_predict(mfru,newdata=data.frame(s1=m2,Location=levels(trte$Location)[7]))
f_overland_big=posterior_predict(mfru,newdata=data.frame(s1=m3,Location=levels(trte$Location)[7]))

sample(f_overland_small,6)
sample(f_overland_medium,6)
sample(f_overland_big,6)

#draws #draws #draws
#for game purposes, I think it is fine to note include zeros in these draws
#appropriate to use for fruit, maybe not for others
f_axiom_small=posterior_predict(mfru,newdata=data.frame(s1=m1,Location=levels(trte$Location)[2]))
f_axiom_medium=posterior_predict(mfru,newdata=data.frame(s1=m2,Location=levels(trte$Location)[2]))
f_axiom_big=posterior_predict(mfru,newdata=data.frame(s1=m3,Location=levels(trte$Location)[2]))

#six quantiles
sample(f_axiom_small,6)
sample(f_axiom_medium,6)
sample(f_axiom_big,6)



msur_s


#appropriate to use for fruit, maybe not for others
s_overland_small=posterior_predict(msur_s,newdata=data.frame(s1=m1,Location=levels(trte$Location)[7]))
s_overland_medium=posterior_predict(msur_s,newdata=data.frame(s1=m2,Location=levels(trte$Location)[7]))
s_overland_big=posterior_predict(msur_s,newdata=data.frame(s1=m3,Location=levels(trte$Location)[7]))

sample(rbinom(6,size=1,plogis(sample(s_overland_small,6))))
sample(rbinom(6,size=1,plogis(sample(s_overland_medium,6))))
sample(rbinom(6,size=1,plogis(sample(s_overland_big,6))))


sample(s_overland_medium,6)
sample(s_overland_big,6)

#draws #draws #draws
#for game purposes, I think it is fine to note include zeros in these draws
#appropriate to use for fruit, maybe not for others
s_axiom_small=posterior_predict(msur_s,newdata=data.frame(s1=m1,Location=levels(trte$Location)[2]))
s_axiom_medium=posterior_predict(msur_s,newdata=data.frame(s1=m2,Location=levels(trte$Location)[2]))
s_axiom_big=posterior_predict(msur_s,newdata=data.frame(s1=m3,Location=levels(trte$Location)[2]))

save(msur_s,file="best_survival.Rdata")
save(mfru,file="best_fruit.Rdata")
save(msite,file="best_growth.Rdata")
