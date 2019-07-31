library(rethinking)
# growth
#msite<-stan_glmer(s2~s1+(1|Location),data=gro_dat) #msite is better model

post_gr=rstanarm:::as.matrix.stanreg(msite,pars=c("(Intercept)","s1"))

#diam0=sqrt(trte$sizet0/pi)*2
#diam1=sqrt(trte$sizet1/pi)*2

plot(trte$s2~trte$s1, xlab="log(Size) at Week 1", 
     ylab="log(Size) at Week 2",pch=8,col="gold",
     bty="n",main="Growth")

mu.link=function(x){post_gr[,1]+post_gr[,2]*x}
x.seq=seq(-2,10,l=500)
mu=sapply(x.seq,mu.link)
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )
#plot(1,type="n",xlim=c(-3,3),ylim=c(0,12))
lines(mu.mean~x.seq, col="black",lwd=5,lty=2)
shade(mu.HPDI, x.seq)

# fruit
#mfru<-stan_glmer.nb(fruitt1~s1+(1|Location),data=fruity)

post_fr=rstanarm:::as.matrix.stanreg(mfru,pars=c("(Intercept)","s1"))

plot(trte$fruitt1~trte$s1, xlab="log(Size)", 
     ylab="Number of Nutlets",pch=8,col="gold",
     bty="n", main="Reproduction")

mu.link=function(x){exp(post_fr[,1]+post_fr[,2]*x)}
x.seq=seq(-2,10,l=500)
mu=sapply(x.seq,mu.link)
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )
#plot(1,type="n",xlim=c(-3,3),ylim=c(0,12))
lines(mu.mean~x.seq, col="black",lwd=5,lty=2)
shade(mu.HPDI, x.seq)

# survival
#msur_s<-stan_glmer(survival~s1+(s1|Location),data=trte)

post_sur=rstanarm:::as.matrix.stanreg(msur_s,pars=c("(Intercept)","s1"))

plot(jitter(trte$survival,.3)~trte$s1, xlab="log(Size)", 
     pch=11,col="gold",ylab="Probability of survival",
     bty="n",yaxt="n", main="Survival")+
  axis(2,at=c(0,.5,1))

mu.link=function(x){plogis(post_sur[,1]+post_sur[,2]*x)}
x.seq=seq(-2,10,l=500)
mu=sapply(x.seq,mu.link)
mu.mean <- apply( mu , 2 , mean )
mu.HPDI <- apply( mu , 2 , HPDI , prob=0.95 )
#plot(1,type="n",xlim=c(-3,3),ylim=c(0,12))
lines(mu.mean~x.seq, col="black",lwd=5,lty=2)
shade(mu.HPDI, x.seq)

