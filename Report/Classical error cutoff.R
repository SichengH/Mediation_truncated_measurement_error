

library(ggplot2)



## Classical measurement error

n<-1000
x<- rnorm(n,2,2)#uniform will cause more difference
#x<- runif(n,0,4)#uniform will cause more difference
beta0<-2
beta1<-4
y<-beta0+beta1*x+rnorm(n,0,1)
w<-x+rnorm(n,0,1)#w and y bi-variate normal

d1<-data.frame(y,x,label = "truth")
d2<-data.frame(y,x = w,label = "measured")
d<-rbind(d1,d2)

ggplot(data = d)+
  geom_point(aes(x,y,col = label),alpha = 0.2)+
  geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")


d.sub<-d%>%filter(x<2)%>%filter(label=="measured")
d.sub$label<-"measured.w/.cutoff"

d<-rbind(d,d.sub)
ggplot(data = d)+
  geom_point(aes(x,y,col = label),alpha = 0.2)+
  geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")



## With numbers

fit1<-lm(y~x,data = d%>%filter(label == "truth"))
fit2<-lm(y~x,data = d%>%filter(label == "measured"))
fit3<-lm(y~x,data = d%>%filter(label == "measured.w/.cutoff"))


sd(d1$x)

sd(d2$x)

sd(d.sub$x)


sigma2.x<-4
sigma2.u<-1


lambda<-sigma2.x / (sigma2.u + sigma2.x) #0.8
lambda1<-sd(d.sub$x)^2 / (sigma2.u + sd(d.sub$x)^2) #0.8






## Classical with treatment,no hetero
n<-1000
t<-c(rep(0,n/2),rep(1,n/2))
alpha<-1
x<- rnorm(n,2,2) + alpha*t#uniform will cause difference
beta0<-2
beta1<-3
beta2<-4
y<-beta0+beta1*t+beta2*x+rnorm(n,0,1)
w<-x+rnorm(n,0,1)

d1<-data.frame(y,t,x,label = "truth")
d2<-data.frame(y,t,x = w,label = "measured")
d<-rbind(d1,d2)

ggplot(data = d)+
  geom_point(aes(x,y,col = label),alpha = 0.2)+
  geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  facet_wrap(~t)


d.sub<-d%>%filter(x<2)%>%filter(label=="measured")
d.sub$label<-"measured.w/.cutoff"

d<-rbind(d,d.sub)
ggplot(data = d)+
  geom_point(aes(x,y,col = label),alpha = 0.2)+
  geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  facet_wrap(~t)



## With numbers, cut off play a role

fit1<-lm(y~x+t,data = d%>%filter(label == "truth"))
fit2<-lm(y~x+t,data = d%>%filter(label == "measured"))
fit3<-lm(y~x+t,data = d%>%filter(label == "measured.w/.cutoff"))

#fit1 and fit2 holds



## Classical with treatment,no hetero, time-to-event
n<-1000
t<-c(rep(0,n/2),rep(1,n/2))
alpha<-1
x<- rnorm(n,2,2) + alpha*t#uniform will cause difference
beta0<-2
beta1<-3
beta2<-4
y<-rweibull(n,shape = 0.7,scale = exp(beta0+beta1*t+beta2*x+rnorm(n,0,1)))
qy<-quantile(y,probs = 0.8)# 80% outcome
cen<-ifelse(y>qy,0,1)
y<-ifelse(y>qy,qy,y)

                                                  
w<-x+rnorm(n,0,1)

d1<-data.frame(y,t,x,cen,label = "truth")
d2<-data.frame(y,t,x = w,cen,label = "measured")
d<-rbind(d1,d2)

ggplot(data = d)+
  geom_point(aes(x,log(y),col = label),alpha = 0.2)+
  geom_smooth(aes(x,log(y),col = label),method = "lm",formula = "y~x")+
  facet_wrap(~t)


d.sub<-d%>%filter(x<2)%>%filter(label=="measured")
d.sub$label<-"measured.w/.cutoff"

d<-rbind(d,d.sub)
ggplot(data = d)+
  geom_point(aes(x,log(y),col = label),alpha = 0.2)+
  geom_smooth(aes(x,log(y),col = label),method = "lm",formula = "y~x")+
  facet_wrap(~t)



## With numbers, cut off play a role

#survreg(Surv(y,cen)~t+x+ldlc_change+ldlcb,data = data,dist = "weibull")

fit1<-survreg(Surv(y,cen)~t+x,data = d%>%filter(label == "truth"))
fit2<-survreg(Surv(y,cen)~t+x,data = d%>%filter(label == "measured"))
fit3<-survreg(Surv(y,cen)~t+x,data = d%>%filter(label == "measured.w/.cutoff"))
fit4<-survreg(Surv(y,cen)~t*x,data = d%>%filter(label == "measured.w/.cutoff"))
summary(fit4)


te1<-survreg(Surv(y,cen)~t,data = d%>%filter(label == "truth"))
te2<-survreg(Surv(y,cen)~t,data = d%>%filter(label == "measured"))
te3<-survreg(Surv(y,cen)~t,data = d%>%filter(label == "measured.w/.cutoff"))

#TE unbiased, hetero treatment effect yes, but not bias
#create a interaction effect(due to censoring???)


#fit5<-survreg(Surv(y,cen)~t*x,data = d%>%filter(label == "measured"))

#fit1 and fit2 holds





######## Part II #######
#Two measurements
#Selection on baseline
## Classical with treatment,no hetero
n<-1000
t<-c(rep(0,n/2),rep(1,n/2))
alpha<-10
x0<- rnorm(n,2,2)#uniform will cause difference
x<-x0+alpha*t
beta0<-2
beta1<-3
beta2<-4
y<-beta0+beta1*t+beta2*x+rnorm(n,0,1)
w1<-x0+rnorm(n,0,1)
w2<-x+rnorm(n,0,1)

d1<-data.frame(y,t,w1,x,x0,label = "truth")
d2<-data.frame(y,t,w1,x = w2,x0 = x,label = "measured_b")
d<-rbind(d1,d2)

ggplot(data = d)+
  geom_point(aes(x,y,col = label),alpha = 0.2)+
  geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  facet_wrap(~t)


d.sub<-d%>%filter(w1<2)%>%filter(label=="measured_b")
d.sub$label<-"measured.w/.cutoff"

d<-rbind(d,d.sub)
ggplot(data = d)+
  geom_point(aes(x,y,col = label),alpha = 0.2)+
  geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  facet_wrap(~t)

d0.sub<-d.sub%>%filter(t==0)
hist(d0.sub$w1)
hist(d0.sub$x)
hist(d0.sub$x0)
var(d0.sub$x)# should be 2
var(d0.sub$x0)# use this variance in selected 


#

sigma2.x<-1.9  #4 without selection 
sigma2.u<-1


lambda<-sigma2.x / (sigma2.u + sigma2.x) #0.8


lambda1<-sd(d.sub$x)^2 / (sigma2.u + sd(d.sub$x)^2) #0.8



## With numbers, cut off play a role

fit1<-lm(y~x+t,data = d%>%filter(label == "truth"))
fit2<-lm(y~x+t,data = d%>%filter(label == "measured_b"))
fit3<-lm(y~x+t,data = d%>%filter(label == "measured.w/.cutoff"))


