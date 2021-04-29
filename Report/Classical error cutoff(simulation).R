

library(ggplot2)
library(dylyr)


## Classical measurement error

true.slope<-NULL
true.intercept<-NULL

m.slope<-NULL
m.intercept<-NULL

ms.slope<-NULL
ms.intercept<-NULL

for(i in 1:1000){
  #print(i)
  n<-1000
  #alpha<-1
  x<- rnorm(n,2,2) #uniform will cause difference
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
  # ggplot(data = d)+
  #   geom_point(aes(x,y,col = label),alpha = 0.2)+
  #   geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")
  
  
  
  ## With numbers
  fit1<-lm(y~x,data = d%>%filter(label == "truth"))
  fit2<-lm(y~x,data = d%>%filter(label == "measured"))
  fit3<-lm(y~x,data = d%>%filter(label == "measured.w/.cutoff"))
  
  
  true.intercept[i]<-fit1$coefficients[1]
  true.slope[i]<-fit1$coefficients[2]
  
  m.intercept[i]<-fit2$coefficients[1]
  m.slope[i]<-fit2$coefficients[2]
  
  ms.intercept[i]<-fit3$coefficients[1]
  ms.slope[i]<-fit3$coefficients[2]
  
  
}
summary(true.slope)
summary(m.slope)
summary(ms.slope)

summary(m.intercept)
summary(ms.intercept)



## Classical measurement error with t

true.slope<-NULL
true.intercept<-NULL
true.t<-NULL

m.slope<-NULL
m.intercept<-NULL
m.t<-NULL

ms.slope<-NULL
ms.intercept<-NULL
ms.t<-NULL

for(i in 1:1000){
  ## Classical with treatment,no hetero
  n<-1000
  t<-c(rep(0,n/2),rep(1,n/2))
  alpha<-2
  x<- rnorm(n,2,2) + alpha*t#uniform will cause difference
  beta0<-2
  beta1<-3
  beta2<-4
  y<-beta0+beta1*t+beta2*x+rnorm(n,0,1)
  w<-x+rnorm(n,0,1)
  
  d1<-data.frame(y,t,x,label = "truth")
  d2<-data.frame(y,t,x = w,label = "measured")
  d<-rbind(d1,d2)
  
  # ggplot(data = d)+
  #   geom_point(aes(x,y,col = label),alpha = 0.2)+
  #   geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  #   facet_wrap(~t)
  
  
  d.sub<-d%>%filter(x<2)%>%filter(label=="measured")
  d.sub$label<-"measured.w/.cutoff"
  
  d<-rbind(d,d.sub)
  # ggplot(data = d)+
  #   geom_point(aes(x,y,col = label),alpha = 0.2)+
  #   geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  #   facet_wrap(~t)
  
  
  
  ## With numbers, cut off play a role
  
  fit1<-lm(y~x+t,data = d%>%filter(label == "truth"))
  fit2<-lm(y~x+t,data = d%>%filter(label == "measured"))
  fit3<-lm(y~x+t,data = d%>%filter(label == "measured.w/.cutoff"))
  
  
  true.intercept[i]<-fit1$coefficients[1]
  true.slope[i]<-fit1$coefficients[2]
  true.t[i]<-fit1$coefficients[3]
  
  m.intercept[i]<-fit2$coefficients[1]
  m.slope[i]<-fit2$coefficients[2]
  m.t[i]<-fit2$coefficients[3]
  
  
  ms.intercept[i]<-fit3$coefficients[1]
  ms.slope[i]<-fit3$coefficients[2]
  ms.t[i]<-fit3$coefficients[3]
  
  
}

summary(true.slope)
summary(m.slope)
summary(ms.slope)

summary(m.intercept)
summary(ms.intercept)

summary(true.t)
summary(m.t)
summary(ms.t)


## Classical measurement error with t, time-to-event

true.slope<-NULL
true.intercept<-NULL
true.t<-NULL

m.slope<-NULL
m.intercept<-NULL
m.t<-NULL

ms.slope<-NULL
ms.intercept<-NULL
ms.t<-NULL

for(i in 1:1000){
  ## Classical with treatment,no hetero
  n<-1000
  t<-c(rep(0,n/2),rep(1,n/2))
  x<- rnorm(n,2,2)#uniform will cause difference
  beta0<-2
  beta1<-3
  beta2<-4
  y<-beta0+beta1*t+beta2*x+rnorm(n,0,1)
  w<-x+rnorm(n,0,1)
  
  d1<-data.frame(y,t,x,label = "truth")
  d2<-data.frame(y,t,x = w,label = "measured")
  d<-rbind(d1,d2)
  
  # ggplot(data = d)+
  #   geom_point(aes(x,y,col = label),alpha = 0.2)+
  #   geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  #   facet_wrap(~t)
  
  
  d.sub<-d%>%filter(x<2)%>%filter(label=="measured")
  d.sub$label<-"measured.w/.cutoff"
  
  d<-rbind(d,d.sub)
  # ggplot(data = d)+
  #   geom_point(aes(x,y,col = label),alpha = 0.2)+
  #   geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  #   facet_wrap(~t)
  
  
  
  ## With numbers, cut off play a role
  
  fit1<-lm(y~x+t,data = d%>%filter(label == "truth"))
  fit2<-lm(y~x+t,data = d%>%filter(label == "measured"))
  fit3<-lm(y~x+t,data = d%>%filter(label == "measured.w/.cutoff"))
  
  
  true.intercept[i]<-fit1$coefficients[1]
  true.slope[i]<-fit1$coefficients[2]
  true.t[i]<-fit1$coefficients[3]
  
  m.intercept[i]<-fit2$coefficients[1]
  m.slope[i]<-fit2$coefficients[2]
  m.t[i]<-fit2$coefficients[3]
  
  ms.intercept[i]<-fit3$coefficients[1]
  ms.slope[i]<-fit3$coefficients[2]
  ms.t[i]<-fit3$coefficients[3]
  
  
}
summary(true.slope)
summary(m.slope)
summary(ms.slope)

summary(m.intercept)
summary(ms.intercept)

summary(true.t)
summary(m.t)
summary(ms.t)






######### Part II ########

## Classical measurement error with t

true.slope<-NULL
true.intercept<-NULL
true.t<-NULL

m.slope<-NULL
m.intercept<-NULL
m.t<-NULL

ms.slope<-NULL
ms.intercept<-NULL
ms.t<-NULL

sigma2.xs<-NULL
lambda.est<-NULL


for(i in 1:10000){
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
  # ggplot(data = d)+
  #   geom_point(aes(x,y,col = label),alpha = 0.2)+
  #   geom_smooth(aes(x,y,col = label),method = "lm",formula = "y~x")+
  #   facet_wrap(~t)
  
  d0.sub<-d.sub%>%filter(t==0)


  
  sigma2.x<-var(d0.sub$x0)
  sigma2.xs[i]<-sigma2.x
  sigma2.u<-1
  
  
  lambda.est[i]<-sigma2.x / (sigma2.u + sigma2.x)
  
  
  
  ## With numbers, cut off play a role
  
  fit1<-lm(y~x+t,data = d%>%filter(label == "truth"))
  fit2<-lm(y~x+t,data = d%>%filter(label == "measured_b"))
  fit3<-lm(y~x+t,data = d%>%filter(label == "measured.w/.cutoff"))
  
  
  true.intercept[i]<-fit1$coefficients[1]
  true.slope[i]<-fit1$coefficients[2]
  true.t[i]<-fit1$coefficients[3]
  
  m.intercept[i]<-fit2$coefficients[1]
  m.slope[i]<-fit2$coefficients[2]
  m.t[i]<-fit2$coefficients[3]
  
  
  ms.intercept[i]<-fit3$coefficients[1]
  ms.slope[i]<-fit3$coefficients[2]
  ms.t[i]<-fit3$coefficients[3]
  
  
}

lambda.m<-mean(lambda.est)#~0.6611
lambda.m
4*0.8
4*0.6611

summary(true.slope)
summary(m.slope)
summary(ms.slope)

summary(m.intercept)
summary(ms.intercept)

summary(true.t)
summary(m.t)
summary(ms.t)

######### Part III ########

## Classical measurement error with exposure-mediator interaction

true.slope<-NULL
true.intercept<-NULL
true.t<-NULL
true.s3<-NULL

m.slope<-NULL
m.intercept<-NULL
m.t<-NULL
m.s3<-NULL


ms.slope<-NULL
ms.intercept<-NULL
ms.t<-NULL
ms.s3<-NULL


sigma2.xs<-NULL
lambda.est<-NULL


for(i in 1:10000){
  n<-1000
  t<-c(rep(0,n/2),rep(1,n/2))
  alpha<-10
  x0<- rnorm(n,2,2)#uniform will cause difference
  x<-x0+alpha*t
  beta0<-2
  beta1<-3
  beta2<-4
  beta3<--5#beta2 = 4 when t = 0 and beta2 = -1 when t = 1
  y<-beta0+beta1*t+beta2*x+beta3*t*x+rnorm(n,0,1)
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
  
  
  
  sigma2.x<-var(d0.sub$x0)
  sigma2.xs[i]<-sigma2.x
  sigma2.u<-1
  
  
  lambda.est[i]<-sigma2.x / (sigma2.u + sigma2.x)
  
  lambda.old<-4 / (sigma2.u + 4)
  lambda.new<-sigma2.x / (sigma2.u + sigma2.x)
  
  
  ## With numbers, cut off play a role
  d0<-d%>%filter(t==0)
  d1<-d%>%filter(t==1)
  
  fit1<-lm(y~x*t,data = d%>%filter(label == "truth"))
  fit2<-lm(y~x*t,data = d%>%filter(label == "measured_b"))
  fit3<-lm(y~x*t,data = d%>%filter(label == "measured.w/.cutoff"))
  
  fit01<-lm(y~x,data = d0%>%filter(label == "truth"))
  fit02<-lm(y~x,data = d0%>%filter(label == "measured_b"))
  fit03<-lm(y~x,data = d0%>%filter(label == "measured.w/.cutoff"))
  
  fit11<-lm(y~x,data = d1%>%filter(label == "truth"))
  fit12<-lm(y~x,data = d1%>%filter(label == "measured_b"))
  fit13<-lm(y~x,data = d1%>%filter(label == "measured.w/.cutoff"))
  
  true.intercept[i]<-fit1$coefficients[1]
  true.slope[i]<-fit1$coefficients[2]
  true.t[i]<-fit1$coefficients[3]
  true.s3[i]<-fit1$coefficients[4]
  
  m.intercept[i]<-fit2$coefficients[1]
  m.slope[i]<-fit2$coefficients[2]
  m.t[i]<-fit2$coefficients[3]
  m.s3[i]<-fit2$coefficients[4]
  
  ms.intercept[i]<-fit3$coefficients[1]
  ms.slope[i]<-fit3$coefficients[2]
  ms.t[i]<-fit3$coefficients[3]
  ms.s3[i]<-fit3$coefficients[4]
  
  
}

lambda.m<-mean(lambda.est)#~0.6611
lambda.m
4*0.8
4*0.6611

summary(true.slope)
summary(m.slope)
summary(ms.slope)

summary(m.intercept)
summary(ms.intercept)

summary(true.t)
summary(m.t)
summary(ms.t)

summary(true.s3)
summary(m.s3)
summary(ms.s3)



