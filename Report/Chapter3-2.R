
#cutoff<-c(80,90,100,110,120,130,140,150,160,1000)

library(ggplot2)
library(dplyr)
library(data.table)
library(survival)
library(DiagrammeR)
library(regmedint)

simulation.function.v6<-function(sample.size = 20000,cutoff = 1000){
  n<-sample.size/2
  osp = 2
  n0<-0
  n1<-0
  data0<-NULL
  data1<-NULL
  #control arm
  for(i in 1:100){
    #print(i)
    if(n0<n){
      ldlcm=rnorm(n = n*osp,mean = 116,sd = 14.2)#mean distribution of LDLC
      d0<-data.frame(ldlcm,ldlcb=rnorm(n = n*osp,mean = ldlcm,sd = 16))
      d0<-d0%>%filter(ldlcb<cutoff)
      data0<-rbind(data0,d0)
      n0<-nrow(data0)
    } else {
      break
    }
  }
  
  data0<-data0[1:n,]
  #data0$ldlc12m<-data0$ldlcm+rnorm(n,0,10)#12m has higher variacne then ldlcm due to possible life style change after entering the trial
  data0$ldlc12m<-data0$ldlcm
  data0$ldlc12<-rnorm(n = n,mean = data0$ldlc12m,sd = 16)
  #treatment arm
  for(i in 1:100){
    #print(i)
    if(n1<n){
      ldlcm=rnorm(n = n*osp,mean = 116,sd = 14.2)
      d1<-data.frame(ldlcm,ldlcb=rnorm(n = n*osp,mean = ldlcm,sd = 16))
      d1<-d1%>%filter(ldlcb<cutoff)
      data1<-rbind(data1,d1)
      n1<-nrow(data1)
    } else {
      break
    }
  }
  data1<-data1[1:n,]
  data1$ldlc12m<-data1$ldlcm - 47
  data1$ldlc12<-data1$ldlc12m + rnorm(n,0,16)
  data0$drug = 0
  data1$drug = 1
  data = rbind(data0,data1)
  data<-data%>%mutate(ldlc_change = ldlc12 - ldlcb)
  return(data)
}

#Set Up
p<<-1.4
#outcome regression AFT
t.theta0<<-7.5
t.theta1<<-1.8 #direct effect, positive is protective
t.theta2<<-0.01  #effect * ldlc_change; negative is protective
t.theta3<<--0.15 # ldlc_change * drug assume no effect (0 or 0.02)
t.theta4<<- 0# cov

# theta0_ni<<-7.7
# theta1_ni<<-0.59
# theta2_ni<<-0.0032 #should be negative
cutoff<-c(90,110,130,150,1000)

#where naturally drop exist


data<-simulation.function.v6(20000,cutoff = cutoff[3])# 3 variance, population, mean change, Um 


#fit<-lm(data,formula = ldlc_change_m~ldlc_change+ldlcb+drug)
#plot(fit$residuals)

#data$ldlc_change_m_pred<-predict(fit,data)
#plot(data$ldlc_change_m,data$ldlc_change_m_pred)#mean prediction works

#using predicted average as mediator


data$cen<-rep(1,20000)

data$y1<-rweibull(20000,shape = 1/p,scale = exp(t.theta0+t.theta1*data$drug+
                                                  t.theta2*data$ldlc12m+
                                                  t.theta3*data$ldlc12m*data$drug
                                                 # t.theta4*data$ldlcb
                                                ))




fit1<-regmedint(data = data,
                ## Variables
                yvar = "y1",
                avar = "drug",
                mvar = "ldlc12",
                #cvar = c("ldlcb"),
                cvar = NULL,
                eventvar = "cen",
                ## Values at which effects are evaluated
                a0 = 0,
                a1 = 1,
                m_cde = 0,
                c_cond = NULL,
                ## Model types
                mreg = "linear",
                #yreg = "survCox",
                yreg = "survAFT_weibull",
                ## Additional specification
                interaction = T,
                
                casecontrol = FALSE)

summ<-summary(fit1)
summ
#invastigate if the adjustment works or not using simulation
sd(data$ldlc12m)
sd(data$ldlcm)
data.sub<-data%>%filter(drug==0)
sd(data.sub$ldlc12m)
#sigma.l<-14.2
sigma.l<-sd(data.sub$ldlc12m)

sigma.u<-16



lambda<- sigma.l^2 / (sigma.l^2 + sigma.u^2) #0.44

theta1<-fit1$yreg_fit$coefficients[2]
theta2<-fit1$yreg_fit$coefficients[3]
beta1<-fit1$mreg_fit$coefficients[2]

theta1_adj<- theta1 - theta2 * ((1/lambda)-1)*beta1
theta2_adj<- theta2 / lambda



n<-100
# Simulation:

# Method: 


sigma.l<-NULL
rd11<-NULL
set.seed(1)
for(j in 1:n){
  
  tryCatch({
    print(paste("1",j))
    e1.te2<-NULL
    e1.te<-NULL
    e1.pnde<-NULL
    e1.tnde<-NULL
    e1.pnie<-NULL
    e1.tnie<-NULL
    e1.pm<-NULL
    e1.beta0<-NULL
    e1.beta1<-NULL
    e1.beta2<-NULL
    e1.sigma<-NULL
    e1.theta1<-NULL
    e1.theta2<-NULL
    e1.theta3<-NULL
    e1.theta4<-NULL
    
    
   
    
    cutoff<-c(90,110,130,150,1000)
    for(i in 1:5){
      data<-simulation.function.v6(20000,cutoff = cutoff[i])
      
      
      data.sub<-data%>%filter(drug==0)
      sigma.l[i]<-sd(data.sub$ldlc12m)
      
      data$cen<-rep(1,20000)
      
      data$y1<-rweibull(20000,shape = 1/p,scale = exp(t.theta0+t.theta1*data$drug+
                                                        t.theta2*data$ldlc12m+
                                                        t.theta3*data$ldlc12m*data$drug))
      
      
      
      
      fit1<-regmedint(data = data,
                      ## Variables
                      yvar = "y1",
                      avar = "drug",
                      mvar = "ldlc12",
                      cvar = NULL,
                      eventvar = "cen",
                      ## Values at which effects are evaluated
                      a0 = 0,
                      a1 = 1,
                      m_cde = 0,
                      c_cond = NULL,
                      ## Model types
                      mreg = "linear",
                      #yreg = "survCox",
                      yreg = "survAFT_weibull",
                      ## Additional specification
                      interaction = F,
                      
                      casecontrol = FALSE)
      
      
      summ1<-summary(fit1)
      
      te.fit<-survreg(Surv(y1,cen)~drug+ldlcb,data = data,dist = "weibull")
      
      e1.te2[i]<-te.fit$coefficients[2]
      e1.te[i]<-summ1$summary_myreg[6]
      e1.pnde[i]<-summ1$summary_myreg[2]
      e1.tnie[i]<-summ1$summary_myreg[3]
      e1.tnde[i]<-summ1$summary_myreg[4]
      e1.pnie[i]<-summ1$summary_myreg[5]
      e1.pm[i]<-summ1$summary_myreg[7]
      e1.beta0[i]<-fit1$mreg_fit$coefficients[1]
      e1.beta1[i]<-fit1$mreg_fit$coefficients[2]
     # e1.beta2[i]<-fit1$mreg_fit$coefficients[3]
      e1.sigma[i]<-summ1$summary_mreg_fit$sigma
      e1.theta1[i]<-fit1$yreg_fit$coefficients[2]
      e1.theta2[i]<-fit1$yreg_fit$coefficients[3]
      e1.theta3[i]<-fit1$yreg_fit$coefficients[4]
     # e1.theta4[i]<-fit1$yreg_fit$coefficients[4]
      
     
      
      
      
      
    }     
    
    results<-data.frame(cutoff,
                        e1.te2,e1.te,e1.pnde,e1.tnie,e1.tnde,e1.pnie,e1.pm,
                        e1.beta0,e1.beta1,e1.sigma,
                        e1.theta1,e1.theta2,e1.theta3,
                        
                  
                        
                        
                        interaction = 1)
    results$iter<-j
    rd11<-rbind(rd11,results)
    
    
  }, error=function(e){} )
  
}





setwd("/Users/sh/Documents/GitHub/Mediation-RGTM/")

fwrite(rd11,file = "v3-0.csv")

#v2-0: messed up adj
#v2-1: updated adj, correctly, positive theta2(harmful)
#v2-2: CATE beta1(negative theta2)
#v2-3: f4, 
#v2-4: f42, CATE beta1


#v3-0: normal x, no direct effect, with interaction in model,no interaction in the model,check
#v3-1: normal x, no direct effect, no interaction in model,,check
#v3-2: normal x, no direct effect, no interaction, but model has interaction
#v3-3: normal x, no direct effect, with interaction,  model has interaction,check

#v4,measurement error



