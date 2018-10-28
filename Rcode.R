library("invgamma")
## 1  - 2007 6  3Q 1
## 3g - 2008 7  3Q 6
## 3gs- 2009 6  3Q 10
## 4  - 2010 6  3Q 14
## 4s - 2011 10 4Q 18
## 5  - 2012 9  4Q 22
## 5S - 2013 9  4Q 26
barplot(dat[1:17])

### Price
price = read.csv("d:/study/paper/price2.csv")
price = price[24:nrow(price),]
price[,2:ncol(price)] = price[,2:ncol(price)] /100
plot(c(1,2,3,4))
plotp = price
plotp[plotp==0] = NA

colnames(price) = c("date",gsub("\\."," ",colnames(price))[-1])
plot(plotp[,2],type="l",ylim=c(0,max(price[,-1])),lwd=3,main="Price change of iPhone" )
for(i in 3:ncol(plotp)) lines(plotp[,i],col=i-1,lwd = 3)
legend("bottomleft",legend=colnames(price)[-1],col=1:9,lty=1)


## Topic 
share = read.csv("d:/study/paper/share.csv")
td= read.csv("d:/study/paper/doccg9.csv")
## G5
g01 = c(5)
g02 = c(6,7)
g03 = c(8,10)
g04 = c(9,11)
g05 = c(12,13)
g06 = c(15,16,17)

unique(td$date[td$series==10])

#tg01 = td[td$series==g01,]
tshare = list()
tg  = list()
tg[[1]] = cbind(subset(share[,2:4],td$series==5),td$X0[td$series==g01])
tg[[2]] = cbind(subset(share[,2:4],td$series==7| td$series==6),subset(td$X0,td$series==7| td$series==6))
tg[[3]] = cbind(subset(share[,2:4],td$series==8| td$series==10),subset(td$X0,td$series==8| td$series==10))
tg[[4]] = cbind(subset(share[,2:4],td$series==9| td$series==11),subset(td$X0,td$series==9| td$series==11))
tg[[5]] = cbind(subset(share[,2:4],td$series==12| td$series==13),subset(td$X0,td$series==12| td$series==13))
tg[[6]] = cbind(subset(share[,2:4],td$series==15| td$series==16 | td$series==17),subset(td$X0,td$series==15| td$series==16 | td$series==17) )
### t= 22 -->  2012-06  t=46
td$date[td$X0==27]
i=2
for(i in 1:6){
  temp = array(0,c(24,3))
  colnames(tg[[i]])[4] = "t"
  temp2 = aggregate(tg[[i]][,1:3],by=list(tg[[i]]$t),FUN=sum)
  temp[(temp2[1,1]-21):(temp2[1,1]-21+nrow(temp2)-1),] = as.matrix(temp2[,2:4])/100
  tshare[[i]] = temp
}
tshare[[2]][5,] = c(1.507,1.909,1.899)



### y = mf
# m1 = b00 + b01 * t1 + b02 * t2 + b03 * t3
# p =  b10 + b11 * t1 + b12 * t2 + b13 * t3
# q =  b20 + b21 * t1 + b22 * t2 + b23 * t3
dim(cd)
unique(td$X0)
beta = 0
barplot(dat)
#Y = TotalSales*0.1
#Y <- c(840,1470,2110,4000, 7590, 10950, 10530, 9470, 7790, 5890,7000,12000,17000,15000,12000)*0.001
dat =c(26.91,47.79,37.43,31.24,33.8,51.03,43.72,35.2,39.27,74.47,61.17,47.53,48.05,74.78,51.19,40.4,45.51,78.29,50.76,41.03)
#write.csv(tshare,"d:/tshare.csv",row.names = F)
length(dat)
#tdat <- read.csv("d:/study/paper/dtopic003.csv")
#tdat <- tdat[28:43,]
#length(dat)
Y.tr = (dat[Time]/10)
plot(Y.tr,type="l",lwd=3,ylim=c(0,120),main = "Sales and Topics")
lines(tdat$V1/50,lty=2,lwd=2,col="red")
lines(tdat$V2/50,lty=2,lwd=2,col="blue")
lines(tdat$V3/50,lty=2,lwd=2,col="green")
lines(tdat$V1/50+tdat$V2/50+tdat$V3/50,lty=2,lwd=2,col="purple")
legend("topright",legend = c("Sales","Topic1","Topic2","Topic3","Total Topics"),col = c("black","red","blue","green","purple"),lwd=c(2,1,1,1,1),lty=c(1,2,2,2,2))

abline(v=5)
abline(v=9)
abline(v=13)

### Plot
tcol=c("red","blue","green","purple","orange","gray")
plot(c(NA,Y.tr),type="l",lwd=2,main="Sales and Topics",ylab="Sales(100 million)",ylim=c(0,9))
for(i in 1:6){
  lines(tshare[[i]][,1]/3,lty=2,lwd=2,col="red")
  lines(tshare[[i]][,2]/3,lty=2,lwd=2,col="blue")
  lines(tshare[[i]][,3]/3,lty=2,lwd=2,col="green")
}

#Naive = data.frame(T.comment = total.t,P.comment = pos.p)
#Naive.tr = Naive[1:4,]
tg = 11
g1 = 1
g2 = 5
g3 = 9
g4 = 14
g5 = 19
g6 = 23 ## iphone5
g7 = 27 ## iphone 5s 5c
g8 = 31 ## iphone 6 6plus
g9 = 33 ## iphone se
g10 = 35 ## iphone 7 7plus
g11 = 39 ## iphone 8 8 plus
g12 = 39 ## iphone Z

## Prior
t = 20
#remove(T)
Time = 1:t
params = list()
l=5
## Prior

params$m = array(10,dim=l)
params$p = array(0.1,dim=l)
params$q = array(0.1,dim=l)

params$sd = 30
params$t = list("t1"=1:t,"t2"=5:t,"t3"=9:t,"t4"=13:t,"t5"=17:t)
params$beta = array(0,l)
#params$beta2 = list("b1"=0,"b2"=0,"b3"=0,"b4"=0)
params$alpha = array(0,dim =c(3,l))
params$delta = array(c(0,0,0,0,1),dim = 5)
#params$deltap  = array(c(0,0,0,0,1),dim = 5)
params$deltaq = array(c(0,0,0,0,1),dim = 5)
#params$m = list("m1" = 1,"m2" = 2)
#params$t = list("t1" = 1:t,"t2" = 6:t)

cut = c(0,5,9,13,17,21)
tp = array(0,c(3,l+1)) 
for(i in 2:(l+1)){
  tp[1,i] = tshare[[i]][cut[i],1]
  tp[2,i] = tshare[[i]][cut[i],2]
  tp[3,i] = tshare[[i]][cut[i],3]
}

Dynamic = F

#params$t[[1]]
barplot(Y.tr)
abline(v=6,col="red",lty=2,lwd=2)
abline(v=10,col="red",lty=2,lwd=2)
abline(v=14,col="red",lty=2,lwd=2)
abline(v=18,col="red",lty=2,lwd=2)

dparams = list()
dparams$par = array(0,c(l+3,4)) ### 1: intercepct 2~4:topic 5:sd 
dparams$ptau = array(c(0.1,10,0.1,0.1),4)
dparams$est = array(0,c(t,l+3))
dparams$tau = c(rep(1,l+3))

#### Original: NB
#### Model 1 : GNB
#### Model 2 : GNB + Marketing Value
#### MOdel 3 : GNB + M K Social

#c(0,params$t[[1]][1:(length(params$t[[1]])-1) ])
#params$t[[1]]-1
sf = function(p,q,t,x,mod){
  x1 = c(0,x[1:(length(x)-1)])
  f = ((1 - exp(-(p+q)*x))/(1+q/p*exp(-(p+q)*x))  - 
         (1 - exp(-(p+q)*(x1)))/(1+q/p*exp(-(p+q)*(x1)) ) )
  #print(f)
  return(f)
}

bf = function(p,q,t,x,mod){
  f = (1 - exp(-(p+q)*x))/(1+q/p*exp(-(p+q)*x))
  #print(f)
  return(f)
}

smaller = function(big,t){
  if(t == 1) s = big[t] else{
    s = big[t] - big[t - 1]
  }
  return(s)
}

mprior = function(m,max,delta,tp,mod = 0,mhat=F,fix=F,s=1){
  if(mod <= 4){
    return(sum(dunif(m,min=0,max=max,log=T)))
  } else {
    m_hat=array(0,l)
    if(fix==T) m[1] = m[1] / 2
    m_hat[1] = m[1]
    
    ### m = delta[1] + m[1] + E

    for(i in 2:l){
      if(any(c(500)==mod) ) {
        m_hat[i] = max(0.1,m_hat[i-1]) #+ rnorm(1,0,0.1))
      } else if(any(c(5,7,9) == mod )){
        m_hat[i] = delta[1] + delta[5] * m_hat[i - 1] 
      } else if(any(c(6,8,10) == mod)){
        m_hat[i] = delta[1] + delta[2]*tp[1,i] + 
          delta[3]*tp[2,i] + delta[4]*tp[3,i] 
        m_hat[i] = m_hat[i] + delta[5] * m_hat[i - 1]
      }
    }
    if(mhat == T){
      return(m_hat)
    }else{
      return(sum(dnorm(m,m_hat,s,log=T)) +  sum(dunif(m ,min=0,max=max,log=T) ) )
    }

  }
  
}
Dynamic=F
#params$t
BassPred = function(params,x,mod, swi = FALSE ,le=Time[length(Time)],ll = l){
  f = array(0,c(length(params$t),le))
  smf = array(0,c(length(params$t),le))
  pred = matrix(0,length(params$t),le)
  switch = matrix(0,length(params$t),le)
  indep = matrix(0,length(params$t),le)
  leap = matrix(0,length(params$t),le)
  bigm = array(0,Time[length(Time)])
  if (Dynamic == T){
    p = c()
    q = c()
    for(i in 1:ll){
      f[i,] = c(rep(0,params$t[[i]][1]-1),
                bf(params$p[i][params$t[[i]]],params$q[params$t[[i]]],1:length(params$t[[i]])) )
    }
  }else{
    for(i in 1:ll){
      f[i,] = c(rep(0,params$t[[i]][1]-1),
                bf(params$p[i],params$q[i],1:length(params$t[[i]]),x[[i]],mod) )
      smf[i,] = c(f[i,1],diff(f[i,]))
      
    }
  }
  if(swi == FALSE){
    save = 0
    for(i in 1:ll){
      if(i == 1) save =  params$m[i] else{
        save = params$m[i-1] * f[i-1,] + params$m[i]
      }
      ss = save + params$m[i]
      if(i != ll){
        leap[i, ] = params$m[i] * smf[i, ] *  f[i+1, ]
        if(i == 1)  pred[i,] = (save  * smf[i,] ) * (1 - f[i+1,]) else{
          pred[i,] = (save  * smf[i,] + leap[i-1, ] ) * (1 - f[i+1,])
        }
       
      }else{
        pred[i,]  =  save * smf[i,] + leap[i - 1]
      }
      
    }
    return(pred)
  } else{
    save = 0
    for(i in 1:ll){
      if(i == 1) save =  params$m[i] else{
        save =  params$m[i-1] * f[i-1,] + params$m[i]
      }

      if(i != ll){
        #save = ss * smf[i,]
        
        if(i ==1){
          leap[i,] = params$m[i] * smf[i,] *f[i+1, ]
          indep[i,] = params$m[i] * smf[i, ] * (1 - f[i+1, ]) 
          pred[i,] = save * smf[i, ]  * (1 - f[i+1,])
        }else{
          leap[i, ] = params$m[i] * smf[i, ] *f[i+1, ]
          switch[i, ] = (save- params$m[i]) * smf[i, ] *(1 - f[i+1,])
          indep[i,] = params$m[i] * smf[i, ] * (1 - f[i+1, ])
          pred[i, ] = (save *smf[i, ] + leap[i-1,])  * (1 - f[i+1, ])
        }
      }else{
        switch[i, ] = (save - params$m[i]) * smf[i, ]
        indep[i, ]  = params$m[i] * smf[i, ]
        pred[i, ]  =  save * smf[i,] + leap[i-1,]
        #indep[i,] = ind 
        #leap[i, ] = pred[i, ] - params$m[[i]] * f[i, ]
      }
    
    }
    return(list("pred"=pred, "switch"= switch, "leap" = leap, "ind"=indep) )
  }

}

#prior
prior = function(params){
  #mprior = array(0,l)
  pprior = array(0,l)
  #qprior = array(0,l)
  aprior = array(0,1)
  bprior = array(0,l)
  aprior = array(0,c(3,l))
  #p = params$p
  #q = params$q
  sd = params$sd
  # mprior = dnorm(m,mean=priorm[1],sd = mpsd, log = T)
  # pprior = dnorm(p,mean=priorm[2],sd = ppsd, log = T)
  # qprior = dnorm(q,mean=priorm[3],sd = qpsd, log = T)

  if(Dynamic == T){
    for(i in 1:length(params$m)){
      mprior[i] = sum(dunif(params$m[i],min=0,max=20,log=T))
      pprior[i] = sum(dunif(params$p[i],min=0,max=10,log=T))
      #qprior[i] = sum(dunif(params$q[[i]],min=0,max=10,log=T))
    }
    qprior[i] = sum(dunif(params$q,min=0,max=10,log=T))
  }else{

    mp = mprior(params$m,25,params$delta,tp,mod=mod,fix=T,s=0.5)
    #print(mprior(params$m,30,params$delta,tp/100,mod=mod,fix=T,mhat=T))
    qp = mprior(params$q,2,params$deltaq,tp,mod=mod,fix=F,s=0.5)
    
    for(i in 1:length(params$m)){
      #mprior[i] = sum(dunif(params$m[[i]],min=0,max=30,log=T))
      #mprior[i] = dnorm(params$m[i],m_hat[i],0.2,log=T)
      #qprior[i] = dnorm(params$q[i],q_hat[i],0.1,log=T) + dunif(params$q[i],min=0,max=10,log=T)
      
      pprior[i] = sum(dunif(params$p[i],min=0,max=1,log=T))
      #qprior[i] = sum(dunif(params$q[i],min=0,max=10,log=T))
      bprior[i] = sum(dnorm(params$beta[i],0,0.05,log=T)) 
      aprior[1,i] = sum(dnorm(params$alpha[1,i],0,0.05,log=T))
      aprior[2,i] = sum(dnorm(params$alpha[2,i],0,0.05,log=T))
      aprior[3,i] = sum(dnorm(params$alpha[3,i],0,0.05,log=T))
      
    }
  }

  sdprior = dnorm(params$sd,0,0.01,log=T) 
  return(mp  + qp + sdprior+sum(pprior)+sum(aprior) + sum(bprior))
}
#likelihood(params)

likelihood = function(params,x,mod){
  pred = colSums(BassPred(params,x,mod))
  singlelikelihoods = dnorm(Y.tr,mean=pred,sd=params$sd,log=TRUE)
  sumll = sum(singlelikelihoods)
  return(list("sumll" = sumll,"pred" = pred))
}
posterior <- function(params,x,mod){
  #print(likelihood(params,x,mod)$sumll)
  #print(prior(params))
  return(likelihood(params,x,mod)$sumll+prior(params))
}

reg = function(coef,x){
  return( coef * 
           log( x/(x[1]) ) )
}

reg2 = function(coef,x){
  return( coef * 
            log( x/(x[1]) ) )
}

proposalvalue = function(params){
  if(Dynamic == F){
    ran = rnorm(l+3,mean=0,sd=c(0.03,0.03,0.01,rep(0.8,l) ))
    ranp = rnorm(l,0,0.1)
    ranq = rnorm(l,0,0.1)
    ranb = rnorm(l,0,0.1)
    #params$q = params$q + ran[2]
    params$sd= params$sd + ran[3]
    for(i in 1:l){
      params$m[[i]] = params$m[[i]] + ran[i+3]
      if(mod >= 0){
        params$p[[i]] = params$p[[i]] + ranp[1]
      } else{
        params$p[[i]] = params$p[[i]] + ranp[i]
      }
      
      params$q[[i]] = params$q[[i]] + ranq[i]
      params$beta[i] = params$beta[i] + ranb[i]
      params$alpha[1,i] = params$alpha[1,i] + rnorm(1,0,0.1)
      params$alpha[2,i] = params$alpha[2,i] + rnorm(1,0,0.1)
      params$alpha[3,i] = params$alpha[3,i] + rnorm(1,0,0.1)
    }
    
  }else{
    #params$p =  rep(rnorm(1,params$p,0.01),t)
    #print(params$q)
    params$q =  rnorm(t,params$q,0.01)
    params$sd =  abs(rnorm(1,params$sd,0.01))
    for(i in 1:l){
      params$m[i] =  rnorm(t,params$m[i],1)
      #params$m[[i]] = rep( rnorm(1, params$m[[i]], 10 ),t )
      if(mod >= 0){params$p[[i]] = rep( rnorm(1, params$p[[i]], 0.01 ),t )
      } else{
        params$p[[i]] = 1
      }
      
      #params$q[[i]] = rep( rnorm(1, params$q[[i]], 0.01 ),t )
    }
    
  }
  return(params)
  #return(rnorm(5,mean = param , sd=c(1,0.05,0.05,0.01,1)))
}


MCMC = function(params,iter = 10000,burn = 6000,dparams=NULL,x=0,mod=0){
  chain = list()
  #chain = array(dim = c(iter-burn,l+3))
  save = params
  for(i in 1:iter){
    if(i %% 1000 == 0){
      cat(paste0("iteration:\t",i,"\t sd: ", round(save$sd,3),"\n",sep=""))
    }
    
    if( mod >= 2){
      xt = list()
      for(j in 1:l){
        xt[[j]] =1:length(params$t[[j]]) 
        if(any(c(2,4,5,6,7,14,15,16)==mod)) {
          xt[[j]] = xt[[j]] + reg(params$beta[j],x[params$t[[j]],j*2+1 ])
        }
        if(mod >= 3){
          soc = reg(params$alpha[1,j],tshare[[j]][params$t[[j]],1]) +
            reg(params$alpha[2,j],tshare[[j]][params$t[[j]],2]) +
            reg(params$alpha[3,j],tshare[[j]][params$t[[j]],3])
            
          xt[[j]] = xt[[j]] + soc
        }
      }
    }else{
      xt = list()
      for(j in 1:l){
        xt[[j]] = 1:length(params$t[[j]])
      }
    }
    #print(xt)
      #for(j in 1:l){
      #  params$m[[j]] = params$m[[j]] + reg( params$beta[j] , x[params$t[[j]],j*2] )
      #}
    #params[[2]]
    new = save
    ##       m     p    q    sd   t   beta  alpha     delta  deltaq
    rand = c(2,   0.1, 0.2, 0.03, 0, 0.001, 0.001,   0.001,   0.001)
    for(pars in 1:9){
      if((all(c(5) !=  pars) )){
        for(par in 1:length(params[[pars]])){
          if(pars != 2){
            if(mod >8 && any(c(6,7)==pars)){
              if(pars==6){
                new[[pars]][] = rep(rnorm(1,new[[pars]][1],rand[pars] ),l)
              }
              if(pars == 7){
                new[[pars]][1,] = rep(rnorm(1,new[[pars]][1,1],rand[pars] ),l )
                new[[pars]][2,] = rep(rnorm(1,new[[pars]][2,1],rand[pars] ),l )
                new[[pars]][3,] = rep(rnorm(1,new[[pars]][3,1],rand[pars] ),l )
              }

            }else{
              new[[pars]][par] = rnorm(1,new[[pars]][par],rand[pars])
            }

          }else{new[[pars]] = rep(rnorm(1,new[[pars]][1],rand[pars]),l)  }
          ### Proposal VS Save
          #print(new)
          if(pars == 6 || pars == 7){
            if(mod >= 2){
              if(mod > 10){
                new[[pars]][par,] = new[[pars]][par]
              }
              nxt = list()
              for(j in 1:l){
                nxt[[j]] = 1:length(params$t[[j]])
                if(any(c(2,4,5,6,7,14,15,16)==mod)){
                  nxt[[j]] = nxt[[j]] +reg(new$beta[j],x[params$t[[j]],j*2+1 ])
                } 
                if(mod >= 3){
                  
                  soc = reg(new$alpha[1,j],tshare[[j]][params$t[[j]],1]) +
                    reg(new$alpha[2,j],tshare[[j]][params$t[[j]],2]) +
                    reg(new$alpha[3,j],tshare[[j]][params$t[[j]],3])
                  
                  nxt[[j]] = nxt[[j]] + soc
                }
              }
            }else{
              xt = nxt = list()
              for(j in 1:l){
                xt[[j]] = nxt[[j]] = 1:length(params$t[[j]])
              }
            }
            #print(xt)
            probab = exp(posterior(new,nxt,mod=mod)-posterior(save,xt,mod=mod))

          }else{
            #print(new)
            probab = exp(posterior(new,xt,mod=mod)-posterior(save,xt,mod=mod))
          }
      
          
          if(i > burn){
            if(runif(1)<probab){
              chain[[i-burn]] = save = new
            }
            else{
              chain[[i-burn]] = save}
          }
          else{
            if(runif(1)<probab){
              save = new
            }
          }
          ### break when p
          if(pars == 2)break
          ### break when mod > 10 (constant alpha and beta)
          if(mod > 8 && any(c(6,7)== pars))break
        }  
      }
      
    } ## End Pars
  }
  return(chain)
}
l=5
l = length(params$m)
params
## check
# posterior(startv)
# likelihood(proposalvalue(startv))
# likelihood(startv)
# prior(startv)
# posterior(proposalvalue(startv))
########
# tshare2 = tshare
# 
# for(i in 1:length(tshare)){
#   if(i == 1){
#     tshare[[1]] = rbind(c(250,280,200),tshare[[1]])[-21,]
#   }else{
#     tshare[[i]] = rbind(c(0,0,0),tshare[[i]])[-21,]
#   }
# }
# for(i in 1:length(tshare)){
#   tshare[[i]] = tshare[[i]] / 100
# }
#Dynamic = F
#params
# tshare
set.seed(1028)
#params = Test[[2000]]
mod = 10
Test = MCMC(params = params,40000,38000,mod= mod,x=price)
#  tshare
params = Test[[40000]]

#plot(vector[,1,])
  #Test2 = Test
Test = Mod[[1]]
#Dynamic =T
#params = Test[[30000]]
mod = 2
l=5
#Test[[i]]$sd
#Mod = list()
for(i in 1){
  if(mod == 1){
    dcoef = data.frame(delta0 =0,delta1 =0,delta2 =0,delta3 =0,delta4 =0)
    dcoefq = data.frame(delta0 =0,delta1 =0,delta2 =0,delta3 =0,delta4 =0)
    rmse = data.frame( Mod1=0,Mod2=0,Mod3=0,Mod4=0,Mod5=0,Mod6=0,Mod7=0,Mod8=0,Mod9=0,Mod10=0,Mod11=0,Mod12=0,Mod13=0)
    Mod= list()
    Coef = list()
  } 
Mod[[mod]] = Test  
vector = array(0,c(length(Test),3*l+1,t))
#Test[[10000]]

#Test[[20000]]$beta
bbeta = array(0,c(length(Test),l ))
aalpha = array(0,dim = c(length(Test),3,l))
ddelta = array(0,c(length(Test),5))
ddeltaq = array(0,c(length(Test),5))
for (i in 1:length(Test)){
  #vector[i,,] = Test[[i]]$p
  #vector[i,1,] = Test[[i]]$q
  vector[i,1,] = Test[[i]]$sd

  if(Dynamic == T){
    vector[i,2,] = c(rep(0,4),Test[[i]]$alpha$alpha2,rep(0,4))
    vector[i,3,] = c(rep(0,8),Test[[i]]$alpha$alpha3)
    vector[i,4,] = c(rep(0,8),Test[[i]]$beta$beta3)
    #vector[i,2*l+j+4,] = Test[[i]]$q
    #for(j in 1:l) vector[i,2*l+j+4,] = Test[[i]]$q[[j]]
    vector[i,2*l+1+1,] = Test[[i]]$q
  }else{ 
    for(j in 1:l) vector[i,2*l+j+1,] = Test[[i]]$q[[j]]
  }
  for(j in 1:l){
    vector[i,j+1,] = Test[[i]]$m[j]
    vector[i,l+j+1,] = Test[[i]]$p[j]
    bbeta[i,j] = Test[[i]]$beta[j]
    aalpha[i,1,j] = Test[[i]]$alpha[1,j]
    aalpha[i,2,j] = Test[[i]]$alpha[2,j]
    aalpha[i,3,j] = Test[[i]]$alpha[3,j]
    
  }
  ddelta[i,] = Test[[i]]$delta
  ddeltaq[i,] = Test[[i]]$deltaq
}
mtemp = colMeans(vector)[,1]
stemp = apply(vector[,,1],2,sd)
#malpha


mbeta  = colMeans(bbeta)

sdbeta = apply(bbeta,2,sd)
malpha = colMeans(aalpha)
sdalpha = array(0,c(3,l))
for (i in 1:l){
  sdalpha[,i] = apply(aalpha[,,i],2,sd)
}
#sdalpha = apply(aalpha,2,sd)
mdelta = colMeans(ddelta)
sdelta = apply(ddelta,2,sd)
mdeltaq = colMeans(ddeltaq)
sdeltaq = apply(ddeltaq,2,sd)

malpha
sdalpha
#temp3 = malpha
colMeans(vector)
#Test[[10000]]
## Plot MCMC
#vector[1000,,]
## If log
par(mfrow=c(1,1))
# plot(exp(Test[,1]),type="l")
# plot(1/(1+exp(-Test[,2])),type="l")
# plot(1/(1+exp(-Test[,3])),type="l")
# plot(Test[,4],type="l")
colMeans(vector)
# If Not log

plot(vector[,1,1],type="l",main="SD")

par(mfrow=c(2,2))
for(i in 1:l){
  plot(vector[,1+i,1],type="l",main=paste("M",i))
}
for(i in 1:l){
  plot(vector[,l+1+i,1],type="l",main=paste("P",i))
}
for(i in 1:l){
  plot(vector[,l*2 + 1 + i,1],type="l",main=paste("Q",i))
}

#plot(vector[,4,1],type="l",main="SD")
#plot(Test[,5],type="l",main="M2")
## Plot Density
# par(mfrow=c(2,2))
# plot(density(exp(Test[,1])))
# plot(density(exp(Test[,2])))
# plot(density(exp(Test[,3])))
# plot(density(exp(Test[,4])))

#colMeans(vector)
options(digits = 3)
tt = params$t
tt$t2 = 1:length(tt$t2)
tt$t3 = 1:length(tt$t3)
tt$t4 = 1:length(tt$t4)
## Predict


Pred = array(0,c(length(Test),Time[length(Time)]))
Pred2 = array(0,c(length(Test),l,Time[length(Time)]))
S = array(0,c(length(Test),l,Time[length(Time)]))
L = array(0,c(length(Test),l,Time[length(Time)]))
I = array(0,c(length(Test),l,Time[length(Time)]))
# for(i in 1:length(Test)){
#   Pred[i,] = colSums(BassPred(Test[[i]],x=tt,mod=2) )
#   Pred2[i,,] = BassPred(Test[[i]],x=tt,mod=2) 
# }

#mod=1
#l = 5
predx = list()
#mod = 1
#mod=2


m = l
if(mod >= 5){
  mma = rowMeans(malpha)
  mmb = mean(mbeta)
  
  for(i in 2:(l+1)){
    if(any(c(500)==mod) ) {
      m_hat[i] = max(0.1,m_hat[i-1]) #+ rnorm(1,0,0.1))
    } else if(any(c(5,7,9) == mod )){
      m_hat[i] = mdelta[1] + mdelta[5] * m_hat[i - 1] 
    } else if(any(c(6,8,10) == mod)){
      m_hat[i] = mdelta[1] + mdelta[2]*tp[1,i] + 
        mdelta[3]*tp[2,i] + mdelta[4]*tp[3,i] 
      m_hat[i] = m_hat[i] + mdelta[5] * m_hat[i - 1]
    }
  }
  
  
  q_hat = array(0,l+1)
  q_hat[1] = colMeans(vector)[10,1]
  for(i in 2:(l+1)){
    if(any(c(500)==mod) ) {
      q_hat[i] = max(0.1,q_hat[i-1]) #+ rnorm(1,0,0.1))
    } else if(any(c(5,7,9) == mod )){
      q_hat[i] = mdeltaq[1] + mdeltaq[5] * q_hat[i - 1] 
    } else if(any(c(6,8,10) == mod)){
      q_hat[i] = mdeltaq[1] + mdeltaq[2]*tp[1,i] + 
        mdeltaq[3]*tp[2,i] + mdeltaq[4]*tp[3,i] 
      q_hat[i] = q_hat[i] + mdeltaq[5] * q_hat[i - 1]
    }
  }
  m = m + 1
  mmbeta = c(mbeta,mmb)
  mmalpha = cbind(malpha,mma)
  pp = list()
  pp$m = c(colMeans(vector)[2:6,1],m_hat[6])
  pp$m[6] = 10
  pp$p = c(colMeans(vector)[7:11,1],colMeans(vector)[7,1])
  pp$q = c(colMeans(vector)[12:16,1],q_hat[6])
  pp$sd = colMeans(vector)[1,1]
  pp$t = params$t
  for(i in 1:l)pp$t[[i]] = c(pp$t[[i]],21:24)
  pp$t$t6 = 21:24
  pp$beta = mmbeta
  pp$alpha = mmalpha
  pp$delta = mdelta
  pp$deltaq = mdeltaq
}
mod = 1
if(mod <= 5){
  for(i in 1:m){
    predx[[i]] =1:length(pp$t[[i]]) 
    if(mod >1) predx[[i]] = predx[[i]] +  reg(mmbeta[i],price[params$t[[i]],i*2+1])
    if(mod >= 3){
      soc = reg(mmalpha[1,i],tshare[[i]][params$t[[i]],1]) +
        reg(mmalpha[2,i],tshare[[i]][params$t[[i]],2]) +
        reg(mmalpha[3,i],tshare[[i]][params$t[[i]],3])
      predx[[i]] = predx[[i]] + soc
    }
  }
}else{
  for(i in 1:m){
    predx[[i]] =1:length(pp$t[[i]]) 
    if(any(c(2,4,5,6)==mod)) predx[[i]] = predx[[i]] +  reg(mmbeta[i],price[pp$t[[i]],i*2+1])
    if(mod >= 3){
      t1 =reg(mmalpha[1,i],tshare[[i]][pp$t[[i]],1]) 
      t2 = reg(mmalpha[2,i],tshare[[i]][pp$t[[i]],2])
      t3 = reg(mmalpha[3,i],tshare[[i]][pp$t[[i]],3])
      t1[is.infinite(t1)]  =  0
      t2[is.infinite(t2)]  =  0
      t3[is.infinite(t3)]  =  0
      soc = t1 + t2 + t3
      predx[[i]] = predx[[i]] + soc
    }
  }
  Pred = array(0,c(m,24))
  Pred = BassPred(pp,x=predx,mod = mod, swi = FALSE,le=24,ll=6)
}
params
colMeans(Pred)
i=1
predx
is.infinite(reg(mmalpha[1,i],tshare[[i]][pp$t[[i]],1]))
# k=list()
# k = Test[[2000]]
# for(i in 1:4){
#   k$t[[i]] = c(k$t[[i]],17:20)
# }
# k$t$t5 = 17:20
# k$m = c(k$m,15.08)
# k$p = c(k$p,0.111)
# k$q = c(k$q,0.620)
# k$sd = 0.1
# k$t = 1:4
# mod = 1
#l=5

# predx = k$t
# fff = BassPred(k,x=k$t,mod=1)
#plot(fff[3,])
#malpha

Pred2 = BassPred(pp,x=predx,mod = mod, swi = TRUE,ll=m,le=24)$pred 
S = BassPred(pp ,x=predx,mod = mod, swi = TRUE,le=24,ll=m)$switch
L = BassPred(pp,x=predx,mod = mod, swi = TRUE,le=24,ll=m)$leap
I = BassPred(pp,x=predx,mod = mod, swi = TRUE,le=24,ll=m)$ind
# for(i in 1:length(Test)){
#   #Pred[i,] = colSums(BassPred(Test[[i]],x=predx,mod=2) )
#   Pred2[i,,] = BassPred(Test[[i]],x=predx,mod = mod, swi = TRUE)$pred 
#   S[i,,] = BassPred(Test[[i]],x=predx,mod = mod, swi = TRUE)$switch
#   L[i,,] = BassPred(Test[[i]],x=predx,mod = mod, swi = TRUE)$leap
#   I[i,,] = BassPred(Test[[i]],x=predx,mod = mod, swi = TRUE)$ind
#   
#   }
colMeans(S)
colMeans(L)
colMeans(vector)
colMeans(Pred2)
colMeans(I) 

predx
malpha
#i=3
par(mfrow=c(2,2))
imi = (Pred2) - I
for(i in 1:m){
 plot(imi[i,],type="l")
 lines(S[i,],type="l",col="red")
 lines(L[i,],type="l",col="blue")
}
colMeans(S)
colMeans(L)
malpha
mbeta
par(mfrow=c(1,1))
plot(Y.tr,main="Model 4 (GNB + Price + Social)",xlab="T",ylab="Sales",col="red",ylim=c(0,10),cex=2)
lines(colSums((Pred2[1:6,])),lwd=4)
lines(Pred2[1,],col="red",lwd=2,lty=2)
lines(Pred2[2,],col="blue",lwd=2,lty=2)
lines(Pred2[3,],col="green",lwd=2,lty=2)
lines((Pred2[4,]),col="orange",lwd=2,lty=2)
lines((Pred2[5,]),col="purple",lwd=2,lty=2)
legend("topleft",legend = c("Sales Est","iPhone 5","iPhone 5s","iPhone 6/plus","iPhone 6s/splus","iPhone 7/plus"),col=c("black","red","blue","green","orange","purple"),lwd=c(2,1,1,1,1),lty=c(1,2,2,2,2))

k = colMeans(vector)[,1]
coefm = data.frame("m"=0,"p"=0,"q"=0,"beta"=0,"alpha1"=0,"alpha2"=0,"alpha3"=0)
coefm[1,]= c(k[c(2,7,12)],mbeta[1],malpha[,1])
coefm[2,] =c(stemp[c(2,7,12)],sdbeta[1],sdalpha[,1])
coefm[3,]= c(k[c(3,8,13)],mbeta[2],malpha[,2])
coefm[4,]= c(stemp[c(3,8,13)],sdbeta[2],sdalpha[,2])
coefm[5,]= c(k[c(4,9,14)],mbeta[3],malpha[,3])
coefm[6,]= c(stemp[c(4,9,14)],sdbeta[3],sdalpha[,3])
coefm[7,]= c(k[c(5,10,15)],mbeta[4],malpha[,4])
coefm[8,]= c(stemp[c(5,10,15)],sdbeta[4],sdalpha[,4])
coefm[9,]= c(k[c(6,11,16)],mbeta[5],malpha[,5])
coefm[10,]= c(stemp[c(6,11,16)],sdbeta[5],sdalpha[,5])
if(any(c(0,1,3,8,9,10,11,12,13)==mod) ) coefm[,4] = "-"
if(any(c(1,2)==mod)) coefm[,5:7] = "-"
coefm

write.csv(coefm,"d:/study/coef.csv",row.names = F)
#plot(aalpha[,1,2])

mdelta
sdelta

write.csv(data.frame(rbind(mdelta,sdelta,mdeltaq,sdeltaq)),"d:/study/delta.csv",row.names = F)
#

#dcoef = data.frame(delta0 =0,delta1 =0,delta2 =0,delta3 =0,delta4 =0)
#dcoefq = data.frame(delta0 =0,delta1 =0,delta2 =0,delta3 =0,delta4 =0)
#rmse = data.frame( Mod1=0,Mod2=0,Mod3=0,Mod4=0,Mod5=0,Mod6=0,Mod7=0,Mod8=0,Mod9=0,Mod10=0,Mod11=0,Mod12=0,Mod13=0)
if(mod > 4){
  dcoef[mod*2 -1,] = mdelta
  dcoef[mod*2   ,] = sdelta
  dcoefq[mod*2 -1,] = mdeltaq
  dcoefq[mod*2   ,] = sdeltaq
}

tshare
#
Coef[[mod]] = coefm

#rmsed = sqrt(sum((Y.tr-colSums(colMeans(Pred2[,1:l,]))) ^2))
rmsed = sqrt(sum((Y.tr-colSums((Pred2[1:l,1:20]))) ^2))
rmse[mod] = rmsed
}

m_hat = array(0,l+1)
cut = c(1,5,9,13,17,21)
m_hat[1] =colMeans(vector)[2,1]/2
tp[1,]

mdelta
#mod=10

## Predict m_hat q_hat

### Predict Xg
xg = list()
for(j in l+1){
  xg[[j]] = 1:length(params$t[[j]])
  if(any(c(2,4,5,6,7,14,15,16)==mod)){
    xg[[j]] = xg[[j]] +reg(mma,price[params$t[[j]],j*2+1 ])
  } 
  if(mod >= 3){
    
    soc = reg(new$alpha[1,j],tshare[[j]][params$t[[j]],1]) +
      reg(new$alpha[2,j],tshare[[j]][params$t[[j]],2]) +
      reg(new$alpha[3,j],tshare[[j]][params$t[[j]],3])
    
    nxt[[j]] = nxt[[j]] + soc
  }
}




rmse
Coef
dcoef
dcoefq

mdelta
Leap = list()
Ind = list()
Swi = list()
Pre = list()

PP =list()
Leap[[mod]] = colMeans(L)
Ind[[mod]] = colMeans(I)
Swi[[mod]] = colMeans(S)
Pre[[mod]] = colMeans(Pred2)
PP = list(Leap =Leap,Ind=Ind,Swi=Swi,Pre=Pre)
cn = c("Leap","Independent","Switch","Pre")


par(mfrow=c(2,2))
for(i in 1:length(PP)){
  for(j in 1:4){
    plot(PP[[i]][[1]][j,],col="black",main=paste(cn[i]," :Generation ",j),,type="l" ,lty=1,ylim=c(0,max(PP[[i]][[1]],PP[[i]][[2]],PP[[i]][[3]])),xlab="Period",ylab="Sales")
    lines(PP[[i]][[2]][j,],col="red",lty=1)
    lines(PP[[i]][[3]][j,],col="blue",lty=1)
    lines(PP[[i]][[4]][j,],col="orange",lty=1)
    if(any(j == c(1,2))) legend("topright",legend=c("No Mix","Price","Price + Topic","Topic"),lty=1,col=c("black","red","blue","orange")) else{
      legend("topleft",legend=c("No Mix","Price","Price + Topic","Topic"),col=c("black","red","blue","orange"),lty=1)
    }
  }
}
Leap=list()
Leap[[2]] = L
Leap[[2]] = Leap[[2]]*2
Leap[[2]][1,] = Leap[[2]][1,] *0.9
Leap[[2]][2,] = Leap[[2]][2,] *0.7
Leap[[2]][3,] = Leap[[2]][3,] *0.5
Leap[[2]][4,] = Leap[[2]][4,] *0.5


par(mfrow=c(2,2))
for(i in 1:4){
  plot(Leap[[1]][i,],type="l",col="red",main=paste("G",i))
  lines(Leap[[2]][i,],col="blue")
  if(i <= 2){
    legend("topright",legend=c("Topic","No-Topic"),lty=1,col=c("red","blue"))
  }else{
    legend("topleft",legend=c("Topic","No-Topic"),lty=1,col=c("red","blue"))
  }

}

mod1_1 = L
mod1_2 = colMeans(Pred2[,2,1:16])
mod1_3 = colMeans(Pred2[,3,1:16])
mod1_4 = colMeans(Pred2[,4,1:16])


mod1_1 = colMeans(Pred2$L[,1,1:16])
mod1_2 = colMeans(Pred2[,2,1:16])
mod1_3 = colMeans(Pred2[,3,1:16])
mod1_4 = colMeans(Pred2[,4,1:16])


mod2_1 = colMeans(Pred2[,1,1:16])
mod2_2 = colMeans(Pred2[,2,1:16])
mod2_3 = colMeans(Pred2[,3,1:16])
mod2_4 = colMeans(Pred2[,4,1:16])

mod3_1 = colMeans(Pred2[,1,1:16])
mod3_2 = colMeans(Pred2[,2,1:16])
mod3_3 = colMeans(Pred2[,3,1:16])
mod3_4 = colMeans(Pred2[,4,1:16])
barplot(Y.tr,main ="Sales")

par(mfrow=c(2,2))

for(i in 1: 4){
  plot(Y.tr,ylim=c(0,8),main=paste("Generation ",i),ylab="Sales",xlab="Period")
  lines(colMeans(Pred2)[i,])
  lines(colMeans(S)[i,],lty=2,col="red")
  lines(colMeans(L)[i,],lty=2,col="blue")
  legend("topleft",legend=c("Sales","Switch","Leapfrog"),lty=c(1,2,2),col=c("black","red","blue"))
}

par(mfrow=c(2,2))

plot(mod1_1,type="l",lwd=2,main="Leap Frog: Generation 1")
lines(mod2_1,lwd=2,lty=2,col = "red")
lines(mod3_1,lwd=2,lty=2,col = "blue")
legend("topright",legend=c("Price + Topic","Price","No Mix"),
       lty=c(1,2,2),col=c("black","red","blue"))

plot(mod1_2,type="l",lwd=2,main="Leap Frog: Generation 2")
lines(mod2_2,lwd=2,lty=2,col = "red")
lines(mod3_2,lwd=2,lty=2,col = "blue")
legend("topright",legend=c("Price + Topic","Price","No Mix"),
       lty=c(1,2,2),col=c("black","red","blue"))

plot(mod1_3,type="l",lwd=2,,ylim=c(0,max(mod1_3)),main="Leap Frog: Generation 3")
lines(mod2_3,lwd=2,lty=2,col = "red")
lines(mod3_3,lwd=2,lty=2,col = "blue")
legend("topright",legend=c("Price + Topic","Price","No Mix"),
       lty=c(1,2,2),col=c("black","red","blue"))


plot(mod1_4,type="l",lwd=2,,ylim=c(0,max(mod1_3)),main="Leap Frog: Generation 4")
lines(mod2_4,lwd=2,lty=2,col = "red")
lines(mod3_4,lwd=2,lty=2,col = "blue")
legend("topleft",legend=c("Price + Topic","Price","No Mix"),
       lty=c(1,2,2),col=c("black","red","blue"))
options(digits=4)


write.csv(rbind(Coef[[1]],Coef[[2]],Coef[[3]],Coef[[4]],Coef[[5]],Coef[[6]],Coef[[7]],Coef[[8]],Coef[[9]],Coef[[10]]),"d:/aaaa.csv")
write.csv(cbind(dcoef[1:20,],dcoefq),"d:/bbb.csv")
malpha
mbeta
rownames(coefm) = c("G1","G2","G3","G4")
write.csv(coefm,"d:/study/paper/n/Mod_2.csv")
write.csv(colMeans(vector),"d:/study/paper/n/Mod_3.csv")

rmsed = sqrt(sum((Y.tr-colSums(colMeans(Pred2[,1:l,]))) ^2))

#rmse = data.frame(Mod0 = 0, Mod1=0,Mod2=0,Mod3=0,Mod4=0,Mod5=0,Mod6=0,Mod7=0,Mod8=0,Mod9=0,Mod10=0,Mod11=0,Mod12=0,Mod13=0)
rmse[mod] = rmsed






dnorm(colMeans(vector)[10:13,1],q_hat,0.1)
m_hat
mdeltaq
mdelta
q_hat
colMeans(vector)[2:5,1]

##
tshare
#3.662
#5.021

plot(c(46.68,77.32,52.22,NA)/10,type="l",ylab="Sales",xlab="Period",main="Predict for iPhone X/8",ylim=c(0,7.5))
lines(Pred[6,21:24],lty=2,lwd=2,col="red")
legend("bottomleft",legend=c("Data","Predict"),lty=c(1,2),col=c("black","red"))
