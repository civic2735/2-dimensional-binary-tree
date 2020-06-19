rm(list=ls())
# hold Y; exchange targer X
binomial_option <- function(S1, S2, r, T, N, sigma1, sigma2, rho, type2, eta1, eta2)
{
  p = p_prob(r= r, delta_t= T/N, sigma= sigma1, sigma2= sigma2, eta1=eta1, eta2=eta2, rho=rho)
  tree_X = tree_stock(S= S1, N= N, delta_t= T/N, sigma= sigma1)
  tree_Y = tree_stock(S= S2, N= N, delta_t= T/N, sigma= sigma2)
  option = value_binomial_option(r= r,N=N, delta_t= T/N, sigma= sigma1, sigma2= sigma2, eta1=eta1, 
                                 eta2=eta2, rho=rho, type2=type2, tree_X, tree_Y)
  #delta = (option[2,2]-option[2,1])/(tree[2,2]-tree[2,1])
  return(list(prob= p, stock_X= tree_X, stock_Y= tree_Y, option= option, price= option[1,1,1]))
  #return(list(prob= p, stock_X= tree_X, stock_Y= tree_Y))
}

#=================================================================================================
# build tree
tree_stock <- function(S, N, delta_t, sigma) {
  tree = matrix(NA, nrow=N+1, ncol=N+1)
  u = exp(sigma*sqrt(delta_t))
  d = (1/u)
  
  for (i in 1:(N+1)) {
    for (j in 1:i) {
      tree[i,j] = S*u^(j-1)*d^(i-j)
    }
  }
  return(tree)
}

#================================================================================================
# probability
p_prob <- function(r, delta_t, sigma1, sigma2, rho, eta1, eta2) {
 a= numeric(2)
 a[1]<- (1/sigma1)*(r-eta1-0.5*sigma1^2)
 a[2]<- (1/sigma2)*(r-eta2-0.5*sigma2^2)
 uu=0.25*(1+rho+(a[1]+a[2])*sqrt(delta_t))
 ud=0.25*(1-rho+(a[1]-a[2])*sqrt(delta_t))
 du=0.25*(1-rho-(a[1]-a[2])*sqrt(delta_t))
 dd=0.25*(1+rho-(a[1]+a[2])*sqrt(delta_t))
 #p=matrix(c(uu,ud,du,dd), nrow=2, ncol=2, byrow=TRUE)
 p=matrix(NA,2,2)
 p[1,1]=uu; p[1,2]=ud; p[2,1]=du; p[2,2]=dd
 return(p)
}
#================================================================================================
# vanila option
value_binomial_option <- function(r, N, delta_t, sigma1, sigma2,rho, eta1, eta2, type2,
                                  tree_X, tree_Y) {
  
  p = p_prob(r, delta_t, sigma1, sigma2, rho, eta1, eta2)
  option_tree= array(NA,dim=c(N+1,N+1,N+1))
  for (i in 1:(N+1)) {
    for (j in 1:(N+1)) {
      option_tree[i,j,(N+1)]= max( tree_X[(N+1),i]-tree_Y[(N+1),j] , 0)
    }
  }
  #American Option
  if (type2=="american") {
    
    for (k in N:1) {
      for (i in 1:k) {
        for(j in 1:k) {
          exercise_payoff <- max(tree_X[k,i]-tree_Y[k,j] , 0)
          hold_payoff <- (p[1,1]*option_tree[i+1,j+1,k+1] + p[1,2]*option_tree[i+1,j,k+1]+
                          p[2,1]*option_tree[i,j+1,k+1] + p[2,2]*option_tree[i,j,k+1] )/exp(r*delta_t)
          option_tree[i,j,k] <- max(exercise_payoff, hold_payoff) 
        }
      }
    }  
  }
  #European Option
  if (type2=="european") {                         
    for (k in N:1) {
      for (i in 1:k) {
        for(j in 1:k) {
          option_tree[i,j,k] <- (p[1,1]*option_tree[i+1,j+1,k+1] + p[1,2]*option_tree[i+1,j,k+1]+
                            p[2,1]*option_tree[i,j+1,k+1] + p[2,2]*option_tree[i,j,k+1] )/exp(r*delta_t)

        }
      }
    }  
  }

  return(option_tree)
}


#=======================================================================================================#
# test
# binomial_option(S1=25, S2=30, r=0.08, T=1/360, N=2, sigma1=5, sigma2=1, rho=0,
#               type2="american", eta1=0, eta2=0)
# binomial_option(S1=25, S2=30, r=0.08, T=1/360, N=2, sigma1=5, sigma2=1, rho=0,
#               type2="european", eta1=0, eta2=0)

#foreign exchange option
# a=binomial_option(S1=60, S2=60, r=0.02, T=90/360, N=80, sigma1=0.02355, sigma2=0.01945, rho=0.3, 
#                 type2="american", eta1=0, eta2=0)
# a=binomial_option(S1=60, S2=60, r=0.02, T=90/360, N=80, sigma1=0.02355, sigma2=0.01945, rho=0.5, 
#                   type2="european", eta1=0, eta2=0)
#================================================================================


#option price is a decreasing function of rho, since when rho is negative 
#price difference of X,Y is large and the value of option is higher
#; when rho is positive price difference of X,Y is small and the value is lower

rho_change=matrix(c('rho','a1','a2','b1','b2','c1','c2','d1','d2'),1,9)

for (i in 0:10){
  temp=matrix(0,1,9)
  rho=-1+0.2*i
  #1USD- 1.3571CAD
  a1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.3667, rho, 
                     type2="american", eta1=0.05*0.01, eta2=0.1*0.01)
  #1.3571CAD- 1USD
  a2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.3667, sigma2=0.1129, rho, 
                     type2="american", eta1=0.1*0.01, eta2=0.05*0.01)
  #1USD- 1.4770AUS
  b1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.1141, rho, 
                     type2="american", eta1=0.05*0.01, eta2=0.05*0.01)
  #1.4770AUS- 1USD
  b2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1141, sigma2=0.1129, rho, 
                     type2="american", eta1=0.05*0.01, eta2=0.05*0.01)
  #1USD- 1315.4185KRW
  c1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.6417, rho, 
                     type2="american", eta1=0.05*0.01, eta2=1.37*0.01)
  #1315.4185KRW- 1USD
  c2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.6417, sigma2=0.1129, rho, 
                     type2="american", eta1=1.37*0.01, eta2=0.05*0.01)
  #1USD- 7.1990CNY
  d1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.0422, rho, 
                     type2="american", eta1=0.05*0.01, eta2=0.3*0.01)
  #7.1990CNY- 1USD
  d2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.0422, sigma2=0.1129, rho, 
                     type2="american", eta1=0.3*0.01, eta2=0.05*0.01)
  temp[1,1]=rho
  temp[1,2]=a1$price
  temp[1,3]=a2$price
  temp[1,4]=b1$price
  temp[1,5]=b2$price
  temp[1,6]=c1$price
  temp[1,7]=c2$price
  temp[1,8]=d1$price
  temp[1,9]=d2$price
  
  rho_change=rbind(rho_change,temp)
}

save(rho_change,file='rho_change.RData')








# tree_X = tree_stock(S= 100, N= 2, delta_t= 5/24, sigma= 0.25)
# tree_Y = tree_stock(S= 20, N= 2, delta_t= 5/24, sigma= 0.1)
# p = p_prob(r=0.08, delta_t=5/24, sigma1=0.25, sigma2=0.1, rho=0.1, eta1=0, eta2=0)
# N=2
# r=0.08
# delta_t= 5/24
# option_tree= array(NA,dim=c(N+1,N+1,N+1))
# for (i in 1:(N+1)) {
#   for (j in 1:(N+1)) {
#     option_tree[i,j,(N+1)]= max( tree_X[(N+1),i]-tree_Y[(N+1),j] , 0)
#   }
# }
# for (k in N:1) {
#   for (i in 1:k) {
#     for(j in 1:k) {
#       option_tree[i,j,k] <- (p[1,1]*option_tree[i+1,j+1,k+1] + p[1,2]*option_tree[i+1,j,k+1]+
#                                p[2,1]*option_tree[i,j+1,k+1] + p[2,2]*option_tree[i,j,k+1] )/exp(r*delta_t)
# 
#     }
#   }
# }




