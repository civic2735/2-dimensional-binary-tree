#兩幣別交換價格極度接近，因為期初執行價值=0(不做深度價內外發行)，略為差異在於eta。

#1USD- 1.3571CAD
a1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.3667, rho=-0.1794, 
                 type2="american", eta1=0.05*0.01, eta2=0.1*0.01)
#1.3571CAD- 1USD
a2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.3667, sigma2=0.1129, rho=-0.1794, 
                  type2="american", eta1=0.1*0.01, eta2=0.05*0.01)
#1USD- 1.4770AUS
b1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.1141, rho=-0.7072, 
                  type2="american", eta1=0.05*0.01, eta2=0.05*0.01)
#1.4770AUS- 1USD
b2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1141, sigma2=0.1129, rho=-0.7072, 
                  type2="american", eta1=0.05*0.01, eta2=0.05*0.01)
#1USD- 1315.4185KRW
c1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.6417, rho=-0.1072, 
                  type2="american", eta1=0.05*0.01, eta2=1.37*0.01)
#1315.4185KRW- 1USD
c2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.6417, sigma2=0.1129, rho=-0.1072, 
                  type2="american", eta1=1.37*0.01, eta2=0.05*0.01)
#1USD- 7.1990CNY
d1=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.1129, sigma2=0.0422, rho=0.4627, 
                  type2="american", eta1=0.05*0.01, eta2=0.3*0.01)
#7.1990CNY- 1USD
d2=binomial_option(S1=29.86, S2=29.86, r=0.04, T=90/360, N=80, sigma1=0.0422, sigma2=0.1129, rho=0.4627, 
                  type2="american", eta1=0.3*0.01, eta2=0.05*0.01)

#====================================================================
#rho_change
#應該要是漂亮的遞減。因為相關係數為負一個漲一個跌，價差大，價值大。
#因為相關係數為正同漲跌，價差小，價值小。

