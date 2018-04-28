# Verification-bias

verification.bias.rates= function(GS,E,S,alpha=0.05){
  # GS: Gold standard
  # E: New marker
  # S: Standard marker (old marker)
  # We need the completed data

# Refference::
#homas Filleron,
#Comparing sensitivity and specificity of medical imaging tests when verification bias is present: The concept of relative diagnostic accuracy,
#European Journal of Radiology,
#Volume 98,
#2018,
#Pages 32-35,
#ISSN 0720-048X,
#https://doi.org/10.1016/j.ejrad.2017.10.022.
#(http://www.sciencedirect.com/science/article/pii/S0720048X17304175)
#Keywords: Screen positive; Relative accuracy; Gold standard




  n.GS= length(GS)
  n.E= length(E)
  n.S= length(S)
  if(!(n.GS==n.E & n.GS==n.S))  stop("Ahad: the lengths of GS, E, and S must be equal")

  data=  data.frame(GS,E,S)

 a= dim(data[GS==1 & E==1 & S== 1,]) [1]
 b= dim(data[GS==1 & E==0 & S== 1,]) [1]
 c= dim(data[GS==1 & E==1 & S== 0,]) [1]
 e= dim(data[GS==0 & E==1 & S== 1,]) [1]
 f= dim(data[GS==0 & E==0 & S== 1,]) [1]
 g= dim(data[GS==0 & E==0 & S== 1,]) [1]

 rTPR=(a+c)/(a+b)
 l.rTPR=rTPR*exp((-1*qnorm(1-(alpha/2))*(b+c))/((a+b)*(a+c)))
 u.rTPR=rTPR*exp((+1*qnorm(1-(alpha/2))*(b+c))/((a+b)*(a+c)))
 rFPR=(e+g)/(e+f)
 l.rFPR=rFPR*exp((-1*qnorm(1-(alpha/2))*(g+f))/((e+g)*(e+f)))
 u.rFPR=rFPR*exp((+1*qnorm(1-(alpha/2))*(g+f))/((e+g)*(e+f)))
 r=function(x) round(x,2)
 cat("rTPR (E:S)= ",r(rTPR), ", 95% CI: (",r(l.rTPR),", ",r(u.rTPR),")\n" )
 cat("rFPR (E:S)= ",r(rFPR), ", 95% CI: (",r(l.rFPR),", ",r(u.rFPR),")\n\n\n" )

 res=data.frame(value=c(rTPR,rFPR),
           lower=c(l.rTPR,l.rFPR),
           upper=c(u.rTPR,u.rFPR))
rownames(res) <- c("rTPR (E:S)" ,"rFPR (E:S)")
invisible ( res)
 }


 dd=verification.bias.rates(rbinom(1000,1,.2),
                         rbinom(1000,1,.2),
                         rbinom(1000,1,.2)
                         )
## rTPR (E:S)=  1.22 , 95% CI: ( 1.14 ,  1.3 )
## rFPR (E:S)=  1 , 95% CI: ( 0.98 ,  1.02 )
 dd
##               value     lower    upper
## rTPR (E:S) 1.216216 1.1359503 1.302154
## rFPR (E:S) 1.000000 0.9819231 1.018410
