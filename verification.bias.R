# verification.bias

 verification.bias.rates= function(GS,E,S,alpha=0.05){
  # GS: Gold standard
  # E: New marker
  # S: Standard marker (old marker)
  # We need the completed data

 # Refference::
    # Comparing sensitivity and specificity of medical imaging tests when
    # verification bias is present: The concept of relative diagnostic accuracy
    # Thomas Filleron



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
 dd



