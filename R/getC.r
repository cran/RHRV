# vector c attached to each node in the modwpt transform (Percival and Walden, page 215)
# vector c will be used to align LA wavelet coefficients
getC=function(j,n)
{
  vectorC=c();
  if (j==1)
  {
    if (n==0) vectorC=0
    else vectorC=1
    return(vectorC)
      
  }
  else
  {
    if (((n%%4)==0)||((n%%4)==3))
      return(c(getC(j-1,floor(n/2)),0))
    else 
      return(c(getC(j-1,floor(n/2)),1))
  }
  
}

# Auxiliary function used to calculate time shifts for LA wavelets(Percival and walden,229-230)
getSc=function(j,n)
{
  vectorC=getC(j,n)
  return(sum(vectorC*2^(0:(j-1))))
  
}
# Auxiliary function used to calculate time shifts for LA wavelets(Percival and walden,229-230)
getLj=function(j,wf)
{
  #length of the wave filter
  L=length(wave.filter(wf)$lpf)
  Lj=(2^j-1)*(L-1)+1
  return(Lj)
}

#getVjnLA returns time shifts for la8,la16 and la20 wavelets in modwpt
getVjnLA=function(j,n,wf)
{
  return(abs(-getLj(j,wf)/2+(2^(j-1)-getSc(j,n))))
}

#performs left circular shift
circularShift=function(x,n)
{
  l=length(x)
  if ((n==0)||(n==l)) return(x)
  if ((n<0)||(n>l)) {stop("Circular shift error\n");return(x)}
  return(c(x[(n+1):l],x[1:n]))
}
# aligns <<x>> wavelet coefficients from modwpt using <<wf>> wavelet. <<x>> are the
# (j.n) node wavelet coefficients. <<wf>> should be "l8","l16" or "l20". If not, not 
# alignment is perform
align=function(x,j,n,wf)
{
  #return(x)
  if ((wf!="la8")&&(wf!="la16")&&(wf!="la20")) 
  {
     return(x)
  }
  else
  {
    return(circularShift(x,getVjnLA(j,n,wf)))
  }
}