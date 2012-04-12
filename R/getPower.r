getPower=function(wx,depth,left,right,wf)
{
  # sum contributions
  power=0;
 
  for (j in left:right)
  {
      # index for wavelets coefficients returned by waveslim::modwt
    
      index=sum(2^(1:depth-1))+j;
      power=align(wx[[index]]^2,depth,j,wf)+power;
  
  }
  
  
  return(power);
}
  
