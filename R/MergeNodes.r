MergeNodes=function(lowerNode,upperNode)
{
  
  if (lowerNode[1]<upperNode[1])
  {
    exponent=upperNode[1] - lowerNode[1] ;
    lowerNode[2] = lowerNode[2]*(2^exponent);
    i=upperNode[1]
  }
  else
  {
    exponent=lowerNode[1] - upperNode[1] ;
    upperNode[2] = upperNode[2]*(2^exponent)+2^exponent-1;
    i=lowerNode[1]
  }

  nodes=c(i,lowerNode[2],upperNode[2])
}