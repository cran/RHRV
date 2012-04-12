getNodes=function(lowerf,upperf,sampling,error,relative)   {
   # put the lower/upper frequency in a node
   
   lowerNode=getNodesAux(lowerf,sampling,error,"lower",abs(upperf-lowerf),relative);
   upperNode=getNodesAux(upperf,sampling,error,"upper",abs(upperf-lowerf),relative);
   
   # Merge the nodes
   if (lowerNode[1]==upperNode[1]) #Nodes at the same level
   {
      nodes=c(lowerNode[1],lowerNode[2],upperNode[2]);
   }
   else #Nodes at different levels
   {
      nodes=MergeNodes(lowerNode,upperNode);
   }
   
   return(nodes)

}
