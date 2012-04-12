 modwptAnalysis=function(x,wavelet="d4", ULFmin = 0, ULFmax = 0.03, VLFmin = 0.03, VLFmax = 0.05, LFmin = 0.05, LFmax = 0.15, HFmin = 0.15, HFmax = 0.4, sampling=4,bandtolerance,relative)
{ # Auxiliary variables
  
  ULFpower=0;
  VLFpower=0;
  LFpower=0;
  HFpower=0;
  # Working with a zero mean signal
  x=x-mean(x)
  # Geting tree nodes
  ULFnodes=getNodes(ULFmin,ULFmax,sampling,bandtolerance,relative)
  VLFnodes=getNodes(VLFmin,VLFmax,sampling,bandtolerance,relative)
  LFnodes=getNodes(LFmin,LFmax,sampling,bandtolerance,relative)
  HFnodes=getNodes(HFmin,HFmax,sampling,bandtolerance,relative)
  
  # Get the maximum depth of the nodes
  depth=max(ULFnodes[1],VLFnodes[1],LFnodes[1],HFnodes[1])
  # Wavelet analysis
  wx=modwpt(x,wf=wavelet,n.levels=depth,boundary="reflection");
  
  # Power calculation
  
  ULFpower=getPower(wx,ULFnodes[1],ULFnodes[2],ULFnodes[3],wavelet)
  VLFpower=getPower(wx,VLFnodes[1],VLFnodes[2],VLFnodes[3],wavelet)
  LFpower=getPower(wx,LFnodes[1],LFnodes[2],LFnodes[3],wavelet)
  HFpower=getPower(wx,HFnodes[1],HFnodes[2],HFnodes[3],wavelet)
  
  # Store the results of the calculation
  power=list(ULF=ULFpower,VLF=VLFpower,LF=LFpower,HF=HFpower,depth=depth)

}