`build_hr` <-
function(beat){
# build the instantaneus heart rate signal from a beat position aray
# 
# Builing the matrix
	N=length(beat)
	hr<-matrix(nrow=N,ncol=2)
# Fill firts column with beat positions
	hr[1:N,1]=beat[1:N]
# and second column with heart rate instant values
	hr[1,2]=1000000/(beat[2]-beat[1])  # not a real data
	hr[2:N,2]=1000000/(beat[2:N]-beat[1:N-1])
	hr[,2]<-as.integer(hr[,2])
	return(hr)
}

