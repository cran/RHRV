`hr_interpolator` <-
function(hr,fsamp=4,mini=hr[1,1],maxi=hr[length(hr)/2,1]){
# spline interpolator for build the sample heart rate signal
#
	npoints=as.integer((maxi-mini)*fsamp/1000)
	List_hr3=spline(hr,n=npoints,method="natural",xmin=mini,xmax=maxi)
        hr3<-matrix(nrow=npoints,ncol=2)
	hr3[1:npoints,1]=as.integer(List_hr3$x)
	hr3[1:npoints,2]=as.integer(List_hr3$y)
	return(hr3)
}

