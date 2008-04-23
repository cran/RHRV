`plot_hr` <-
function(data,width=12,height=5,colour=4){
# simple plot with configurable size and colour
#
	X11(width=width,height=height)
	plot(data,type='l',main='Heart rate',xlab='time', ylab='Frequency',col=colour)
}

