`load_beat_file` <-
function(file){
# build an array of beats postions from and ascii file
#
 	x=read.table(file)
# converting seconds to miliseconds
	x$V1=x$V1*1000
# copying data in an integer array
	beat<-integer(length(x$V1))
	beat<-as.integer(x$V1)
#
 	return(beat)
}

