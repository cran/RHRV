`wfdb_import` <-
function(filein,annotator="qrs",PATH='.'){
# import data from a record in wfdb format
	dir=getwd()
	fileout=paste(filein,"_ascii",sep="")
	annotator="qrs"
	setwd(PATH)
	command=paste("rdann -r ",filein," -a",annotator," -p \'N\' -x")
	x1=system(command,intern=TRUE)
	x2=substring(x1,1,9)
	write(x2,file=fileout)
	setwd(dir)
}

