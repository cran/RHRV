stringToStringList <- function(string) {
#-------------------------------
# convert one string in a list with the single strings 
# example: "aaa bbb ccc" -> c("aaa","bbb","ccc")
#-------------------------------
 
	limit = nchar(string)
	aux=""
	final=c()
	for(i in 1:limit)
	{
		if(substr(string,i,i) == " ")
		{
			if(nchar(aux) > 0)
			{
				final=c(final,aux)
				aux=""
			}
		}
		else
		{
			aux = paste(aux,substr(string,i,i),sep="")
		}
	}
	return(final)
}
