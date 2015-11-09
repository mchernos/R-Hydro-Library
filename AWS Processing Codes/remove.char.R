remove.char = function(x,char, n = 2){
	# n--> 2 = character before #, 1 = after #
	x[grep(char,x)] = matrix(unlist(strsplit(x[grep(char,x)], char)), ncol = 2, byrow = T)[,n]
	return(as.numeric(x))
}