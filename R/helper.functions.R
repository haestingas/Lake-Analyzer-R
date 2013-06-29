## Helper functions for Lake Analyzer R

get.offsets <- function(data){
  
  header = names(data)
  
  header = header[header != "datetime"] #Drop datetime
  
  matches = regexpr("(\\d+\\.?\\d*)" ,header)
  
  lengths = attr(matches,'match.length')
  offsets = vector(mode="numeric", length=length(matches))
  
  for(i in 1:length(matches)){
    offsets[i] = as.numeric(substr(header[i], matches[i], matches[i] + lengths[i]))
  }
  
  return(offsets)
}


# Generic function to determine first differences of a vector x
# returns a vector of same length as x
# y is spacing between x measurements, ie differences are dx/dy
get.drho_dz <- function(x, y){

	# Assert x and y are same length
	stopifnot(length(x) == length(y))

	
	# first differences are computed as central differences where possible,
	# 	and forward/back differences otherwise
	# These vectors determine where to use which method
	c_back = unique( c( ( which( is.nan( x[-length(x)] ) ) - 1 ), length(x) ) )
	c_forward = setdiff( unique( c( 1, ( which( is.nan( x[-1] ) ) + 2) ) ), c_back )
	c_center = c(1:length(x))[-c(c_forward, c_back)]
	
	# differences are computed using R's vector math
	res = c(
		(x[c_forward + 1] - x[c_forward]) / (y[c_forward + 1] - y[c_forward]),
		(x[c_center + 1] - x[c_center - 1]) / (y[c_center + 1] - y[c_center - 1]),
		(x[c_back] - x[c_back - 1]) / (y[c_back] - y[c_back - 1])
	)
	
	# returns differences in appropriate order
	return(res[order(c(c_forward, c_center, c_back))])
}






