f <- function(yaxis=NULL,yaxis2=NULL,yaxis3=NULL){
  return(list(yaxis,yaxis2,yaxis3))
}

do.call(f(yaxis2='a',...),list(yaxis=2))
