filter <- function(class_in,filter_span) {
  filt_class<-0
  k<-0
  
  for(k in 1:length(class_in)){
    
    if (k < (filter_span + 1)){
      filt_class[k]<-class_in[k]
    } else {
      if(k > (length(class_in) - filter_span)){
        filt_class[k]<- class_in[k] 
      } else {
        filt_class[k]<-Mode(class_in[(k - filter_span):(k + filter_span)])
      }
    }
  }
  return(filt_class)
  }