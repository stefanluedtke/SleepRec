

applyPretrained = function(feature,data){
  #get correct model
  model = models[[feature]]


  #get correct data
  xval = data.frame(V1=data[,feature])


    res = applyHmm(model,xval)

  res
}


applyHmm = function(model,xval){
  xvalj=data.frame(apply(xval,2,jitter))
  xvalj=abs(xvalj)

  xvalj=log(xvalj)

  traindata=list(x=xvalj[,1],N=length(xvalj[,1]))

  yfit=predict(model,traindata)

  p=(yfit$s-2)*(-1)

}
