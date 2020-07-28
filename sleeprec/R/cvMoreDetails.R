
doCVMoreDetails = function(features){
  tp = function(actual,prediction){
    sum(actual==1 & prediction == 1,na.rm=T)
  }
  fn = function(actual,prediction){
    sum(actual==1 & prediction == 0,na.rm=T)
  }
  tn = function(actual,prediction){
    sum(actual==0 & prediction == 0,na.rm=T)
  }
  fp = function(actual,prediction){
    sum(actual==0 & prediction == 1,na.rm=T)
  }
  sens = function(actual,prediction){
    tp = tp(actual,prediction)
    fn = fn(actual,prediction)
    tp/(tp+fn)
  }
  spec = function(actual,prediction){
    tn = tn(actual,prediction)
    fp = fp(actual,prediction)
    tn/(tn+fp)
  }
  precision = function(actual,prediction){
    tp = tp(actual,prediction)
    fp = fp(actual,prediction)
    tp/(tp+fp)
  }
  f1 = function(actual,prediction){
    prec = precision(actual,prediction)
    rec = sens(actual,prediction)
    2 * (prec*rec) / (prec+ rec)
  }
  
  
  
  if(!"label" %in% colnames(features)){
    setnames(features, old="groundtruth", new="label")
  }
  
  
  features=features[!is.na(features$label),]
  l=rep(0,length(features$label))
  l[features$label=="n1"|features$label=="n2"|features$label=="n3"|features$label=="n4"|
      features$label=="rem"]=1
  features$label=l
  
  #take the temperature stuff out
  features=features[,!(colnames(features) %in% c("tempskin_f","tempref_f"))]
  
  ### get list of patients
  patients=unique(features$PATIENT)
  findices=c(3:(ncol(features)-2))
  
  
  erg = do.call("rbind",lapply(1:length(findices),function(j){
    print(j)
    train=getTrain(features,findices[j],patients)
    label=getLabel(features,findices[j],patients)
    
    cole = lopocvComplete(function(xtrain,ytrain,xval){coleCV(xtrain,ytrain,xval)},train,label,patients)
    sadeh =lopocvComplete(function(xtrain,ytrain,xval){sadehCV(xtrain,ytrain,xval)},train,label,patients)
    lda = lopocvComplete(ldaCV,train,label,patients)
    logreg = lopocvComplete(logregCV,train,label,patients)
    hmm = lopocvComplete(hmmCV,train,label,patients)
    
    
    dd=data.frame(cole=cole$predict,sadeh=sadeh$predict,lda=lda$predict,logreg=logreg$predict,
                  hmm=hmm$predict,actual=cole$actual,subject=cole$subject,findex=findices[j],
                  fname=colnames(features)[findices[j]],time=features$time_f[!is.na(features[,findices[j]])])
  }))
  
  #-----------------evaluate-------------
  ergm = melt(erg,id.vars = c("actual","subject","findex","fname","time"))
  colnames(ergm) = c("actual","subject","findex","fname","time","algorithm","prediction")
  
  ergm %>%
    group_by(subject,fname,algorithm) %>%
    dplyr::summarise(acc = mean(actual==prediction,na.rm = T),
                     sens = sens(actual,prediction),
                     spec = spec(actual,prediction),
                     recall = sens(actual,prediction),
                     precision = precision(actual,prediction),
                     youden = sens(actual,prediction)+spec(actual,prediction)-1,
                     f1 = f1(actual,prediction)) -> performance
  
  performancem = melt(performance,id.vars=c("subject","fname","algorithm"))
  colnames(performancem) = c("subject","fname","algorithm","measure","value")
  
  
  performancem
  
}