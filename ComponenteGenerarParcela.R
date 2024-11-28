

 source(paste0(DIR, 'FUNCIONES.R'))
 source(paste0(DIR, 'fsimulacion.R'))
 
 
 
 
funcionParalelaGPosiciones<-cmpfun(function(x) {
  
  x1<-x[!is.na(x$comercial), ]
  if(any(x1$comercial=='C' & x1$dbh >=dbhAprov)) {
  return(cbind(x, GenerarPosiciones(nrows=nrows, ncols=ncols, 
      distUmbralGrand=distUmbralGrand,
       distUmbralMed=distUmbralMed, distUmbralSmall=distUmbralSmall, 
           dbh=x$dbh, dbhUmbralGrand=dbhUmbralGrand, 
               dbhUmbralSmall=dbhUmbralSmall)[,c('Var1', 'Var2')])
  )
  } else {return(NA)}
})








#V<-c()
# for(i in 1:10000) V[i]<-any(L[[16]]$comercial=='C' & L[[16]]$dbh >=60 )



