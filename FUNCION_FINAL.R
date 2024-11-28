# Funcion general para simular parcelas, indicadores y perturbacion 


fun_sim <- cmpfun(function(Ddt, Dq0, Ddensidad, Dreglas, DAb, DStress, ncols, nrows, distUmbralGrand, distUmbralMed,distUmbralSmall, dbhUmbralGrand, dbhUmbralSmall, porcentajeAprov, dbhAprov, distUmbral2, PorcentajeareaDef, areaParcela) {
  #for(i in 1:100){
 
# 1. Generacion de parcelas
 
  parcela <-fplot(Ddt, Dq0, Ddensidad, Dreglas, DAb)
 
  #preguntar si tiene comerciales, si tiene, que estime estres, posiciones, indic antes y despues, en caso contra
  #rio necesito otra parcela
  
  # 
  
  # 2. estres para indicadores tb1 y tb2
  
  estres<- fstress(DStress)
  
  
  # 3. Indicadores antes de la perturbacion
  
    tb1 <- data.frame(perturbacion = "No", aprovechamiento = "No", findi(parcela, estres))
    
    
    
    # 4. Posiciones de arboles
 
    
    d<-cbind(parcela, GenerarPosiciones(nrows=nrows, ncols=ncols, distUmbralGrand=distUmbralGrand,
                                         distUmbralMed=distUmbralMed, distUmbralSmall=distUmbralSmall, 
                                        dbh=parcela$dbh, dbhUmbralGrand=dbhUmbralGrand, dbhUmbralSmall=dbhUmbralSmall)[,c('Var1', 'Var2')])
    
    #fwrite(d, paste0('C:/simulacionPLot/fNP', i, '.csv') )
    
    # 5. Perturbacion 
    
   dd<-plot_perturbada(d, porcentajeAprov, dbhAprov, distUmbral2, PorcentajeareaDef, areaParcela)
  
     #fwrite(dd, paste0("C:/simulacionPLot/fP", i, ".csv"))
  

    # 6. Indicadores post perturbacion 
   

   tb2 <- indicadores_post(dd, estres)

   
columnas_faltantes_tb2 <- setdiff(colnames(tb1), colnames(tb2))
for (col in columnas_faltantes_tb2) {
  tb2[[col]] <- NA
}

tb2 <- tb2[, colnames(tb1)]

  tbfinal<-rbind(tb1, tb2)
  
#}
})
