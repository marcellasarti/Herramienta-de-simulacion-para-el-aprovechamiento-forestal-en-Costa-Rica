library(future.apply)
options(future.globals.maxSize = 1e9)

ncols = 100
nrows = 100
distUmbralGrand = 10
distUmbralMed = 4
distUmbralSmall = 2
dbhUmbralGrand = 60
dbhUmbralSmall=30
porcentajeAprov = 0.20 
dbhAprov = 60 
distUmbral2=8.92
PorcentajeareaDef = 0.12
areaParcela = 10000

ff<-cmpfun(function() {
  
  # Generar parcela ##################################
  
  plan(multisession,  workers = procesadores)
  L <- future_replicate(nperm, fplot(Ddt, Dq0, Ddensidad, Dreglas, DAb), simplify = FALSE)
  
  # Abundancias antes de la perturbacion
  plan(multisession,  workers = procesadores)
  L<- future_lapply(L, abun, future.seed=TRUE)
  
  # Generar posiciones
  plan(multisession,  workers = procesadores)
  L<-future_lapply(L, function(x) funcionParalelaGPosiciones(x), future.seed=TRUE)
  
  # Elimina los parcelas que no cumplen con especies comerciales
  L<- Filter(function(x) !(length(x) == 1 && is.na(x) || length(x) == 0), L)

  
  # Generar perturbación ##################################
  
  plan(multisession,  workers = procesadores)
  L.pert<- future_lapply(L, function(x) funcionParalelaPerturbado(x), future.seed=TRUE)
  
  # Abundancias despues de la perturbacion
  
  plan(multisession,  workers = procesadores)
  L.pert<- future_lapply(L.pert, abun, future.seed=TRUE)
  
 saveRDS(L, paste0(DIR, ZZ,'plotNperturbado2.rds'))
 saveRDS(L.pert, paste0(DIR, ZZ,'plotPerturbado2.rds'))
  
# Estimar los indicadores ################################
  

 L<-ftablaresult(L=L)
 
 L$Perturbacion<-'NO'
 
  # Despues de la perturbacion

 
 L.pert<-ftablaresult(L=L.pert)
 
 L.pert$Perturbacion<-'SI'
 
  # End ############
  
  #rbind(L, L.pert)
  return(list(L = L, L.pert = L.pert))
  
})