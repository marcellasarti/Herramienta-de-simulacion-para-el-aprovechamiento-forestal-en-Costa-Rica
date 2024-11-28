
# 1. Asignar clases diametricas

Asignar_clases <- function(df, x) {
  df[, clases := fifelse(x >= 10 & x < 20, 'C1', 
                         fifelse(x >= 20 & x < 30, 'C2', 
                                 fifelse(x >= 30 & x < 40, 'C3', 
                                         fifelse(x >= 40 & x < 50, 'C4', 
                                                 fifelse(x >= 50 & x < 60, 'C5', 'C6')))))]
  return(df)
}

###################################

# 2. Función para seleccionar un individuo aleatorio

findv <- function(x) {
  xy <- density(x)$x
  sampled_value <- sample(xy[xy >= min(x) & xy <= max(x)], 1, replace = FALSE)
  round(sampled_value)  
}

# x es numero de individuos por parcela de bosques de ref. 

##################################

# 3. Vector de dbh


fsample <- function(x,n) {
  #xx<-table(x)
  prox<-prop.table(x)
  sample(x, n, replace = TRUE,prob=prox)

}


##################################

# 3. Area basal en metros

area_basal <- function(x) {
  dbh_m = x / 100
  (pi / 4) * (dbh_m^2)
  
}


################################

# 4. Area basal al azar

fab <- function(x) { 
  xy <- density(x)$x
  sampled_value <- sample(xy[xy >= min(x) & xy <= max(x)], 1, replace = FALSE)
}

#x es vector de ab de parcelas observadas

###############################

# 5. Vector de dbh acorde a las areas basales

Gsim <- function(n, G, Gesperada) {
  
  AB <- rexp(n, rate = 1/mean(G))
  
  AB <- pmin(pmax(AB, min(G)), max(G))
  
  fA <- Gesperada/ sum(AB)
  if(fA < 1){
    AbCor<-ifelse(AB >= 0.008835729, AB*fA, AB)  
} else {
  
  AbCor<-AB * fA
}
  D<-sqrt((4 * AbCor) / pi)*100
  pmin(pmax(D, 10), max(D))
  
}


#n <- length(dbh)

###############################
  
# 6. Numero sp en la parcela

fsp <- function(x) {
  xy <- density(x)$x
  sampled_value <- sample(xy[xy >= min(x) & xy <= max(x)], 1, replace = FALSE) 
  round(sampled_value)
}



# x es la riqueza de sp en las parcelas de referencia (q0)
##########################


# 7. etiqueta nueva 

etiqueta <- function(nsp, dbh) {
  sp<-paste0("sp", c(1:nsp))
  x<- c(sp,sample(sp, length(dbh)-nsp, replace = TRUE, prob = density(dbh, n=nsp)$y))
  
}
