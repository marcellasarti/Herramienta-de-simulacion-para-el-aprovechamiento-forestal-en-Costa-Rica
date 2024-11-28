# 1. J invertida
distrbucion_J <- function(x){
  n <- c()
  #for (i in 1:5) {
  i <- 1
  while(i < (length(x)+1)) {  
    if (i < length(x)) { 
      if (x[i] > x[i + 1] && x[i] > 0 && x[i + 1] > 0) {
        n[i] <- (length(x)/ i) * 2
      } else if (x[i] == x[i + 1] && x[i] > 0) {
        n[i] <- length(x) / i
      } else {
        n[i] <- 0 
      }
    } else {
      if (x[i - 1] > x[i] && x[i] > 0) {
        n[i] <- (length(x) / i) * 2 
      } else if (x[i - 1] == x[i] && x[i] > 0) {
        n[i] <- length(x) / i
      } else {
        n[i] <- 0 
        
      }
    }

    i<-i+1
  }
  return(sum(n)*length(x)/6)
}

#x<- c(300,200,100,75,70,30)
############################

# 2. Area basal

area_basal <- function(x) {
  dbh_m = x / 100
  (pi / 4) * (dbh_m^2)
}


###########################

# 3. Funcion asignacion estres

fstress <- function(x) {
  if (length(unique(x)) == 1) {
    return(unique(x))
  } else {
    xy <- density(x)$x
    sampled_value <- sample(xy[xy >= min(x) & xy <= max(x)], 1, replace = FALSE)
    return(sampled_value)
  }
}

#########################

# 4. Biomasa

biomasa <- function(dbh, WD, Stress)exp(-2.024-(0.896*Stress)+(0.920*log(WD))+(2.795*log(dbh))-(0.0461*log(dbh)^2))/1000


##########

# Asignar clases

# 1. Asignar clases diametricas

Asignar_clases <- function(df, x) {
  df[, clases := fifelse(x >= 10 & x < 20, 'C1', 
                         fifelse(x >= 20 & x < 30, 'C2', 
                                 fifelse(x >= 30 & x < 40, 'C3', 
                                         fifelse(x >= 40 & x < 50, 'C4', 
                                                 fifelse(x >= 50 & x < 60, 'C5', 'C6')))))]
  return(df)
}
