# 1. Funcion Shannon 
H <- function(x) -sum(x/sum(x)*log(x/sum(x)))


########################

#2. Funcion proporciones Shannon
prop_cat <- function(v1, v2) as.vector(unlist(by(v1,v2, H)))/ H(v1)

########################

# 3. Funcion proporciones protegido

prop_pro  <- function(v1, v2) sum(v1[v2], na.rm = TRUE)/sum(v1, na.rm = TRUE)

#######################


# 4. Funcion riqueza

S <- function(x) length(unique(x))

######################

# 5. Shannon exponencial, q1

H1 <- function(x) exp(-sum(x/sum(x)*log(x/sum(x))))

#####################

# 6. Funcion inverso de Simpson

inv_D <- function (x) 1/sum((x / sum(x))^2)

#####################

# 7. Funcion AB comercial
AB_comer <- function(v1, v2){
  
  dbh_filtrado <- v1[v2 == "C"]
  AB_comer_indv <- area_basal(dbh_filtrado)
  AB_comer_total <- sum(AB_comer_indv, na.rm = TRUE)
  
}

#######################

# 8. Funcion AB comercial y cosechable

AB_comer60 <- function(v1, v2){
  
  dbh_filtrado <- v1[v2 == "C" & v1 >= 60]
  AB_comer_indv <- area_basal(dbh_filtrado)
  AB_comer_total <- sum(AB_comer_indv, na.rm = TRUE)
  
}