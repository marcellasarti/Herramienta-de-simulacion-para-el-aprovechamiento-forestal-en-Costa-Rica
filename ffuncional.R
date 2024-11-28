# 1. Cociente de biomasa


cociente_biomasa <- function(ab, especie) {
  
  AB_por_sp <- by(ab, especie, sum)
  AB_por_sp <- data.frame(sp = names(AB_por_sp), AB = as.vector(unlist(AB_por_sp)))
  AB_por_sp$prop <- AB_por_sp$AB / sum(AB_por_sp$AB)
  AB_por_sp <- AB_por_sp[rev(order(AB_por_sp$AB)),]
  especies_80 <- AB_por_sp[cumsum(AB_por_sp$prop) <= 0.8, ]$sp
}


############################

# 2. Data para funcional

data_funcional <- function(data1, data2) {
  
  data_funcional <- data.frame(data1[data1$sp %in% cociente_biomasa(ab = data2$ab, especie = data2$sp),])
  
  
  data_funcional <- data_funcional %>% distinct(sp, .keep_all = TRUE)
  
  return(data_funcional)
}

##########################

# 3. Proporcion de N y riqueza de TFP o adquisitivas conservativas

prop_fun <- function(abun, TFP, sp) 
{
  data.frame(
    Sp = round(as.vector(unlist(by(abun, TFP, FUN = function(x) length(x)))) / length(sp), 2),
    Abun = round(as.vector(unlist(by(abun, TFP, sum))) / sum(abun), 2)
  )
}


#######

# 4 Indices funcionales

f_indices <- function(plot_80, parcela) {
  
plot_80<-as.data.table(plot_80)


if (all(is.na(plot_80$N_imput))) {
 
  plot_80 <- plot_80[complete.cases(sp, ab_sp, meanDM, AFE_imput, P_imput), 
                     .(sp, ab_sp, meanDM, AFE_imput, P_imput)]
} else {

  plot_80 <- plot_80[complete.cases(sp, ab_sp, meanDM, N_imput, AFE_imput, P_imput), 
                     .(sp, ab_sp, meanDM, N_imput, AFE_imput, P_imput)]
}

# Crear la matriz de rasgos funcionales

if (all(is.na(plot_80$N_imput))) {
  traits <- plot_80[, .(sp, meanDM, P_imput, AFE_imput)]
} else {
  traits <- plot_80[, .(sp, meanDM, N_imput, P_imput, AFE_imput)]
}


traits <- as.data.frame(traits)
rownames(traits) <- traits$sp
traits <- traits[, -1]
traits <- traits[order(rownames(traits)), ]


# Crear la matriz de abundancia
abundancia <- dcast(plot_80, . ~ sp, value.var = "ab_sp", fill = 0)
abundancia <- abundancia[, -1]

# Calcular la diversidad funcional
fd <- dbFD(x = traits, a = abundancia, stand.x = TRUE, corr = "none")

}