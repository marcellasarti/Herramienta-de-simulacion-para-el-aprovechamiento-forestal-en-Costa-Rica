# 1 PARCELAS

fplot<-cmpfun(function(Ddt, Dq0, Ddensidad, Dreglas, DAb){
  
  
  Ddt <- Asignar_clases(Ddt, Ddt$dbh_cm) #asigna clases diametricas
  
  # Generar parcela
  
  #dbh<-genplot(Ddt, fclases(Dprop_clases, findv(Ddensidad)))
  dbh<-try(fsample(Ddt$dbh_cm, findv(Ddensidad)), silent=TRUE)
           if(class(dbh)=='try-error'){
             Lerror<-saveRDS(list(Ddt$dbh_cm, findv(Ddensidad)), file ='C:\\error\\error1.rds' , ascii = FALSE, version = NULL,
                             compress = TRUE, refhook = NULL)
             stop()
           }

  G <- area_basal(dbh) #vector de ab
  Gesperada <-fab(DAb)
  n<- length(dbh)
  dbh <- Gsim(n, G, Gesperada)
  nsp<-fsp(Dq0)
  nombre<- c('meanDM', 'gremio_ecologico', 'comercial', 'protegido','TFP', 'classification', 'N_imput', 'AFE_imput', 'P_imput')
  parcela<-data.frame(sp=etiqueta(nsp, dbh),dbh)
  parcela<-merge(parcela, data.table(sp=paste0('sp', 1:nsp), Ddt[sample(1:nrow(Ddt),nsp, replace = TRUE),..nombre]), by= 'sp')
  
  
  
})


# 1.2. ABUNDANCIAS

abun <- cmpfun(function(df) {
  ab <-area_basal(df$dbh) # area basal
  df$ab <- ab
  
  df <- df %>%
    group_by(sp) %>%
    mutate(abun = n()) %>%
    mutate(ab_sp = sum(ab)) %>%
    ungroup()
})

# 2 INDICADORES

findi <- cmpfun(function(parcela, estres) {
  
  
  resultados_vector <- list()
  

  
  parcela$biomasa <- biomasa(parcela$dbh, parcela$meanDM, estres) # biomasa
  
  
  # Guardar resultados en el vector
  resultados_vector$G_plot_ha_m2 <- sum(parcela$ab)
  resultados_vector$AGB_ha_plot_ton <- sum(parcela$biomasa)
  
  # Densidad de arboles de >= 40cm, >=10cm, >=60cm
  
  resultados_vector$Densidad_10cm_ha <- sum(parcela$dbh >= 10)
  #print(parcela$dbh >= 10)
  resultados_vector$Densidad_15cm_ha <- sum(parcela$dbh >= 15)
  #print(parcela$dbh >= 15)
  resultados_vector$Densidad_40cm_ha <- sum(parcela$dbh >= 40)
  resultados_vector$Densidad_60cm_ha <- sum(parcela$dbh > 60)
  
  # AB de emergentes >= 40cm
  
  resultados_vector$G_emergentes_ha = sum(parcela$ab[parcela$dbh >= 40])
  
  # J invertida
  setDT(parcela)
  parcela <- Asignar_clases(parcela, parcela$dbh)
  ConteoClases<-parcela[,.N, by=.(clases)]
  
  clases <- dcast(ConteoClases, . ~ clases, value.var='N', fill=0)
  clases <- as.vector(unlist(clases[1, -1]))
  dt.clases <- distrbucion_J(clases)
  #dt.clases <- data.table(Dclases = apply(clases, 1, distrbucion_J))
  resultados_vector$Dclases <- dt.clases # agrego al vector 
  
  
  
  
  # INDICADORES DE BIODIVERSIDAD
  resultados_vector$ab_comer = sum(parcela$ab[parcela$comercial == "C"], na.rm = TRUE)
  resultados_vector$ab_comer_60 = sum(parcela$ab[parcela$comercial == "C" & parcela$dbh >= 60], na.rm = TRUE)
  
  parcela_div <- parcela %>%
    distinct(sp, .keep_all = TRUE)
  
  resultados_vector$H<-H(parcela_div$abun) #Shannon
  
  gremios<- prop_cat(parcela_div$abun, parcela_div$gremio_ecologico) # proporcion Shannon gremios
  
  # Inicializamos los valores de proporciones a NA
  resultados_vector$propor_ESCIOFITO <- NA
  resultados_vector$propor_HELIOFITO <- NA
  resultados_vector$propor_INDETERMINADO <- NA
  
  # Obtener los nombres de los gremios
  nombres_gremios <- sort(unique(parcela_div$gremio_ecologico))
  
  # Asignar proporciones a resultados_vector, si no existen se mantienen como NA
  for (i in seq_along(nombres_gremios)) {
    columna_abun <- paste0("propor_", nombres_gremios[i])
    
    # Verificamos si hay un valor para la clasificación, de lo contrario, asignamos NA
    if (!is.na(gremios[i])) {
      resultados_vector[[columna_abun]] <- gremios[i]
    }
  }

  resultados_vector$n_heliofito = sum(parcela$gremio_ecologico == "HELIOFITO", na.rm = TRUE)
  resultados_vector$n_esciofito = sum(parcela$gremio_ecologico == "ESCIOFITO", na.rm = TRUE)
  
   comerciales <- prop_cat(parcela_div$abun, parcela_div$comercial) # proporcion Shannon comercial
  
  nombres_comercial <- sort(unique(parcela_div$comercial))
  
  # Agregar resultados a parcela
  for (i in seq_along(nombres_comercial)) {
    columna<- paste0("propor_", nombres_comercial[i])
    resultados_vector[[columna]] <- comerciales[i]
  }
  
  resultados_vector$n_comercial = sum(parcela$comercial == "C", na.rm = TRUE)
  resultados_vector$n_nocomercial = sum(parcela$comercial == "NC", na.rm = TRUE)
  
  # Individuos protegidos
  resultados_vector$propor_protegi<- prop_pro(v1=parcela_div$abun, v2=ifelse(parcela_div$protegido=="P", TRUE, FALSE))
  
  
  resultados_vector$q0<- S(parcela_div$sp) # riqueza de sp (q0)
  
  resultados_vector$q1<- H1(parcela_div$abun) # Exponencial de Shannon (q1)

  resultados_vector$dif01 <- resultados_vector$q0-resultados_vector$q1
  
  resultados_vector$q2<- inv_D(parcela_div$abun) #Inverso de Simpson (q2)
  
  resultados_vector$dif12 <- resultados_vector$q0-resultados_vector$q2
  
  
  
  # Verificar si hay datos en la columna AFE_imput para descartar bst y bhp6
  if (all(is.na(parcela$AFE_imput))) {
    return(data.table(do.call(cbind,resultados_vector))) 
  }
  
  # INDICADORES FUNCIONALES
  plot_80 <- data_funcional(parcela, parcela)
  
  # Verificar si hay más de 3 valores únicos en AFE_imput
  if (length(unique(plot_80$AFE_imput)) <= 3 || length(unique(plot_80$sp)) < 2) {
    # Si la condición se cumple, asignamos NA solo a los indicadores funcionales
   
    func <- c("FRic", "FEve", "FDiv", "FDis", "CWM_DM", "CWM_N", 
                  "CWM_P", "CWM_AFE", "propor_TFP1", "propor_TFP2", 
                  "propor_TFP3", "propor_TFP4", "propor_TFP5", 
                  "S_prop_TFP1", "S_prop_TFP2", "S_prop_TFP3", 
                  "S_prop_TFP4", "S_prop_TFP5", "propor_conservativa", 
                  "propor_adquisitiva", "propor_intermedia", 
                  "n_conservativas", "n_adquisitivas", "n_intermedias")
    
   
    resultados_vector[func] <- NA
  } else {
    
  # Proporción de TFP
  resultados <- prop_fun(plot_80$abun, plot_80$TFP, plot_80$sp)
  
  Abun <- resultados$Abun
  Sp <- resultados$Sp
  
  nombres_TFP <- 1:5  # TFP1 a TFP5
  
  for (i in nombres_TFP) {
    columna_abun <- paste0("propor_TFP", i)
    columna_sp <- paste0("S_prop_TFP", i)
    
    # Verificar si el TFP actual existe en los resultados
    if (i %in% sort(unique(plot_80$TFP))) {
      index <- which(sort(unique(plot_80$TFP)) == i)
      resultados_vector[[columna_abun]] <- Abun[index]
      resultados_vector[[columna_sp]] <- Sp[index]
    } else {
      # Si el TFP no existe, agregar NA
      resultados_vector[[columna_abun]] <- NA
      resultados_vector[[columna_sp]] <- NA
    }
  }
  
  
  # Proporcion adquisitivas y conservativas
  conservativas <- prop_fun(plot_80$abun, plot_80$classification, plot_80$sp)
  Abun_con <- conservativas$Abun # solo hay indicador de proporcion
  nombres_conservativas <- sort(unique(plot_80$classification))
  
  # Inicializamos los valores de proporciones a NA
  resultados_vector$propor_conservativa <- NA
  resultados_vector$propor_adquisitiva <- NA
  resultados_vector$propor_intermedia <- NA
  
  for (i in seq_along(nombres_conservativas)) {
    columna_abun <- paste0("propor_", nombres_conservativas[i])
    
    # Verificamos si hay un valor para la clasificación, de lo contrario, asignamos NA
    if (!is.na(Abun_con[i])) {
      resultados_vector[[columna_abun]] <- Abun_con[i]
    } else {
      resultados_vector[[columna_abun]] <- NA
    }
  }
  
  # Conteo de cada clasificación
  resultados_vector$n_conservativas <- sum(plot_80$classification == "conservativa", na.rm = TRUE)
  resultados_vector$n_adquisitivas <- sum(plot_80$classification == "adquisitiva", na.rm = TRUE)
  resultados_vector$n_intermedias <- sum(plot_80$classification == "intermedia", na.rm = TRUE)
  
  
  # Indices funcionales y CWM
 
  fd <- f_indices(plot_80, parcela)
  
  resultados_vector$FRic <- ifelse("FRic" %in% names(fd), fd$FRic, NA)
  resultados_vector$FEve <- ifelse("FEve" %in% names(fd), fd$FEve, NA)
  resultados_vector$FDiv <- ifelse("FDiv" %in% names(fd), fd$FDiv, NA)
  resultados_vector$FDis <- ifelse("FDis" %in% names(fd), fd$FDis, NA)
  
 
  if ("CWM" %in% names(fd)) {
    # Extraer valores de CWM
    resultados_vector$CWM_DM <- ifelse("meanDM" %in% names(fd$CWM), fd$CWM$meanDM[1], NA)
    resultados_vector$CWM_N <- ifelse("N_imput" %in% names(fd$CWM), fd$CWM$N_imput[1], NA)
    resultados_vector$CWM_P <- ifelse("P_imput" %in% names(fd$CWM), fd$CWM$P_imput[1], NA)
    resultados_vector$CWM_AFE <- ifelse("AFE_imput" %in% names(fd$CWM), fd$CWM$AFE_imput[1], NA)
  } else {
    # Si CWM no está presente, asignar NA a todos los CWM
    resultados_vector$CWM_DM <- NA
    resultados_vector$CWM_N <- NA
    resultados_vector$CWM_P <- NA
    resultados_vector$CWM_AFE <- NA
  }
  
  # Redondear 
  resultados_vector <- lapply(resultados_vector, function(x) if (is.numeric(x)) round(x, 4) else x)
  
  }
  
  return(data.table(do.call(cbind,resultados_vector)))
  
})

# 3 GENERAR POSICIONES

# Detectar vecinos de un individuo a todos los arboles.

detectarVecinos <- cmpfun(function(p1, p2, x, y, distUmbral) {
  
  dd <- sqrt((p1 - x)^2 + (p2 - y)^2)
  dd < distUmbral
  
  
})

# Generar posiciones version 30/8

GenerarPosiciones <- cmpfun(function(nrows, ncols, dbh, distUmbralSmall, distUmbralMed,distUmbralGrand,
                                     dbhUmbralGrand, dbhUmbralSmall) {
  
  x <- seq(1, nrows, by = 1)
  y <- seq(1, ncols, by = 1)
  xy <- expand.grid(x, y)
  #sal<-data.frame(ID=which(dbh>dbhUmbral), xy1,dbh=dbh[dbh>dbhUmbral])
  #fdist<-function(DU) {
  x1 <- seq(1, nrows, by = distUmbralGrand)
  y1 <- seq(1, ncols, by = distUmbralGrand)
  
  xy1<-xy[xy$Var1 %in% x1 & xy$Var2 %in% y1, ]
  xyend <- xy1[sample(1:nrow(xy1), sum(dbh>dbhUmbralGrand)),] 
  xy<-xy[!(xy$Var1 %in% xyend$Var1 & xy$Var2 %in% xyend$Var2), ]
  #}
  dbhSub<-dbh[dbh<=dbhUmbralGrand]
  ################
  xyend$dbh<-dbh[which(dbh>dbhUmbralGrand)]
  xyend$ID<-which(dbh>dbhUmbralGrand)
  
  
  if(nrow(xyend)==0){
    dat <- xy[sample(1:nrow(xy), 1), ]
    xyend[1,c('Var1', 'Var2')]<-dat[,c('Var1', 'Var2')]
    xy <- xy[-which(rownames(xy) == rownames(dat)), ]
    xyend$dbh<-dbh[1]
    xyend$ID<-1
    dbhSub<- dbh[-1] 
    ID<-which(dbh<=dbhUmbralGrand)[-1]
  } else{
    ID<-which(dbh<=dbhUmbralGrand)
  }
  
  
  L<-list()
  for(i in 1:length(dbhSub)) {
    intentos <- TRUE
    #contar<-1
    dbh2<-dbhSub[i]
    while (isTRUE(intentos)) {
      dat <- xy[sample(1:nrow(xy), 1), ]
      xy <- xy[-which(rownames(xy) == rownames(dat)), ] 
      dat$dbh <-dbh2 
      
      
      
      if(dbh2>dbhUmbralSmall && dbh2<=dbhUmbralGrand) {
        TestDist <- detectarVecinos(p1 = dat$Var1, p2 = dat$Var2,
                                    x = xyend$Var1, y = xyend$Var2,
                                    distUmbral = distUmbralMed)
      } else {
        TestDist <- detectarVecinos(p1 = dat$Var1, p2 = dat$Var2,
                                    x = xyend$Var1, y = xyend$Var2,
                                    distUmbral = distUmbralSmall)
      }
      if (!any(TestDist)) {
        sal<-dat
        intentos <- FALSE
      }
      #contar<-contar+1
      #print(contar)
    }
    sal$ID<-ID[i]
    xyend<-rbind(xyend, sal)
  }
  #nrow(xyend)    
  xyend[sort(xyend$ID),][,c('Var1', 'Var2')]  
})

# 4 PERTURBACION

# Funcion perturbacion

plot_perturbada <- cmpfun(function(d, porcentajeAprov, dbhAprov, distUmbral2, PorcentajeareaDef, areaParcela,clarobinario) {
  d <- as.data.frame(d)
  ab20 <- sum(d$ab) * porcentajeAprov
  
  claro <- round((areaParcela * PorcentajeareaDef) / (pi * distUmbral2^2))  # distUmbral es el radio de un claro de 250m2
  
  # Seleccionar species comerciales con dap aprovechable
  dsub <- subset(d, comercial == 'C' & dbh >= dbhAprov)
  
  xy <- dsub[ifelse(cumsum(dsub$ab) > ab20,F,T), c('Var1', 'Var2')]
  
  if (nrow(xy) == 0) {
    return(NULL)  
  } else {
    if(clarobinario) if (nrow(xy) > claro) xy <- xy[1:claro,] else xy <- xy 
    
    V <- list()
    
    for (i in 1:nrow(xy)) {
      V[[i]] <- detectarVecinos(p1 = xy$Var1[i], p2 = xy$Var2[i],
                                x = d$Var1, y = d$Var2,
                                distUmbral2)
    }
    
    dd <- d[apply(do.call(cbind, V), 1, sum) < 1,]
    return(dd)
  }
})

