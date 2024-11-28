library(FD)



source(paste0(DIR, 'festructura.r'))
source(paste0(DIR, 'ffuncional.r'))
source(paste0(DIR, 'fdiversidad.r'))




funcionParalelaIndicadores<-cmpfun(function(x) {
    findi(x, fstress(Destres))
})