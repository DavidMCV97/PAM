source("C:\\Users\\david.martindelcampo\\Documents\\Tesis\\R_rotacion\\funciones.R") #importamos las funciones del archivo auxiliar
library(dplyr) # libreria con funciones especiales

################### VARIABLES X DEFINIR MANUALMENTE ############################

E<- 5 # numero de negociadores
cartera <- c(1,0.5,1,1,1.2) # porcentaje de cartera que le toca a cada negociador
max_tol_a <- 1000 # tolerancia objetivo ahorro
max_tol_d <- 100000 # tolerancia objetivo deuda
max_tol_nd <- 10 # tolerancia objetivo numero de deudas
min_tol_a <- 300 # tolerancia mínima ahorro
min_tol_d <- 30000 # tolerancia mínima deudas
min_tol_nd <- 3 # tolerancia mínima numero de deudas
max_it <- 10000 # máximas iteraciones
# max_t <- 10 # tiempo máximo en minutos

########################## importación de los datos ############################

df <- generar_df("C:/Users/david.martindelcampo/Documents/Tesis\\base_3_R.xlsx")
df <- snake(df,E)
seguimiento <- data.frame(matrix(NA,floor(max_it/9)+1,10))
colnames(seguimiento) <- c("mins","max_dist_a_0","max_dist_a_1","max_dist_a_2","max_dist_a_3","max_dist_a_4","max_dist_a_5","max_dist_a_6","max_dist_d","max_dist_nd")

######################## PREPARACIÓN PARA ROTACIÓN #############################

inicio <- Sys.time() # guardamos la hora de inicio del proceso
counter <- 0 # contamos cuantos ciclos llevamos

ahorros <- val_a(df,E) # calculamos el ahorro de cada nego de cada potencial
deuda <- val_d(df,E) # calculamos la deuda de cada nego
num_deudas <- val_nd(df,E) # calculamos el número de deudas de cada nego

for (p in 0:6) { # generamos los ideales de ahorro
  assign(paste("alpha_",p,sep = ""),sum(ahorros[,p+1])/sum(cartera))
}
delta <- sum(df$d)/sum(cartera) # ideal de la deuda
mu <- sum(df$nd)/sum(cartera) # ideal del número de deudas

dist_a <- ahorros #calculamos cuánto dista del ideal cada ahorro
for (p in 0:6) {
  dist_a[,p+1] <- dist_a[,p+1] - get(paste("alpha_",p,sep = ""))*cartera
}
dist_d <- deuda - delta*cartera # cuanto dista del ideal cada deuda
dist_nd <- num_deudas - mu*cartera # cuanto dista del ideal cada # de deudas

mins <- 0 # cuantos minutos han pasado
seg_a0 <- max(abs(dist_a[,1]))
seg_a1 <- max(abs(dist_a[,2]))
seg_a2 <- max(abs(dist_a[,3]))
seg_a3 <- max(abs(dist_a[,4]))
seg_a4 <- max(abs(dist_a[,5]))
seg_a5 <- max(abs(dist_a[,6]))
seg_a6 <- max(abs(dist_a[,7]))
seg_d <- max(abs(dist_d))
seg_nd <- max(abs(dist_nd))
seguimiento[1,] <- c(mins,seg_a0,seg_a1,seg_a2,seg_a3,seg_a4,seg_a5,seg_a6,seg_d,seg_nd)

################################### ROTACIÓN ###################################


obj <- 0
while ((max(abs(dist_a)) > max_tol_a || max(abs(dist_d)) > max_tol_d || max(abs(dist_nd)) > max_tol_nd) & counter <= max_it) { # mientras las tolerancias fijas se superen...
  pasa <- TRUE
  while (obj <= 6 & pasa) { # si el objetivo es ahorro...
    if (max(abs(dist_a[,obj+1])) > min_tol_a) {
      M <- which.max(dist_a[,obj+1]) # para el potencial obj, cuál es el nego con más ahorro
      m <- which.min(dist_a[,obj+1]) # cuál es el nego con menos ahorro
      tol_din_d <- min(abs(dist_d[M]),abs(dist_d[m])) # tolerancia dinámica de la deuda
      tol_din_nd <- min(abs(dist_nd[M]),abs(dist_nd[m])) # tolerancia dinámica # de deudas
      referencias <- sample(rownames(df[(df$n == M)&(df$p == obj)&(df$d < tol_din_d)&(df$nd < tol_din_nd),])) # sample aleatorio de los candidatos
      n <- length(referencias) # vemos cuántos candidatos tenemos
      if (n>0) { # mientras haya candidatos
        rengl <- 1 # empezamos por el primer candidato
        referencia <- referencias[rengl] # sacamos la referencia del primer candidato
        dist_0 <- dist_a[M,obj+1] - dist_a[m,obj+1] # la meta a mejorar es la suma de cuánto distan M y m del óptimo (m siempre es negativo)
        dist_1 <- abs(dist_a[M,obj+1] - df[referencia,]$a) + abs(dist_a[m,obj+1] + df[referencia,]$a) # la suma de distancias con el cambio del candidato
        while (dist_1 >= dist_0 & rengl+1 <= n) { # si el candidato no mejora la distancia, y aún tenemos candidatos...
          rengl <- rengl + 1 #pasamos al siguiente candidato
          referencia <- referencias[rengl] # pasamos al siguiente candidato
          dist_1 <- abs(dist_a[M,obj+1] - df[referencia,]$a) + abs(dist_a[m,obj+1] + df[referencia,]$a) # volvemos a calcular la suma de distancias
        } # se termina el ciclo...
        dist_a[M,obj+1] <- dist_a[M,obj+1] - df[referencia,]$a # calcula las nuevas distancias (si no hubo mejora posible, deja el cambio en el último cliente)
        dist_a[m,obj+1] <- dist_a[m,obj+1] + df[referencia,]$a
        dist_d[M] <- dist_d[M] - df[referencia,]$d
        dist_d[m] <- dist_d[m] + df[referencia,]$d
        dist_nd[M] <- dist_nd[M] - df[referencia,]$nd
        dist_nd[m] <- dist_nd[m] + df[referencia,]$nd
      }
      counter <- counter + 1
      obj <- obj + 1
      pasa <- FALSE
    } else(obj <- obj + 1)
  }
  if (pasa & obj == 7){
    if (max(abs(dist_d)) > min_tol_d) {
      M <- which.max(dist_d)
      m <- which.min(dist_d)
      tol_din_nd <- max(min_tol_nd,min(abs(dist_nd[M]),abs(dist_nd[m])))
      referencias<- c()
      for (p in 0:6) {
        tol_din_a <- max(min_tol_a,min(abs(dist_a[M,p+1]),abs(dist_a[m,p+1])))
        referencias <- c(referencias,rownames(df[(df$n == M)&(df$p == p)&(df$nd < tol_din_nd)&(df$a < tol_din_a),]))
      }
      referencias <- sample(referencias)
      n <- length(referencias)
      if (n > 0) {
        rengl <- 1
        referencia <- referencias[rengl]
        dist_0 <- dist_d[M] - dist_d[m]
        dist_1 <- abs(dist_d[M] - df[referencia,]$d) + abs(dist_d[m] + df[referencia,]$d)
        while (dist_1 >= dist_0 & rengl+1 <= n){
          rengl <- rengl
          referencia <- referencias[rengl]
          dist_1 <- abs(dist_d[M] - df[referencia,]$d) + abs(dist_d[m] + df[referencia,]$d)
        }
        p <- df[referencia,]$p
        dist_a[M,p+1] <- dist_a[M,p+1] - df[referencia,]$a
        dist_a[m,p+1] <- dist_a[m,p+1] + df[referencia,]$a
        dist_d[M] <- dist_d[M] - df[referencia,]$d
        dist_d[m] <- dist_d[m] + df[referencia,]$d
        dist_nd[M] <- dist_nd[M] - df[referencia,]$nd
        dist_nd[m] <- dist_nd[m] + df[referencia,]$nd
      }
      counter <- counter + 1
      obj <- 8
      pasa <- FALSE
    } else{obj <- 8}
  }
  if (pasa & obj == 8){
    if (max(abs(dist_nd)) > min_tol_nd) {
      M <- which.max(dist_nd)
      m <- which.min(dist_nd)
      tol_din_d <- max(min_tol_d,min(abs(dist_d[M]),abs(dist_d[m])))
      referencias<- c()
      for (p in 0:6) {
        tol_din_a <- max(min_tol_a,min(abs(dist_a[M,p+1]),abs(dist_a[m,p+1])))
        referencias <- c(referencias,rownames(df[(df$n == M)&(df$p == p)&(df$d < tol_din_d)&(df$a < tol_din_a),]))
      }
      referencias <- sample(referencias)
      n <- length(referencias)
      if (n>0){
        rengl <- 1
        referencia <- referencias[rengl]
        dist_0 <- dist_nd[M] - dist_nd[m]
        dist_1 <- abs(dist_nd[M] - df[referencia,]$nd) + abs(dist_nd[m] + df[referencia,]$nd)
        while (dist_1 >= dist_0 & rengl+1 <= n){
          rengl <- rengl
          referencia <- referencias[rengl]
          dist_1 <- abs(dist_nd[M] - df[referencia,]$nd) + abs(dist_nd[m] + df[referencia,]$nd)
        }
        p <- df[referencia,]$p
        dist_a[M,p+1] <- dist_a[M,p+1] - df[referencia,]$a
        dist_a[m,p+1] <- dist_a[m,p+1] + df[referencia,]$a
        dist_d[M] <- dist_d[M] - df[referencia,]$d
        dist_d[m] <- dist_d[m] + df[referencia,]$d
        dist_nd[M] <- dist_nd[M] - df[referencia,]$nd
        dist_nd[m] <- dist_nd[m] + df[referencia,]$nd
      }
      counter <- counter + 1
      obj <- 0
      pasa <- FALSE
    }else{obj <- 0}
  }
  
  if (counter%%9 == 0) {
    mins <- difftime(Sys.time(),inicio,units = "mins") # cuantos minutos han pasado
    seg_a0 <- max(abs(dist_a[,1]))
    seg_a1 <- max(abs(dist_a[,2]))
    seg_a2 <- max(abs(dist_a[,3]))
    seg_a3 <- max(abs(dist_a[,4]))
    seg_a4 <- max(abs(dist_a[,5]))
    seg_a5 <- max(abs(dist_a[,6]))
    seg_a6 <- max(abs(dist_a[,7]))
    seg_d <- max(abs(dist_d))
    seg_nd <- max(abs(dist_nd))
    seguimiento[counter/9 + 1,] <- c(mins,seg_a0,seg_a1,seg_a2,seg_a3,seg_a4,seg_a5,seg_a6,seg_d,seg_nd)
    if (all(seguimiento[counter/9,2:10] == seguimiento[counter/9 + 1,2:10])) {
      browser()
    }
  }
}




