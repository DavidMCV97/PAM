library(readxl) # libreria para leer excel

generar_df <- function(direccion){
  #importamos info, limpiamos, ordenamos y agregamos columna de nego.
  df <- data.frame(read_excel(direccion)) # leemos la base de datos, la volvemos en dataframe
  rownames(df) <- df$ref # que la referencia sea el nombre de la columna
  df$ref <- NULL # eliminamos la columna de referencia
  colnames(df) <- c("nd","d","a","p")
  df$n <- NA # agregamos una columna vacía para el negociador
  df <- arrange(df,p,desc(a),desc(d),nd) # ordenamos el df
  return(df)
}

snake <- function(df,E){
  #asignación inicial por metodo de snake-draft.
  snake <- c(1:E,E:1) # generamos el snake
  suppressWarnings(df$n[1:nrow(df)] <- snake) #asignamos el snake
  return(df)
}

val_a <- function(df,E){
  # calcula el ahorro de cada negociador para cada potencial
  res <- matrix(data = 0, nrow = E, ncol = 7)
  for (e in 1:E) {
    for (pot in 0:6) {
      res[e,pot+1] <- sum(df[(df$n == e)&(df$p == pot),]$a)
    }
  }
  return(res)
}

val_d <- function(df,E){
  res <- numeric(E)
  for (e in 1:E) {
    res[e] <- sum(df[df$n == e,]$d)
  }
  return(res)
}

val_nd <- function(df,E){
  res <- numeric(E)
  for (e in 1:E) {
    res[e] <- sum(df[df$n == e,]$nd)
  }
  return(res)
}
