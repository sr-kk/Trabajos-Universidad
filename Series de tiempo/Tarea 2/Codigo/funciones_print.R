summary_coef <- function(tabla, caption = NULL){
  nombres <- rownames(tabla)
  l <- length(nombres)
  coeficientes <- vector(length = l)
  for(i in 1:l){
      if(str_sub(nombres[i], 1, 2) == "ar"){
          coeficientes[i] <- str_c("\\phi", "_",str_sub(nombres[i], 3))
        }
      else{
          coeficientes[i] <- str_c("\\theta", "_",str_sub(nombres[i], 3))
        }
    }

  rownames(tabla) <- coeficientes
  colnames(tabla) <- NULL
  knitr::kable(tabla, digits = 3, format = "latex", booktab = TRUE,
               col.names = c("Estimacion", "Error est.", "Estadístico Z", "Valor-p"),
               row.names = TRUE, escape = FALSE, caption = caption)
}

