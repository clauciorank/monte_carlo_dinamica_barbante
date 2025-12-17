library(ggplot2)


original_sequence <- seq(1, 11)

last_row <- original_sequence
rows <- list()
rows[[1]] <- last_row

i <- 1
while(length(last_row) > 1){
  # drop first
  last_row <- last_row[2:length(last_row)]
  # Drop last
  last_row <- last_row[-length(last_row)]
  i <- i + 1 
  rows[[i]] <- c(rep(NA, (last_row[1] -1)), last_row, rep(NA, (last_row[1] -1)))
}


df <- t(data.frame(rows))
colnames(df) <- as.character(seq(1, ncol(df)))
rownames(df) <- NULL

# df[linhas, colunas]
df <- df[nrow(df):1, ]

tabuleiro <-
  data.frame(df) |> 
  mutate(camada = nrow(df):1) |> 
  tidyr::pivot_longer(-camada) |> 
  mutate(name = as.integer(stringr::str_remove(name, 'X'))) |> 
  tidyr::drop_na()

## Simulation
jogada <- function(numero_de_linhas_tabuleiro){
  
  inicio <- numero_de_linhas_tabuleiro # No seu código, inicio começa em 11
  posicao <- c(inicio)
  
  # CORREÇÃO: Parênteses em volta da subtração
  for(i in 1:(numero_de_linhas_tabuleiro - 1)){
    sorteio <- sample(c(-1, 1), 1)
    inicio <- inicio + sorteio
    posicao <- c(posicao, inicio)
  }
  
  return(list(
    posicoes = posicao, # Agora terá exatamente 11 elementos
    resultado = inicio
  ))
}

numero_de_bolinhas <- 1000

resultados <- purrr::map(1:numero_de_bolinhas, ~jogada(nrow(df)))

func_df_posicoes <- function(result){
  data.frame(
    jogada = (length(result$posicoes)):1,
    result = result$posicoes
  )
}

criar_grafico_bolinha <- function(df_position_bolinha){
  lista_graficos <- list()
  
  for(i in 1:nrow(df_position_bolinha)){
    p <-
      tabuleiro |> 
      ggplot(aes(name, camada))+
      geom_point()+
      geom_point(data = df_position_bolinha |> filter(jogada >= i), aes(x = result, jogada), color = 'red')
    lista_graficos[[i]] <- p
  }
  return(lista_graficos)
}

gerar_grafico_para_uma_bola <- function(numero_da_bola){
  position_bolinha_filtrado <- positions |> filter(bolinha == numero_da_bola)
  
  graficos <- rev(criar_grafico_bolinha(position_bolinha_filtrado))
}

positions <- purrr::map_df(resultados, func_df_posicoes) |> 
  mutate(bolinha = sort(rep(1:numero_de_bolinhas, nrow(df))))


algumas_bolinha <- map(1:10, gerar_grafico_para_uma_bola)


somente_saidas <- map_vec(resultados, \(x) x$resultado)


somente_saidas[1]

algumas_bolinha[[1]][1]


