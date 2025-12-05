library(ggplot2)
library(dplyr)

calcular_coordenadas <- function(angulo_graus, raio, centro_x = 0, centro_y = 0) {
  
  # CONVERSÃO: Graus * (pi / 180) = Radianos
  angulo_rad <- angulo_graus * (pi / 180)
  
  x <- centro_x + raio * cos(angulo_rad)
  y <- centro_y + raio * sin(angulo_rad)
  angulo <- angulo_graus
  
  return(data.frame(x = x, y = y, angulo = angulo))
}

simulate_run <- function(coordendas, simulation_number){
  if(simulation_number %% 100 == 0){
    cat(paste('\rRunning Simulation:', simulation_number))  
    flush.console()
  }
  data_sim <-
    slice_sample(coordendas, prop = 1) |> 
    dplyr::rename(angulo_saida = angulo,
                  x_saida = x,
                  y_saida = y) |> 
    mutate(angulo_chegada = lead(angulo_saida),
           x_chegada = lead(x_saida),
           y_chegada = lead(y_saida)) |> 
    #filter(!is.na(angulo_chegada)) |> 
    mutate(distancia = sqrt(
      (x_saida - x_chegada)^2 + (y_saida - y_chegada)^2
    ))
  
  data_sim |> 
    mutate(simulation_number) |> 
    # comentar para fazer o gráfico da simulacao
    summarise(distancia = sum(distancia, na.rm = TRUE))
}


n_pessoas <- 35

angulos <- seq(0, 360, length.out = n_pessoas+1)
angulos <- angulos[-1]


raio_circulo <- 2.5
pontos_coord <- purrr::map_df(angulos, ~ calcular_coordenadas(angulo = .x, raio_circulo))


results_list <- list()

all_simulations <- c(30, 100, 1000, 10000, 100000)

for(i in all_simulations){
  print('Evaluating')
  print(paste(i, 'Simulations'))
  simulations <- seq(1:i)
  
  final_df <- 
    purrr::map_df(simulations, ~ simulate_run(coordendas = pontos_coord, simulation_number = .x))
  
  results_list[as.character(i)] <- final_df
}


concat_results <- function(x){
  data <- x
  
  data.frame(
    numero_rep = rep(length(data), length(data)),
    resultados = data
  )
}

all_info <- purrr::map_df(results_list, concat_results)

df_medias <- all_info |>
  group_by(numero_rep) |>
  summarise(media_valor = mean(resultados),
            p_95 = quantile(resultados, .99),
            p_05 = quantile(resultados, .05))
 

all_info |> 
  ggplot(aes(resultados))+
  geom_histogram(color = 'white')+
  geom_vline(data = df_medias, aes(xintercept = media_valor), 
             color = "black", linetype = "dashed", size = 1) +
  geom_vline(data = df_medias, aes(xintercept = p_95), 
             color = "red", linetype = "dashed", size = 1)+
  # --- NOVOS TEXTOS ---
  
  # 1. Texto da Média
  geom_text(data = df_medias, 
            aes(x = media_valor, y = Inf, label = round(media_valor, 2)), 
            color = "black", angle = 90, vjust = -0.5, hjust = 1.2) +
  
  # 2. Texto do P95
  geom_text(data = df_medias, 
            aes(x = p_95, y = Inf, label = round(p_95, 2)), 
            color = "red", angle = 90, vjust = -0.5, hjust = 1.2) +
  labs(title = 'Histograma dos resultados das simulações', subtitle = 'Foram testadas 30, 100, 1000, 10 mil e 100 mil simulações')+
  facet_wrap(~numero_rep, ncol = 1, scales = 'free_y')+
  theme_bw()


# final_df |> 
#   group_by(simulation_number) |> 
#   summarise(d = sum(distancia, na.rm = TRUE)) |> 
#   ggplot(aes(x=d))+
#     geom_histogram()

# Exemplo simulações gráfico
# final_df |> 
#   ggplot()+
#   geom_point(aes(x_saida, y_saida))+
#   geom_segment(aes(x = x_saida, y = y_saida, 
#                    xend = x_chegada, yend = y_chegada),
#                arrow = arrow(length = unit(0.3, "cm"))
#                )+
#   coord_fixed()+
#   labs(title = 'Resultado de 30 Simulações utilizando 10 pontos')+
#   facet_wrap(~simulation_number)+
#   theme_bw()+
#   theme(axis.title = element_blank())
