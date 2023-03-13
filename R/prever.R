#' prever
#' Função que prevê o resultado de determinada partida.
#' Para utilizar: prever(url)
#' @export
prever <- function(link){

  # Pegando os dados no link da partida ----------------------------------------------------------------------
  partida <- medias_Times(link)

  jogos_scale <- read.csv2('csv/partidas_teste.csv') %>% dplyr::select(-X, -ganhador)

  jogos_scale <- rbind(jogos_scale, partida)

  jogos_scale <- scale(jogos_scale)

  partida <- jogos_scale[nrow(jogos_scale),]

  partida <- t(partida)

  partida <- as.data.frame(partida)

  previsao <- compute(n, partida)

  previsao <- previsao$net.result[1]

  previsao <- previsao * 100

  return(previsao)

}
