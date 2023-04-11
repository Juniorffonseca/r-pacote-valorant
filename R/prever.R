#' prever
#' Função que prevê o resultado de determinada partida.
#' Para utilizar: prever(url)
#' @export
prever <- function(link){

  # Pegando os dados no link da partida ----------------------------------------------------------------------
  partida <- medias_Times(link)

  jogos_scale <- read.csv2('csv/partidas_teste_10_04_2023.csv') %>% dplyr::select(-X, -ganhador)

  jogos_scale <- rbind(jogos_scale, partida)

  jogos_scale <- scale(jogos_scale)

  partida <- jogos_scale[nrow(jogos_scale),]

  partida <- t(partida)

  partida <- as.data.frame(partida)

  previsao <- compute(n, partida)

  previsao <- previsao$net.result[1]

  partida_reversa <- partida

  partida_reversa[1:30] <- partida_reversa[, c(16:30, 1:15)]

  previsao2 <- compute(n, partida_reversa)

  previsao2 <- previsao2$net.result[1]

  a <- previsao
  b <- previsao2

  transforma_probabilidade <- function (y, x){
    z = y / (y + x)
    w = x / (x + y)
    c = as.matrix(c(z,w))
    return(c)
  }

  previsao <- transforma_probabilidade(a,b)

  previsao <- previsao * 100

  previsao <- paste0(round(previsao, 2), '%', '')

  nomes_times <- read_html(link) %>%
    html_nodes('div.wf-title-med') %>%
    html_text() %>% str_replace_all('\n', '') %>% str_replace_all('\t', '')

  previsao <- paste(nomes_times, previsao, ' ')

  return(previsao)

}
