#' prever
#' Função que prevê o resultado de determinada partida.
#' Para utilizar: prever(url)
#' @export
prever <- function(link){

  # Pegando os dados no link da partida ----------------------------------------------------------------------
  partida <- medias_Times(link)

  vars <- c('RND', 'R', 'ACS', 'KAST', 'KD', 'ADR', 'KPR', 'APR', 'FKPR', 'FDPR', 'K', 'D', 'A', 'FK', 'FD')

  for (i in vars) {
    new_var <- paste0(i, "_diff")
    partida[[new_var]] <- partida[[paste0("time1", i)]] - partida[[paste0("time2", i)]]
  }

  partida <- select(partida, ends_with("_diff"))

  partida_reversa <- partida* -1

  jogos_scale <- read.csv2('csv/partidas_teste_10_04_2023.csv') %>% dplyr::select(-X, -ganhador)

  jogos_scale <- rbind(jogos_scale, partida) %>% scale()

  partida <- jogos_scale[nrow(jogos_scale),]

  jogos_scale <- read.csv2('csv/partidas_teste_10_04_2023.csv') %>% dplyr::select(-X, -ganhador)

  jogos_scale_reverso <- rbind(jogos_scale, partida_reversa) %>% scale()

  partida_reversa <- jogos_scale_reverso[nrow(jogos_scale_reverso),]

  partida <- t(partida) %>% as.data.frame()

  previsao <- compute(n, partida)

  previsao <- previsao$net.result[1]

  partida_reversa <- t(partida_reversa) %>% as.data.frame()

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
