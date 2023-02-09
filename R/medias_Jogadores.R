#' Médias dos jogadores
#' Função que calcula as médias individuais para os dez jogadores de uma determinada partida.
#' Para utilizar: medias_Jogadores()
# Função medias_jogadores ---------------------------------------------------------------------------------
medias_Jogadores <- function (url_jogador) {
  html_lido <- read_html(as.character(url_jogador))
  
  dados_jogador <- html_nodes(html_lido, 'table') %>%
    html_table()
  dados_jogador <- dados_jogador %>% map_df(as_tibble, .name_repair = 'minimal') %>%
    dplyr::select(Rating, ACS, KAST, 'K:D', ADR)
  
  dados_jogador$KAST <- parse_number(dados_jogador$KAST)
  
  means_jogador <- round(colMeans(dados_jogador, na.rm = T), 2)
  
  means_jogador[['KAST']] <- round(means_jogador[['KAST']], 0)
  
  return(means_jogador)
}