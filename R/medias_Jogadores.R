#' Médias dos jogadores
#' Função que calcula as médias individuais para os dez jogadores de uma determinada partida.
#' Para utilizar: medias_Jogadores()
#' @export
# Função medias_jogadores ---------------------------------------------------------------------------------
medias_Jogadores <- function(url_jogador){

  html_lido <- read_html(as.character(url_jogador))

  dados_jogador <- html_nodes(html_lido, 'table') %>%
    html_table()
  dados_jogador <- dados_jogador %>% map_df(as_tibble, .name_repair = 'minimal') %>%
    dplyr::select('Use', 'RND', 'Rating', 'ACS', 'KAST', 'K:D', 'ADR', 'KPR', 'APR', 'FKPR', 'FDPR',
                  'K', 'D', 'A', 'FK', 'FD')

  dados_jogador$Use <- as.numeric(gsub(".*\\((.*)\\).*", "\\1", dados_jogador$Use))

  dados_jogador$KAST <- parse_number(dados_jogador$KAST)

  dados_jogador[,2:ncol(dados_jogador)] <- lapply(dados_jogador[,2:ncol(dados_jogador)],
                                                  function(x, y) x * y, y = dados_jogador$Use)

  dados_jogador <- lapply(dados_jogador, sum, na.rm = T)

  dados_jogador <- lapply(dados_jogador, function(x, y) round(x / y, 2), dados_jogador$Use)

  medias_jogador <- dados_jogador

  medias_jogador[['KAST']] <- round(medias_jogador[['KAST']], 0)
  return(medias_jogador)
}
