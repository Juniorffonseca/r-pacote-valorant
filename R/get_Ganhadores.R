#' get_Ganhadores
#' Função que identifica o ganhador de determinada partida.
#' Para utilizar: get_Ganhadores(url)
#' @export
# Função get_Ganhadores ---------------------------------------------------------------------------------
get_Ganhadores <- function(url_partida){
  placar <- read_html(url_partida) %>%
    html_nodes("div.js-spoiler") %>% html_text(trim=T)

  placar <- str_replace_all(placar, '\t', '') %>% str_replace_all('\n', '')

  placar <- as.data.frame(placar[1])

  placar <- separate(placar, 'placar[1]', into = c('Time1', 'Time2'), sep = ':', extra = 'merge')

  if(placar$Time1 != placar$Time2){
  ifelse(placar$Time1 > placar$Time2, ganhador <- 1, ganhador <- 0)
  }

  if(placar$Time1 == placar$Time2){
    ganhador <- 'empate'
  }

  return(ganhador)
}
