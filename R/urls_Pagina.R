#' urls_Pagina
#' Função que retorna o url de cada partida.
#' Para utilizar: urls_Pagina(pagina)
#' @export
urls_Pagina <- function(pagina){

  partidas <- read_html(pagina) %>%
    html_nodes('a') %>% html_attr('href') # Nessa parte ele pega todos os urls que estão contidos na página.

  partidas <- partidas[15:64] # Aqui é separado os urls que são efetivamente de partidas.

  n <- 1

  for (i in partidas){
    partidas[n] <- paste('https://www.vlr.gg', partidas[n], sep = '') # Salvando urls dentro da variável partida
    n = n + 1
  }

  return(partidas)

}
