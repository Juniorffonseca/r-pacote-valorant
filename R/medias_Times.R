#' Médias dos times.
#' Função que calcula as médias de cada estatística para os dois times de uma determinada partida.
#' Para utilizar: medias_Times()
#' @export
# Função medias_times -------------------------------------------------------------------------------------
medias_Times <- function (url_partida, resultado = F){
  tryCatch(
    {
  # Pegando os dados no link da partida -------------------------------------------------------------------
  links_jogadores <- read_html(url_partida) %>%
    html_nodes('td.mod-player a') %>%
    html_attr('href')

  # Separando os nomes dos jogadores de cada time em 2 arrays
  timeA <- links_jogadores[1:5]
  timeB <- links_jogadores[6:10]

  # Criando os links usando os nomes dos jogadores para ficar entre '...vlr.gg' e '/?timespan...'
  n <- 1
  for (i in timeA){
    timeA[n] <- paste('https://www.vlr.gg', '/?timespan=all', sep = i)
    n = n + 1
  }

  n <- 1
  for(i in timeB){
    timeB[n] <- paste('https://www.vlr.gg', '/?timespan=all', sep = i)
    n = n + 1
  }

  timeA_medias <- list()
  timeB_medias <- list()

  for (i in timeA){
    timeA_medias[[length(timeA_medias)+1]] <- medias_Jogadores(i)
  }

  for (i in timeB){
    timeB_medias[[length(timeB_medias)+1]] <- medias_Jogadores(i)
  }

  timeA_medias <- timeA_medias %>% map_df(as_tibble)
  timeB_medias <- timeB_medias %>% map_df(as_tibble)

  timeA_medias <- dplyr::select(timeA_medias, -Use)
  timeB_medias <- dplyr::select(timeB_medias, -Use)

  partida <- cbind(timeA_medias, timeB_medias)

  colnames(partida) <- c('time1R', 'time1ACS', 'time1KAST', 'time1KD', 'time1ADR',
                         'time2R', 'time2ACS', 'time2KAST', 'time2KD', 'time2ADR')

  partida <- select(partida, 'time1R', 'time2R', 'time1ACS', 'time2ACS', 'time1KAST', 'time2KAST', 'time1KD', 'time2KD',
                    'time1ADR', 'time2ADR')

  partida <- as.data.frame(t(colMeans(partida)))

  if(resultado == TRUE){

  ganhador <- get_Ganhadores(url_partida)
  partida <- cbind(partida, ganhador)

  }

  return(partida)
    }
  , error = function(e){cat('error:', conditionMessage(e), '\n')})
}
