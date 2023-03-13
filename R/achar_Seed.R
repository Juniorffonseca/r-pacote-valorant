#' achar_Seed
#' Função que testa seed com alguns parametros passados.
#' Para utilizar: achar_Seed(seed, prob_a, prob_b, hidden_n)
#' seed: número da seed.
#' prob_a: proporção da divisão da base de treinamento.
#' prob_b: proporção da divisão da base de testes.
#' hidden_n: numero de neuronios na(s) camada(s) escondida(s).
#' @export
achar_Seed <- function(seed, hidden_n, t){
  set.seed(seed)

  data_split <- initial_split(jogos, prop = 0.7, strata = "ganhador")

  training_data <- training(data_split)
  test_data <- testing(data_split)

  normalizando_test <- dplyr::select(test_data, -ganhador)
  normalizando_test <- as.data.frame(scale(normalizando_test))
  test_data <- dplyr::select(test_data, ganhador)
  test_data <- cbind(normalizando_test, test_data)

  normalizando_training <- dplyr::select(training_data, -ganhador)
  normalizando_training <- as.data.frame(scale(normalizando_training))
  training_data <- dplyr::select(training_data, ganhador)
  training_data <- cbind(normalizando_training, training_data)

  training_data$ganhador <- as.factor(training_data$ganhador)
  test_data$ganhador <- as.factor(test_data$ganhador)

  n <- neuralnet(formula,
                 data = training_data,
                 hidden = hidden_n,
                 err.fct = "sse",
                 linear.output = F,
                 threshold = t,
                 lifesign = 'minimal',
                 rep = 1,
                 algorithm = 'rprop-',
                 stepmax = 10000)

  Predict = compute(n, test_data)

    nn2 <<- ifelse(Predict$net.result[,1]>0.5,1,0)

  predictVstest <- cbind(test_data, Predict$net.result)
  i <<- sum(predictVstest$ganhador == nn2)/ nrow(test_data)

  print(i)

}
