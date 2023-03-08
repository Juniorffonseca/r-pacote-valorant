#' achar_Seed
#' Função que testa seed com alguns parametros passados.
#' Para utilizar: achar_Seed(seed, prob_a, prob_b, hidden_n)
#' seed: número da seed.
#' prob_a: proporção da divisão da base de treinamento.
#' prob_b: proporção da divisão da base de testes.
#' hidden_n: numero de neuronios na(s) camada(s) escondida(s).
#' @export
achar_Seed <- function(seed, prob_a, prob_b, hidden_n){
  set.seed(seed)
  inp <- sample(2, nrow(jogos), replace = TRUE, prob = c(prob_a, prob_b))
  training_data <- jogos[inp==1, ]
  test_data <- jogos[inp==2, ]

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
                 threshold = 1,
                 lifesign = 'minimal',
                 rep = 1,
                 algorithm = 'rprop-',
                 stepmax = 10000)

  Predict = compute(n, test_data)

  ifelse(n2 = T, nn2 <<- ifelse(Predict$net.result[,1]>Predict$net.result[,2],1,0),
         nn2 <<- ifelse(Predict$net.result[,1]>0.5),1,0)

  predictVstest <- cbind(test_data, Predict$net.result)
  i <<- sum(predictVstest$ganhador == nn2)/ nrow(test_data)

  print(i)

}
