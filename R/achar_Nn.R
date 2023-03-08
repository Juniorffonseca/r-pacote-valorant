#' achar_Nn
#' Função que itera para achar uma rede neural.
#' Para utilizar: achar_Nn()
#' @export
achar_Nn <- function(){

  n <<- neuralnet(formula,
                  data = training_data,
                  hidden = hidden_n,
                  err.fct = "sse",
                  linear.output = F,
                  threshold = 1,
                  lifesign = 'minimal',
                  rep = 1,
                  algorithm = 'rprop-',
                  stepmax = 10000)

  Predict <<- compute(n, test_data)

  nn2 <<- ifelse(paste(condicao),1,0)

  predictVstest <<- cbind(test_data, Predict$net.result)
  i <<- sum(predictVstest$ganhador == nn2)/nrow(test_data)
  print(i)

  z <<- ifelse(i>z, z <<- i, z <<- z)

  print(z)
}
