#' @title teste
#'
#' @description teste
#'
#' @param symbol
#'
#' @return NULL
#'
#' @examples
#'
#' @export
sapevo_ranking <- function(projeto, decisores, alternativas, criterios,
                           vetor_peso, listas_notas_decisores_criterios) {

  numero_alternativas=length(alternativas)
  num_criterios=length(criterios)
  total_criterio1=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios1)) {

    if (is.null(listas_notas_decisores_criterios1[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios1[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      #print(vetorpesonorm1)
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      #print(vetorpesonorm1)
      total_criterio1=colSums(rbind(total_criterio1,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio2=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios2)) {

    if (is.null(listas_notas_decisores_criterios2[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios2[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio2=colSums(rbind(total_criterio2,vetorpesonorm))
    } else {

      cat("")

    }
  }

  total_criterio3=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios3)) {

    if (is.null(listas_notas_decisores_criterios3[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios3[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio3=colSums(rbind(total_criterio3,vetorpesonorm))

    } else {

      cat("")

    }
  }


  total_criterio4=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios4)) {

    if (is.null(listas_notas_decisores_criterios4[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios4[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio4=colSums(rbind(total_criterio4,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio5=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios5)) {

    if (is.null(listas_notas_decisores_criterios5[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios5[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio5=colSums(rbind(total_criterio5,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio6=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios6)) {

    if (is.null(listas_notas_decisores_criterios6[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios6[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio6=colSums(rbind(total_criterio6,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio7=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios7)) {

    if (is.null(listas_notas_decisores_criterios7[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios7[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio7=colSums(rbind(total_criterio7,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio8=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios8)) {

    if (is.null(listas_notas_decisores_criterios8[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios8[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio8=colSums(rbind(total_criterio8,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterio9=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios9)) {

    if (is.null(listas_notas_decisores_criterios9[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios9[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio9=colSums(rbind(total_criterio9,vetorpesonorm))

    } else {

      cat("")

    }
  }

  total_criterios10=data.frame()
  for (i in 1:length(listas_notas_decisores_criterios10)) {

    if (is.null(listas_notas_decisores_criterios10[[i]])==F) {

      decisorpeso = matrix(listas_notas_decisores_criterios10[[i]],
                           nrow=numero_alternativas,
                           ncol=numero_alternativas,
                           byrow = TRUE)

      vetorpesonorm=rowSums(data.frame(decisorpeso))
      vetorpesonorm=(vetorpesonorm-min(vetorpesonorm))/(max(vetorpesonorm)-min(vetorpesonorm))
      total_criterio10=colSums(rbind(total_criterio10,vetorpesonorm))

    } else {

      cat("")

    }
  }


  if (num_criterios==1) {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1))
    print(matriz_avaliacao)
    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==2)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==3)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==4)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)
  } else if (num_criterios==5)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==6)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==7)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else if (num_criterios==8)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7,total_criterio8))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)
  } else if (num_criterios==9)  {

    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7,total_criterio8,total_criterio9))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  } else {
    matriz_avaliacao=data.matrix(data.frame(total_criterio1,total_criterio2,total_criterio3,total_criterio4,total_criterio5,
                                            total_criterio6,total_criterio7,total_criterio8,total_criterio9,total_criterio10))

    rownames(matriz_avaliacao)=alternativas

    colnames(matriz_avaliacao)=criterios

    vetor_ranking=matriz_avaliacao %*% vetor_peso

    names(vetor_ranking)=alternativas

    vetor_ranking_ordenado=cbind(sort(vetor_ranking,decreasing = T),1:length(alternativas))

    colnames(vetor_ranking_ordenado)=c("grau obtido","ranking")

    criterios_pontuacao=colSums(matriz_avaliacao)

    assign("matriz_avaliacao",matriz_avaliacao,envir = .GlobalEnv)
    assign("vetor_ranking_ordenado",vetor_ranking_ordenado,envir = .GlobalEnv)

    cat("---------O nome do seu projeto ?:",projeto,"---------","\n")
    cat("-------- Aternativas do seu projeto:",alternativas,"---------","\n")
    cat("---------Crit?rios do seu projeto:",criterios,"---------","\n")
    cat("---------Os pesos do m?todo sapevo-m s?o:",vetor_peso,"---------","\n\n")
    cat("---------A matriz avalia??o do seu projeto ?:","---------","\n")
    print(matriz_avaliacao)
    cat("\n")
    cat("---------A pontua??o dos crit?rios foi:","---------","\n")
    print(criterios_pontuacao)
    cat("\n")
    cat("---------O ranking do seu projeto ?:","---------","\n")
    print(vetor_ranking_ordenado)

  }
}
