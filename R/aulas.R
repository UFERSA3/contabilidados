cntdd.carregaPacotes <- function (
  pcts = c("tidyverse", "data.table", "readxl",
           "tseries", "ggplot2", "ggrepel",
           "quantmod", "PerformanceAnalytics",
           "jsonlite")){
  
  # # # # # # # #  Instruções  # # # # # # # # # # # # # # # # # # # # # # # # # #
  #
  # Objetivo: Carregar um pacote. Caso não exista, instala e carrega
  # Informa um vetor com os pacotes a serem instalados
  # Se nada informado, por padrão, instala os pacotes necessarios aos demais
  # codigos do contabilidados
  # Esses pacotes são necessários para manipulação de dados
  # 
  # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #  #
  
  for (i in pcts) {
    if (suppressWarnings(suppressMessages(
      !require(i, character.only = T, quietly = T)))) {
      install.packages(i)
      suppressWarnings(suppressMessages(
        require(i, character.only = T, quietly = T)))
    }
  }
}

cntdd.carregaPacotes()


# Compara duas ações em relação a média e desvio

aula.comparaAcoes <- function(papel1, papel2, anoInicial, mesInicial, anoFinal, mesFinal){
  acao1 <- suppressMessages(suppressWarnings(getSymbols(papel1, auto.assign = F)))
  
  acao1 <-  suppressMessages(suppressWarnings(
    TTR::ROC(Ad(to.monthly(acao1[paste0(anoInicial,"-",
                                        mesInicial,
                                        "/",
                                        anoFinal, "-",
                                        mesFinal)])), na.pad = F)))
  
  acao2 <- suppressMessages(suppressWarnings(getSymbols(papel2, auto.assign = F)))
  
  acao2 <- suppressMessages(suppressWarnings(
    TTR::ROC(Ad(to.monthly(acao2[paste0(anoInicial,"-",
                                        mesInicial,
                                        "/",
                                        anoFinal, "-",
                                        mesFinal)])), na.pad = F)))
  
  ativo_A <- as.vector(coredata(acao1))
  ativo_B <- as.vector(coredata(acao2))
  
  rbind(
    data.frame(fct.genero = papel1, retorno = ativo_A),
    data.frame(fct.genero = papel2, retorno = ativo_B)
  ) %>% 
    ggplot(aes(x = retorno)) +
    geom_histogram(aes(color = fct.genero, fill = fct.genero),
                   bins = 10, alpha = 0.3, position = "identity") +
    ylab("Frequência") +
    xlab("Retornos das ações") +
    labs(title = "Comparação de ações para análise de média e desvio",
         subtitle = paste(papel1, "vs", papel2, "(",
                          paste0(anoInicial,"-",
                                 mesInicial,
                                 " a ",
                                 anoFinal, "-",
                                 mesFinal), ")"),
         caption = "@2021 contabiliDados") +
    scale_color_manual(name = "Ações", values = c("#E7B800", "#00AFBB")) +
    scale_fill_manual(name = "Ações", values = c("#E7B800", "#00AFBB")) +
    theme_classic() +
    theme(legend.position = "bottom") -> grafico
  
  tabela <- data.frame(Papel = c(papel1, papel2),
                       Media = round(c(mean(ativo_A), mean(ativo_B)),3),
                       Desvio = round(c(sd(ativo_A), sd(ativo_B)),3),
                       CoefVar = round(c(sd(ativo_A)/mean(ativo_A), sd(ativo_B)/mean(ativo_B)),3))
  
  resultado <- list(grafico, tabela)
  
  return(resultado)
  
}
