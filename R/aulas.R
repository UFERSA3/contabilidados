# Scripts para aula de Finanças

cntdd.carregaPacotes <- function (
  pcts = c("tidyverse", "data.table", "readxl",
           "tseries", "ggplot2", "ggrepel",
           "quantmod", "PerformanceAnalytics",
           "jsonlite", "scales")){
  
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

fin.comparaAcoes <- function(papel1 = "EGIE3",
                             papel2 = "TIET11",
                             anoInicial = "2016",
                             mesInicial = "01",
                             anoFinal = "2019",
                             mesFinal = "12"){
  acao1 <- suppressMessages(suppressWarnings(getSymbols(paste0(papel1, ".SA"), auto.assign = F)))
  
  acao1 <-  suppressMessages(suppressWarnings(
    TTR::ROC(Ad(to.monthly(acao1[paste0(anoInicial,"-",
                                        mesInicial,
                                        "/",
                                        anoFinal, "-",
                                        mesFinal)])), na.pad = F)))
  
  acao2 <- suppressMessages(suppressWarnings(getSymbols(paste0(papel2, ".SA"), auto.assign = F)))
  
  acao2 <- suppressMessages(suppressWarnings(
    TTR::ROC(Ad(to.monthly(acao2[paste0(anoInicial,"-",
                                        mesInicial,
                                        "/",
                                        anoFinal, "-",
                                        mesFinal)])), na.pad = F)))
  
  ativo_A <<- as.vector(coredata(acao1))
  ativo_B <<- as.vector(coredata(acao2))
  
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


# Calcula retorno e desvio, considerando probabilidade

fin.RetProbab <- function(retorno = c(600, 650, 700, 750, 800),
                          prob = c(0.1, 0.15, 0.5, 0.15, 0.1),
                          digitos = 3){
  
  retornoA <- retorno
  probabA <- prob
  
  retornoF <- sum(retornoA*probabA)

  difRet <- retornoA - retornoF
  difRet2 <- difRet^2
  probdifRet <- difRet2*probabA
  tabela <- data.frame(Retorno = retornoA,
                       Probab = probabA,
                       RetornoProb = retornoA*probabA,
                       Dif_Media = difRet,
                       Dif_Media_Quad = difRet2,
                       Dif_Media_Quad_Prob = probdifRet)
  
  retornoEsperado <- paste0("Soma de RetornoProb: ",
                            formatC(retornoF, digits = digitos, format = "f"))
  variancia <- paste0("Soma de Dif_Media_Quad_Prob: ",
                      formatC(sum(tabela$Dif_Media_Quad_Prob), digits = digitos, format = "f"))
  variancian <- sum(tabela$Dif_Media_Quad_Prob)
  desvio <- paste0("Raiz quadrada da variância: ",
                   formatC(sqrt(variancian), digits = digitos, format = "f"))
  
  legenda <- data.table(Coluna_Tabela = names(tabela),
                        Legenda_Formula = c("Retorno esperado da ação",
                                    "Probabilidade de ocorrer o retorno",
                                    "Retorno * Probab",
                                    "Retorno - Retorno_Esperado",
                                    "Dif_Media elevado ao quadrado",
                                    "DifMediaQuad * Probab")
                        )

  lista <- list(Tabela = tabela,
                Retorno_Esperado = retornoEsperado,
                Variancia = variancia,
                Desvio = desvio,
                Legenda = legenda)
  lista
}


# Curva de indiferenças - relação risco-retorno

fin.indiferencas <- function(utilidade = 0.08, aversao = 0){
  
  curva_indif <- function(risco, utilidadebase, aversaobase){
    ret <- utilidadebase + (1/2)*aversaobase*risco^2
    return(ret)
  }
  
  dados <- data.frame(
    risco = 1:20/100,
    retorno = curva_indif(1:20/100,
                          utilidadebase = 0.15*(1+utilidade),
                          aversaobase = aversao))
  
  
  ggplot(dados,
         aes(y = retorno, x = risco)) +
    
    geom_line(col = "green", size = 1) +
    
    geom_line(aes(y = curva_indif(1:20/100,
                                  utilidadebase = 0.1*(1+(utilidade/4)),
                                  aversaobase = aversao),
                  x = risco),
              col = "red", size = 1) + 
    
    geom_line(aes(y = curva_indif(1:20/100,
                                  utilidadebase = 0.05*(1+(utilidade/2)),
                                  aversaobase = aversao),
                  x = risco),
              col = "blue", size = 1) +
    
    xlim(0, 0.2) +
    
    coord_cartesian(ylim = c(0, 1)) +
    ylab("Retorno") +
    xlab("Risco") +
    labs(title = "Relação Risco-Retorno",
         subtitle =  "Curva de Indiferenças",
         caption = "@2021 contabiliDados") +
    
    theme_light()
}

# Curva de indiferenças - preferências do investidor

fin.indifPreferInvestidor <- function(utilidade = 0.05, aversao = 30){
  
  curva_indif <- function(risco, utilidadebase, aversaobase){
    ret <- utilidadebase + (1/2)*aversaobase*risco^2
    return(ret)
  }
  
  dados <- data.frame(
    risco = 1:20/100,
    retorno = curva_indif(1:20/100,
                          utilidadebase = 0.15*(1+utilidade),
                          aversaobase = aversao))
  
  ggplot(dados,
         aes(y = retorno, x = risco)) +
    
    geom_line(col = "green", size = 1) +
    
    
    xlim(0, 0.4) +
    
    coord_cartesian(ylim = c(0, 1)) +
    ylab("Retorno") +
    annotate("pointrange",
             x = dados$risco[length(dados$risco)/2],
             y = dados$retorno[length(dados$risco)/2],
             ymin = dados$retorno[length(dados$risco)/2] - 0.25,
             ymax = dados$retorno[length(dados$risco)/2] + 0.25,
             size = 0.8, color = "red", linetype="dashed") +
    xlab("Risco") +
    labs(title = "Curva de Indiferenças",
         subtitle =  "Preferências do investidor",
         caption = "@2021 contabiliDados") +
    
    theme_light()
  
  
}

# Seleção racional de um ativo

fin.selecaoRacional <- function(){
  
  dados <- data.frame(Risco = c(0.1, 0.1, 0.3, 0.3),
                      Retorno = c(0.02, 0.08, 0.02, 0.08))
  
  ggplot(dados, aes(Risco, Retorno)) +
    geom_point() +
    xlim(0, 0.8) +
    coord_cartesian(ylim = c(0, 0.1)) +
    ylab("Retorno Esperado") +
    xlab("Risco") +
    labs(title = "Seleção racional de um ativo",
         subtitle = "Oportunidades de investimentos para um investidor",
         caption = "@2021 contabiliDados") +
    theme_light()
  
}

# Curvas de indiferenças de um investidor

fin.indiferInvestidor <- function(utilidade = 0.05, aversao = 30){
  
  curva_indif <- function(risco, utilidadebase, aversaobase){
    ret <- utilidadebase + (1/2)*aversaobase*risco^2
    return(ret)
  }
  
  dados <- data.frame(
    risco = 1:20/100,
    retorno = curva_indif(1:20/100,
                          utilidadebase = 0.15*(1+utilidade),
                          aversaobase = aversao),
    retorno2 = curva_indif(1:20/100,
                            utilidadebase = 0.05*(1+utilidade),
                            aversaobase = aversao))
    
  
  ggplot(dados,
         aes(y = retorno, x = risco)) +
    
    geom_line(col = "green", size = 1) +
    geom_point(x = sort(dados$risco)[round(length(dados$risco)/3, 0)],
               y = sort(dados$retorno)[round(length(dados$risco)/3, 0)]) +
    geom_point(x = sort(dados$risco)[round(length(dados$risco)*4/5, 0)],
               y = sort(dados$retorno2)[round(length(dados$risco)*4/5, 0)]) +
    geom_point(x = sort(dados$risco)[round(length(dados$risco)*3/6, 0)],
               y = sort(dados$retorno2)[round(length(dados$risco)*3/6, 0)]) +
    geom_line(aes(y = retorno2, x = risco),
              col = "red", size = 1) +
    annotate("text",
             x = sort(dados$risco)[round(length(dados$risco)/3)],
             y = sort(dados$retorno)[round(length(dados$risco)/3, 0)],
             label = "X", color = "red", vjust = -1) +
    annotate("text",
             x = sort(dados$risco)[round(length(dados$risco)*3/6)],
             y = sort(dados$retorno2)[round(length(dados$risco)*3/6, 0)],
             label = "W", color = "green", vjust = -0.5) +
    annotate("text",
             x = sort(dados$risco)[round(length(dados$risco)*4/5)],
             y = sort(dados$retorno2)[round(length(dados$risco)*4/5, 0)],
             label = "Y", color = "green", vjust = -0.5) +
    ylab("Retorno Esperado") +
    xlab("Risco") +
    labs(title = "Curvas de indiferenças",
         subtitle = "Oportunidades de investimentos para um investidor",
         caption = "@2021 contabiliDados") +
    theme_light()
  
}

# Retornos esperados - introdução ao portfolio

fin.retornoEspPortfolio <- function(papel1 = "TIET11",
                                    papel2 = "EGIE3",
                                    peso1  = 0.4,
                                    inicio = "2020-12-01",
                                    fim = "2020-12-31",
                                    exemplo = T){
  
  if(exemplo == T){
    Papel1 = 0.2; Papel2 = 0.4; namePapel1 <- "Papel 1"; namePapel2 <- "Papel 2"
    } else {
      Papel1 = mean(TTR::ROC(Cl(get.hist.quote(instrument = paste0(papel1, ".SA"),
                                               start = inicio,
                                               end = fim,
                                               quiet = T)), na.pad = F));
      namePapel1 <- papel1;
      Papel2 = mean(TTR::ROC(Cl(get.hist.quote(instrument = paste0(papel2, ".SA"),
                                               start = inicio,
                                               end = fim,
                                               quiet = T)), na.pad = F));
      namePapel2 <- papel2}
  Peso1 = peso1
  Peso2 = 1-peso1
  
  dados <- data.frame(Papel1, Papel2, Peso1, Peso2)

  retornoPorfolio <- 
    dados %>% 
    mutate(retPortfolio = (Papel1 * Peso1) + (Papel2 * Peso2),
           portPapel1.100_Papel2.0 = (Papel1 * 1) + (Papel2 * 0),
           portPapel1.60_Papel2.40 = (Papel1 * 0.6) + (Papel2 * 0.4),
           portPapel1.40_Papel2.60 = (Papel1 * 0.4) + (Papel2 * 0.6),
           portPapel1.0_Papel2.100 = (Papel1 * 0) + (Papel2 * 1)
           )
  
  retornoPorfolio %>%
    dplyr::select(starts_with("port")) %>% 
    pivot_longer(cols = starts_with("port"), names_to = "Portfolio", values_to = "Retorno") %>% 
    mutate(ordem = rank(Retorno), ordem2 = 1:4, ranking = ifelse(ordem==ordem2, ordem2, ordem)) -> grf.retornoPorfolio
  
  ggplot(grf.retornoPorfolio,
         aes(x = ranking, y = Retorno)) +
    geom_line(col = "darkgray") +
    annotate("text", x = grf.retornoPorfolio$ranking, y = grf.retornoPorfolio$Retorno,
             label = c(paste0("100%", namePapel1, "\n", "0%", namePapel2),
                       paste0("60%", namePapel1, "\n", "40%", namePapel2),
                       paste0("40%", namePapel1, "\n", "60%", namePapel2),
                       paste0("0%", namePapel1, "\n", "100%", namePapel2)),
             size = 4, color = "red") +
    coord_cartesian(xlim = c(0.5, 4.5), ylim = c(min(grf.retornoPorfolio$Retorno),
                                                 max(grf.retornoPorfolio$Retorno))) +
    scale_x_continuous(breaks = 1:4, labels = paste0("Portfolio ", 1:4)) +
    ylab("Retornos") +
    xlab("Composição das carteiras") +
    labs(title = "Comparação entre retorno de portfolios",
         subtitle = "Composições baseadas no peso de cada ativo",
         caption = "@2021 contabiliDados") +
    theme_light() -> grafico
  
  
    resultado <- list(Portfolio = retornoPorfolio, Grafico = grafico)
    
  return(resultado)
  
  
}


# Visualiza correlação

fin.visualizaCorrel <- function(correlacao = -1){
  
  if(correlacao > 1 | correlacao < -1){
    stop("O valor de correlação informado tem que está entre -1 e 1")} else{
      M<-matrix(correlacao,2,2)
      diag(M)<-1
      X <- round(MASS::mvrnorm(n = 50,mu = rep(0.05,2),
                               Sigma = M,empirical=TRUE), 3)
      
      X <- data.frame(
        data = as.yearmon(seq.Date(as.Date("2016-12-01"),
                                   by = "month", length.out = 50)), X)
      
      ggplot(X, aes(x = data, y = X1)) +
        geom_line(size = 1) +
        geom_line(aes(x = data, y = X2), size = 0.8, color = "red")+
        ylab("Retornos ficticios") +
        xlab("Meses") +
        labs(title = "Visualização de duas séries, conforme sua correlação",
             subtitle = paste0("Exemplo de dois ativos com correlação igual a ",
                               as.character(correlacao)),
             caption = "@2021 contabiliDados") +
        theme_light()
    }
}


fin.efeitoCorrelRiscoCarteira <- function(
  retEsp_A = 0.12,
  retEsp_B = 0.24,
  desvio_A = 0.18,
  desvio_B = 0.27,
  correlacao = 1
){
  if(correlacao > 1 | correlacao < -1){
    stop("O valor de correlação informado tem que está entre -1 e 1")} else{
      
      dados <- data.frame(
        pesoPerc_A = c(1, 0.8, 0.6, 0.4, 0.2, 0),
        pesoPerc_B = c(0, 0.2, 0.4, 0.6, 0.8, 1)
      )
      
      dados$retPerc_Portf <- round((dados$pesoPerc_A*retEsp_A + dados$pesoPerc_B*retEsp_B), 3)
      
      dados$riscoPerc_Portf_Pos <- round(((((dados$pesoPerc_A^2 * desvio_A^2) +
                                              (dados$pesoPerc_B^2 * desvio_B^2) + 
                                              2 * dados$pesoPerc_A * dados$pesoPerc_B *
                                              desvio_A * desvio_B * abs(correlacao)))^0.5), 3)
      
      dados$riscoPerc_Portf_Neg <- round(((((dados$pesoPerc_A^2 * desvio_A^2) +
                                              (dados$pesoPerc_B^2 * desvio_B^2) + 
                                              2 * dados$pesoPerc_A * dados$pesoPerc_B *
                                              desvio_A * desvio_B * (abs(correlacao)*-1)))^0.5), 3)
      
      dados <- sapply(dados, label_percent())
      
      return(dados)
    }
  }


fin.correlAcoes <- function(papel1 = "ABEV3",
                            papel2 = "TOTS3",
                            anoInicial = "2016",
                            mesInicial = "01",
                            anoFinal = "2019",
                            mesFinal = "12"){
  acao1 <- suppressMessages(suppressWarnings(getSymbols(paste0(papel1, ".SA"),
                                                        auto.assign = F)))
  
  acao1 <-  suppressMessages(suppressWarnings(
    TTR::ROC(Ad(to.monthly(acao1[paste0(anoInicial,"-",
                                        mesInicial,
                                        "/",
                                        anoFinal, "-",
                                        mesFinal)])), na.pad = F)))
  
  acao2 <- suppressMessages(suppressWarnings(getSymbols(paste0(papel2, ".SA"),
                                                        auto.assign = F)))
  
  acao2 <- suppressMessages(suppressWarnings(
    TTR::ROC(Ad(to.monthly(acao2[paste0(anoInicial,"-",
                                        mesInicial,
                                        "/",
                                        anoFinal, "-",
                                        mesFinal)])), na.pad = F)))
  
  dados <- data.frame(data = index(acao1),
                      ativoA = as.vector(coredata(acao1)),
                      ativoB = as.vector(coredata(acao2)))
  
  correlAcoes <- round(cor(dados[, 2:3])[1,2], 2)

  assign(papel1, acao1, envir = .GlobalEnv)
  assign(papel2, acao2, envir = .GlobalEnv)
  
  ggplot(dados, aes(x = data, y = ativoA)) +
    geom_line(size = 1) +
    geom_line(aes(x = data, y = ativoB), size = 0.8, color = "red")+
    annotate("text", label = paste0("Correlação = ", label_percent()(correlAcoes)),
             x = dados$data[20],
             y = max(c(dados$ativoA, dados$ativoB))) +
    ylab("Retornos das ações") +
    xlab("Meses") +
    labs(title = "Análise da correlação de séries",
         subtitle = paste0(papel1, " (Preto)", " vs ", papel2, " (Vermelho)"),
         caption = "@2021 contabiliDados") +
    theme_light()
  
}



definePeso <- function(desvioA = 0.15, desvioB = 0.12, correlAB = -1){
  
  pesoA <- (desvioB^2-correlAB*desvioB*desvioA)/
    ((desvioA^2 + desvioB^2)-(2*correlAB*desvioB*desvioA))
  
  print(paste0("O peso de A na carteira é :", label_percent()(pesoA)))
  print(paste0("O peso de B na carteira é :", label_percent()(1-pesoA)))
  
}


definePeso()
fin.comparaAcoes(papel1 = "ABEV3",
                 papel2 = "TOTS3",
                 anoInicial = "2016",
                 mesInicial = "01",
                 anoFinal = "2019",
                 mesFinal = "12")
fin.correlAcoes()
fin.efeitoCorrelRiscoCarteira(retEsp_A = mean(ABEV3), retEsp_B = mean(TOTS3),
                         desvio_A = sd(ABEV3), desvio_B = sd(TOTS3),
                         correlacao = as.numeric(cor(ABEV3, TOTS3)))
