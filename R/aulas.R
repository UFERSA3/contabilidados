# Scripts para aula de Finanças

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
    
    geom_line(aes(y = retorno2, x = risco),
              col = "red", size = 1) +
    annotate("text",
             x = sort(dados$risco)[round(length(dados$risco)/4)],
             y = sort(dados$retorno)[round(length(dados$risco)/3, 0)],
             label = "X", color = "red") +
    annotate("text",
             x = sort(dados$risco)[round(length(dados$risco)/2)],
             y = sort(dados$retorno2)[round(length(dados$risco)*2/5, 0)],
             label = "W", color = "green") +
    annotate("text",
             x = sort(dados$risco)[round(length(dados$risco)*3/4)],
             y = sort(dados$retorno2)[round(length(dados$risco)*4/5, 0)],
             label = "Y", color = "green") +
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




