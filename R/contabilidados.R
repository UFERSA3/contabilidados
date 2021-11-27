cntdd.lista <- function(){
  cat("cntdd.carregaPacotes:
      Carregar um pacote. Caso não exista, instala e carrega", "\n")
  cat("\n")
  cat("cntdd.BaixaDados:
      Baixar dados da aba de uma planilha coletados por meio de matrix do economatica", "\n")
  cat("\n")
  cat("cntdd.BaixaDadosReuters:
      Baixar dados da aba de uma planilha coletados por meio da base Reuters", "\n")
  cat("\n")
  cat("cntdd.mediaGeometrica:
      Gerar a média geométrica do vetor informado", "\n")
  cat("\n")
  cat("cntdd.baixaPrecos:
      Baixar precos de papeis negociados na bolsa e gerar grafico", "\n")
  cat("\n")
  cat("cntdd.variosMatrix:
      Criar vetores com base em abas do excel coletadas no economatica por meio de matrix", "\n")
  cat("\n")
  cat("cntdd.limpaMatrix:
      Limpar objetos temporarios criados durante a uniao de matrix por meio da funcao 'cntdd.uneMatrix'", "\n")
  cat("\n")
  cat("cntdd.uneMatrix:
      Transformar em painel variaveis coletadas em matrix (Economatica)", "\n")
  cat("\n")
}

cntdd.carregaPacotes <- function (
  pcts = c("tidyverse", "data.table", "readxl",
           "tseries", "ggplot2", "ggrepel",
           "quantmod", "PerformanceAnalytics",
           "jsonlite", "xts")){

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

cntdd.BaixaDados <- function (Nome, PathFile, Periodo, Planilha, ClassPeriodo = "date", 
                        ClassValue = "numeric") {
################ Instruções #####################################################
# Objetivo: Baixar dados da aba de uma planilha coletados por meio de matrix
#           do economatica
#           
# Nome: Informe o nome da variável coletada no matrix do economatica
# PathFile: Informe o caminho do arquivo xlsx onde está o matrix do economatica
# Periodo: Informe "trim" para trimestre, "data" para data, "ano" para ano, etc.
# Planilha: Informe o número da planilha ou nome da aba que contém o matrix
# ClassPeriodo: Por padrão é formato data, mas se no matrix o período contiver
#               letras, informar "text" para indicar ser um texto (string)
# ClassValue: Por padrão é formato numérico, mas se os dados coletados forem
#             texto (nome do sócio, por exemplo), infomar "text".
#################################################################################
  
    a <- ncol(read_xlsx(PathFile, sheet = Planilha, skip = 1, 
                      na = "-"))
  assign(toupper(paste0("BD", Nome)), read_xlsx(PathFile, sheet = Planilha, 
                                                skip = 1, na = "-", col_types = c(ClassPeriodo, rep(ClassValue, 
                                                                                                    a - 1))), envir = .GlobalEnv)
  setnames(get(toupper(paste0("BD", Nome))), 1L, Periodo)
  assign(toupper(paste0("BD", Nome)), data.frame(melt(data.table(get(toupper(paste0("BD", 
                                                                         Nome)))), id.vars = Periodo, variable.name = "cod", value.name = Nome, 
                                           variable.factor = F, value.factor = F)), envir = .GlobalEnv)
}


cntdd.BaixaDadosReuters <- function (Nome, PathFile, Planilha, RANGE = "P:R", SKIP = 0) 
{
  assign(toupper(Nome), read_xlsx(PathFile, sheet = Planilha, 
                                  skip = SKIP, na = c("-", "NULL", "#N/A"), range = cell_cols(RANGE)), 
         envir = .GlobalEnv)
  setnames(get(toupper(Nome)), 1L:3L, c("cod", "data", Nome))
  setDT(get(toupper(Nome)))
}


cntdd.mediaGeometrica <- function(x){

################ Instruções #####################################################
# Objetivo: Gerar a média geométrica do vetor informado
# Informa um vetor com os números que comporão a média
# Valores NA serão desconsiderados
#################################################################################
  
  round(prod(x, na.rm = T)^(1/length(x[!is.na(x)])), 3)
}

cntdd.baixaPrecos <- function(nome = "precos", x = "^BVSP", inicio = Sys.Date()-30,
                        fim = Sys.Date()-1, freq = "d"){
  
################ Instruções #####################################################
# Objetivo: Baixar precos de papeis negociados na bolsa e gerar grafico.
# Informa o nome do objeto que pretende criar e escolhe os papeis desejados,
# período e frequencia dos dados, conforme exemplo abaixo:
# 
#           cntdd.baixaPrecos(nome = "precos",
#                             x = c("RADL3.SA", "PETR3.SA", "^BVSP"),
#                             inicio = "2010-01-01",
#                             fim = "2010-01-31",
#                             freq = "d")
# 
# nome: Nome do objeto a ser criado no environment do R. Se nada informado,
#       o padrao eh "precos".
# x = Vetor com os papeis de interesse na serie de precos. Se nada informado,
#     o padrao eh o indice bovespa "^BVSP".
# inicio: Data inicial no formato "yyyy-mm-dd". Se nada informado, o padrao
#         eh 30 dias anteriores a data do sistema.
# fim: Data final no formato "yyyy-mm-dd". Se nada informado, o padrao eh o dia
#      anterior a data do sistema.
# freq: informar "d" para frequencia diaria dos precos ou "m" para mensal. Se
#       nada informado, o padrao eh diario ("d")
# 
# Obs.: Ao final eh plotado um grafico com os valores dos retornos na base 100
#       com benchmark na data de inicio da serie. A analise consiste em observar
#       o comportamento do retorno de cada papel do inicio da serie (quando
#       todos estao com mesmo valor - 100) ate a data final. Serve para comparar
#       o desempenho de cada papel no periodo informado.
#################################################################################
  
  LISTA <- toupper(x)
  dados <- list()
  
  for (i in seq_along(LISTA)) {
    dados[[LISTA[i]]] <- get.hist.quote(LISTA[i], start = as.character(inicio),
                                        end = as.character(fim), compression = freq)
  }
  
  for (i in 1:length(LISTA)) {
    dados[[i]] <-
      cbind(
        as.data.table(time(dados[[i]])),
        as.data.table(dados[[i]]))
  }
  
  precos <- 
    dados %>%
    bind_rows(.id = "codigo") %>% rename(data = V1) %>%
    group_by(codigo) %>%
    mutate(closeb100 = Close/ first(Close) * 100) %>% na.omit %>%
    setDT

  assign(nome, precos, envir = .GlobalEnv)

  precos %>%
    mutate(label = if_else(data == max(data), as.character(codigo), NA_character_)) %>%
    ggplot(aes(x = data, y = closeb100, group = codigo, colour = codigo)) +
    geom_line() + theme(legend.position = "none") +
    geom_label_repel(aes(label = label), nudge_x = 1, na.rm = T)
    
}

cntdd.variosMatrix <- function(Arquivo, SeqVarPlan, index = c("cod", "trim"), clsPer = "text", clsVlr = "numeric"){
  
################ Instruções #####################################################
# Objetivo: Criar vetores com base em abas do excel coletadas no economatica por
#           meio de matrix.
# Apos salvar cada variavel de interesse em uma aba do excel no formato
# matrix do economatica, aplica essa funcao, conforme exemplo:
# 
#     cntdd.variosMatrix(Arquivo = "Caminho/Arquivo.xlsx",
#                        SeqVarPlan = c("atvTot", "patLiq"),
#                        index = c("cod", "ano"),
#                        clsPer = "numeric", clsVlr = "numeric")
#
# Arquivo: Informe o caminho do arquivo com sua extensão xlsx
# SeqVarPlan: Crie um vetor com o nome de cada variavel coletada na sequencia
#             das abas do arquivo em excel 
# index: Informar sempre cod e, no período, colocar conforme a frequencia
#        coletada (ano, trimestre, mes, data). Serve para nomear as colunas,
#        portanto, outros valores tambem sao aceitos
# clsPer: Informa a classe do período. Quando Data e mês, informa "date";
#         Quando ano, informa "numeric" e quando trimestre, informa "text".
# clsVlr: O padrao eh numerico, porem se for coletado algum dado com texto,
#         como o nome do acionista, coloca "text".
#         
# Obs.: Ao final eh demonstrada uma auditoria identificando o nome dado pelo
#       usuario para cada variavel e o cabecalho da planilha com o nome da
#       variavel coletada no economatica. Serve para saber se a sequencia dada
#       pelo usuario realmente reflete a sequencia das abas da planilha
#
# A função 'cntdd.uneMatrix' une todos os vetores criados por essa funcao.
# Entao se usar essa funcao, o usuario tera cada variavel da planilha em um
# vetor separado. Caso queira todas as variaveis em formato painel, eh
# preferivel usar a funcao 'cntdd.uneMatrix.
#################################################################################
  
  # Unir diversas variaveis coletadas em matrix (economatica) para o
  # formato painel  

  bds <<- list()
  listaVar <<- toupper(paste0("BD", SeqVarPlan))
  AuditaVetores <- data.frame(Variavel = NA, Descricao = NA)
  
  for (i in seq_along(SeqVarPlan)) {
    
    bds[[SeqVarPlan[i]]] <<- cntdd.BaixaDados(SeqVarPlan[i], Arquivo, index[2], i, ClassPeriodo = clsPer, ClassValue = clsVlr)
    
    AuditaVetores[i,1] = toupper(paste0("BD", SeqVarPlan))[i]; AuditaVetores[i, 2] = names(setDT(read_xlsx(Arquivo, sheet = i, skip = 0, na = "-", range = "B1:B3")))
    
  }
  bds <<- lapply(bds, data.frame)
  print(AuditaVetores)
  
}


cntdd.limpaMatrix <- function(){
  
################ Instruções #####################################################
# Objetivo: Limpar objetos temporarios criados durante a uniao de matrix por
#           meio da funcao 'cntdd.uneMatrix'. Eh executado automaticamente.
#################################################################################
  
  rm(list = ls(envir = .GlobalEnv)[ls(envir = .GlobalEnv) %in% c(listaVar, "listaVar", "bds")],
     envir = .GlobalEnv)
}

cntdd.uneMatrix <- function(Arquivo, SeqVarPlan, index, clsPer, clsVlr){
  
################ Instruções #####################################################
# Objetivo: Transformar em painel variaveis coletadas em matrix (Economatica)
# Apos salvar cada variavel de interesse em uma aba do excel no formato
# matrix do economatica, aplica essa funcao, conforme exemplo:
# 
#     cntdd.uneMatrix(Arquivo = "Caminho/Arquivo.xlsx",
#                     SeqVarPlan = c("atvTot", "patLiq"),
#                     index = c("cod", "ano"),
#                     clsPer = "numeric", clsVlr = "numeric")
#
# Arquivo: Informe o caminho do arquivo com sua extensão xlsx
# SeqVarPlan: Crie um vetor com o nome de cada variavel coletada na sequencia
#             das abas do arquivo em excel 
# index: Informar sempre cod e, no período, colocar conforme a frequencia
#        coletada (ano, trimestre, mes, data). Serve para nomear as colunas,
#        portanto, outros valores tambem sao aceitos
# clsPer: Informa a classe do período. Quando Data e mês, informa "date";
#         Quando ano, informa "numeric" e quando trimestre, informa "text".
# clsVlr: O padrao eh numerico, porem se for coletado algum dado com texto,
#         como o nome do acionista, coloca "text".
#         
# Obs.: Ao final eh demonstrada uma auditoria identificando o nome dado pelo
#       usuario para cada variavel e o cabecalho da planilha com o nome da
#       variavel coletada no economatica. Serve para saber se a sequencia dada
#       pelo usuario realmente reflete a sequencia das abas da planilha
#################################################################################
  
  cntdd.variosMatrix(Arquivo = Arquivo, SeqVarPlan = SeqVarPlan, index = index, clsPer = clsPer, clsVlr = clsVlr)
  
  bds <<- lapply(bds, data.frame)
  bdPainel <<- Reduce(inner_join, bds)
  
  cntdd.limpaMatrix()
  
  cat("Os dados foram armazenados, em painel, no objeto 'bdPainel' ", "\n")
  
}


cntdd.corstars <- function(x, method=c("pearson", "spearman"),
                     removeTriangle=c("upper", "lower"),
                     result=c("none", "html", "latex"),
                     digits = 4){
  
  ################ Instruções #####################################################
  # Objetivo: Gerar uma tabela de correlacao com identificação da significância
  # estatística.
  # 
  # Adaptado de:
  # http://www.sthda.com/english/wiki/elegant-correlation-table-using-xtable-r-package
  #################################################################################
  
  #Compute correlation matrix
  require(Hmisc, warn.conflicts = F)
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .01, "***",
                    ifelse(p < .05, "** ",
                           ifelse(p < .1, "*   ", "    ")))
  
  ## trunctuate the correlation matrix to four decimal (default)
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), digits))[,-1]
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  if (result[1]=="none") return(Rnew)
  else{
    if(result[1]=="html") print(xtable(Rnew), type="html")
    else print(xtable(Rnew), type="latex") 
  }
} 




cntdd.baixaBCB <- function(codSerie, inicio = "01/01/2001"){

################ Instruções #####################################################
# Objetivo: Baixar series temporais do gerenciador do Banco Central
# 
# Input: Código da série no SGS/BACEN
# Output: data.frame com a série
# 
# Baseado no codigo de:
# https://gabrielrega.com/2017/07/26/baixando-series-diretamente-do-sgs-do-banco-central-pelo-r/
#################################################################################
  
  fromJSON(
    paste0(
      "http://api.bcb.gov.br/dados/serie/bcdata.sgs.",
      codSerie, 
      "/dados?formato=json&amp;dataInicial=",
      inicio
    )
  )
}




cntdd.TesteMedia <- function(a, b, pvalor = 0.05){
  
################ Instruções #####################################################
# Objetivo: Gerar uma tabela com resultados de teste de media
# 
# Input: Dois vetores com valores correspondentes a grupos diferentes
# Output: data.frame com resultados
#
# Um exemplo do teste incorporando uma tabela no rmarkdown:
# precisa instalar o pacote kableExtra - install.packages("kableExtra")
#   
#   a <- vetor1
#   b <- vetor2
#   
#   TesteMedia(a, b) %>% 
#     kbl(booktabs = TRUE, digits = 3, align = "l",
#         caption = "Teste de media") %>% 
#     kable_paper(full_width = F) %>% 
#     pack_rows("Testes de Normalidade", 1, 3) %>%  
#     pack_rows("Teste de Média", 4, 6) %>%  
#     pack_rows("Médias vs Observações", 7, 9)
#  
#################################################################################
  
  A <- a
  B <- b
  
  qdeA <- length(A); qdeB <- length(B)
  
  norm.A <- lillie.test(A)
  norm.B <- lillie.test(B)
  pvNormA <- norm.A$p.value
  pvNormB <- norm.B$p.value
  
  varAB <- var.test(A, B)
  pvVarAB <- varAB$p.value
  
  if(pvVarAB > pvalor) {parVar <- T} else {parVar <- F}
  
  if(pvNormA > pvalor & pvNormB > pvalor) {difmedia <- "Teste t";
  pvteste <- t.test(A, B, var.equal = parVar);
  Normalidade <- "Ambos grupos tem distribuição normal"} else {difmedia <- "Mann-Whitney";
  pvteste <- suppressMessages(suppressWarnings(wilcox.test(A, B)));
  Normalidade <- "Pelo menos um dos grupos não tem distribuição normal"}
  
  if(pvteste$p.value > pvalor) {result <- "Médias iguais"} else {result <- "Médias diferentes"}
  
  dados <- data.frame(
    normA = formatC(pvNormA, digits = 3, format = "f", decimal.mark = ","),
    normB = formatC(pvNormB, digits = 3, format = "f", decimal.mark = ","),
    Normalidade = Normalidade,
    teste =  difmedia,
    pvalue = formatC(pvteste$p.value, digits = 3, format = "f", decimal.mark = ","),
    resultado = result,
    mediaA = formatC(mean(A, na.rm = T), digits = 3, format = "f", decimal.mark = ","),
    mediaB = formatC(mean(B, na.rm = T), digits = 3, format = "f", decimal.mark = ","),
    spread = formatC(mediaA - mediaB, digits = 3, format = "f", decimal.mark = ","),
    qdeObs = paste0("A: ", qdeA, " | B: ", qdeB))
  
  tabDados <<- as.data.frame(t(dados))
  rownames(tabDados) <-c(
    "p-Value A", "p-Value B", "Normalidade", "Teste Usado", "p-Value", "Análise",
    "Média de A", "Média de B", "Qde de Obs"
  )
  
  colnames(tabDados) <- "Resultados"
  
  return(tabDados)
}



cntdd.CNPJ_Mascara <- function(cnpj){
  
  ################ Instruções #####################################################
  # Objetivo: Gerar a máscara do CNPJ quando o CNPJ vier como numero
  # 
  # Input: CNPJ em formato numérico
  # Output: CNPJ em formato string com a máscara de 18 dígitos
  #################################################################################
  
  qdeCar <- nchar(cnpj)
  cnpj1 <- paste0(substr("00000000000000", 1, (14-qdeCar)),
                  substr(cnpj, 1, qdeCar-2), "-",
                  substr(cnpj, qdeCar-1, qdeCar))
  cnpjFinal <- paste0(substr(cnpj1, 1, 2), ".",
                      substr(cnpj1, 3, 5), ".",
                      substr(cnpj1, 6, 8), "/",
                      substr(cnpj1, 9, 15))
  
  return(cnpjFinal)
  
}



cntdd.utils <-
  list(
    cntdd.theme =
      theme(legend.position = "bottom", legend.title = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(),
            axis.text.y = element_text(color = "snow4"),
            axis.text.x = element_text(color = "snow4"),
            plot.title = element_text(color = 'lightblue3', size = 15),
            plot.subtitle = element_text(color = "snow4", size = 10),
            plot.caption = element_text(color = "snow3", size = 8),
            panel.background = element_blank(),
            panel.grid = element_line(color = "snow2")),
    
    meses =
      data.frame(mes.num = 1:12,
                 mes.nome = c("Janeiro", "Fevereiro", "Março", "Abril", "Maio",
                              "Junho", "Julho", "Agosto", "Setembro", "Outubro",
                              "Novembro", "Dezembro"),
                 mes.abb  = c("Jan", "Fev", "Mar", "Abr", "Mai", "Jun",
                              "Jul", "Ago", "Set", "Out", "Nov", "Dez"),
                 month.name = month.name,
                 month.abb = month.abb,
                 mes.chr = c("01", "02", "03", "04", "05", "06", "07", "08",
                             "09", "10", "11", "12"),
                 bim  = rep(1:6, each = 2),
                 trim = rep(1:4, each = 3),
                 quad = rep(1:3, each = 4),
                 sem  = rep(1:2, each = 6)),
    
    ufsRegiao =
      data.frame(
        uf = c("DF", "GO", "MS", "MT", "CE", "AL", "BA", "MA", "PB", "PE", "PI",
               "RN", "SE", "AC", "AM", "AP", "PA", "RO", "RR", "TO", "RJ", "ES",
               "MG", "SP", "PR", "RS", "SC", "EX", "BR"),
        descUF = c("Distrito Federal", "Goiás", "Mato Grosso do Sul",
                   "Mato Grosso", "Ceará", "Alagoas", "Bahia", "Maranhão",
                   "Paraíba", "Pernambuco", "Piauí", "Rio Grande do Norte",
                   "Sergipe", "Acre", "Amazonas", "Amapá", "Pará", "Rondônia",
                   "Roraima", "Tocantins", "Rio de Janeiro", "Espírito Santo",
                   "Minas Gerais", "São Paulo", "Paraná", "Rio Grande do Sul",
                   "Santa Catarina", "Exército", "Multiplos Estados"),
        codRegiao = c("CO", "CO", "CO", "CO", "NE", "NE", "NE", "NE", "NE", "NE",
                      "NE", "NE", "NE", "NO", "NO", "NO", "NO", "NO", "NO", "NO",
                      "SD", "SD", "SD", "SD", "SU", "SU", "SU", "FAB", "BR"),
        regiao = c("Centro-Oeste", "Centro-Oeste", "Centro-Oeste", "Centro-Oeste",
                   "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Nordeste",
                   "Nordeste", "Nordeste", "Nordeste", "Nordeste", "Norte",
                   "Norte", "Norte", "Norte", "Norte", "Norte", "Norte",
                   "Sudeste", "Sudeste", "Sudeste", "Sudeste", "Sul", "Sul",
                   "Sul", "Brasil", "Multiplos Estados")
      )
    
  )



cntdd.AlocMemoria <- function(x){
  memoriaMb <- x %>% object.size()/(1000^2)
  resultado <- paste0(round(memoriaMb[1], 2), " Megabytes | Previsto para RDS: ",  round(memoriaMb[1], 2)*.1, " Megabytes (", round(memoriaMb[1], 2)*.1*1000, " Kilobytes)")
  return(resultado)
}


# Cria função de pesos para Elicit
pesosElicit <- function(n_amostra = 1000, n_pesos = 5, minimo = 0.01, maximo = 2){
  
  wElicit <- function(n = 5, p = 0.5){
    df <- data.frame(ord = 1:n)
    df$num <- (n-df$ord+1)^p
    df$weight <- df$num/sum(df$num)
    
    return(df$weight)
  }
  
  penal <- runif(n_amostra, minimo, maximo)
  
  resultado <- list()
  
  for (i in 1:n_amostra) {
    resultado[[paste0("w",i)]] <- wElicit(n = n_pesos, p = penal[i])
  }
  
  
  as.data.frame(Reduce(rbind, resultado)) %>% 
    mutate(
      indiv = paste0("Indiv", 1:n_amostra)) %>% 
    pivot_longer(cols = starts_with("V"), names_to = "Pesos", values_to = "Valores") %>% 
    arrange(indiv, Pesos) %>% 
    group_by(indiv) %>% 
    mutate(
      verifOrd =
        case_when(
          Pesos == "V1" ~ TRUE,
          TRUE ~ Valores < dplyr::lag(Valores)
        ),
      verifSum = sum(Valores)
    ) -> bd1
  
  
  bd1 %>%
    group_by(Pesos) %>%
    summarise(Media = mean(Valores), Desvio = sd(Valores)) %>% 
    data.frame -> bd2
  
  resultado <- list(bdCompleto = bd1, Tabela = bd2)
  
  return(resultado)
  
}


# Comportamento médio e de desvio de uma variável, baseado em grupo criado a
# partir de uma variável do banco

mediaDesvioPorGrupo <- function(varGrupo, varInteresse, ngrupos = 10){
  
  dados %>% 
    mutate(grupo = cut(get(varGrupo), quantile(get(varGrupo), probs = 0:ngrupos/ngrupos),
                       include.lowest = T,
                       labels = paste0("p", 1:ngrupos))) %>% 
    group_by(grupo) %>% 
    summarise(across(varInteresse, list(mean = mean, sd = sd))) %>% 
    pivot_longer(cols = -grupo,
                 names_to = "estat",
                 values_to = "valores") %>% 
    pivot_wider(names_from = "grupo",
                values_from = "valores")
}

cntdd.novos <- 
  list(
    elicit =
      function(n_amostra = 1000, n_pesos = 5, minimo = 0.01, maximo = 2){
        
        wElicit <- function(n = 5, p = 0.5){
          df <- data.frame(ord = 1:n)
          df$num <- (n-df$ord+1)^p
          df$weight <- df$num/sum(df$num)
          
          return(df$weight)
        }
        
        penal <- runif(n_amostra, minimo, maximo)
        
        resultado <- list()
        
        for (i in 1:n_amostra) {
          resultado[[paste0("w",i)]] <- wElicit(n = n_pesos, p = penal[i])
        }
        
        
        as.data.frame(Reduce(rbind, resultado)) %>% 
          mutate(
            indiv = paste0("Indiv", 1:n_amostra)) %>% 
          pivot_longer(cols = starts_with("V"), names_to = "Pesos", values_to = "Valores") %>% 
          arrange(indiv, Pesos) %>% 
          group_by(indiv) %>% 
          mutate(
            verifOrd =
              case_when(
                Pesos == "V1" ~ TRUE,
                TRUE ~ Valores < dplyr::lag(Valores)
              ),
            verifSum = sum(Valores)
          ) -> bd1
        
        
        bd1 %>%
          group_by(Pesos) %>%
          summarise(Media = mean(Valores), Desvio = sd(Valores)) %>% 
          data.frame -> bd2
        
        resultado <- list(bdCompleto = bd1, Tabela = bd2)
        
        return(resultado)
        
      },
    MediaDesvioGrupo =
      function(df, varGrupo, varInteresse, ngrupos = 5, VarMedia1 = NULL, VarMedia2 = NULL){
        
        dados <- df
        
        bd1 <-
          suppressMessages(suppressWarnings(
            dados %>% 
              mutate(grupo = cut(get(varGrupo), quantile(get(varGrupo), probs = 0:ngrupos/ngrupos),
                                 include.lowest = T,
                                 labels = paste0("p", 1:ngrupos)))
          )) -> bd1
        
        res1 <-
          bd1 %>% 
          group_by(grupo) %>% 
          summarise(across(varInteresse, list(mean = mean, sd = sd))) %>% 
          pivot_longer(cols = -grupo,
                       names_to = "estat",
                       values_to = "valores") %>% 
          pivot_wider(names_from = "grupo",
                      values_from = "valores")
        
        VarMedia1 <- 2    
        if(is.null(VarMedia1)) {
          grp1 <- bd1 %>% filter(grupo == paste0("p", 1)) %>% dplyr::select(contains(varInteresse)) %>% pull()
        } else {
          grp1 <- bd1 %>% filter(grupo == paste0("p", VarMedia1)) %>% dplyr::select(contains(varInteresse)) %>% pull()
        }
        
        VarMedia2 <- 5
        if(is.null(VarMedia2)) {
          grp2 <- bd1 %>% filter(grupo == paste0("p", ngrupos)) %>% dplyr::select(contains(varInteresse)) %>% pull()
        } else {
          grp2 <- bd1 %>% filter(grupo == paste0("p", VarMedia2)) %>% dplyr::select(contains(varInteresse)) %>% pull()
        }
        
        res2 <- cntdd.TesteMedia(grp1, grp2)
        
        bd1 %>% dplyr::select(contains(varInteresse), grupo) -> bd3
        
        formula <- paste(names(bd3), collapse = "~")
        mod1 <- lm(formula, data = bd3)
        mod.av <- aov(mod1)
        TkTest <- TukeyHSD(mod.av)$grupo
        
        resultado <- list(MediaDesvioGrupo = res1,
                          TesteMedia = res2,
                          Tukey = TkTest)
        
        
        return(resultado)
        
      }
    
  )



