
cntdd.Carteira.GeraRetornos <- function(
  semana = 1,
  caminho = "sheets/controleCarteiras.xlsx"){
  
  escolheSemana <- semana
  
  paramCarteiras <- read_xlsx(caminho)
  
  lst.Parametros <- sapply(paramCarteiras[, 1:length(paramCarteiras)], list)
  
  lst.ParametrosPapel <-
    lapply(lst.Parametros[4:length(lst.Parametros)],
           function(x) x[escolheSemana])
  
  periodo <- paste0(as.character(lst.Parametros$inicio[escolheSemana]),
                    " a ", as.character(lst.Parametros$fim[escolheSemana]))
  
  # Gera lista com todos os dados semanais dos papeis -----------------------
  
  # Coleta dados do yahoo finance (get.hist.quote - tseries)
  lst.Papeis <- toupper(c("^BVSP",
                          as.character(unlist(lst.ParametrosPapel[
                            !is.na(lst.ParametrosPapel)]))))
  
  lst.precos <- list()
  
  for (i in seq_along(lst.Papeis)) {
    lst.precos[[lst.Papeis[i]]] <- tseries::get.hist.quote(
      lst.Papeis[i], 
      start = as.character(lst.Parametros$inicio[escolheSemana]),
      end = as.character(lst.Parametros$fim[escolheSemana]),
      compression = "d")
  }
  
  # Inclui retorno na lista de precos
  
  lst.PrecoRetorno <-
    lapply(
      lst.precos,
      function(x) cbind(x, retorno = PerformanceAnalytics::Return.calculate(Op(x))))

    # Gera banco de dados dos retornos dos papeis da carteira e do Ibovespa
  
  lapply(lst.PrecoRetorno,
         function(x) data.frame(
           data = as.character(index(x)),
           retorno = as.numeric(x[ ,5]))) %>% 
    bind_rows(.id = "codigo") %>%
    select(codigo, data, retorno) %>% 
    spread(codigo, retorno) %>% 
    na.omit -> bd.retornosIbovPapeis
  
  bd.tsRetornosIbovPapeis <- xts(bd.retornosIbovPapeis[, -1],
                                 order.by = as.Date(bd.retornosIbovPapeis[, 1]))
  
  papeisCarteira <- bd.tsRetornosIbovPapeis[,-1]
  ibovespa <- bd.tsRetornosIbovPapeis[,1]
  
  # Gera retorno do portfolio
  
  bd.RetCarteira <- PerformanceAnalytics::Return.portfolio(na.omit(papeisCarteira))
  
  # Reune todos os resultados
  
  bd.TodosRetornos <-   merge(bd.RetCarteira, ibovespa, papeisCarteira)
  names(bd.TodosRetornos) <- c("UFERSA3", "Ibovespa",
                               names(bd.TodosRetornos)[3:ncol(bd.TodosRetornos)])
  
  
  # Calcula o retorno acumulado
  
  lst.CumRet <- list()
  
  for (i in 1:nrow(bd.TodosRetornos)) {
    cumRet <- data.frame(
      data = time(bd.TodosRetornos)[i],
      PerformanceAnalytics::Return.cumulative(bd.TodosRetornos[1:i]))
    rownames(cumRet) <- NULL
    lst.CumRet[[i]] <- cumRet
    
  }
  
  RetornoAcumulado <- Reduce(rbind, lst.CumRet)
  bd.RetornoAcumulado <- xts(RetornoAcumulado[, -1],
                             order.by = as.Date(RetornoAcumulado[, 1]))
  
  # Prepara dados para gráfico de desempenho acumulado
  
  RetornoAcumulado %>% 
    select(data, UFERSA3, Ibovespa) %>% 
    gather(papeis, retorno, UFERSA3:Ibovespa) %>% 
    mutate(retorno = round(retorno * 100, 2)) %>% 
    data.frame -> bd.GrafAcum
  
  
  ggplot(bd.GrafAcum, aes(x = as.Date(data), y = retorno, color = papeis)) +
    geom_line(size= 1, alpha = 0.5) +
    geom_text_repel(
      aes(label = retorno), data = bd.GrafAcum %>% filter(data == max(data)),
      color = "blue", size = 4, nudge_y = -0.5, alpha = 0.5
    ) +
    labs(y = NULL, 
         x = NULL,
         title = paste0("Retorno acumulado: Carteira UFERSA3 vs Ibovespa ",
                        "(", min(bd.GrafAcum$data), " a ", max(bd.GrafAcum$data) , ")"),
         subtitle = paste0("Valores em percentual", "\n",
                           "Disclaimer: Esse é um trabalho acadêmico e não constitui",
                           " recomendação de investimento", "\n",
                           "Composição da carteira igualmente ponderada: ", paste0(names(papeisCarteira), collapse = " | ")),
         caption = "Elaboração: contabiliDados") +
    cntdd.theme -> graficoAcum
  
  # Gera gráfico para base 100 ----------------------------------------------
  
  lapply(lst.precos,
         function(x) data.frame(data = as.Date(index(x)),
                                Open100 = as.numeric(Op(x))/as.numeric(Op(x)[1])*100)) %>%
    bind_rows(.id = "codigo") %>%
    select(codigo, data, Open100) %>%
    mutate(carteira = ifelse(codigo == "^BVSP", "Ibovespa", "UFERSA3")) %>% 
    group_by(carteira, data) %>% 
    summarise(Open100 = round(mean(Open100), 2)) -> bd.GrafBase100 
  
  
  ggplot(bd.GrafBase100, aes(x = as.Date(data), y = Open100, color = carteira)) +
    geom_line(size= 1, alpha = 0.5) +
    geom_text_repel(
      aes(label = Open100), data = bd.GrafBase100 %>% filter(data == max(data)),
      color = "blue", size = 4, nudge_y = -0.5, alpha = 0.5
    ) +
    labs(y = NULL, 
         x = NULL,
         title = paste0("Projeção em base 100 (primeiro dia): Carteira UFERSA3 vs Ibovespa ",
                        "(", min(bd.GrafBase100$data), " a ", max(bd.GrafBase100$data) , ")"),
         subtitle = paste0("Valores em percentual", "\n",
                           "Disclaimer: Esse é um trabalho acadêmico e não constitui",
                           " recomendação de investimento", "\n",
                           "Composição da carteira igualmente ponderada: ", paste0(names(papeisCarteira), collapse = " | ")),
         caption = "Elaboração: contabiliDados") +
    cntdd.theme -> graficoBase100
  
  listaGeral <-
    list(
      Semana = as.character(lst.Parametros$Semana[escolheSemana]),
      Periodo = periodo,
      listaPrecoRetorno = lst.PrecoRetorno,
      serieRetornosIbovPapeis = bd.tsRetornosIbovPapeis,
      serieTodosRetornos = bd.TodosRetornos,
      serieRetornosAcumulados = bd.RetornoAcumulado,
      bdDadosGrafAcum = bd.GrafAcum,
      bdDadosGrafBase100 = bd.GrafBase100,
      graficoAcum = graficoAcum,
      graficoBase100 = graficoBase100
    )   
  
  saveRDS(listaGeral,
          paste0("lists/dadosSemana_",
                 lst.Parametros$Semana[escolheSemana],
                 ".rds"))
  
}


cntdd.Carteira.GeraSeries <- function(
  semana = 1,
  caminho = "sheets/controleCarteiras.xlsx"){
  
  serieRetornos <- list()
  serieRetornosAcumulados <- list()
  
  totalSemanas <- nrow(read_xlsx(caminho))
  
  for (i in 1:totalSemanas) {
    a <-  readRDS(paste0("lists/dadosSemana_S", i, ".rds"))
    dados1 <- cbind(semana = as.character(a$Semana),
                    data = index(a$serieTodosRetornos),
                    data.frame(a$serieTodosRetornos))
    rownames(dados1) <- NULL
    dados2 <- cbind(semana = as.character(a$Semana),
                    data = index(a$serieRetornosAcumulados),
                    data.frame(a$serieRetornosAcumulados))
    rownames(dados1) <- NULL
    
    serieRetornos[[paste0("S", i)]] <- dados1
    serieRetornosAcumulados[[paste0("S", i)]] <- dados2
    
  }
  
  # Salva serie completa
  saveRDS(Reduce(full_join, serieRetornos), "bd/SerieRetornosDiariosUFERSA3.rds")
  saveRDS(Reduce(full_join, serieRetornosAcumulados), "bd/SerieRetornosSemanaisUFERSA3.rds")
  
}
  

cntdd.Carteira.ConsultaSemana <- function(
  semana = 1
){
  assign(paste0("dadosSemana", semana),
         readRDS(paste0("lists/dadosSemana_S",semana, ".rds")),
         envir = .GlobalEnv)
  readRDS(paste0("lists/dadosSemana_S",semana, ".rds"))
}


cntdd.Carteira.BaixaSeries <- function(){
  
  bd.RetDiarioCartSemanal <<- readRDS("bd/SerieRetornosDiariosUFERSA3.rds")
  
  bd.RetAcumCartSemanal <<-readRDS("bd/SerieRetornosSemanaisUFERSA3.rds")
  
}


cntdd.Carteira.BaixaRetSemanal <- function(){
  
  bd.RetCartSemanal <<-
    readRDS("bd/SerieRetornosSemanaisUFERSA3.rds") %>% 
    group_by(semana) %>% 
    slice(n())
    
  
}

