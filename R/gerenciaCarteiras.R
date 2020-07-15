# UFERSA3 - Liga de Mercado Financeiro da UFERSA #-#-#-#-#-#-#-#-#-#-#-#-#-#
# Acompanhamento do desempenho das carteiras semanais da UFERSA3  #-#-#-#-#


# Orientações para criação das pastas

  # 1) Dentro da pasta criada no seu computador crie:

    # a) uma pasta chamada 'sheets'. Dentro dela salve a planilha
    # 'controleCarteiras.xlsx' que está no github/UFERSA3 dentro
    # de uma pasta com mesmo nome

    # b) crie uma pasta vazia chamada 'bd'

    # c) crie uma pasta vazia chamada 'lists'


# Procedimentos preliminares ----------------------------------------------

  # Baixa os pacotes (arquivos R) do github

baseURL <- paste0("https://raw.githubusercontent.com/",
                  "UFERSA3/contabilidados/master/R")

cntdd1 <- file.path(baseURL, "contabilidados.R")

cntdd2 <- file.path(baseURL, "avaliaCarteiraSemanal.R")

source(cntdd1, encoding = "UTF-8")
source(cntdd2, encoding = "UTF-8")

cntdd.carregaPacotes("tidyverse")

# Define pasta de trabalho que criou no seu computador
# e criou as pastas indicadas nas orientações

setwd("C:\\Users\\kleber\\Dropbox\\Xtemp\\minhapasta")

# Gerar informações semanais

  # Informar a semana que deseja processar
  # Em tese, só precisa processar a semana
  # apenas uma vez, quando ela é definida
  # na planilha

cntdd.Carteira.GeraRetornos(1)


# Gerar as séries diárias
  
  # Será gerada uma série diária dos retornos diários
  # e outra diaria com o retorno acumulado EM CADA SEMANA

cntdd.Carteira.GeraSeries()


# Consultar dados de uma semana específica
  
  # Informa qual semana e toda a lista fica salva
  # como dadosSemanaX, em que X é a semana indicada

cntdd.Carteira.ConsultaSemana()
dadosSemana1


# Baixar as séries diárias

  # Baixará as duas séries diárias geradas
  # A de retorno diário será 'bd.RetDiarioCartSemanal'
  # A de retorno diário acumulado por semana será 'bd.RetAcumCartSemanal'
  # Não possui parâmetros, ao executar baixa tudo que está disponível

cntdd.Carteira.BaixaSeries()

# Baixa retornos semanais

  # Caso queira saber o retorno de cada semana
  # Não possui parâmetros, ao executar baixa tudo que está disponível

cntdd.Carteira.BaixaRetSemanal()
