#Abrindo pacotes necessários

install.packages("PNSIBGE")
library(PNSIBGE)
library(tidyverse)

#Abrindo o banco de dados

dados <- read_pns(
  microdata = "C:/estatística/Trabalho de MET2/PNS_2019.txt", 
  input_txt = "C:/estatística/Trabalho de MET2/input_PNS_2019.txt")

#Filtrando as exigencias do professor

dados1 <- dados%>%
  filter(V0001 == 53, V0031 == 1, V0025A == 1, C008 >= 18, V0015 == "01")
dados1$V0015 == "01"

#Selecionando a amostra

set.seed(019)
amostra <- dados1%>%
  sample_n(40)
