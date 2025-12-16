#Abrindo pacotes necessários

library(PNSIBGE)
library(tidyverse)

#Abrindo o banco de dados

dados <- read_pns(
  microdata = "C:/Users/user/Documents/trabalho_met2/Trabalho_metest_2_smks/PNS_2019.txt", 
  input_txt = "C:/Users/user/Documents/trabalho_met2/Trabalho_metest_2_smks/input_PNS_2019.txt")

#Filtrando as exigencias do professor

dados1 <- dados %>%
  filter(V0001 == 53, V0031 == 1, V0025A == 1, C008 >= 18, V0015 == "01")

#Selecionando a amostra

set.seed(019)
amostra <- dados1%>%
  sample_n(40)

#Caracterização da amostra

percent <- function(absolute, digits = 2) {
  return(round(100 * absolute / sum(absolute), digits))
}

##Total de moradores por domicílio

graf5 <- ggplot(amostra) +
  aes(
    x = factor(""),
    y = V0022
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Total de moradores por domicílio") + theme_bw()

resumo1 <- amostra%>%
  summarise(n = n(),
            Media = mean(V0022, na.rm = TRUE),
            DP = sd(V0022, na.rm = TRUE),
            Min = min(V0022, na.rm = TRUE),
            Q1 = quantile(V0022, 0.25, na.rm = TRUE),
            Mediana = median(V0022, na.rm = TRUE),
            Q3 = quantile(V0022, 0.75, na.rm = TRUE),
            Max = max(V0022, na.rm = TRUE))
print(resumo1)


##Tipo de domicílio

classes2 <- amostra %>%
  filter(!is.na(A001)) %>%
  mutate(A001 = factor(A001, levels = c(1, 2, 3))) %>%
  count(A001, .drop = F) %>%
  mutate(
    freq = n 
    %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

graf4 <- ggplot(classes2) +
  aes(
    x = A001,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  scale_x_discrete(labels = c("1" = "Casa", "2" = "Apartamento", "3" = "Cortiço")) +
  labs(x = "Tipo de domicílio", y = "Frequência") +
  theme_bw()

##Sexo

classes3 <- amostra %>%
  filter(!is.na(C006)) %>%
  count(C006) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

graf2 <- ggplot(classes3) +
  aes(
    x = C006,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  scale_x_discrete(labels = c("1" = "Homem", "2" = "Mulher")) +
  labs(x = "Sexo", y = "Frequência") +
  theme_bw()

##Idade

graf1 <- ggplot(amostra) +
  aes(
    x = factor(""),
    y = C008
  ) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  guides(fill = FALSE) +
  stat_summary(
    fun = "mean", geom = "point", shape = 23, size = 3, fill = "white"
  ) +
  labs(x = "", y = "Idade dos respondentes") + theme_bw()
graf1

resumo <- amostra%>%
  summarise(n = n(),
            Media = mean(C008, na.rm = TRUE),
            DP = sd(C008, na.rm = TRUE),
            Min = min(C008, na.rm = TRUE),
            Q1 = quantile(C008, 0.25, na.rm = TRUE),
            Mediana = median(C008, na.rm = TRUE),
            Q3 = quantile(C008, 0.75, na.rm = TRUE),
            Max = max(C008, na.rm = TRUE))
print(resumo)

##Estado civil

classes4 <- amostra %>%
  filter(!is.na(C011)) %>%
  count(C011) %>%
  mutate(
    freq = n %>% percent(),
  ) %>%
  mutate(
    freq = gsub("\\.", ",", freq) %>% paste("%", sep = ""),
    label = str_c(n, " (", freq, ")") %>% str_squish()
  )

graf3 <- ggplot(classes4) +
  aes(
    x = C011,
    y = n,
    label = label
  ) +
  geom_bar(stat = "identity", fill = "#A11D21", width = 0.7) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = -0.5, # hjust = .5,
    size = 3
  ) +
  scale_x_discrete(labels = c("1" = "Casado(a)", "2" = "Divorciado(a)", "3" = "Viúvo(a)", "4" = "Solteiro(a)")) +
  labs(x = "Estado civil", y = "Frequência") +
  theme_bw()

#Testes de hipotéses
library(tidyverse)
options(scipen = 999)

# 1. PREPARAÇÃO E CORREÇÃO DE TIPOS 

dados <- dados %>%
  mutate(
    N001 = as.numeric(N001)
  )

dados_limpos <- amostra %>%
  mutate(
    N001 = as.numeric(N001),
    I00101 = as.numeric(I00101)
  ) %>%
  # Filtrando NAs
  filter(!is.na(N001), !is.na(I00101)) %>%
  # Criamos a categoria
  mutate(
    Status_Plano = ifelse(I00101 == 1, "Com Plano", "Sem Plano")
  )


# 2. TESTE DE NORMALIDADE (Shapiro-Wilk)
grupo_com <- dados_limpos %>% filter(I00101 == 1)
grupo_sem <- dados_limpos %>% filter(I00101 == 2)

print("--- Shapiro-Wilk: Grupo COM Plano ---")
# Verificação de segurança para N < 3
if(nrow(grupo_com) >= 3) {
  print(shapiro.test(grupo_com$N001))
} else {
  print("Amostra insuficiente para Shapiro neste grupo.")
}

print("--- Shapiro-Wilk: Grupo SEM Plano ---")
if(nrow(grupo_sem) >= 3) {
  print(shapiro.test(grupo_sem$N001))
} else {
  print("Amostra insuficiente para Shapiro neste grupo.")
}

# 3. TESTE DE HIPÓTESE
# Mann-Whitney
teste_mwu <- wilcox.test(N001 ~ I00101, data = dados_limpos)

print(teste_mwu)

library(tidyverse)
options(scipen = 999) # Remove notação científica

library(tidyverse)
options(scipen = 999) 

# --- 1. PREPARAÇÃO DOS DADOS ---
# Certifique-se de que 'amostra' está carregado
dados_limpos <- amostra %>%
  select(C008, M002) %>%
  mutate(
    Idade = as.numeric(C008),
    EstCivil_Cod = as.numeric(M002)
  ) %>%
  filter(!is.na(Idade), !is.na(EstCivil_Cod)) %>%
  mutate(
    Estado_Civil = case_when(
      EstCivil_Cod == 1 ~ "Solteiro",
      EstCivil_Cod == 2 ~ "Casado",
      EstCivil_Cod == 3 ~ "Divorciado/Separado",
      EstCivil_Cod == 4 ~ "Viúvo",
      TRUE ~ "Outro"
    )
  )

# --- 2. TESTES DE PRESSUPOSTOS (Shapiro-Wilk) ---
print("--- Teste de Normalidade (Shapiro-Wilk) por Grupo ---")

# Filtrando explicitamente usando a coluna Qtd criada acima
grupos_validos <- descritiva %>% 
  dplyr::filter(Qtd >= 3) %>% 
  pull(Estado_Civil)

if(length(grupos_validos) == 0) {
  print("Nenhum grupo tem tamanho suficiente (>=3) para o teste de Shapiro.")
} else {
  for(grupo in grupos_validos) {
    sub_dados <- dados_limpos %>% dplyr::filter(Estado_Civil == grupo)
    tryCatch({
      teste <- shapiro.test(sub_dados$Idade)
      print(paste("Grupo:", grupo, "| p-valor:", round(teste$p.value, 4)))
    }, error = function(e) {
      print(paste("Grupo:", grupo, "| Erro: Variância constante ou dados insuficientes."))
    })
  }
}

# --- 3. TESTE DE HIPÓTESE (Kruskal-Wallis) ---
# O Kruskal-Wallis é robusto e funciona mesmo com N pequeno e não-normal
teste_kw <- kruskal.test(Idade ~ Estado_Civil, data = dados_limpos)

print("--- Resultado Final: Kruskal-Wallis ---")
print(teste_kw)



