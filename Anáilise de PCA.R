# Análise fatorial por componentes principais - Indicadores de saúde pública dos municípios do estado de São Paulo.

# Curso: MBA DSA (USP ESALQ)
# Aluno: Victor Cypriano

# A partir da correlação de Pearson, seguimos com a análise

# Estabelecendo uma nova matriz de correlações de Pearson
rho <- cor(dados[,3:18])

# Elaborando um mapa de calor das correlações
dados[,3:18] %>% 
  cor() %>% 
  melt() %>% 
  rename(Correlação = value) %>%
  ggplot() +
  geom_tile(aes(x = Var1, y = Var2, fill = Correlação)) +
  geom_text(aes(x = Var1, y = Var2, label = format(Correlação, digits = 2)),
            size = 5) +
  scale_fill_gradient2(low = "brown", 
                       mid = "white", 
                       high = "dodgerblue4",
                       midpoint = 0) +
  labs(x = NULL, y = NULL) +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom",
        axis.text.x = element_text(angle = 30, hjust = 1, size = 12),
        axis.text.y = element_text(angle = 30, hjust = 1, size = 12))

# Selecionando apenas os dados para análise
dados_analise <- (dados[,3:18])

# Aplicando o teste de esfericidade de Bartlett
cortest.bartlett(dados_analise, n= 16)

# Análise Fatorial por Componentes Principais (PCA)
fatorial <- principal(dados_analise,
                      nfactors = length(dados_analise),
                      rotate = "none",
                      scores = TRUE)

# Identificação inicial de todos os autovalores
eigenvalues <- round(fatorial$values, 5)
print(eigenvalues)
sum(eigenvalues)

# Quantidade de autovalores maiores que 1 (critério de Kaiser)
k <- sum(eigenvalues > 1)
print(k)

#Obs.: Essa informação só está sendo gerada para análise, não usaremos esse critério para exclusão de valores no estudo


# Identificação da variância compartilhada em cada fator extraído
variancia_compartilhada <- as.data.frame(fatorial$Vaccounted) %>% 
  slice(1:3)

rownames(variancia_compartilhada) <- c("Autovalores",
                                       "Prop. da Variância",
                                       "Prop. da Variância Acumulada")

# Gerando gráfico com os autovalores para cada fator
variancia_compartilhada %>%
  slice(1) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*1, 2)) , vjust = -0.5))+
  labs(x = "Fatores",
       y = "Autovalores") +
  theme_bw()

# Gerando gráfico com a proporção da variância compartilhada em cada fator
variancia_compartilhada %>%
  slice(2) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.5))+
  labs(x = "Fatores",
       y = "Variância Compartilhada") +
  theme_bw()

# Gerando gráfico com a proporção da variância compartilhada acumulada em cada fator
variancia_compartilhada %>%
  slice(3) %>% 
  melt() %>% 
  ggplot(aes(x = variable, 
             y = value)) + 
  geom_col(fill = "orange", color = "black") +
  geom_text(aes(label = paste0(round(value*100, 2),"%") , vjust = -0.5))+
  labs(x = "Fatores",
       y = "Variância Compartilhada Acumulada") +
  theme_bw()

# Extraindo as Cargas Fatoriais
cargas_fatoriais <- as.data.frame(unclass(fatorial$loadings))

# Visualizando as cargas fatoriais
cargas_fatoriais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Extraindo as Comunalidades
comunalidades <- as.data.frame(unclass(fatorial$communality)) %>%
  rename(Comunalidades = 1)

# Visualizando as Comunalidades
comunalidades %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Relatório das cargas fatoriais e das comunalidades
cargas_fatoriais %>%
  mutate(Comunalidades = rowSums(cargas_fatoriais ^ 2)) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Identificação dos Scores Fatoriais
scores_fatoriais <- as.data.frame(fatorial$weights)

# Visualizando os Scores Fatoriais
scores_fatoriais %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Cálculo dos fatores extraídos
fatores <- as.data.frame(fatorial$scores)

dados <- bind_cols(dados,
                   "fator_1" = fatores$PC1, 
                   "fator_2" = fatores$PC2,
                   "fator_3" = fatores$PC3,
                   "fator_4" = fatores$PC4,
                   "fator_5" = fatores$PC5,
                   "fator_6" = fatores$PC6,
                   "fator_7" = fatores$PC7,
                   "fator_8" = fatores$PC8,
                   "fator_9" = fatores$PC9,
                   "fator_10" = fatores$PC10,
                   "fator_11" = fatores$PC11,
                   "fator_12" = fatores$PC12,
                   "fator_13" = fatores$PC13,
                   "fator_14" = fatores$PC14,
                   "fator_15" = fatores$PC15,
                   "fator_16" = fatores$PC16)


# Construção de um ranking

# Assumindo todos os fatores calculados anteriormente, calcula-se a Pontuação
# Trata-se do fator * variância compartilhada por aquele fator
dados <- dados %>% 
  mutate(Pontuacao = fator_1 * variancia_compartilhada$PC1[2] +
           fator_2 * variancia_compartilhada$PC2[2] +
           fator_3 * variancia_compartilhada$PC3[2] +
           fator_4 * variancia_compartilhada$PC4[2] +
           fator_5 * variancia_compartilhada$PC5[2] +
           fator_6 * variancia_compartilhada$PC6[2] +
           fator_7 * variancia_compartilhada$PC7[2] +
           fator_8 * variancia_compartilhada$PC8[2] +
           fator_9 * variancia_compartilhada$PC9[2] +
           fator_10 * variancia_compartilhada$PC10[2] +
           fator_11 * variancia_compartilhada$PC11[2] +
           fator_12 * variancia_compartilhada$PC12[2] +
           fator_13 * variancia_compartilhada$PC13[2] +
           fator_14 * variancia_compartilhada$PC14[2] +
           fator_15 * variancia_compartilhada$PC15[2] +
           fator_16 * variancia_compartilhada$PC16[2])

# Visualizando o ranking final em ordem decrescente
dados[,c(2, 35)] %>%
  arrange(desc(Pontuacao)) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)

# Fim!