# Análise Crítica do Ranking - Indicadores de saúde pública dos municípios do estado de São Paulo.

# Curso: MBA DSA (USP ESALQ)
# Aluno: Victor Cypriano

# Visualizando os 10 primeiros do ranking final
Top_10 <- dados[,c(2, 35)] %>%
  arrange(desc(Pontuacao)) %>%
  head(10) %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
Top_10

# gerando tabela de dados dos 10 primeiros do ranking final
dados_top_10 <- dados[,c(1:18, 35)] %>%
  arrange(desc(Pontuacao)) %>%
  head(10)

dados_top_10 <- subset(dados_top_10, select = -Pontuacao)


# Calcula a média de cada Variáveis do data frame 'dados_top_10'
media_colunas <- colMeans(dados_analise, na.rm = TRUE)

# Criando uma nova linha com as médias
linha_média <- data.frame(matrix("Média", nrow = 1, ncol = ncol(dados_top_10)))
names(linha_média) <- names(dados_top_10)
linha_média[, 3:ncol(linha_média)] <- media_colunas

# Adiciona a média como uma nova linha ao final do data frame 'dados'
dados_top_10_com_media <- rbind(dados_top_10, linha_média)




# Boxplot da Cobertura de Saúde Suplementar
boxplot(dados$Cobertura_SSuple, dados_top_10$Cobertura_SSuple,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Cobertura Saúde Suplementar",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da Cobertura Vacinal Tetravalente
boxplot(dados$Cobertura_vacinal_tetra_menor_1ano, dados_top_10$Cobertura_vacinal_tetra_menor_1ano,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Cobertura Vacinal Tetravalente",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da Despesa_em_saude_per_capita
boxplot(dados$Despesa_em_saude_per_capita, dados_top_10$Despesa_em_saude_per_capita,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Despesa em saúde per capita",
        ylab = "Valor por Habitante",
        col = c("orange","gray"))

# Boxplot da Leitos_TOTAL
boxplot(dados$Leitos_TOTAL, dados_top_10$Leitos_TOTAL,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Número de Leitos por 1.000 habitantes",
        ylab = "N.º de leitos",
        col = c("orange","gray"))

# Boxplot da Tx_Mortalidade_Infantil
boxplot(dados$Tx_Mortalidade_Infantil, dados_top_10$Tx_Mortalidade_Infantil,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot Tx_Mortalidade_Infantil",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da Tx_Mort_Apar_Circulatorio
boxplot(dados$Tx_Mort_Apar_Circulatorio, dados_top_10$Tx_Mort_Apar_Circulatorio,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot Tx_Mort_Apar_Circulatorio",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da Tx_Mort_Total_Neoplasias
boxplot(dados$Tx_Mort_Total_Neoplasias, dados_top_10$Tx_Mort_Total_Neoplasias,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot Tx_Mort_Total_Neoplasias",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da Tx_Mort_Causas_Externas_Total
boxplot(dados$Tx_Mort_Causas_Externas_Total, dados_top_10$Tx_Mort_Causas_Externas_Total,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot Tx_Mort_Causas_Externas_Total",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_internacoes_por_CSAB
boxplot(dados$perc_internacoes_por_CSAB, dados_top_10$perc_internacoes_por_CSAB,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot perc_internacoes_por_CSAB",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_NV_Baixo_peso_nascer_menos_2.5kg
boxplot(dados$perc_NV_Baixo_peso_nascer_menos_2.5kg, dados_top_10$perc_NV_Baixo_peso_nascer_menos_2.5kg,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot perc_NV_Baixo_peso_nascer_menos_2.5kg",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_NV_Maes_menos_20anos
boxplot(dados$perc_NV_Maes_menos_20anos, dados_top_10$perc_NV_Maes_menos_20anos,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot perc_NV_Maes_menos_20anos",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_internacoes_pop_total
boxplot(dados$perc_internacoes_pop_total, dados_top_10$perc_internacoes_pop_total,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot perc_internacoes_pop_total",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_obitos_definidos_geral
boxplot(dados$perc_obitos_definidos_geral, dados_top_10$perc_obitos_definidos_geral,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Boxplot perc_obitos_definidos_geral",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_qualidade_trat_agua_BAC
boxplot(dados$perc_qualidade_trat_agua_BAC, dados_top_10$perc_qualidade_trat_agua_BAC,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Qualidade de tratamento de água - Bacteriológico",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_qualidade_trat_agua_CLO
boxplot(dados$perc_qualidade_trat_agua_CLO, dados_top_10$perc_qualidade_trat_agua_CLO,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Qualidade de tratamento de água - Cloro",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_qualidade_trat_agua_FLU
boxplot(dados$perc_qualidade_trat_agua_FLU, dados_top_10$perc_qualidade_trat_agua_FLU,
        names = c("Dados Gerais", "Ranking 10 Primeiros"),
        horizontal = FALSE,
        main = "Qualidade de tratamento de água - Fluor",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))

# Boxplot da perc_qualidade_trat_agua
boxplot(dados$perc_qualidade_trat_agua_BAC, dados_top_10$perc_qualidade_trat_agua_BAC, 
        dados$perc_qualidade_trat_agua_CLO, dados_top_10$perc_qualidade_trat_agua_CLO, 
        dados$perc_qualidade_trat_agua_FLU, dados_top_10$perc_qualidade_trat_agua_FLU,
        names = c("Dados Gerais - BAC", "Ranking 10 Primeiros - BAC",
                  "Dados Gerais - CLO", "Ranking 10 Primeiros - CLO",
                  "Dados Gerais - FLU", "Ranking 10 Primeiros - FLU"),
        horizontal = FALSE,
        main = "Qualidade de tratamento de água - Bacteriológico, Cloro e Fluor",
        ylab = "Porcentagem (%)",
        col = c("orange","gray"))




# Calcula a média de cada Variáveis do data frame
media_colunas <- colMeans(dados_analise, na.rm = TRUE)

# Calcula a média de cada Variáveis do data frame
media_colunas_top_10 <- colMeans(dados_top_10[,3:18], na.rm = TRUE)

# Diferença entre as médias
diferença <- media_colunas_top_10 - media_colunas


# Criando uma nova linha com as médias
média_indicadores <- data.frame(matrix("Média Geral", nrow = 1, ncol = ncol(dados_top_10)))
names(média_indicadores) <- names(dados_top_10)
média_indicadores[, 3:ncol(média_indicadores)] <- media_colunas

média_indicadores <- subset(média_indicadores, select = -c(Codigo, Municipio))
média_indicadores <- rbind(média_indicadores, media_colunas_top_10, diferença)

rownames(média_indicadores) <- c("Média Total",
                                 "Média ranking 10",
                                 "Diferença")

# Visualizando o tabela de médias
média_indicadores %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = T, 
                font_size = 12)
