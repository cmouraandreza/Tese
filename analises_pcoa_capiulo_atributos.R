################################################################################
#             Rotina de análises básicas do capítulo 1                        #
#     para a PCoA com envfit, Sihouette, hclust e permanova                   #
#                                                             Maio de 2025    #
################################################################################


# Carregando pacotes necessários ----
library(vegan)
library(cluster)
library(ggplot2)
library(dplyr)
library(ggrepel)

# carregando os dados -----
dados <- readxl::read_xlsx('C:/Users/nome_do_arquivo.xlsx',
                             sheet = 6) # numero da pagina que você quer importar os dados

#filtrando os dados
dados_filt<-dados[, c("atri1","atri2","atri3",
                      "atri4","atri5","ambientais1","ambientais2" )]

# Preparação dos dados
dados_filt <- dados_filt %>%
  mutate_if(is.character, as.factor) # tranforma os dados que sã caracteres em fatores

# separa dos dados
funcionais <- dados_filt[, c("atri1","atri2","atri3",
                             "atri4","atri5",)]
ambientais <- dados_filt[, c("ambientais1","ambientais2")]


# PCOA ----
# daisy() lida automaticamente com variáveis mistas
dist_gower <- daisy(funcionais, metric = "gower")

# Realizar PCoA
pcoa <- cmdscale(dist_gower, k = 2, eig = TRUE)

# Criar dataframe com scores da PCoA
scores_df <- data.frame(
  PCo1 = pcoa$points[,1],
  PCo2 = pcoa$points[,2],
  ambientais2 = ambientais$ambientais2,
  ambientais1 = ambientais$ambientais1)

# Calcular correlações das variáveis com os eixos
# Para atributos funcionais
cor_func <- data.frame(
  variavel = colnames(funcionais),
  tipo = "Atributos",
  stringsAsFactors = FALSE)

for(i in 1:ncol(funcionais)) {
  if(is.numeric(funcionais[[i]])) {
    cor_func$PCo1[i] <- cor(funcionais[[i]], scores_df$PCo1, use = "complete.obs")
    cor_func$PCo2[i] <- cor(funcionais[[i]], scores_df$PCo2, use = "complete.obs")
  } else {
    # Para variáveis categóricas, usar análise de redundância
    dummy <- model.matrix(~ funcionais[[i]] - 1)
    cor_func$PCo1[i] <- mean(cor(dummy, scores_df$PCo1))
    cor_func$PCo2[i] <- mean(cor(dummy, scores_df$PCo2))
  }
}


# Para variáveis ambientais
cor_amb <- data.frame(
  variavel = colnames(ambientais),
  PCo1 = NA,
  PCo2 = NA,
  tipo = "Ambientais",
  stringsAsFactors = FALSE
)
for(i in 1:ncol(ambientais)) {
  cor_amb$PCo1[i] <- cor(ambientais[[i]], scores_df$PCo1, use = "complete.obs")
  cor_amb$PCo2[i] <- cor(ambientais[[i]], scores_df$PCo2, use = "complete.obs")
}

# Combinar correlações
cor_todas <- rbind(cor_func, cor_amb)

# Calcular variância explicada
var_explicada <- (pcoa$eig / sum(pcoa$eig)) * 100

#criando estratos de um atributo ambiental
#cortando a ambientais2  
scores_df$Estrato_ambientais2 <- cut(scores_df$ambientais2, breaks = 3)

# Nomeando os estratos
scores_df$Estrato_ambientais2 <- cut(scores_df$ambientais2, 
                                    breaks = 3,
                                    labels = c( "Média", "Alta", "Muito Alta"))
# Plot da PCoA ----

png('C:/Users/imagem_pcoa.png',
    width = 3000, height = 3000, res = 300 ) # 
ggplot() +
  # Pontos das observações
  geom_point(data = scores_df, aes(x = PCo1, y = PCo2, fill = Estrato_ambientais2),
             shape = 21, size = 4, alpha = 0.6, color = "white") +
  scale_fill_manual(values = c(
    "Média" = "#3B528BFF",
    "Alta" = "#21908CFF",
    "Muito Alta" = "gold")) +  # paleta inspirada no viridis
  # Setas para variáveis
  geom_segment(data = cor_todas, 
               aes(x = 0, y = 0, xend = PCo1 * 2, yend = PCo2 * 2,
                   color = tipo),
               arrow = arrow(length = unit(0.2, "cm"))) +
  
  # Labels das variáveis
  geom_text_repel(data = cor_todas,
                  aes(x = PCo1 * 2, y = PCo2 * 2, 
                      label = variavel, color = tipo),
                  size = 5) +
  
  # Personalização
  scale_color_manual(values = c("Atributos" = "black",
                                "Ambientais" = "red")) +
  labs(
    x = paste0("PCoA1 (", round(var_explicada[1], 2), "%)"),
    y = paste0("PCoA2 (", round(var_explicada[2], 2), "%)"),
    color = "Tipo de Variável"
  ) +
  theme(
    #panel.grid = element_blank(),
    axis.text.y = element_text(angle = 90, hjust = 0.5), # Gira os números da latitude
    panel.background = element_rect(fill = "white"),  # fundo branco
    panel.border = element_rect(color = "gray", fill = NA),
    legend.position = "top",  # legenda horizontal embaixo
    legend.direction = "horizontal"
  ) +
  coord_equal()+
  annotate("text", x = -Inf, y = Inf, label = "(b)", hjust = -0.2, vjust = 1.2,
           fontface = "bold", size = 8)

dev.off() #Salva definitivamente o conteúdo no arquivo



# verificando envfit -----

env <- envfit(scores_df[, c("PCo1", "PCo2")], ambientais, permutations = 9999)
        # Coloque as permutações que quiser

# 11. Extrair vetores significativos (p < 0.05)
vetores_env <- as.data.frame(scores(env, display = "vectors"))
vetores_env$variavel <- rownames(vetores_env)
vetores_env$p_valor <- env$vectors$pvals
colnames(vetores_env)[1:2] <- c("Dim1", "Dim2")
vetores_env_sig <- vetores_env %>% filter(p_valor < 0.05)


# Rodar envfit com as variaveis amnbientais -----

envfit_result <- envfit(scores_df[, c("PCo1", "PCo2")], 
                        scores_df[, c("ambientais1", "ambientais2")], 
                        permutations = 999)
# Resultados do envfit
fit_vectors <- scores(envfit_result, "vectors")

# Multiplicar por um fator mais suave
fator <- 1.5  # Não exagera!

# Plot 
ggplot(scores_df, aes(x = PCo1, y = PCo2)) +
  geom_point(aes(color = as.factor(hc_cluster)), size = 3, alpha = 0.7) +
  theme_minimal() +
  labs(color = "Cluster") +
  
  # Vetores ajustados
  geom_segment(data = as.data.frame(fit_vectors),
               aes(x = 0, y = 0, 
                   xend = PCo1 * fator, 
                   yend = PCo2 * fator),
               arrow = arrow(length = unit(0.2, "cm")),
               color = "black") +
  
  # Rótulos ajustados
  geom_text(data = as.data.frame(fit_vectors),
            aes(x = PCo1 * (fator + 0.2), y = PCo2 * (fator + 0.2), label = rownames(fit_vectors)),
            color = "black", size = 4)




# Autovalores  e scores da PCoA -----
autovalores <- data.frame(
  Eixo = paste0("PCo", 1:length(pcoa$eig)),
  Autovalor = as.numeric(pcoa$eig),
  Variancia_Explicada = round((as.numeric(pcoa$eig) / sum(as.numeric(pcoa$eig))) * 100, 2)
)

# Coordenadas (scores das amostras nos eixos da PCoA) 
scores_pcoa <- as.data.frame(pcoa$points)
colnames(scores_pcoa) <- c("PCo1", "PCo2")
pcoa$eig

# salvando..
writexl::write_xlsx(list(
  Autovalores = autovalores,
  Scores_PCoA = scores_pcoa
), "C:/Users/resultado_pcoa.xlsx")


# testes a posteriori - hclust  -----
dev.new()

# 3. (Opcional) Fazer um hclust (clusterização hierárquica) 
# Distância e clusterização
d <- dist(scores_df[, c("PCo1", "PCo2")])
hc <- hclust(d, method = "ward.D2")

# Dendrograma
plot(hc, main = "Dendrograma - hclust (ward.D2)", xlab = "", sub = "")
# Cortar em  grupos
scores_df$hc_cluster <- cutree(hc, k = 4)
#aqui cortei em 4 grupos, mas fiz testes com 2, 3 grupos, mas não tivemos uma alta coesão

# PCoA colorido por cluster

png('C:/Users/pcoa_hcluster.png',
    width = 3000, height = 2040, res = 300 )

ggplot(scores_df, aes(x = PCo1, y = PCo2, color = as.factor(hc_cluster))) +
  geom_point(size = 2.5, alpha = 0.6) +
  theme(
    legend.position = "top",
    legend.direction = "horizontal",
    axis.text.y = element_text(angle = 90, hjust = 0.5),
    panel.background = element_rect(fill = "white"),
    panel.border = element_rect(color = "gray", fill = NA)
    
  ) +
  annotate("text", x = -Inf, y = Inf, label = "(e)", hjust = -0.2, vjust = 1.2,
           fontface = "bold", size = 8)
dev.off()



# calculo e plot de silhouette -----
hc_cluster <- cutree(hc, k = 2) # aqui você escolhe a quantidade de clusters para formar, 
 #com base no hcluster

sil <- silhouette(scores_df$hc_cluster, d)
sil <- silhouette(hc_cluster, d)

head(sil)

dev.new()  # abre nova janela gráfica, se não abrir novamente

plot(sil, main = "Gráfico de Silhueta - hclust")
mean(sil[, 2])  # Média do índice de silhueta

scores_df$hc_cluster <- as.factor(hc_cluster)

# Filtrando os pontos do cluster 2 e plotando
ggplot(scores_df[scores_df$hc_cluster == 2, ],
       aes(x = PCo1, y = PCo2, color = as.factor(hc_cluster))) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Observações do Cluster 2")



# mantem os dois clusters
png('C:/Users/pco.png',
    width = 3000, height = 3000, res = 300 )

ggplot(scores_df, aes(x = PCo1, y = PCo2)) +
  geom_point(aes(color = hc_cluster == 2), size = 3, alpha = 0.7) +
  scale_color_manual(values = c("gray80", "blue"), labels = c("Outros", "Cluster 2")) +
  theme_minimal() +
  annotate("text", x = -Inf, y = Inf, label = "(f)", hjust = -0.2, vjust = 1.2,
           fontface = "bold", size = 8)+
  labs(
    x = paste0("PCoA1 (", round(var_explicada[1], 2), "%)"),
    y = paste0("PCoA2 (", round(var_explicada[2], 2), "%)"))

dev.off()


# PERMANOVA com os dados de ambientais -----


# Ajustar a matriz de dados
d <- dist(scores_df[, c("PCo1", "PCo2")])

# PERMANOVA - Testando o efeito de ambientais1 e ambientais2
permanova_env <- adonis2(
  scores_df[, c("PCo1", "PCo2")] ~ ambientais1 + ambientais2,
  data = scores_df,
  permutations = 999,
  method = "gower" # Usando gower 
)

# Ver o resultado
print(permanova_env)

# permanova testando o h_cluster

permanova_result <- adonis2(
  dist_matrix ~ as.factor(hc_cluster),
  data = scores_df,
  permutations = 9999
)

print(permanova_result)


