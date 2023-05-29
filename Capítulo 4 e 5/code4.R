library(gapminder)
library(ggplot2)
library(socviz)
library(tidyverse)
library(dplyr)
library(ggrepel)

# Definindo tema global para todos os gráficos
theme_set(theme_minimal())

# Gráfico 1
grafico1 <- ggplot(gapminder, aes(x = year, y = gdpPercap)) +
  geom_line() +
  labs(x = "Ano", y = "PIB per Capita", title = "Variação do PIB per capita ao longo do tempo") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico1

# Gráfico 2
grafico2 <- ggplot(gapminder, aes(x = year, y = gdpPercap)) +
  geom_line(aes(group=country), color = "#0072B2") +
  labs(x = "Ano", y = "PIB per Capita", title = "Variação do PIB per capita por país") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico2

# Gráfico 3
grafico3 <- ggplot(gapminder, aes(x = year, y = gdpPercap)) +
  geom_line(aes(group=country), color = "#0072B2") +
  facet_wrap(~continent) +
  labs(x = "Ano", y = "PIB per Capita", title = "Variação do PIB per capita por continente") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico3

# Gráfico 4
grafico4 <- ggplot(gapminder, aes(x = year, y = gdpPercap)) +
  geom_line(color = "gray70", aes(group=country)) +
  geom_smooth(size=1.1, method="loess", se=FALSE, color = "#E69F00") +
  scale_y_log10(labels=scales::dollar) +
  facet_wrap(~ continent, ncol=5) +
  labs(x = "Ano", y = "PIB per Capita", title = "Variação do PIB per capita por continente") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico4

# Gráfico 5
grafico5 <- ggplot(data = gss_sm, aes(x = age, y = childs)) +
  geom_point(alpha=0.2, color = "#0072B2") +
  geom_smooth(color = "#D55E00") +
  facet_grid(sex~race) +
  labs(x = "Idade", y = "Número de Filhos", title = "Relação entre Idade e Número de Filhos") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico5

# Gráfico 6
grafico6 <- ggplot(data = gss_sm, aes(x = bigregion)) +
  geom_bar(fill = "#0072B2") +
  labs(x = "Região", y = "Contagem", title = "Distribuição por Região") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico6

# Gráfico 7
grafico7 <- ggplot(data = gss_sm, aes(x = religion, fill = religion)) +
  geom_bar() +
  guides(fill = FALSE) +
  labs(x = "Religião", y = "Contagem", title = "Distribuição por Religião") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico7

# Gráfico 8
grafico8 <- ggplot(data = gss_sm, aes(x = bigregion, fill = religion)) +
  geom_bar() +
  labs(x = "Região", y = "Contagem", title = "Distribuição por Religião e Região") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico8

# Gráfico 9
grafico9 <- ggplot(data = gss_sm, aes(x = bigregion, fill = religion)) +
  geom_bar(position = "fill") +
  labs(x = "Região", y = "Proporção", title = "Distribuição Proporcional por Religião e Região") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico9

# Gráfico 10
grafico10 <- ggplot(data = gss_sm, aes(x = bigregion, fill = religion)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = religion)) +
  labs(x = "Região", y = "Proporção", title = "Distribuição Proporcional por Religião e Região") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico10

# Definindo tema global para todos os gráficos
theme_set(theme_minimal())

# Gráfico 11
grafico11 <- ggplot(data = gss_sm, aes(x = religion, fill = bigregion)) +
  geom_bar(position = "dodge", aes(y = ..prop.., group = bigregion)) +
  facet_wrap(~bigregion, ncol = 2) +
  labs(x = "Religião", y = "Proporção", title = "Distribuição Proporcional de Religião por Região") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico11

# Gráfico 12
grafico12 <- ggplot(data = midwest, aes(x = area, fill = state)) +
  geom_histogram(bins = 10) +
  labs(x = "Área", y = "Contagem", title = "Distribuição da Área por Estado") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico12

# Gráfico 13
oh_wi <- c("OH", "WI")

grafico13 <- ggplot(data = subset(midwest, state %in% oh_wi),
                    aes(x = percollege, fill = state)) +
  geom_histogram(bins = 20, alpha = 0.4) +
  labs(x = "% com Educação Universitária", y = "Contagem", title = "Distribuição da Educação Universitária em OH e WI") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico13

# Gráfico 14
grafico14 <- ggplot(data = midwest, aes(x = area)) +
  geom_density(fill = "#0072B2", color = "white") +
  labs(x = "Área", y = "Densidade", title = "Distribuição de Densidade da Área") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico14

# Gráfico 15
grafico15 <- ggplot(data = midwest, aes(x = area, fill = state, color = state)) +
  geom_density(alpha = 0.3) +
  labs(x = "Área", y = "Densidade", title = "Distribuição de Densidade da Área por Estado") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico15

# Gráfico 16
grafico16 <- ggplot(data = subset(midwest, state %in% oh_wi), aes(x = area, fill = state, color = state)) +
  geom_density(alpha = 0.3, aes(y = ..scaled..)) +
  labs(x = "Área", y = "Densidade", title = "Distribuição de Densidade da Área Normalizada em OH e WI") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico16

# Gráfico 17
grafico17 <- ggplot(data = titanic, aes(x = fate, y = percent, fill = sex)) +
  geom_bar(position = "dodge", stat = "identity") +
  labs(x = "Destino", y = "Porcentagem", title = "Distribuição de Sobreviventes por Gênero") +
  theme(plot.title = element_text(size = 16, face = "bold"), legend.position = "top")

grafico17

# Gráfico 18
grafico18 <- ggplot(data = oecd_sum, aes(x = year, y = diff, color = hi_lo)) +
  geom_col(fill = "#0072B2") +
  guides(fill = FALSE) +
  labs(x = NULL, 
       y = "Diferença nos Anos",
       title = "Diferença na Expectativa de Vida Americana",
       subtitle = "A diferença entre a expectativa de vida dos Estados Unidos e da OECD, 1960-2015",
       caption = "Data: OECD, publicado no Washington Post, 27 de Dezembro de 2017") +
  theme(plot.title = element_text(size = 16, face = "bold"))

grafico18


## P
## PPP
## Parte 2

rel_by_region <- gss_sm %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N/sum(N),
         pct = round((freq*100), 0))

rel_by_region

rel_by_region %>% 
  group_by(bigregion) %>% 
  summarize(total = sum(pct))

rel_by_region

# Gráfico A
graficoA <- ggplot(rel_by_region, aes(x = bigregion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = "Região", y = "Percentagem", fill = "Religião") +
  theme(legend.position = "top")

graficoA

# Gráfico B
graficoB <- ggplot(rel_by_region, aes(x = religion, y = pct, fill = religion)) +
  geom_col(position = "dodge2") +
  labs(x = "Religião", y = "Percentagem", fill = "Religião") +
  guides(fill = FALSE) +
  coord_flip() +
  facet_grid(~bigregion) +
  scale_fill_brewer(palette = "Set3") +
  theme(strip.text = element_text(size = 12, face = "bold"))

graficoB

# Gráfico C
graficoC <- ggplot(organdata, aes(x = year, y = donors)) +
  geom_line(aes(group = country, color = country)) +
  facet_wrap(~country) +
  labs(x = "Ano", y = "Doadores") +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Paired")

graficoC

# Gráfico D
graficoD <- ggplot(organdata, aes(x = country, y = donors)) +
  geom_boxplot(fill = "#FF6F00") +
  coord_flip() +
  labs(x = "País", y = "Doadores") +
  theme(legend.position = "top")

graficoD

# Gráfico E
graficoE <- ggplot(organdata, aes(x = reorder(country, donors, na.rm = TRUE), y = donors, fill = world)) +
  geom_boxplot() +
  labs(x = NULL, y = "Doadores") +
  coord_flip() +
  theme(legend.position = "top") +
  scale_fill_brewer(palette = "Pastel1") +
  guides(fill = guide_legend(title = "Mundo"))

graficoE

# Gráfico F
graficoF <- ggplot(organdata, aes(x = reorder(country, donors, na.rm = TRUE), y = donors, color = world)) +
  geom_point(size = 3) +
  labs(x = NULL, y = "Doadores") +
  coord_flip() +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Set2") +
  guides(color = guide_legend(title = "Mundo"))

graficoF

by_country <- organdata %>% 
  group_by(consent_law, country) %>%
  summarize(donors_mean = mean(donors, na.rm = TRUE),
            donors_sd = sd(donors, na.rm = TRUE),
            gdp_mean = mean(health, na.rm = TRUE),
            health_mean = mean(health, na.rm = TRUE),
            roads_mean = mean(roads, na.rm = TRUE),
            cerebvas_mean = mean(cerebvas, na.rm = TRUE))

by_country

by_country <- organdata %>% group_by(consent_law, country) %>%
  summarize_if(is.numeric, funs(mean, sd), na.rm = TRUE) %>%
  ungroup()

by_country


# Gráfico G
graficoG <- ggplot(by_country, aes(x = donors_mean, y = reorder(country, donors_mean), color = consent_law)) +
  geom_point(size = 3) +
  labs(x = "Taxa de Captação de Doadores", y = NULL, color = "Lei de Consentimento") +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

graficoG

# Gráfico H
graficoH <- ggplot(by_country, aes(x = donors_mean, y = reorder(country, donors_mean))) +
  geom_point(size = 3, aes(color = consent_law)) +
  labs(x = "Doadores", y = NULL, color = "Lei de Consentimento") +
  facet_wrap(~consent_law, scales = "free_y", ncol = 1) +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Dark2")

graficoH

# Gráfico I
graficoI <- ggplot(by_country, aes(x = reorder(country, donors_mean), y = donors_mean)) +
  geom_pointrange(aes(ymin = donors_mean - donors_sd, ymax = donors_mean + donors_sd)) +
  labs(x = NULL, y = "Doadores") +
  coord_flip() +
  theme(legend.position = "top")

graficoI


elections_historic %>% select(2:7)

p_title <- "Eleições Presidenciais: Colégio Eleitoral Popular e Marginal"
p_subtitle <- "1824-2016"
p_caption <- "Dados de 2016 são provisórios."
x_label <- "Porcentagem de Voto Popular do Vencedor"
y_label <- "Porcentagem de Votos de Colégios Eleitorais do Vencedor"


# Gráfico J
graficoJ <- ggplot(by_country, aes(x = roads_mean, y = donors_mean)) +
  geom_point(aes(color = consent_law), size = 3) +
  geom_text(aes(label = country), hjust = 0, nudge_y = 1) +
  labs(x = "Estradas", y = "Doadores") +
  theme(legend.position = "top") +
  scale_color_brewer(palette = "Paired")

graficoJ

# Gráfico K
graficoK <- ggplot(elections_historic, aes(x = popular_pct, y = ec_pct, label = winner_label)) +
  geom_hline(yintercept = 0.5, size = 1.4, color = "gray80") +
  geom_vline(xintercept = 0.5, size = 1.4, color = "gray80") +
  geom_point(size = 3, color = "#E41A1C") +
  geom_text_repel() +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Porcentagem de Votos Populares do Vencedor", y = "Porcentagem de Votos dos Colégios Eleitorais do Vencedor") +
  theme(plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 12, face = "italic"),
        plot.caption = element_text(size = 10),
        legend.position = "top",
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())

graficoK

# Gráfico L
graficoL <- ggplot(by_country, aes(x = gdp_mean, y = health_mean)) +
  geom_point(size = 3, color = "#377EB8") +
  geom_text_repel(data = subset(by_country, gdp_mean > 25000), aes(label = country), color = "#377EB8") +
  labs(x = "Média de PIB", y = "Média de Saúde") +
  theme(legend.position = "top")

graficoL

# Gráfico M
graficoM <- ggplot(by_country, aes(x = gdp_mean, y = health_mean)) +
  geom_point(size = 3, color = "#984EA3") +
  geom_text_repel(data = subset(by_country, gdp_mean > 25000 | health_mean < 1500 | country %in% "Belgium"),
                  aes(label = country), color = "#984EA3") +
  labs(x = "Média de PIB", y = "Média de Saúde") +
  theme(legend.position = "top")

graficoM

# Gráfico N
organdata$ind <- organdata$ccode %in% c("Ita", "Spa") & organdata$year > 1998

graficoN <- ggplot(organdata, aes(x = roads, y = donors, color = ind)) +
  geom_point(size = 3) +
  geom_text_repel(data = subset(organdata, ind), aes(label = ccode)) +
  guides(label = FALSE, color = FALSE) +
  labs(x = "Estradas", y = "Doadores") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("#D55E00", "#0072B2"))

graficoN

# Gráfico O
graficoO <- ggplot(organdata, aes(x = roads, y = donors)) +
  geom_point(size = 3) +
  annotate(geom = "text", x = 91, y = 33, label = "Um número surpreendente\nde recuperações",
           hjust = 0, color = "#56B4E9", size = 6) +
  labs(x = "Estradas", y = "Doadores") +
  theme(legend.position = "top")

graficoO

# Gráfico P
graficoP <- ggplot(organdata, aes(x = roads, y = donors)) +
  geom_point() +
  annotate(geom = "rect", xmin = 125, xmax = 155, ymin = 30, ymax = 35, fill = "red", alpha = 0.2) +
  annotate(geom = "text", x = 157, y = 33, label = "Um número surpreendente\nde recuperações", hjust = 0) +
  labs(x = "Estradas", y = "Doadores") +
  theme(legend.position = "top")

graficoP

# Gráfico Q
graficoQ <- ggplot(organdata, aes(x = roads, y = donors, color = world)) +
  geom_point() +
  labs(x = "Estradas", y = "Doadores", color = "Mundo") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("blue", "red", "black"))

graficoQ

# Gráfico R
graficoR <- ggplot(organdata, aes(x = roads, y = donors, color = world)) +
  geom_point() +
  scale_x_log10() +
  scale_x_continuous(breaks = c(5, 15, 25), labels = c("Cinco", "Quinze", "Vinte e Cinco")) +
  labs(x = "Mortes na Estrada (log)", y = "Pedido por Sangue", color = "Mundo") +
  theme(legend.position = "top") +
  scale_color_manual(values = c("blue", "red", "black"))

graficoR

# Gráfico S
graficoS <- ggplot(organdata, aes(x = roads, y = donors, color = world)) +
  geom_point() +
  labs(x = "Mortes na Estrada", y = "Pedido por Sangue") +
  theme(legend.position = "top") +
  guides(color = FALSE)

graficoS