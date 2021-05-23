# Análise de Confiabilidade de uma Rede de Distribuição de Água

## Clear all ----

# Clear plots
if(!is.null(dev.list())) dev.off()
# Clear console
cat("\014") 
# Clean workspace
rm(list=ls())


# Bibliotecas----

if(!require("readxl")) install.packages("readxl") ; library(readxl)
if(!require("dplyr")) install.packages("dplyr") ; library(dplyr)
if(!require("survminer")) install.packages("survminer") ; library(survminer)    # Usar funcao surv_fit
if(!require("survival")) install.packages("survival") ; library(survival)       # Usar a funcao surv
if(!require("ggthemes")) install.packages("ggthemes") ; library(ggthemes)       # Usar temas nos graficos
if(!require("ggplot2")) install.packages("ggplot2") ; library(ggplot2) 
if(!require("reshape2")) install.packages("reshape2") ; library(reshape2) 
if(!require("gridExtra")) install.packages("gridExtra") ; library(gridExtra)

# Importando Banco de Dados----

setwd("C:/Users/lukas/Desktop/compilado/IC_ReliabilityAnalysis")   # Apontando para pasta de trabalho

dados_manutencao <- read_excel("data/MANUTENCAO_REDE.xlsx")      # Importando os dados

dados_manutencao <- dados_manutencao %>% 
  filter(ANO != 2005 & ANO != 2006 & ANO != 2007)

# Filtrando Banco de Dados----

# Extraindo as datas
# Tirando os acentos e Agrupando as solicitacoes
# Filtrando os bairros da z42
# Filtrando o evento topo do estudo (Vazamento de agua)
# Selecionando variaveis de interesse

dados_psam <- dados_manutencao %>% 
  mutate(MES = substr(DATA_SOLICITACAO, 6, 7), 
         DIA = substr(DATA_SOLICITACAO, 9, 10),
         HORA = substr(HORA_SOLICITACAO, 12, 19)) %>% 
  select(ANO, MES, DIA, HORA, BAIRRO, DISCRIMINACAO) %>% 
  mutate(MINUTO = substr(HORA, 4, 5), HORA = substr(HORA, 1, 2), 
         BAIRRO = ifelse(BAIRRO == "LUCAS10" | 
                           BAIRRO == "LUCAS16" | 
                           BAIRRO == "LUCAS5" | 
                           BAIRRO == "LUCASx" | 
                           is.na(BAIRRO) == T, 
                         "SEM_INFO",
                         ifelse(BAIRRO == "PATAMARES" | 
                                  BAIRRO == "JAGUARIBE", 
                                "PATAMARES",
                                ifelse(BAIRRO == "IMBUÍ", 
                                       "IMBUI",
                                       ifelse(BAIRRO == "PIATÃ", 
                                              "PIATA",
                                              ifelse(BAIRRO == "PITUAÇU", 
                                                     "PITUACU", BAIRRO)))))) %>% 
  filter(BAIRRO %in% c("SEM_INFO", "BOCA DO RIO", 
                       "PATAMARES", "IMBUI", "PIATA", "PITUACU")) %>% 
  filter(grepl("CAPEAMENTO EM VAZ", DISCRIMINACAO) == T |
           grepl("CONSERTO", DISCRIMINACAO) == T |
           grepl("Conserto", DISCRIMINACAO) == T |
           grepl("CORREÇÃO", DISCRIMINACAO) == T |
           grepl("ESCAV", DISCRIMINACAO) == T |
           grepl("FURO", DISCRIMINACAO) == T |
           grepl("Ponto de pressão", DISCRIMINACAO) == T |
           grepl("Outros", DISCRIMINACAO) == T |
           grepl("OUTROS", DISCRIMINACAO) == T |
           grepl("REATERRO /ESC", DISCRIMINACAO) == T |
           grepl("VERIFICAÇÃO DE VAZ", DISCRIMINACAO) == T) %>% 
  select(ANO, MES, DIA, MINUTO, HORA, BAIRRO, DISCRIMINACAO) %>% 
  mutate(MODOS = ifelse(grepl("RAMAL", DISCRIMINACAO) == T, "RAMAL", 
                        ifelse(grepl("REDE", DISCRIMINACAO) == T, "REDE", 
                               ifelse(grepl("HIDRO", DISCRIMINACAO) == T, "HIDRO", "OUTROS"))))

# Inserindo Censura Intervalar no Banco de Dados----

# Criando objetos para identificar mudanca de mes e ano bissexto
# Criando variavel com unidade de tempo em dias (tempo.R)
# Criando variavel admitindo tzero o tempo inicial do estudo
# Criando varivael tempo.L

meses = as.numeric(c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334))
meses_b = as.numeric(c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335))
bissexto = as.numeric(c(2008, 2012))

dados_psam <- dados_psam %>% 
  mutate(t.R = as.numeric(MINUTO)*(1/1440) + as.numeric(HORA)*(1/24) + as.numeric(DIA) + 
           (ifelse(ANO == bissexto[1] | ANO == bissexto[2], meses_b[as.numeric(MES)], meses[as.numeric(MES)])) + 
           as.numeric(ANO)*365) %>%
  arrange(ANO, MES, DIA, HORA, MINUTO) %>%
  mutate(t.R = t.R - t.R[1]) %>% 
  mutate(t.L = ifelse(HORA >= 12, t.R, -Inf)) %>% 
  mutate(Intervalar = ifelse(HORA >= 12, 0, 1))

# Criando intervalos

dados_psam$t.R[1] = 0.0000001

for (i in c(2:length(dados_psam$t.L))) {
  dados_psam$t.L[i] <- ifelse(dados_psam$t.L[i] == -Inf, 
                              ifelse(dados_psam$DIA[i] == dados_psam$DIA[i-1], 
                                     dados_psam$t.L[i-1], 
                                     dados_psam$t.R[i-1]), 
                              dados_psam$t.L[i])
}

dados_psam <- dados_psam %>% 
  mutate(t.R = c(t.R[2:length(t.R)], t.R[length(t.R)]), 
         t.L = c(t.L[2:length(t.L)], t.L[length(t.L)]))

# Criando intervalo dos tempos entre falhas

dados_psam <- dados_psam %>% 
  mutate(t.failure.L = (c(t.R[1], diff(t.R)))) %>% 
  mutate(t.failure.R = (t.failure.L + (t.R - t.L))) %>% 
  select(t.failure.L, t.failure.R, BAIRRO, DISCRIMINACAO, MODOS)

dados_psam$t.failure.R[1] <- dados_psam$t.failure.L[1]

# Separando modos de falha----

d_ramal <- dados_psam %>% 
  filter(MODOS == "RAMAL")
d_hidro <- dados_psam %>% 
  filter(MODOS == "HIDRO")
d_rede <- dados_psam %>% 
  filter(MODOS == "REDE")
d_outros <- dados_psam %>% 
  filter(MODOS == "OUTROS")

# RAMAL - Nao-Parametrica----

# Criando objeto utilizando o metodo Turnbull (Intervalar)

ramal.np <- survfit(Surv(time = t.failure.L, time2 = t.failure.R, type="interval2")~1, data=d_ramal)

resumo.ramal.np <- surv_summary(ramal.np)

# Dando um zoom em xlim = 0 ate 2

ramal.np$n.event <- round(ramal.np$n.event)

gg.zoomramal.np <- ggsurvplot(fit = ramal.np, 
                                conf.int = F, 
                                surv.median.line = "none", 
                                risk.table = "percentage", 
                                cumevents = T, 
                                tables.height = 0.25, 
                                ggtheme = theme_light(), 
                                tables.theme = theme_light(), 
                                title = "Non-Parametric Survival Curve", 
                                xlab = "Time in days",
                                legend.title = "Individuals",
                                #legend.labs = c("Boca do Rio", "Imbui", "Patamares"),
                                break.time.by = 0.2,
                                surv.scale = "percent",
                                xlim = c(0,1.2),
                                conf.int.style = "step",
                                risk.table.y.text = F,
                                cumevents.y.text = F)

gg.zoomramal.np$plot <- gg.zoomramal.np$plot + 
  labs(subtitle = "Based on Turnbull estimates")

gg.zoomramal.np <- ggpar(p = gg.zoomramal.np,
                           font.title    = c(15, "plain"),         
                           font.subtitle = c(14),        
                           font.x        = c(13),          
                           font.y        = c(13),      
                           font.xtickslab = c(12))

gg.zoomramal.np

g2 = 1
if(g2 == 0){
  ggsave(filename = "gg.zoomramal.np.png", print(gg.zoomramal.np),                                                    # Salvando grafico
         width = 13, height = 14, dpi = 600)
}

# RAMAL - Modelagem parametrica----

# Arrumando banco de dados a funcao survreg nao aceita zeros
d_ramal <- d_ramal %>% 
  mutate(t.failure.L = ifelse(t.failure.L == 0, 0.000000001, t.failure.L),
         t.failure.R = ifelse(t.failure.R == 0, 0.000000001, t.failure.R))

# Criando objeto de sobrevivencia e o respectivo tempo

ramal.mp.surv <- Surv(d_ramal$t.failure.L, d_ramal$t.failure.R, type="interval2")
ramal.mp.t <- ramal.np$time

# RAMAL - Weibull model----

ramal.mp.W <- survreg(ramal.mp.surv ~ 1, dist="weibull")

ramal.mp.a1.W <- as.numeric(exp(ramal.mp.W$coefficients))
ramal.mp.y1.W <- 1/(ramal.mp.W$scale)

ramal.mp.s.W <- exp(-((ramal.mp.t/ramal.mp.a1.W)^ramal.mp.y1.W))

ramal.mp <- data.frame(ramal.mp.t, ramal.mp.s.W)

# RAMAL - Exponential model----

ramal.mp.E <- survreg(ramal.mp.surv ~ 1, dist="exponential")

ramal.mp.a1.E <- as.numeric(exp(ramal.mp.E$coefficients))

ramal.mp.s.E <- exp(-(ramal.mp.t/ramal.mp.a1.E))

ramal.mp <- cbind(ramal.mp, ramal.mp.s.E)

# RAMAL - Gaussian model----

ramal.mp.G <- survreg(ramal.mp.surv ~ 1, dist="gaussian")

ramal.mp.u1.G <- as.numeric(ramal.mp.G$coefficients)
ramal.mp.o1.G <- ramal.mp.G$scale

ramal.mp.s.G <- pnorm(ramal.mp.t, mean = ramal.mp.u1.G, sd = ramal.mp.o1.G, lower.tail = FALSE)

ramal.mp <- cbind(ramal.mp, ramal.mp.s.G)

# RAMAL - Logistic model----

ramal.mp.LG <- survreg(ramal.mp.surv ~ 1, dist="logistic")

ramal.mp.u1.LG <- as.numeric(ramal.mp.LG$coefficients)
ramal.mp.o1.LG <- ramal.mp.LG$scale

ramal.mp.s.LG <- 1/(1 + (exp((ramal.mp.t - ramal.mp.u1.LG)/ramal.mp.o1.LG)))

ramal.mp <- cbind(ramal.mp, ramal.mp.s.LG)

# RAMAL - Log-normal model----

ramal.mp.LN <- survreg(ramal.mp.surv ~ 1, dist="lognormal")

ramal.mp.u1.LN <- as.numeric(ramal.mp.LN$coefficients)
ramal.mp.o1.LN <- ramal.mp.LN$scale

ramal.mp.s.LN <- plnorm(ramal.mp.t, meanlog = ramal.mp.u1.LN, sdlog = ramal.mp.o1.LN, lower.tail=FALSE)

ramal.mp <- cbind(ramal.mp, ramal.mp.s.LN)

# RAMAL - Log-logistic model----

ramal.mp.LL <- survreg(ramal.mp.surv ~ 1, dist="loglogistic")

ramal.mp.a1.LL <- as.numeric(exp(ramal.mp.LL$coefficients))
ramal.mp.y1.LL <- 1/(ramal.mp.LL$scale)

ramal.mp.s.LL <- 1/(1 + ((ramal.mp.t/ramal.mp.a1.LL)^ramal.mp.y1.LL))

ramal.mp <- cbind(ramal.mp, ramal.mp.s.LL)

# RAMAL - Escolha do modelo (Metodo Grafico)----

ramal.tot <- cbind(ramal.mp, resumo.ramal.np$surv)

colnames(ramal.tot) <- c("Time", "Weibull", "Exponential", "Gaussian",
                         "Logistic", "Log_normal", "Log_logistic", "Non_parametric")

ramal.tot.melt <- ramal.tot %>% 
  select(-Time, -Non_parametric) %>% 
  melt() %>% 
  mutate(t = c(ramal.tot$Time, ramal.tot$Time, ramal.tot$Time, ramal.tot$Time,
               ramal.tot$Time, ramal.tot$Time),
         non_parametric = c(ramal.tot$Non_parametric, ramal.tot$Non_parametric, 
                            ramal.tot$Non_parametric, ramal.tot$Non_parametric,
                            ramal.tot$Non_parametric, ramal.tot$Non_parametric))

gg.ramal.tot.sxs <- ggplot(ramal.tot.melt, aes(x = non_parametric)) +
  geom_line(aes(y = non_parametric), size = 1.5) +
  geom_point(aes(y = value, color = variable), alpha = 0.85) +
  theme_light() +
  labs(title = "Survival Function - Empirical vs theoretical",
       x = "S(t) Non-parametric",
       y = "S(t) Models parametrics",
       subtitle = "Graphical Check") +
  scale_color_manual(name="Distribution", 
                     labels = c("Weibull", 
                                "Exponential", 
                                "Gaussian", 
                                "Logistic", 
                                "Log-normal",
                                "Log-logistic"),
                     values = c("red2","midnightblue","chartreuse2",
                                "deeppink","gold","deepskyblue")) + 
  theme(legend.position="top")

gg.ramal.tot.sxs <- ggpar(gg.ramal.tot.sxs, 
                          font.title = c(26, "bold"), 
                          font.subtitle = c(25), 
                          font.legend = c(23),
                          font.x = c(25), 
                          font.y = c(25), 
                          font.tickslab = c(21, "bold"))


gg.ramal.tot.sxs      # grafico S(t)nonparametric vs S(t)parametric

gg.ramal.tot.sxs.f <- ggplot(ramal.tot.melt, aes(x = non_parametric)) +
  geom_line(aes(y = non_parametric), size = 1.5) +
  geom_point(aes(y = value, color = variable), size = 1, alpha = 1) +
  facet_wrap(facets = ~variable, nrow = 2)+
  theme_light() +
  labs(title = "Survival Function - Empirical vs theoretical",
       x = "S(t) Non-parametric",
       y = "S(t) Models parametrics",
       subtitle = "Graphical Check") +
  scale_color_manual(name="Distribution", 
                     labels = c("Weibull", 
                                "Exponential", 
                                "Gaussian", 
                                "Logistic", 
                                "Log-normal",
                                "Log-logistic"),
                     values = c("red2","midnightblue","chartreuse2",
                                "deeppink","gold","deepskyblue"))

gg.ramal.tot.sxs.f <- ggpar(gg.ramal.tot.sxs.f, font.title = 14, 
                            font.subtitle = 13, font.legend = 13,
                            font.x = 13, font.y = 13, font.tickslab = 13)

gg.ramal.tot.sxs.f      # grafico S(t)nonparametric vs S(t)parametric

gg.ramal.tot.cdfxt <- ggplot(ramal.tot.melt, aes(x = t)) +
  geom_line(aes(y = 1 - non_parametric), size = 2, alpha = 1) +
  geom_line(aes(y = 1 - value, color = variable), size = 1, alpha = 1) +
  theme_light() +
  labs(title = "Cumulative Distribution Function vs Time",
       x = "Time in days",
       y = "CDF(t)",
       subtitle = "Graphical Check") +
  scale_color_manual(name="Distribution", 
                     labels = c("Weibull", 
                                "Exponential", 
                                "Gaussian", 
                                "Logistic", 
                                "Log-normal",
                                "Log-logistic"),
                     values = c("red2","midnightblue","chartreuse2",
                                "deeppink","gold","deepskyblue"))

gg.ramal.tot.cdfxt <- ggpar(gg.ramal.tot.cdfxt, font.title = 14, 
                            font.subtitle = 13, font.legend = 13,
                            font.x = 13, font.y = 13, font.tickslab = 13)

gg.ramal.tot.cdfxt

gg.ramal.tot.cdfxt.f <- ggplot(ramal.tot.melt, aes(x = t)) +
  geom_line(aes(y = 1 - non_parametric), size = 2, alpha = 0.85) +
  geom_line(aes(y = 1 - value, color = variable), size = 1.25, alpha = 1) +
  theme_light() +
  labs(title = "Cumulative Distribution Function vs Time",
       x = "Time in days",
       y = "CDF(t)",
       subtitle = "Graphical Check") +
  scale_color_manual(name="Distribution", 
                     labels = c("Weibull", 
                                "Exponential", 
                                "Gaussian", 
                                "Logistic", 
                                "Log-normal",
                                "Log-logistic"),
                     values = c("red2","blue","chartreuse2",
                                "deeppink","gold","deepskyblue")) +
  facet_wrap(facets = ~variable, nrow = 2) + 
  theme(legend.position="top")

gg.ramal.tot.cdfxt.f <- ggpar(gg.ramal.tot.cdfxt.f, 
                              font.title = c(26, "bold"), 
                              font.subtitle = c(25), 
                              font.legend = c(23),
                              font.x = c(25), 
                              font.y = c(25), 
                              font.tickslab = c(21, "bold"))

gg.ramal.tot.cdfxt.f

g3 = 1
if(g3 == 0){
  ggsave(filename = "gg.ramal.tot.sxs.png", gg.ramal.tot.sxs,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
  ggsave(filename = "gg.ramal.tot.sxs.f.png", gg.ramal.tot.sxs.f,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
  ggsave(filename = "gg.ramal.tot.cdfxt.png", gg.ramal.tot.cdfxt,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
  ggsave(filename = "gg.ramal.tot.cdfxt.f.png", gg.ramal.tot.cdfxt.f,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
}

# RAMAL - Parametros do modelo----

ramal.mp.parametros <- data.frame( c("Alpha", "Gama"),
                                   c(ramal.mp.a1.W, ramal.mp.y1.W),
                                   c(exp(ramal.mp.o1.LN), (1/ramal.mp.u1.LN)), 
                                   c(ramal.mp.a1.LL, ramal.mp.y1.LL))
colnames(ramal.mp.parametros) <- c("Parameters and Test", "Weibull", "Log-normal", "Log-logistic")

# RAMAL - Testes de bondade do modelo----

# Teste de AIC 

ramal.mp.parametros <- rbind(ramal.mp.parametros, c(NA,
                                                    extractAIC(ramal.mp.W)[2],
                                                    extractAIC(ramal.mp.LN)[2],
                                                    extractAIC(ramal.mp.LL)[2]))

ramal.mp.parametros <- ramal.mp.parametros %>% 
  mutate(`Parameters and Test` = c("Alpha", "Gama", "AIC"))

# # Anderson Darling
# require(goftest)
# ad.test(1- ramal.mp.s.W, "pweibull")
# 
# # Wald
# require(aods3)
# wald.test(b = coef(ramal.mp.E), varb = vcov(ramal.mp.E), Terms = 1)
# 
# # TTT
# require(AdequacyModel)
# TTT(dados_psam$t.failure.R, col = "red", lwd = 2.5, grid = TRUE, lty = 2)



# RAMAL - Funcao taxa de falha----

ramal.mp.t[1] <- 0.000000000

ramal.mp.h.W <- data.frame(ramal.mp.t, (ramal.mp.y1.W/(ramal.mp.a1.W^ramal.mp.y1.W))*((ramal.mp.t)^(ramal.mp.y1.W - 1)))

colnames(ramal.mp.h.W) <- c("Time", "Hazard")

ramal.mp.acch.W <- data.frame(ramal.mp.t, (ramal.mp.t/ramal.mp.a1.W)^ramal.mp.y1.W)

colnames(ramal.mp.acch.W) <- c("Time", "Hazard")

gg.ramal.h.W <- ggplot(ramal.mp.h.W, aes(x = Time)) +
  geom_line(aes(y = Hazard), color = "red2", size = 1.75) +
  theme_light() +
  labs(title = "Hazard Function vs Time",
       x = "Time in days",
       y = "Hazard (t)",
       subtitle = "Weibull Model (\u{3B1} = 0.4427051, γ = 0.7025420)") +
  scale_x_continuous(breaks = seq(0, 43, by = 2)) +
  scale_y_continuous(breaks = seq(0, 7, by = 1), limits = c(0,NA))

gg.ramal.h.W <- ggpar(gg.ramal.h.W, 
                      font.title = c(21, "bold"),
                      font.subtitle = c(21),
                      font.legend = c(23),
                      font.x = c(25), 
                      font.y = c(25), 
                      font.tickslab = c(21, "bold"))

gg.ramal.h.W

gg.ramal.acch.W <- ggplot(ramal.mp.acch.W, aes(x = Time)) +
  geom_line(aes(y = Hazard), color = "red2", size = 2) +
  theme_light() +
  labs(title = "Cumulative Hazard Function vs Time",
       x = "Time in days",
       y = "Cumulative Hazard (t)",
       subtitle = "Weibull Model")

gg.ramal.acch.W <- ggpar(gg.ramal.acch.W, font.title = 14,
                         font.subtitle = 13,font.x = 13, 
                         font.y = 13, font.tickslab = 13)

gg.ramal.acch.W

g4 = 1
if(g4 == 0){
  ggsave(filename = "gg.ramal.h.W.png", gg.ramal.h.W,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
  ggsave(filename = "gg.ramal.acch.W.png", gg.ramal.acch.W,                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
}


# RAMAL - Resultados finais----

# Gerando tabela de parametros

ramal.mp.parametros <- ramal.mp.parametros %>% 
  mutate(Statistics = c("Scale", "Shape", "AIC")) %>% 
  select(Statistics, Weibull)

# Probability density function

ramal.mp.d.W <- data.frame(ramal.mp.t, ramal.mp.s.W*ramal.mp.h.W$Hazard)
colnames(ramal.mp.d.W) <- c("Time", "Density")

gg.ramal.d.W <- ggplot(ramal.mp.d.W, aes(x = Time)) +
  geom_line(aes(y = Density), color = "red2", size = 2) +
  theme_light() +
  labs(title = "Probability Density Function vs Time",
       x = "Time in days",
       y = "Density (t)",
       subtitle = "Weibull Model") + 
  annotation_custom(tableGrob(ramal.mp.parametros, theme=ttheme_minimal()), 
                    xmin=35, xmax=45, ymin=5, ymax=7)

gg.ramal.d.W <- ggpar(gg.ramal.d.W, font.title = 14,
                      font.subtitle = 13,font.x = 13, 
                      font.y = 13, font.tickslab = 13)

gg.ramal.d.W

# Survival function

gg.ramal.s.W <- ggplot(ramal.tot, aes(x = Time)) +
  geom_line(aes(y = Weibull), color = "red2", size = 1.75) +
  theme_light() +
  labs(title = "Survival Function vs Time",
       x = "Time in days",
       y = "Survival (t)",
       subtitle = "Weibull Model (\u{3B1} = 0.4427051, γ = 0.7025420)") +
  scale_x_continuous(breaks = seq(0, 43, by = 2), limits = c(0,NA))



gg.ramal.s.W <- ggpar(gg.ramal.s.W, 
                      font.title = c(21, "bold"),
                      font.subtitle = c(21),
                      font.legend = c(23),
                      font.x = c(25), 
                      font.y = c(25), 
                      font.tickslab = c(21, "bold"))

gg.ramal.s.W

# Hazard and Acumulation Hazard

gg.ramal.h.W <- gg.ramal.h.W + 
  annotation_custom(tableGrob(ramal.mp.parametros, theme=ttheme_minimal()), 
                    xmin=35, xmax=45, ymin=5, ymax=7)

gg.ramal.acch.W <- gg.ramal.acch.W + 
  annotation_custom(tableGrob(ramal.mp.parametros, theme=ttheme_minimal()), 
                    xmin=35, xmax=45, ymin=0.5, ymax=5)

# Colocando os graficos em uma unica imagem

gg.resultados.ramal.2 <- grid.arrange(arrangeGrob(gg.ramal.s.W, gg.ramal.acch.W, ncol=1, nrow = 2))

gg.resultados.ramal.2final <- grid.arrange(arrangeGrob(gg.ramal.s.W, gg.ramal.h.W, ncol=1, nrow = 2))

gg.resultados.ramal.4 <- grid.arrange(arrangeGrob(gg.ramal.d.W, gg.ramal.s.W, 
                                                  gg.ramal.h.W, gg.ramal.acch.W, 
                                                  ncol=2, nrow = 2))

g5 = 1
if(g5 == 0){
  ggsave(filename = "gg.ramal.s.W.png", 
         (gg.ramal.s.W),                                                    # Salvando grafico
         width = 13, height = 7, dpi = 600)
  ggsave(filename = "gg.resultados.ramal.2final.png", 
         (gg.resultados.ramal.2final),                                                    # Salvando grafico
         width = 13, height = 14, dpi = 600)
  ggsave(filename = "gg.resultados.ramal.4.png", 
         (gg.resultados.ramal.4),                                                    # Salvando grafico
         width = 26, height = 14, dpi = 600)
}


