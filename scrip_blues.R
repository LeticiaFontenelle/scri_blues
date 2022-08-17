
                        #-----------------------------------#
                        # Introdução (tratamento de dados)  #
                        #    Análise-> ceteris paribus      #
                        #-----------------------------------#

##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################
#Pacotes utilizados
pacotes <- c("plotly","tidyverse","ggrepel","fastDummies","knitr","kableExtra",
             "splines","reshape2","PerformanceAnalytics","metan","correlation",
             "see","ggraph","nortest","rgl","car","olsrr","jtools","ggstance",
             "magick","cowplot","beepr","Rcpp")

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}



# Convertendo a planilha -> dataframe
install.packages("readxl")
install.packages("scatter3d")
install.packages("car")


#Carregar  


#Gráficos
library(tidyverse)
library(plotly)
library(ggrepel)
library(PerformanceAnalytics)
library(reshape2)
library(car)
library(readxl)
library(scatter3d)



#Lendo arquivos
dados = read_excel(file.choose())
dados

#Criado o objeto dados
load(file = "dados")

#Estatísticas univariadas
summary(dados)


#Gráficos diversos -> sintaxe básica do ggplot2
ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao ))


ggplot(dados) +
  geom_point(aes(x = visitantes, y = finalsemana))


ggplot(dados) +
  geom_point(aes(x = ferias , y = arrecadacao))



ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = arrecadacao))



ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = finalsemana))


#comporta
ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = finalsemana, color = visitantes > 1000))



ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = ferias, color = visitantes > 1000))


ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = feriado, color = visitantes > 1000))



ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = finalsemana, color =  arrecadacao > 5000))



ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = ferias, color =  arrecadacao > 5000))



ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, size = feriado, color =  arrecadacao > 5000))



ggplot(dados) +
  geom_point(aes(x = finalsemana, y = arrecadacao, size = arrecadacao))


ggplot(dados) +
  geom_point(aes(x = feriado, y = arrecadacao, size = arrecadacao))


ggplot(dados) +
  geom_point(aes(x = ferias, y = arrecadacao, size = arrecadacao))


ggplot(dados) +
  geom_point(aes(x = finalsemana, y = arrecadacao, size = arrecadacao, color = arrecadacao > 5000))


ggplot(dados) +
  geom_point(aes(x = feriado, y = arrecadacao, size = arrecadacao, color = arrecadacao > 5000))


ggplot(dados) +
  geom_point(aes(x = ferias, y = arrecadacao, size = arrecadacao, color = arrecadacao > 5000))



#Estratificando 
ggplot(dados) +
  geom_point(aes(x = finalsemana, y = arrecadacao, 
                 size = visitantes, color = visitantes > 1000, 
                 shape = arrecadacao > 5000)) +
  labs(title = "Arrecadação por dia",
       x = "finalsemana",
       y = "arrecadacao") +
  theme_bw()


#Observação: Atenção no comportamento da magnitudade e das taxas (desenvolver visão econômica). 

#Traçando a linha de tendências:
ggplot(dados) +
  geom_point(aes(x = finalsemana, y = arrecadacao, 
                 size = visitantes, color = visitantes > 1000, 
                 shape = arrecadacao > 5000)) +
  geom_smooth(aes(x = finalsemana, y = arrecadacao), 
              method = "loess", se = FALSE) +
  labs(title = "Arrecadação por dia",
       x = "finalsemana",
       y = "arrecadacao") +
  theme_bw()



#Traçando a linha de tendências:
ggplot(dados) +
  geom_point(aes(x = visitantes, y = arrecadacao, 
                 size = visitantes, color = visitantes > 1000, 
                 shape = arrecadacao > 5000)) +
  geom_smooth(aes(x = visitantes, y = arrecadacao), 
              method = "loess", se = FALSE) +
  labs(title = "Arrecadação da bilheteria",
       x = "visitantes",
       y = "arrecadacao") +
  theme_bw()





##################################################################################
#                               CORRELAÇÕES                                      #
##################################################################################

#Estatisticas univariadas
summary(dados)


#Visualizando a base de dados
dados %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 20)


dados %>%
  correlation(method = "pearson") %>%
  plot()


chart.Correlation((dados[2:6]), histogram = TRUE)



##################################################################################
#     ESTIMAÇÃO DO MODELO MÚLTIPLA COM AS VARIÁVEIS DA BASE DE DADOS             #
##################################################################################

#Regressao Múltipla
modelo_dados <- lm(formula = arrecadacao ~ . - semana,
                   data = dados)

#Parâmetros do modelo
summary(modelo_dados)
confint(modelo_dados, level = 0.95) # siginificância de 5% (0,05)


#Apresentação dos outputs do modelo - pacote jtools
summ(modelo_dados, confint = T, digits = 3, ci.width = .95)
export_summs(modelo_dados, scale = F, digits = 5)


#Salvando os fitted values na base de dados
dados$arrecadacaofit <- modelo_dados$fitted.values


#Gráfico 3D com scatter e fitted values
scatter3d(arrecadacao ~ visitantes + finalsemana + feriado + ferias,
          data = dados,
          surface = T, fit = "linear",
          point.col = "#440154FF",
          axis.col = rep(x = "black",
                         times = 3))



##################################################################################
#              REGRESSÃO COM VARIÁVEL EXPLICATIVA (X) QUALITATIVA                #
#                               CARREGAMENTO DA BASE DE DADOS                    #
##################################################################################


#################################################################################
#                            PROCEDIMENTO N-1 DUMMIES                           #
#################################################################################
#Dummizando 

dados_dummies <- dummy_columns(.data = dados,
                               select_columns = "finalsemana",
                               remove_selected_columns = T,
                               remove_most_frequent_dummy = T)

#Visualizando a base de dados dummizada
dados_dummies %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)



##################################################################################
#                        ESTIMAÇÃO DO MODELO DE REGRESSÃO                        #
##################################################################################
#Modelagem com todas as variáveis
modelo_dados_dummies <- lm(arrecadacao ~ . - semana, dados_dummies)

#Parâmetros do modelo_dados_dummies
summary(modelo_dados_dummies)

#Plotando o modelo_dados_dummies de forma interpolada
my_plot3 <- 
  dados %>%
  mutate(rotulo = paste(semana, arrecadacao)) %>%
  ggplot(aes(x = as.numeric(visitantes), y = arrecadacao, label = rotulo)) +
  geom_point(color = "#FDE725FF") +
  stat_smooth(aes(color = "Fitted Values"),
              method = "lm", 
              formula = y ~ bs(x, df = 4)) +
  labs(x = "visitantes",
       y = "areecadacao") +
  scale_x_discrete(labels = c("1" = "sim", 
                              "2" = "não"
  )) +
  scale_color_manual("Legenda:",
                     values = "#440154FF") +
  geom_text_repel() +
  theme_bw()
my_plot3




##################################################################################
#             REGRESÃO NÃO LINEAR SIMPLES E TRANSFORMAÇÃO DE BOX-COX             #
#                                CARREGAMENTO DA BASE DE DADOS                   #
##################################################################################
#Efetuar o procedimento inicial - Carregar

#Gráfico de dispersão
ggplotly(
  dados %>% 
    ggplot() +
    geom_point(aes(x = visitantes, y = arrecadacao),
               color = "grey20", alpha = 0.6, size = 2) +
    labs(x = "visitantes por dia",
         y = "valores de arrecadação diária") +
    theme_bw()
)

#Gráfico de dispersão com ajustes (fits) linear e não linear
ggplotly(
  dados %>% 
    ggplot() +
    geom_point(aes(x = visitantes, y = arrecadacao),
               color = "grey20", alpha = 0.6, size = 2) +
    geom_smooth(aes(x = visitantes, y =  arrecadacao),
                method = "lm", color = "#FDE725FF", se = F) +
    geom_smooth(aes(x = visitantes, y = arrecadacao),
                color = "#440154FF", se = F) +
    labs(x = "visitantes",
         y = "arrecadacao") +
    theme_bw()
)



#Estimação do modelo OLS linear
modelo_linear <- lm(formula = arrecadacao ~ . - semana,
                    data = dados)


summary(modelo_linear)



##################################################################################
#          TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE          #
#                               SHAPIRO-FRANCIA                                  #
##################################################################################

#Shapiro-Francia: n > 30
sf.test(modelo_linear$residuals) #pacote nortest


#Histograma dos resíduos do modelo OLS linear
dados %>%
  mutate(residuos = modelo_linear$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "grey50", 
                 fill = "grey90", 
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(modelo_linear$residuals),
                            sd = sd(modelo_linear$residuals)),
                aes(color = "Curva Normal T?cnica"),
                size = 2) +
  scale_color_manual("Legenda:",
                     values = "#FDE725FF") +
  labs(x = "Res?duos",
       y = "Frequencia") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")



##################################################################################
#                            TRANSFORMAÇÃO DE BOX-COX                            #
##################################################################################

lambda_BC <- powerTransform(dados$arrecadacao) #função powerTransform do pacote car#
lambda_BC

#Inserindo o lambda de Box-Cox na base de dados para a estimação de um novo modelo
dados$bcarrecadacao <- (((dados$arrecadacao ^ lambda_BC$lambda) - 1) / 
                          lambda_BC$lambda)


#Visualizando a nova variável na base de dados
dados %>%
  select(dados, arrecadacao, bcarrecadacao, everything()) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 16)

#Estimando um novo modelo múltiplo com variável dependente transformada por Box-Cox
modelo_bc <- lm(formula = bcarrecadacao ~ . -dados -arrecadacao, 
                data = dados)

#Parâmetros do modelo
summary(modelo_bc)

#Aplicando o procedimento Stepwise
step_modelo_bc <- step(modelo_bc, k = 3.841459)

#comentário: Já tinha tansformado Y porque os resíduos não passavam no Shapiro-Francia 

summary(step_modelo_bc)
#Note que a variável 'ferias' saiu

#Verificando a normalidade dos resíduos do modelo step_modelo_bc
sf.test(step_modelo_bc$residuals) #função sf.test do pacote nortest

#SF para os resíduos deste modelo passou.


#Plotando os novos resíduos do step_modelo_bc
dadoss %>%
  mutate(residuos = step_modelo_bc$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..),
                 color = "white",
                 fill = "#287D8EFF",
                 bins = 30,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_modelo_bc$residuals),
                            sd = sd(step_modelo_bc$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()

#Modelos obtidos pelo procedimento Stepwise (linear e com Box-Cox)
#Função export_summs do pacote jtools
export_summs(step_dados, step_modelo_bc, scale = F, digits = 6)

#Parâmetros reais do modelo com procedimento Stepwise e Box-Cox
confint(step_modelo_bc, level = 0.95) # siginificância 5%
plot_summs(step_modelo_bc, colors = "#287D8EFF") #função plot_summs do pacote ggstance

#Parâmetros padronizados
plot_summs(step_modelo_bc, scale = TRUE, colors = "#287D8EFF")

#Adicionando caracterização da distribuição normal no IC de cada parâmetro beta
plot_summs(step_modelo_bc, scale = TRUE, plot.distributions = TRUE,
           inner_ci_level = .95, colors = "#287D8EFF")

#Comparando os ICs do betas dos modelos sem e com Transformação de Box-Cox
plot_summs(step_dados, step_modelo_bc, scale = T, plot.distributions = TRUE,
           inner_ci_level = .95, colors = c("#440154FF", "#287D8EFF"))

#Fazendo predições com o step_modelo_bc, usando o princípio de economia ceteris paribus. 
predict(object = step_modelo_bc, 
        data.frame(finalsemana = 50, 
                   feriado = 14, 
                   ferias = 4000),
        interval = "confidence", level = 0.95)

#Cálculo para a obtenção do fitted
#value de Y (arrecadacao)
(((3.702015 * -0.02256414) + 1)) ^ (1 / -0.02256414)

#Salvando os fitted values dos modelos step_dados e step_modelo_bc no dataset dados
dados$yhat_step_dados <- step_dados$fitted.values
dados$yhat_step_modelo_bc <- (((step_modelo_bc$fitted.values*(lambda_BC$lambda))+
                                 1))^(1/(lambda_BC$lambda))

#Visualizando os dois fitted values no dataset
#modelos step_dados e step_modelo_bc
dados %>%
  select(dados, arrecadacao, yhat_step_dados, yhat_step_modelo_bc) %>%
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = F, 
                font_size = 18)

#Ajustes dos modelos: valores previstos (fitted values) X valores reais
dados %>%
  ggplot() +
  geom_smooth(aes(x = arrecadacao, y = yhat_step_dados, color = "Stepwise"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = arrecadacao, y = yhat_step_dados),
             color = "#440154FF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = arrecadacao, y = yhat_step_modelo_bc, color = "Stepwise Box-Cox"),
              method = "lm", se = F, formula = y ~ splines::bs(x, df = 5), size = 1.5) +
  geom_point(aes(x = arrecadacao, y = yhat_step_modelo_bc),
             color = "#287D8EFF", alpha = 0.6, size = 2) +
  geom_smooth(aes(x = arrecadacao, y = arrecadacao), method = "lm", 
              color = "grey30", size = 1.05,
              linetype = "longdash") +
  scale_color_manual("Modelos:", 
                     values = c("#287D8EFF", "#440154FF")) +
  labs(x = "Arrecadação", y = "Fitted Values") +
  theme(panel.background = element_rect("white"),
        panel.grid = element_line("grey95"),
        panel.border = element_rect(NA),
        legend.position = "bottom")





##################################################################################
#            DIAGNÓSTICO DE MULTICOLINEARIDADE EM MODELOS DE REGRESSÃO           #
#                    CARREGAMENTO DA BASE DE DADOS                               #
##################################################################################

load("dados.RData")

##################################################################################
#                OBSERVANDO OS DADOS CARREGADOS DA BASE                          #
#                  DIAGNÓSTICO DE MULTICOLINEARIDADE                             #
##################################################################################

#Estatísticas univariadas
summary(dados)

#Correlação baixa:
cor(dados$finalsemana, dados$feriado)

#  [1] -0.1380131
#Comentários: correlação baixa

dados %>% select(2:6) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo1 <- lm(formula = arrecadacao ~ finalsemana + feriado,
              data = dados)

summary(modelo1)

ols_vif_tol(modelo1)


#Correlação baixa:
cor(dados$finalsemana, dados$ferias)

dados %>% select(2:6) %>% 
  correlation(method = "pearson") %>%
  plot()

modelo3 <- lm(formula = arrecadacao ~ finalsemana + ferias,
              data = dados)


summary(modelo3)


#Diagnóstico de multicolinearidade (Variance Inflation Factor e Tolerance)
ols_vif_tol(modelo3)


##################################################################################
#           DIAGNÓSTICO DE HETEROCEDASTICIDADE EM MODELOS DE REGRESSÃO           #
#                         ARREGAMENTO DA BASE DE DADOS                           #
##################################################################################


##################################################################################
#               OBSERVANDO OS DADOS CARREGADOS DA BASE dados                    #
##################################################################################

#Estatísticas univariadas
summary(dados)

#Tabela de frequências absolutas das variáveis 'finalsemana' e 'ferias'
table(dados$finalsemana)
table(dados$ferias)

#Plotando dados 
ggplotly(
  ggplot(dados, aes(x = arrecadacao, y = visitantes)) +
    geom_point(size = 1, color = "#FDE725FF") +
    geom_smooth(method = "lm", 
                color = "grey40", se = F) +
    xlab("arrecadacao") +
    ylab("visitantes") +
    theme_classic()
)


#Plotando em função de arrecadacao, com destaque para o finalsemana (movimentação elevada)
ggplotly(
  ggplot(dados, aes(x = arrecadacao, y = visitantes, color = finalsemana, shape = finalsemana)) +
    geom_point(size = 1) +
    xlab("arrecadacao") +
    ylab("visitantes") +
    scale_colour_viridis_d() +
    theme_classic()
)


##################################################################################
#                       ESTIMAÇÃO DO MODELO DE REGRESSÃO E                       #
#                       DIAGNÓSTICO DE HETEROCEDASTICIDADE                       #                                                            
##################################################################################
#Estimação do modelo
modelosheter <- lm(formula = arrecadacao ~ . - semana,
                   data = dados)


summary(modelosheter)

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(modelosheter)
#função ols_test_breusch_pagan do pacote olsrr


#################################################################################
#              PROCEDIMENTO N-1 DUMMIES PARA UNIDADES FEDERATIVAS               #
#################################################################################

saeb_rend_dummies_uf <- dummy_columns(.data = saeb_rend,
                                      select_columns = "uf",
                                      remove_selected_columns = T,
                                      remove_most_frequent_dummy = T)




##################################################################################
#                    REGRESSÃO NÃO LINEAR MÚLTIPLA COM DUMMIES                   #
#                                                                                #
##################################################################################



##################################################################################
#              OBSERVANDO OS DADOS CARREGADOS DA BASE DADOS                      #
##################################################################################
#glimpse(dados)

#Estatísticas univariadas
summary(dados)

#Categorias da variável 'finalsemana'
levels(factor(dados$finalsemana))

#Tabela de frequências absolutas das variáveis 'finalsemana' 'ferias' e 'feriado'
table(dados$finalsemana)
table(dados$ferias)
table(dados$feriado)



##################################################################################
#                               ESTUDO DAS CORRELAÇÕES                           #
##################################################################################

library(PerformanceAnalytics)

chart.Correlation((dados[2:6]), histogram = TRUE)

##################################################################################
#                       ESTIMAÇÃO DA REGRESSÃO LINEAR MÚLTIPLA                   #
##################################################################################
#Modelagem com todas as variáveis
modelo_dados_2 <- lm(arrecadacao ~ . - semana, dados)

#Parâmetros do modelo_planosaude
summary(modelo_dados_2)


##################################################################################
#                               PROCEDIMENTO STEPWISE                            #
##################################################################################

step_dados2 <- step(modelo_dados_2, k = 3.841459)

summary(step_dados2)


##################################################################################
#            TESTE DE VERIFICAÇÃO DA ADERÊNCIA DOS RESÍDUOS À NORMALIDADE        #
##################################################################################

#Teste de Shapiro-Francia
sf.test(step_dados2$residuals) #função sf.test do pacote nortest


#Plotando os resíduos do modelo step_dados2 
dados %>%
  mutate(residuos = step_dados2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  labs(x = "Resíduos",
       y = "Frequências") + 
  theme_bw()

#Acrescentando uma curva normal teórica para comparação entre as distribuições
dados %>%
  mutate(residuos = step_dados2$residuals) %>%
  ggplot(aes(x = residuos)) +
  geom_histogram(aes(y = ..density..), 
                 color = "white", 
                 fill = "#55C667FF", 
                 bins = 15,
                 alpha = 0.6) +
  stat_function(fun = dnorm, 
                args = list(mean = mean(step_dados2$residuals),
                            sd = sd(step_dados2$residuals)),
                size = 2, color = "grey30") +
  scale_color_manual(values = "grey50") +
  labs(x = "Resíduos",
       y = "Frequência") +
  theme_bw()


##################################################################################
#                        DIAGNÓSTICO DE HETEROCEDASTICIDADE                      #
##################################################################################

#Teste de Breusch-Pagan para diagnóstico de heterocedasticidade
ols_test_breusch_pagan(step_dados2)

##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################

#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(dados$arrecadacao)
lambda_BC


#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um novo modelo
dados$bcarrecadacao <- (((dados$arrecadacao ^ lambda_BC$lambda) - 1) / 
                          lambda_BC$lambda)


#Estimando um novo modelo múltiplo com dummies
modelo_bc_dados2 <- lm(formula = bcarrecadacao ~ . -semana -arrecadacao, 
                       data = dados)

#Parâmetros do modelo
summary(modelo_bc_dados2)

#Aplicando o procedimento Stepwise
step_bc_dados2 <- step(modelo_bc_dados2, k = 3.841459)

summary(step_bc_dados2)

#Verificando a normalidade dos resíduos do modelo 
#Teste de Shapiro-Francia
sf.test(step_bc_dados2$residuals) #função sf.test do pacote nortest

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_dados2)



#Lembrete -> refeito(aba)
##################################################################################
#                 INSTALAÇÃO E CARREGAMENTO DE PACOTES NECESSÁRIOS               #
##################################################################################


##################################################################################
#                              TRANSFORMAÇÃO DE BOX-COX                          #
##################################################################################
#Para calcular o lambda de Box-Cox
lambda_BC <- powerTransform(dados1$arrecadacao)
lambda_BC

#Inserindo o lambda de Box-Cox na nova base de dados para a estimação de um #novo modelo
dados1$bcarrecadacao <- (((dados$arrecadacao ^ lambda_BC$lambda) - 1) / 
                           lambda_BC$lambda)
lambda_BC


#Estimando um novo modelo múltiplo com dummies
modelo_bc_dados2 <- lm(formula = bcarrecadacao ~ . -arrecadacao, 
                       data = dados1)

#Parâmetros do modelo
summary(modelo_bc_dados2)

 
Call:
  lm(formula = bcarrecadacao ~ . - arrecadacao, data = dados1)

#Aplicando o procedimento Stepwise
step_bc_dados2 <- step(modelo_bc_dados2, k = 3.841459)

summary(step_bc_dados2)

sf.test(step_bc_dados2$residuals) #função sf.test do pacote nortest

#Diagnóstico de Heterocedasticidade para o Modelo Stepwise com Box-Cox
ols_test_breusch_pagan(step_bc_dados2)
