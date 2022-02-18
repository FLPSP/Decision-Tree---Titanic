## Instalando e carregando packages
pacotes <- c('titanic',    # carrega a base original titanic_treino 
             'tidyverse',  # Pacote básico de datawrangling
             'rpart',      # Biblioteca de árvores
             'rpart.plot', # Conjunto com Rpart, plota a parvore
             'gtools',     # funções auxiliares como quantcut,
             'Rmisc',      # carrega a função sumarySE para a descritiva
             'scales',     # importa paletas de cores
             'caret'       # Funções úteis para machine learning
             'plotROC'     # Plotar curva ROC
)

if(sum(as.numeric(!pacotes %in% installed.packages())) != 0){
  instalador <- pacotes[!pacotes %in% installed.packages()]
  for(i in 1:length(instalador)) {
    install.packages(instalador, dependencies = T)
    break()}
  sapply(pacotes, require, character = T) 
} else {
  sapply(pacotes, require, character = T) 
}

## Visualizando a base de dados
titanic %>% head

## Criando uma cópia da base original, para possíveis erros
tmp <- titanic
tmp$survived <- as.integer(titanic$Survived=="Y")

## Criando uma função para realizar uma análise descritiva das variáveis
descritiva <- function(var){
  ## Sumariza a taxa de sobreviventes por categoria da variável em análise
  tgc <- Rmisc::summarySE(tmp, measurevar="survived", groupvars=c(var))
  
  ggplot(tgc) + 
    ## Plota o gráfico de barras com as frequências
    geom_bar(aes(x=tgc[,var], weight=N/891, fill=as.factor(tgc[,var]))) + 
    ## Plota as barras de erro
    geom_errorbar(aes(x=tgc[,var], y=survived, ymin=survived-se, ymax=survived+se, colour='1'), width=.1) +
    ## Plota as médias de cada grupo
    geom_point(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    ## Plota as linhas que conectam as médias
    geom_line(aes(x=tgc[,var], y=survived, colour='1', group='1')) +
    ## Escala de cores do gráfico de médias
    scale_color_viridis_d(direction = -1, begin=0, end=.25) +
    ## Escala de cores do gráfico de barras
    scale_fill_viridis_d(direction = -1, begin=.85, end=.95) +
    ## Estética mais 'leve' do gráfico
    theme(panel.background = element_rect(fill = "white", colour = "grey", linetype = "solid"),
          panel.grid.major = element_line(size = 0.15, linetype = 'solid', colour = "grey")) + 
    ## Remove a legenda
    theme(legend.position = "none") +
    ## Rótulo dos eixos
    xlab(var) + ylab("Taxa de sobreviventes") + 
    ## Marcas do eixo secundário
    scale_y_continuous(sec.axis = sec_axis(~.*891, name = "Frequencia"), labels = scales::percent)
}

descritiva("Sex")
descritiva("Pclass")
descritiva("Embarked")
descritiva("SibSp")
descritiva("Parch")

## Categorizando as variáveis contínuas para poder aplicar a função e poder analisar
tmp$cat_age <- quantcut(tmp$Age, 20)
descritiva("cat_age")

tmp$cat_fare <- quantcut(tmp$Fare, 10)
descritiva("cat_fare")

## Construindo a árvore de classificação de forma rápida, simplesmente buscando avaliar o dataset cru
arvore <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked,
                data=titanic,
                parms = list(split = 'gini'), # podemos trocar para  'information'
                method='class' # Essa opção indica que a resposta é qualitativa
)

## Visualizando plotando a árvore
paleta = scales::viridis_pal(begin=.75, end=1)(20)
rpart.plot::rpart.plot(arvore, 
                       box.palette = paleta)

## Avaliação da árvore
## Probabilidade de sobreviver
prob = predict(arvore, titanic)

## Classificação dos sobreviventes
class = prob[,2]>.5
## Matriz de confusão
tab <- table(class, titanic$Survived)
tab

acc <- (tab[1,1] + tab[2,2])/ sum(tab)
acc

## Separando a base em treinamento e teste
set.seed(1)
bool_treino <- stats::runif(dim(titanic)[1])>.25

treino <- titanic[bool_treino,]
teste  <- titanic[!bool_treino,]

## Avaliando a árvore na base de treino
p_treino = stats::predict(arvore, treino)
c_treino = base::factor(ifelse(p_treino[,2]>.5, "Y", "N"))
p_teste = stats::predict(arvore, teste)
c_teste = base::factor(ifelse(p_teste[,2]>.5, "Y", "N"))

tab <- table(c_treino, treino$Survived)
acc <- (tab[1,1]+tab[2,2])/nrow(treino)
sprintf('Acurácia na base de treino: %s ', percent(acc))

tab <- table(c_teste, teste$Survived)
acc <- (tab[1,1]+tab[2,2])/nrow(teste)
sprintf('Acurácia na base de treino: %s ', percent(acc))

## Calculando a área da curva ROC
aval_treino <- data.frame(obs=treino$Survived, 
                         pred=c_treino,
                         Y = p_treino[,2],
                         N = 1-p_treino[,2]
                         )

caret::twoClassSummary(aval_treino, lev=levels(aval_treino$obs))

## Utilizando o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot2::ggplot(aval_treino, aes(d = obs, m = Y, colour='1')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de treino")
  
CurvaROC

## Avaliando a árvore na base de teste
aval_teste <- data.frame(obs=teste$Survived, 
                         pred=c_teste,
                         Y = p_teste[,2],
                         N = 1-p_teste[,2]
)

twoClassSummary(aval_teste, lev=levels(aval_teste$obs))

## Utilizando o mesmo dataframe para fazer a curva ROC:
CurvaROC <- ggplot(aval_teste, aes(d = obs, m = Y, colour='a')) + 
  plotROC::geom_roc(n.cuts = 0) +
  scale_color_viridis_d(direction = -1, begin=0, end=.25) +
  theme(legend.position = "none") +
  ggtitle("Curva ROC - base de teste")

CurvaROC

