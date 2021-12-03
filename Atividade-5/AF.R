require (psych)
require (corrplot)

df <- read.csv(file = "Database/DataFrame.csv") %>% select(2:6)
summary(df) 
print(summary(df))

matrizcorrelacao <-cor(df)
print (matrizcorrelacao, digits = 2)

corrplot (matrizcorrelacao)

cortest.bartlett(df)

det(matrizcorrelacao)

KMO (df)

#############################
## Análise Fatorial - Método de Análise
#############################
analise<-princomp (df, cor=TRUE)
#analise
summary(analise)
screeplot(analise)
plot(analise, type="lines")

PCA<-principal(df, nfactors=3, rotate ="none")
PCA

PCAVariMax<-principal(df, nfactors=2, rotate ="varimax", n.obs = 467, scores = TRUE)
PCAVariMax

biplot(PCAVariMax)

factor.scores(df,PCAVariMax, 
              Phi = NULL, 
              method = c("Thurstone", "tenBerge", "Anderson",
                         "Bartlett", "Harman","components"),
              rho=NULL)
