#Descriptores y medidas

data("iris")
iris
#calcular promedio
dim(iris)
length(iris)
nrow(iris)
length(iris$Petal.Length)
length(iris$Petal.Length[which(iris$Species=='setosa')])
n<-length(iris$Petal.Length[which(iris$Species=='setosa')])
n
promedio<-pl.setosa/n
promedio
mean(iris$Petal.Length[which(iris$Species=='setosa')])
hist(iris$Petal.Length[which(iris$Species=='setosa')])


##valores promedio totuga
install.packages("ade4")
library(ade4)
data("tortues")
tortues
mean(tortues$long)
mean(tortues$larg)
mean(tortues$haut)
tortuess<-tortues[,-4]
tortuess
apply(tortuess,2,mean)

#media armonica
harmonic.mean(iris$Petal.Length[which(iris$Species=='setosa')])
geometric.mean(iris$Petal.Length[which(iris$Species=='setosa')])
setosa <- iris$Petal.Length[which(iris$Species=='setosa')]
sort(setosa)
n
median(setosa)


#Función
mi.mediana <- function(x) sum(setosa_ordenado[m/2], setosa_ordenado[(m/2)+1])/2
mi.mediana(setosa)

##modo##
mode(setosa)
freq <- table(setosa)
freq
freq[which.max(freq)]
range(setosa)
min(freq)
max(freq)
li

##grafica
hist(setosa, 
     col = "peachpuff", # column color
     border = "black", 
     prob = TRUE, # show densities instead of frequencies
     xlim = c(0.5,2.5),
     xlab = "Longitud de pétalo",
     main = "Frecuencias de longitud de pétalo de setosa")
lines(density(setosa),
      lwd = 2, # thickness of line
      col = "chocolate3") # Generar la distribución continua distribución de masa a partir de los datos observados

# Graficar la media
abline(v = mean(setosa),
       col = "royalblue",
       lwd = 2)

abline(v = median(setosa),
       col = "red",
       lwd = 2)
abline(v = freq <- table(setosa)
       freq[which.max(freq)],
       col = "gren",
       lwd = 2)


legend(x = "topright", # location of legend within plot area
       c("Density plot", "Mean", "Median"),
       col = c("chocolate3", "royalblue", "red"),
       lwd = c(2, 2, 2))

#medidas de posición 
density(setosa)
quantile (setosa, probs =0.8)
quantile(setosa,probs = c(seq(0, 1, 0.25)))
quantile(setosa,probs = c(seq(0, 1, 0.5)))
summary(setosa)
str(summary(setosa))
x <- summary(setosa)
class(x)
x[1]
hist(setosa, 
     col = "grey80", 
     border = "black", 
     prob = TRUE, 
     xlim = c(0.5,2.5),
     xlab = "Longitud de pétalo",
     main = "Frecuencias de longitud de pétalo de setosa")
lines(density(setosa),
      lwd = 2, 
      col = "chocolate3") 

# Graficar la media

abline(v = x[2],
       col = "red1",
       lwd = 2)

abline(v = x[3],
       col = "red4",
       lwd = 2)

abline(v = x[4],
       col = "blue",
       lwd = 2)

abline(v = x[5],
       col = "red1",
       lwd = 2)

legend(x = "topright", # location of legend within plot area
       c("Density plot","1st Qu", "Mean", "Median","3st Qu" ),
       col = c("chocolate3", "royalblue", "red"),
       lwd = c(2, 2, 2))
# aggregate()

aggregate(iris[,1:4], list(Especies = iris$Species), mean)
aggregate(iris, list(Especies = iris$Species), mean)
aggregate(iris[,1:4], list(Especies = iris$Species), median)
aggregate(iris[,1:4], list(Especies = iris$Species), quantile)
  
#box-plot
pop1 <-abs(rnorm(200,mean = 6,sd = 1))

pop2 <-abs(rnorm(200,mean = 6,sd = 5))


pops<-cbind(pop1,pop2)
pops
boxplot(x = pops,notch=T)

#Varianza y Desviación
apply(pops,2,var)
apply(pops,2,sd)
plot(density(pops[,1]))
plot(density(pops[,2]))
describe(pops)

#medidas de forma
for (i in 1:4){
  
  plot(density(iris[,i]), main = colnames(iris)[i])
}
#asimetria
library(EnvStats)
skewness(pops[,1], na.rm = FALSE, method = "fisher")
plot(density(pops[,1]), main= 'Pop1')
abline(v= mean(pops[,1]))
skewness(pops[,2], na.rm = FALSE, method = "fisher")
plot(density(pops[,2]), main= 'Pop3')
abline(v= mean(pops[,2]))
aggregate(iris[,1:4],list(Especies= iris$Species), skewness, na.rm = FALSE, method = "fisher")
set.seed(999)
n.sample <- rnorm(n = 10000, mean = 55, sd = 4.5)

skewness(n.sample, na.rm = FALSE, method = "fisher")
plot(density(n.sample), main= 'Pop3')
abline(v= mean(n.sample))

#medidas de apuntamiento
apply(pops,2,kurtosis)
kurtosi(iris$Sepal.Length[which(iris$Species == 'setosa')])
kurtosi(n.sample)
describe.by(iris[,1:4],group = iris$Species)
