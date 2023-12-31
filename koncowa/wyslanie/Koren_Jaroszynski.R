# CZĘŚĆ 1, Koren

#kmr_uk_d <- read.csv("C:/Users/maria/Desktop/modelowanie_matematyczne/kmr_uk_d.csv")
# View(kmr_uk_d)
library(fitdistrplus)
library(e1071)
library(ggplot2)
library(gridExtra)
library(goftest)
library(dplyr)
library(ggExtra)

# zadanie 1
zamkniecie <- kmr_uk_d$Zamkniecie
data <- kmr_uk_d$Data
df <- data.frame(data = data, zamkniecie = zamkniecie)

wykres <- ggplot(df, aes(x =as.Date(data), y = zamkniecie, group = 1)) + geom_line(color = "blue")+labs(x="data", y="cena podczas zamknięcia")
plot(wykres)
#ggsave("cena_podczas_zamkniecia.jpg", plot = plot, width = 12, height = 9, units = "cm", dpi = 480)
histogram <- hist(zamkniecie, prob = TRUE, xlab = "Zamknięcie", ylab = "Gęstość")


# zadanie 2

srednia_zamkniecie <- mean(kmr_uk_d$Zamkniecie)
odchylenie_zamkniecie <- sd(kmr_uk_d$Zamkniecie)
kurtoza <- kurtosis(kmr_uk_d$Zamkniecie)
skosnosc <- skewness(kmr_uk_d$Zamkniecie)
# print(srednia_zamkniecie) # 442.8741
# print(odchylenie_zamkniecie) # 26.7262
# print(kurtoza) # 3.574323
# print(skosnosc) # 0.52783


# Zinterpretuj skosnosc i kurtoze

interpretation <- function(skosnosc, kurtoza) {
  if (skosnosc > 0) {
    skosnosc_interpretacja <- "Prawostronnie skośny (przewaga wartości wyższych)"
  } else if (skosnosc < 0) {
    skosnosc_interpretacja <- "Lewostronnie skośny (przewaga wartości niższych)"
  } else {
    skosnosc_interpretacja <- "Skośność zerowa (rozkład symetryczny)"
  }

  if (kurtoza > 3) {
    kurtoza_interpretacja <- "Grubsze ogony niż rozkład normalny (bardziej szpiczasty)"
  } else if (kurtoza < 3) {
    kurtoza_interpretacja <- "Cieńsze ogony niż rozkład normalny (bardziej płaski)"
  } else {
    kurtoza_interpretacja <- "Równa kurtoza (porównywalna do rozkładu normalnego)"
  }

  return(c(skosnosc_interpretacja, kurtoza_interpretacja))
}

# print(interpretation(skosnosc, kurtoza))
# [1] "Prawostronnie skośny (przewaga wartości wyższych)"   
# [2] "Cieńsze ogony niż rozkład normalny (bardziej płaski)"


# zadanie 3
normalny <- fitdist(kmr_uk_d$Zamkniecie, 'norm')
#wyestymatowane parametry
mu<-normalny$estimate[1] #442.8741
sigma <- normalny$estimate[2]  #26.67269
normalny

lnormalny <- fitdist(kmr_uk_d$Zamkniecie, 'lnorm')
#wyestymatowane parametry
mu2<-lnormalny$estimate[1] #6.091499
sigma2 <- lnormalny$estimate[2] #0.05957788
lnormalny


weibull <- fitdist(kmr_uk_d$Zamkniecie,'weibull')
#wyestymatowane parametry
#shape_est <- weibull$estimate[["shape"]] #15.61307
#scale_est <- weibull$estimate[["scale"]]#455.8914
weibull


legendary <- c("norm", "lnorm", "weibull")
par(mar = c(2, 2, 2, 2))
denscomp(list(normalny, lnormalny, weibull), legendtext = legendary) # histogram z wykresami


# zadanie 4
# WYKRESY DIAGNOSTYCZNE

qqcomp(list(normalny, lnormalny, weibull))
cdfcomp(list(normalny, lnormalny, weibull))


stat <- gofstat(
        list(normalny, lnormalny, weibull),
        fitnames = legendary
)

# print(stat)
# Goodness-of-fit statistics
#                                    norm     lnorm   weibull
# Kolmogorov-Smirnov statistic 0.09168955 0.0798396 0.1384442
# Cramer-von Mises statistic   0.36130631 0.2485924 1.2576772
# Anderson-Darling statistic   2.00546921 1.4125472 7.4165930

# Goodness-of-fit criteria
#                                    norm    lnorm  weibull
# Akaike's Information Criterion 2355.289 2348.983 2415.692
# Bayesian Information Criterion 2362.332 2356.026 2422.735

# zadanie 5

N <- 10000
n <- length(kmr_uk_d$Zamkniecie)
Dln <- c()

for (i in 1:N) { 
  Yln <- rlnorm(n,lnormalny$estimate[1],lnormalny$estimate[2])
  Dln[i] <-  ks.test(Yln,plnorm, lnormalny$estimate[1],lnormalny$estimate[2],exact=TRUE)$statistic
}

dn_ln <-  ks.test(kmr_uk_d$Zamkniecie,plnorm,lnormalny$estimate[[1]],lnormalny$estimate[[2]],exact=TRUE)$statistic
dn_ln


par(mfrow=c(1,1))
hist(Dln,prob=T)
points(dn_ln,0,pch=19,col=2)

p_value_ln <- length(Dln[Dln>dn_ln])/N
print(p_value_ln) #0.0827

#4. Przyjmujemy poziom istotnosci alpha=0.05
alpha <- 0.05
# p_value_ln >= alpha

#Wartosc p-value jest większa od przyjetego poziomu istotnosci,
#zatem hipoteze o rownosci dystrybuant akceptujemy.




#CZĘŚĆ 1, Jaroszyński
# install.packages("ggplot2")
# install.packages("e1071")
# install.packages("fitdistrplus")
library(ggplot2)
library(e1071)
library(fitdistrplus)

#ccc <- read.csv("jjb_d.csv")

Zamkniecie <- jjb_d$Zamkniecie
Data <- jjb_d$Data

df <- data.frame(data = Data, zamkniecie = Zamkniecie)

# Zadanie 1
plot <- ggplot(df, aes(x = as.Date(data), y = zamkniecie, group = 1)) +
  geom_line(color = "red") +
  labs(x = "data", y = "Cena podczas zamknięcia")
plot
ggsave(
  "cena_podczas_zamkniecia.jpg",
  plot = plot,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

hist_df <- data.frame(zamkniecie = Zamkniecie)
hist <- ggplot(
  hist_df,
  aes(x = Zamkniecie)
) + geom_histogram(
  binwidth = 0.1,
  color = "black",
  fill = "white"
)
hist
ggsave(
  "histogram.jpg",
  plot = hist,
  width = 12,
  height = 9,
  units = "cm",
  dpi = 480
)

# Zadanie 2
ccc_mean     <- mean(Zamkniecie)
ccc_sd       <- sd(Zamkniecie)
ccc_skewness <- skewness(Zamkniecie)
ccc_kurtosis <- kurtosis(Zamkniecie)

ccc_mean
ccc_sd
# skośna
ccc_skewness
# rozkład leptokurtyczny
ccc_kurtosis

# Zadanie 3

decay_norm  <- fitdist(Zamkniecie, "norm")
decay_lnorm <- fitdist(Zamkniecie, "lnorm")
decay_gamma <- fitdist(Zamkniecie, "gamma")

decay_norm
decay_lnorm
decay_gamma

# Zadanie 4

legendary <- c("norm", "lnorm", "gamma")
jpeg("comp.jpg", width = 1920, height = 1080, quality = 100, res = 140)
par(mfrow = c(1, 1))
hw <- denscomp(list(decay_norm, decay_lnorm, decay_gamma), legendtext = legendary)
hw
qq <- qqcomp(list(decay_norm, decay_lnorm, decay_gamma), legendtext = legendary)
qq
cdf <- cdfcomp(list(decay_norm, decay_lnorm, decay_gamma), legendtext = legendary)
cdf
dev.off()

gofstat(
        list(decay_norm, decay_lnorm, decay_gamma),
        fitnames = legendary
)

# Zadanie 5
N <- 10000
n <- 100
D <- c()

for (i in 1:N) {
  y_lnorm <- rlnorm(n, decay_lnorm$estimate[1], decay_lnorm$estimate[2])
  D[i] <- ks.test(
                     y_lnorm,
                     plnorm,
                     decay_lnorm$estimate[1],
                     decay_lnorm$estimate[2],
                     exact = TRUE
  )$statistic
}

dn_length <- ks.test(
                     Zamkniecie,
                     plnorm,
                     decay_lnorm$estimate[1],
                     decay_lnorm$estimate[2],
                     exact = TRUE,
)$statistic

dn_length

jpeg("hist2.jpg", width = 1920, height = 1080, quality = 100, res = 140)
par(mfrow = c(1, 1))
hist(D, prob = TRUE)
points(dn_length, 0, pch = 19, col = "purple")
dev.off()

p_value <- length(D[D > dn_length]) / N
p_value



# CZĘŚĆ 2, wspólna
# A analiza rozkładów brzegowych

# 2.1 badanie danych spólki lmr_uk_d
dane1 <- kmr_uk_d$Zamkniecie
ilosc1 <- length(dane1)
#stopy zwrotu
r1<-diff(log(dane1))

#1 wykres log-zwrotów
plot(r1)
lines(r1, type = "l", col = "brown")
#2 histgram
h1<-hist(r1)

#3 dowasowac rozklad normalny
norm1 <- fitdist(r1, 'norm')
n1<-denscomp(list(norm1))
n1
norm1
# mean = -0.002326204
# sd = 0.0184629646
# dopasowany rozklad X ~ N(-0.0023,0.018 )
#4 wykresy diagnostyczne
qqcomp(list(norm1))
cdfcomp(list(norm1))

#5 test równości dla rozkładu X ~ N(-0.0023,0.018 )
N1 <- 10000
n1 <- length(r1)
Dn1 <- c()

for (i in 1:N1) { 
  Yn1 <- rnorm(n1,norm1$estimate[1],norm1$estimate[2])
  Dn1[i] <-  ks.test(Yn1 ,pnorm, norm1$estimate[1],norm1$estimate[2],exact=TRUE)$statistic
}

dn_n1 <-  ks.test(r1,pnorm,norm1$estimate[[1]],norm1$estimate[[2]],exact=TRUE)$statistic
#dn_n1


par(mfrow=c(1,1))
hist(Dn1,prob=T)
points(dn_n1,0,pch=19,col=2)

p_value_n1 <- length(Dn1[Dn1>dn_n1])/N
p_value_n1 #0.5656 => hipoteze ze rozklad stopów jest  X ~ N(-0.0023,0.018 ) przyjęta


#2.2 badanie danych spólki jjb_d
dane2 <- jjb_d$Zamkniecie
ilosc2 <- length(dane2)
#stopy zwrotu
r2<-diff(log(dane2))

#1 wykres log-zwrotów
plot(r2)
lines(r2, type = "l", col = "brown")
#2 histgram
hist(r2)
#3 dowasowac rozklad normalny
norm2 <- fitdist(r2, 'norm')
denscomp(list(norm2))
norm2
# mean = -0.002507597
# sd = 0.057104817
# dopasowany rozklad X ~ N(-0.0025,0.057 )
#4 wykresy diagnostyczne
qqcomp(list(norm2))
cdfcomp(list(norm2))

#5 test równości dla rozkładu X ~  N(-0.0025,0.057 )
N2 <- 10000
n2 <- length(r2)
Dn2 <- c()

for (i in 1:N2) { 
  Yn2 <- rnorm(n2,norm2$estimate[1],norm2$estimate[2])
  Dn2[i] <-  ks.test(Yn2 ,pnorm, norm2$estimate[1],norm2$estimate[2],exact=TRUE)$statistic
}

dn_n2 <-  ks.test(r2,pnorm,norm2$estimate[[1]],norm2$estimate[[2]],exact=TRUE)$statistic
#dn_n2


par(mfrow=c(1,1))
hist(Dn2,prob=T)
points(dn_n2,0,pch=19,col=2)

p_value_n2 <- length(Dn1[Dn1>dn_n1])/N
p_value_n2 #0.5709 => hipoteza że rozkład stopów jest X ~  N(-0.0025,0.057 ) przyjęta


# B Estymacja parametrów rozkładu dwuwymiarowego normalnego oraz analiza dobrości dopasowania

# 1 Wykres rozrzutu z histogramami brzegowymi

kursy <- data.frame(r1=r1, r2=r2)
kursy
p <-  ggplot(kursy, aes(x=r1, y=r2)) + geom_point()
ggMarginal(p, type="histogram")

# 2 Korzystając z omówionych na wykładzie estymatorów, wyznacz wektor
# średnich µˆ, kowariancjji, współczynnik korelacji, macierz kowariancji Σˆ
# oraz macierz korelacji

# wektor średnich
mu <- colMeans(kursy)
# mu
#r1            r2 
#-0.0002403422 -0.0024970847


# kowarjacja
Sigma <- cov(kursy) #estymator nieobciazony
#Sigma
#    r1           r2
#r1 3.529462e-04 7.719315e-05
#r2 7.719315e-05 3.181408e-03

# kowarjancja: 7.719315e-05

#macierz korelacji
P <- cor(kursy)
#P

#    r1         r2
#r1 1.00000000 0.07284753
#r2 0.07284753 1.00000000

# wspólczynnik korelacji 0.07284753 - wartość w macierzy korelacji na pozycjach (1,2) oraz (2,1)


# 3 Zapisać wzór gęstości oraz wzóry gęstości rozkładów brzegowych
library(mvtnorm)
s1 <- sqrt(Sigma[1, 1])
s1
s2 <- sqrt(Sigma[2, 2])
s2

# Tworzenie siatki punktów
x <- seq(-3 * s1+mu[1], 3 * s1+mu[1], 0.005)
y <- seq(-3 * s2+mu[2], 3 * s2+mu[2], 0.005)

# Wykres gęstości dwuwymiarowego rozkładu normalnego
f <- function(x, y) dmvnorm(cbind(x, y), mu, Sigma)
z <- outer(x, y, f)
#persp(x, y, z, theta = 30, phi = 30, col = "lightblue")
persp(x, y, z, theta = -30, phi = 25, 
      shade = 0.75, col = "lightblue", expand = 0.5, r = 2, 
      ltheta = 25, ticktype = "detailed")


density_r1 <- dnorm(x, mean = mu[1], sd = s1)
plot(x, density_r1, type = 'l', col = 'blue', lwd = 2, xlab = 'r1', ylab = 'Density', main = 'Wykres gęstości r1')

# Wykres gęstości jednowymiarowej dla r2
density_r2 <- dnorm(y, mean = mu[2], sd = s2)
plot(y, density_r2, type = 'l', col = 'blue', lwd = 2, xlab = 'r2', ylab = 'Density', main = 'Wykres gęstości r2')


# C Analiza dopasowania rozkładu N(ˆµ, Σ) ˆ do danych
# 1 

library(MASS)
n <- nrow(kursy); n

set.seed(242)
Z <- MASS::mvrnorm(n,mu=mu,Sigma=Sigma) # Z - wygenerowany rozkład

#wykresy rozrzutu
par(mfrow=c(2,2))
plot(kursy, xlim = c(-0.15, 0.15), ylim = c(-0.20, 0.20), main = "Wykres rozrzutu dla log-zwrotów z danych", pch = 16, col = "blue")
plot(Z, xlim = c(-0.15, 0.15), ylim = c(-0.20, 0.20), main = "Wykres rozrzutu dla wygenerowanrgo rozkładu", pch = 16, col = "red")




#2

# Obliczenie kwadratów odległości Mahalanobisa
mahalanobis_distances <- mahalanobis(kursy, center = mu, cov = Sigma)
#mahalanobis_distances
#wyniki na histogramie
par(mfrow=c(1,1))
hist(mahalanobis_distances,prob=TRUE)



n <- dim(kursy)[1]; n
alpha <- ppoints(n)
q_emp <- quantile(mahalanobis_distances,alpha)
q_teo <- qchisq(alpha,2)

plot(q_emp,q_teo,pch=19)
abline(a=0,b=1,col=2)

#testujemyhipoteze, ze kwadraty odleglosci Mahalanobisa maja rozklad chi(2)

ks.test(mahalanobis_distances,'pchisq',2)
#Asymptotic one-sample Kolmogorov-Smirnov test
#data:  mahalanobis_distances
#D = 0.13942, p-value = 0.0001707 < 0.05 => otrzycamy hipotezęze kwadraty odleglosci Mahalanobisa maja rozklad chi(2)
# zatem odrzucamy również hipoteze o normalności rozkładu log-zwrotów 


