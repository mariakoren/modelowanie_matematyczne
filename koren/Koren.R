kmr_uk_d <- read.csv("kmr_uk_d.csv")
# View(kmr_uk_d)
library(fitdistrplus)
library(e1071)
library(ggplot2)
library(gridExtra)
library(goftest)

# zadanie 1
zamkniecie <- kmr_uk_d$Zamkniecie
data <- kmr_uk_d$Data
df <- data.frame(data = data, zamkniecie = zamkniecie)

wykres <- ggplot(df, aes(x =as.Date(data), y = zamkniecie, group = 1)) + geom_line(color = "blue")+labs(x="data", y="cena podczas zamknięcia")
plot(wykres)
ggsave("cena_podczas_zamkniecia.jpg", plot = plot, width = 12, height = 9, units = "cm", dpi = 480)
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