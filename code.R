library(tidyverse)
library(dplyr)
library(ggplot2)
library(forcats)
library(cowplot)
library(ggpubr)

ds_caba_totales <- ds_caba %>%
  summarize (H_91 = sum(H_91), H_01 = sum(H_01), H_10 = sum(H_10), M_91 = sum(M_91), M_01 = sum(M_01), M_10 = sum(M_10))

ds_caba_totales

ds_pais_totales <- ds_pais %>%
  summarize (H_91 = sum(H_91), H_01 = sum(H_01), H_10 = sum(H_10), M_91 = sum(M_91), M_01 = sum(M_01), M_10 = sum(M_10))

ds_pais_totales

caba_adultos <- c(sum(ds_caba[66:100,2]), sum(ds_caba[66:100,3]), sum(ds_caba[66:100,4]), sum(ds_caba[66:100,5]), sum(ds_caba[66:100,6]), sum(ds_caba[66:100,7]))
caba_adultos

names(caba_adultos) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

pais_adultos <- c(sum(ds_pais[66:100,2]), sum(ds_pais[66:100,3]), sum(ds_pais[66:100,4]), sum(ds_pais[66:100,5]), sum(ds_pais[66:100,6]), sum(ds_pais[66:100,7]))
pais_adultos

names(pais_adultos) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

a <- data.frame(pais_adultos / ds_pais_totales * 100)
b <- data.frame(caba_adultos / ds_caba_totales * 100)

e_pais <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(a$H_91, a$M_91, a$H_01, a$M_01, a$H_10, a$M_10))

e_caba <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(b$H_91, b$M_91, b$H_01, b$M_01, b$H_10, b$M_10))

ggplot(e_pais, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución de la proporción de la población adulta mayor", subtitle="Alcance: Nacional") +
  theme_minimal ()

ggplot(e_caba, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución de la proporción de la población adulta mayor", subtitle="Alcance: Ciudad Autónoma de Buenos Aires") +
  theme_minimal ()

e_total1 <- rbind(e_pais, e_caba)
e_total1 <- data.frame (e_total1, alcance = c("pais", "pais", "pais", "pais", "pais", "pais", "caba", "caba", "caba", "caba", "caba", "caba"))

ggplot(e_total1, aes(x = año, y = porcentaje, color = sexo, linetype = alcance)) +
  geom_smooth(size = 1.1) +
  ggtitle ("Evolución de la proporción de la población adulta mayor") +
  xlim(1990,2010) +
  theme_minimal ()

#---------------------------------------------

#Ejercicio 2
  
caba_joven <- c(sum(ds_caba[21:65,2]), sum(ds_caba[21:65,3]), sum(ds_caba[21:65,4]), sum(ds_caba[21:65,5]), sum(ds_caba[21:65,6]), sum(ds_caba[21:65,7]))

names(caba_joven) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

c <- data.frame(caba_adultos / caba_joven * 100)
c <- as.data.frame(t(c))

e_caba_comb <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(c$H_91, c$M_91, c$H_01, c$M_01, c$H_10, c$M_10))

ggplot(e_caba_comb, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución del indice de dependencia adulta", subtitle="Alcance: Ciudad Autónoma de Buenos Aires") +
  theme_minimal ()

pais_joven <- c(sum(ds_pais[21:65,2]), sum(ds_pais[21:65,3]), sum(ds_pais[21:65,4]), sum(ds_pais[21:65,5]), sum(ds_pais[21:65,6]), sum(ds_pais[21:65,7]))

names(pais_joven) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")
d<- data.frame(pais_adultos / pais_joven * 100)
d <- as.data.frame(t(d))

e_pais_comb <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(d$H_91, d$M_91, d$H_01, d$M_01, d$H_10, d$M_10))

ggplot(e_pais_comb, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución del indice de dependencia adulta", subtitle="Alcance: Pais") +
  theme_minimal ()

e_total_comb <- rbind(e_pais_comb, e_caba_comb)
e_total_comb <- data.frame (e_total_comb, alcance = c("pais", "pais", "pais", "pais", "pais", "pais", "caba", "caba", "caba", "caba", "caba", "caba"))

ggplot(e_total_comb, aes(x = año, y = porcentaje, color = sexo, linetype = alcance)) +
  ggtitle ("Evolución del indice de dependencia adulta") +
  xlim(1990,2010) +
  theme_minimal () +
  geom_smooth (size = 1.1)

#-----------------------------------

#Ejercicio 3

lifeexp <- lifeexp %>%
  arrange(año) %>%
  mutate (edadr = c(65, 65, 65, 65, 65.56, 66.10, 66.30, 67.06, 66.50, 66.77, 67.44, 67.44))
  
pais_adultos2 <- c(sum(ds_pais[66:100,2]), sum(ds_pais[67:100,3], ds_pais[66,3]*(1-0.56)), sum(ds_pais[68:100,4], ds_pais[67,4]*0.5), sum(ds_pais[66:100,5]), sum(ds_pais[68:100,6], ds_pais[67,6]*0.9), sum(ds_pais[68:100,7], ds_pais[67,7]*(1-0.77)))
names(pais_adultos2) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

e <- data.frame(pais_adultos2 / ds_pais_totales * 100)

caba_adultos2 <- c(sum(ds_caba[66:100,2]), sum(ds_caba[68:100,3], ds_caba[67,3]*(1-0.3)), sum(ds_caba[69:100,4], ds_caba[68,4]*(1-0.44)), sum(ds_caba[66:100,5]), sum(ds_caba[69:100,6], ds_caba[68,6]*(1-0.06)), sum(ds_caba[69:100,7], ds_caba[68,7]*(1-0.44)))
names(caba_adultos2) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

f <- data.frame(caba_adultos2 / ds_caba_totales * 100)

e_pais2 <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(e$H_91, e$M_91, e$H_01, e$M_01, e$H_10, e$M_10))
e_caba2 <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(f$H_91, f$M_91, f$H_01, f$M_01, f$H_10, f$M_10))

ggplot(e_pais2, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución de la proporción de la población adulta mayor \najustado por la esperanza de vida.", subtitle="Alcance: Nacional") +
  theme_minimal ()

ggplot(e_caba2, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución de la proporción de la población adulta mayor \najustado por la esperanza de vida.", subtitle="Alcance: Ciudad Autónoma de Buenos Aires") +
  theme_minimal ()

e_total2 <- rbind(e_pais2, e_caba2)
e_total2 <- data.frame (e_total2, alcance = c("pais", "pais", "pais", "pais", "pais", "pais", "caba", "caba", "caba", "caba", "caba", "caba"))

ggplot(e_total2, aes(x = año, y = porcentaje, color = sexo, linetype = alcance)) +
  ggtitle ("Evolución de la proporción de la población adulta mayor,\najustado por la esperanza de vida.") +
  xlim(1990,2010) +
  theme_minimal () +
  geom_smooth (size = 1.1)

#------------------------------------------

caba_joven2 <- c(sum(ds_caba[21:65,2]), sum(ds_caba[21:66,3], ds_caba[67,3]*0.3), sum(ds_caba[21:67,4], ds_caba[68,4]*0.44), sum(ds_caba[21:65,5]), sum(ds_caba[21:67,6], ds_caba[68,6]*0.06), sum(ds_caba[21:67,7], ds_caba[68,7]*0.44))

names(caba_joven2) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

pais_joven2 <- c(sum(ds_pais[21:65,2]), sum(ds_pais[21:65,3], ds_pais[66,3]*0.56), sum(ds_pais[21:66,4], ds_pais[67,4]*0.5), sum(ds_pais[21:65,5]), sum(ds_pais[21:66,6], ds_pais[67,6]*0.1), sum(ds_pais[21:66,7], ds_pais[67,7]*0.77))
names(pais_joven2) <- c("H_91", "H_01", "H_10", "M_91", "M_01", "M_10")

g <- data.frame(caba_adultos2 / caba_joven2 * 100)
g <- as.data.frame(t(g))

h <- data.frame(pais_adultos2 / pais_joven2 * 100)
h <- as.data.frame(t(h))

e_caba_comb2 <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(g$H_91, g$M_91, g$H_01, g$M_01, g$H_10, g$M_10))
e_pais_comb2 <- data.frame (año = c(1991, 1991, 2001, 2001, 2010, 2010), sexo = c("Hombres", "Mujeres", "Hombres", "Mujeres", "Hombres", "Mujeres"), porcentaje = c(h$H_91, h$M_91, h$H_01, h$M_01, h$H_10, h$M_10))

e_total_comb2 <- rbind(e_pais_comb2, e_caba_comb2)
e_total_comb2 <- data.frame (e_total_comb2, alcance = c("pais", "pais", "pais", "pais", "pais", "pais", "caba", "caba", "caba", "caba", "caba", "caba"))

ggplot(e_caba_comb2, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución del indice de dependencia adulta, \najustado por la esperanza de vida", subtitle="Alcance: Ciudad Autónoma de Buenos Aires") +
  theme_minimal ()

ggplot(e_pais_comb2, aes(x = año, y = porcentaje, color = sexo)) +
  geom_smooth(size = 1.1) +
  xlim(1990,2010) +
  ggtitle ("Evolución del indice de dependencia adulta, \najustado por la esperanza de vida", subtitle="Alcance: Pais") +
  theme_minimal ()

ggplot(e_total_comb2, aes(x = año, y = porcentaje, color = sexo, linetype = alcance)) +
      ggtitle ("Evolución del indice de dependencia adulta, \najustado por la esperanza de vida") +
      xlim(1990,2010) +
      theme_minimal () +
      geom_smooth (size = 1.1)

e_total_comb$type <- "sin ajuste"
e_total_comb2$type <- "con ajuste"
e_total_comb3 <- rbind(e_total_comb, e_total_comb2)

ggplot(filter(e_total_comb3, alcance == "caba"), aes(x=año, y=porcentaje, color = sexo, linetype = type)) +
  ggtitle("Comparativa de la evolución del indice de dependencia adulta", subtitle = "Alcance: Ciudad Autónoma de Buenos Aires") +
  xlim(1990, 2010) +
  theme_minimal() +
  geom_smooth()

ggplot(filter(e_total_comb3, alcance == "pais"), aes(x=año, y=porcentaje, color = sexo, linetype = type)) +
  ggtitle("Comparativa de la evolución del indice de dependencia adulta", subtitle = "Alcance: País") +
  xlim(1990, 2010) +
  theme_minimal() +
  geom_smooth()

e_total1$type <- "sin ajuste"
e_total2$type <- "con ajuste"
e_total3 <- rbind(e_total1, e_total2)

ggplot(filter(e_total3, alcance == "caba"), aes(x=año, y=porcentaje, color = sexo, linetype = type)) +
  ggtitle("Comparativa de la evolución de la proporción de la población mayor", subtitle = "Alcance: Ciudad Autónoma de Buenos Aires") +
  xlim(1990, 2010) +
  theme_minimal() +
  geom_smooth()

ggplot(filter(e_total3, alcance == "pais"), aes(x=año, y=porcentaje, color = sexo, linetype = type)) +
  ggtitle("Comparativa de la evolución de la proporción de la población mayor", subtitle = "Alcance: País") +
  xlim(1990, 2010) +
  theme_minimal() +
  geom_smooth()

