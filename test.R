library(shiny)
library(dplyr)
library(ggplot2)
suaci_ori <- read.csv("sistema-unico-de-atencion-ciudadana-2020.csv")
suaci <- mutate(suaci_ori, mes = substr(periodo, 5, 6))
canal     <- arrange(distinct(suaci, canal), canal)
categoria <- arrange(distinct(suaci, categoria), categoria)
barrios   <- arrange(distinct(suaci, domicilio_barrio), domicilio_barrio)
canal <- "App"
categoria<- "TRÁNSITO"
barrio <- "CABALLITO"
flt_suaci <- filter(suaci,canal==canal, categoria==categoria, domicilio_barrio==barrio)
grp_suaci <- group_by(flt_suaci, mes)
smr_suaci <- summarise(grp_suaci, total=n())
p <- ggplot(smr_suaci, aes(x=mes)) + 
  geom_bar(aes(x = mes, weight = total))
print(p)
