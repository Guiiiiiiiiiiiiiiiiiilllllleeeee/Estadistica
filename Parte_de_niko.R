


##Codigo sacado directamente de la fuente https://ourworldindata.org/grapher/gdp-worldbank?time=2019&overlay=download-data

## se descarga con install.packages("jsonlite")
library(jsonlite)

# Fetch the data
df <- read.csv("https://ourworldindata.org/grapher/gdp-per-capita-worldbank.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019")

df$entity=NULL
df$year=NULL



mean(df$ny_gdp_pcap_pp_kd)

var(df$ny_gdp_pcap_pp_kd)

median(df$ny_gdp_pcap_pp_kd)

##Barplot 
barplot(df$ny_gdp_pcap_pp_kd / 1000,
        xlab = "PIB PER CÁPITA (miles)",
        ylab = "Países",
        col = "white",
        col.axis = "darkgreen",
        col.lab = "darkgreen",
        yaxt = "n")

##Dividimos la barra y si no nocaben los números
axis(2,
     at = pretty(c(0, max(df$ny_gdp_pcap_pp_kd / 1000)), n = 15),
     las = 1,
     col.axis = "darkgreen")



boxplot(df$ny_gdp_pcap_pp_kd/10000,
        main = "PIB PER CÁPITA (10 miles)",
        xlab = "
        Datos atipicos:
      Luxemburgo,  Singapur, Catar, Irlanda, Noruega, Suiza, Brunéi.
      132724.750, 120678.340, 113160.980, 96205.800, 90772.900, 81694.630,80228.160")
  



df2 <- read.csv("https://ourworldindata.org/grapher/total-alcohol-consumption-per-capita-litres-of-pure-alcohol.csv?v=1&csvType=filtered&useColumnShortNames=true&time=2019")

df2$entity=NULL
df2$year=NULL






consumo = df_final$sh_alc_pcap_li
PIB = df_final$ny_gdp_pcap_pp_kd

# 2. Creamos el dataframe

datos <- df_final

# 3. Ajustamos el modelo de regresion lineal

reg_lin <- lm(consumo ~ PIB, data = datos)

# 4. Obtenemos los resultados

summary(reg_lin)

# 5. Grafico de dispersion

plot(PIB, consumo,
     main = "Relacion entre PIB per capita y consumo de alcohol",
     xlab = "PIB per capita",
     ylab = "Consumo de alcohol per capita",
     pch = 19,
     col = "blue")

# 6. Anadimos la recta de regresion

abline(reg_lin, col = "red", lwd = 2)

