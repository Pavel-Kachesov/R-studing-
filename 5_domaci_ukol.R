############ 1 uloha ###########################################################
############ i #################################################################
d = data("Titanic", package = "datasets")



############ ii ################################################################
prezili_m= sum(Titanic[,"Male", "Adult", "Yes"])

prezili_z= sum(Titanic[,"Female", "Adult", "Yes"])

neprezil_m = sum(Titanic[,"Male", "Adult", "No"])

neprezil_z= sum(Titanic[,"Female", "Adult", "No"])



############ iii ###############################################################
# Počty dospělých mužů a žen, kteří přežili
survived_adults <- sum(Titanic[, , "Adult", "Yes"])
not_survived_adults <- sum(Titanic[, , "Adult", "No"])

# Vytvoření matice 2x2
result_matrix <- matrix(c(prezili_m, prezili_z, neprezil_m, neprezil_z),
                        nrow = 2, byrow = TRUE,
                        dimnames = list(c("Survived", "Not Survived"), c("Men", "Women")))

# Výpis matice
print(result_matrix)



############ iv ################################################################
# Vytvoření sloupcového diagramu
barplot(result_matrix, beside = TRUE, col = c("lightblue", "lightcoral"),
        legend.text = rownames(result_matrix), args.legend = list(title = "Survival"))

# Přidání popisků
title(main = "Survival of Adults on Titanic", sub = "Separated by Gender",
      xlab = "Survival Status", ylab = "Number of Individuals")



############ v #################################################################
# Uložení diagramu jako obrázku
png("titanic.png", width = 1200, height = 1200, res = 120)

# Vykreslení diagramu do aktuálního zařízení (png souboru)

barplot(result_matrix, beside = TRUE, col = c("lightblue", "lightcoral"),
        legend.text = rownames(result_matrix), args.legend = list(title = "Survival"))

# Ukončení grafického zařízení (ukončení kreslení do png souboru)
dev.off()





############ 2 uloha ###########################################################
############ i #################################################################
airquality
t = data("airquality", package = "datasets")
airquality$Date <- as.Date(paste(1973, airquality$Month, airquality$Day, sep = "-"))

############ ii ################################################################
months <- c("květen", "červen", "červenec", "srpen", "září")
airquality$Month <- factor(airquality$Month, levels = 5:9, labels = months)
airquality



############ iii ###############################################################
png("kvalita_ozonove_vrstvy.png", width = 1200, height = 600, res = 120)
boxplot(Ozone ~ Month, data = airquality, main = "Kvalita ozonové vrstvy v New Yorku (1973)", xlab = "Měsíc", ylab = "Ozone")
dev.off()



############ iv ################################################################


plot(airquality$Date, airquality$Temp, type = "l", main = "Maximální denní teploty v New Yorku (1973)", xlab = "Datum", ylab = "Teplota")
png("teploty_od_kvetna_do_zari.png", width = 1600, height = 800, res = 120)
plot(airquality$Date, airquality$Temp, type = "l", main = "Maximální denní teploty v New Yorku (1973)", xlab = "Datum", ylab = "Teplota")
dev.off()



############ 3 uloha ###########################################################
############ i #################################################################
# Vlastní Eulerova funkce
euler <- function(n) {
  result <- 0
  for (i in 1:n) {
    if (gcd(i, n) == 1) {
      result <- result + 1
    }
  }
  return(result)
}

# Funkce pro výpočet největšího společného dělitele
gcd <- function(a, b) {
  while (b != 0) {
    t <- b
    b <- a %% b
    a <- t
  }
  return(a)
}

# Vytvoření vektoru hodnot Eulerových funkcí pro prvních 500 přirozených čísel
n_values <- 1:500
euler_values <- sapply(n_values, euler)

# Vykreslení diagramu
plot(n_values, euler_values, xlab = "Přirozená čísla", ylab = "Hodnoty Eulerovy funkce", main = "Eulerova funkce pro prvních 500 přirozených čísel")



############ ii ################################################################


#Rovná čára bodů kolem vrcholu "červená" představuje všechna prvočísla.
#Prvočísla nemají žádného společného dělitele, kromě čísla 1,
#takže Fí jakéhokoliv prvočísla 'n' je (n mínus 1).


# modrá čára je n/2, protože
#Vezmeme-li například číslo 'N', tak funkce vrátí počet celých čísel, 
# které jsou menší nebo rovna 'N' a nemají společného prvočíselného dělitele 
# s 'N'. Pokud budeme chtít vědět, kolik je například Fí(8), tak se podíváme na 
# všechny hodnoty čísel od 1 do 8 a spočítáme počet celých čísel, se kterými 
# nemá 8 společného dělitele většího než 1. Vynecháme 6, neboť 6 a 8 mají 
# společného dělitele 2, ale 1, 3, 5 a 7 se započítají, protože společným 
# dělitelem je pouze 1. Proto platí Fí(8) = 4.



############ 4 uloha ###########################################################
install.packages(c("rvest", "ggplot2", "dplyr"))

library(rvest)
library(ggplot2)
library(dplyr)

url <- "https://en.wikipedia.org/wiki/Albert_Einstein"
html_content <- read_html(url)

text <- html_text(html_content)

text <- text[nchar(trimws(text)) > 0]

tokens <- unlist(strsplit(tolower(gsub("[[:punct:]]", "", text)), "\\s+"))

token_counts <- table(tokens) %>%
  sort(decreasing = TRUE) %>%
  head(40)

df <- data.frame(token = names(token_counts), count = as.numeric(token_counts))



############ i #################################################################
ggplot(df, aes(x = reorder(token, -count), y = count)) +
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "počet výskytu slova v textu")



############ ii ################################################################


# Přidání sloupce s pořadím výskytu
df$rank <- rank(-df$count)

# Vytvoření logaritmického grafu
ggplot(df, aes(x = log(rank), y = log(count))) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Přidání lineární regrese
  labs(title = "Logaritmický diagram Zipfova zákona",
       x = "Logaritmus pořadí",
       y = "Logaritmus frekvence")




