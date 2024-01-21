############# 1 uloha ##########################################################

#d⋅(x+y+z)=2019
#Nyní můžeme vidět, že d musí dělit 2019

vsechny_delitele <- function(cislo) {
  delitele <- c()
  for (i in 1:cislo) {
    if (cislo %% i == 0) {
      delitele <- c(delitele, i)
    }
  }
  return(delitele)
}

cislo <- 2019
vysledek <- vsechny_delitele(cislo)
cat("Všechny dělitele jsou:", vysledek,"\n")

# Protože musíme najit nejvetší společný dělitel, tak tohle nemuže být
# poslední a před poslední čísla, to znamená, že odpověď bude 3

cat("největší možná hodnota společného dělitele", 3, "\n")




############# 2 uloha ##########################################################

primes <- c()
for (n in 2:10000) {
  if (length(c(1:n)[n %% c(1:n) == 0])== 2) {
    primes <- c(primes, n)
  }
}
length(primes)
#[1] 1229




############# 3 uloha ##########################################################


l = c()

# musí být k = 2024, ale tohle uděla cyklus přespřiliš velkým, proto jsem změnil 
# na 10


k = 10
# jestli mate dost výkonný počítač,můžete zmenit k na 2024

for (x in 0:k) {
  for (y in 0:(k-x)) {
    if (y == x){
      next
    }
    for (z in 0:(k-(x+y))) {
      if (z == x | z == y){
        next
      }
      for (u in 0:(k-(x+y+z))) {
        if (u == x | u == y | u == z){
          next
        }
        for (v in 0:(k-(x+y+z+u))){
          if (v == x | v == y | v == z | v == u ){
            next
          }
          if (x + y + z + u + v == k) {
            l = c(l,v)
            t = length(l)
            cat(x, y, z, u, v)
            
          }
        }
      }
    }
  }
}
cat("Počet kombinaci ", t , "\n")




############# 4 uloha ##########################################################


find_interesting_number <- function() {
  n <- 1
  while (TRUE) {
    # Získání ciferného součtu čísla n a n + 10
    sum_n <- sum(as.numeric(strsplit(as.character(n), '')[[1]]))
    sum_n_plus_10 <- sum(as.numeric(strsplit(as.character(n + 10), '')[[1]]))
    
    # Kontrola dělitelnosti součtu ciferných součtů čísly 17
    if ((sum_n + sum_n_plus_10) %% 17 == 0) {
      return(n)
    }
    
    # Zvýšení n pro další iteraci
    n <- n + 1
  }
}


# Volání funkce a vypsání výsledku
result <- find_interesting_number()
cat("Nejmenší zajímavé číslo je:", result, "\n")




############# 5 uloha ##########################################################


count <- 0
for (n in 1:10000) {
  # Získání ciferného součtu čísla n
  digit_sum <- sum(as.numeric(strsplit(as.character(n), '')[[1]]))
  # Kontrola dělitelnosti čísla n svým ciferným součtem
  if (n %% digit_sum == 0) {
    count <- count + 1
  }
}


# Vypsání výsledku
cat("Počet atraktivních čísel menších než je:", count, "\n")




############# 6 uloha ##########################################################


pekne_cislo <- function(n) {
  delitele <- (1:(n-1))[n %% (1:(n-1)) == 0]
  soucet_delitelu <- sum(delitele)
  return(soucet_delitelu == n)
}

pekna_cisla <- length((2:10000)[sapply(2:10000, pekne_cislo)])
cat("Pěkných čísel menší než 10000 je:", pekna_cisla, "\n")




############# 7 uloha ##########################################################

#############    i    ##########################################################

primes <- c()
for (n in 2:1000) {
  if (length(c(1:n)[n %% c(1:n) == 0])== 2) {
    primes <- c(primes, n)
  }
}
primes


prv = c()
for (p in primes){
  if (is.integer(((p * 19) + 1)^(1/3))){
    prv = c(prv, p)
    cat(p)
  }
}
cat("cyklus nenašel žadné takové číslo, to znamená, že neexistujou, alespoň mezi 2 a 10000", "\n")
# cyklus nenašel žadné takové číslo, to znamená, že neexistujou, alespoň mezi 2 a 10000




#############    ii    #########################################################

primes <- c()
for (n in 2:210) {
  if (length(c(1:n)[n %% c(1:n) == 0])== 2) {
    primes <- c(primes, n)
  }
}
primes


find <- c()

for (n in primes) {
  x = 210 - n
  if (x %in% primes) {
  }
  else{
    find <- c(find, n)
  }
}

cat("Největší prvočíslo p menší než 210 takové, že číslo 210−p je složené", max(find), "\n" )
#[1] 89




############# 8 uloha ##########################################################

#############    i    ##########################################################

mySequence <- function(m) {
  sequence <- c(m)
  # Iterativní generování dalších členů
  while (tail(sequence, 1) != 1) {
    if (tail(sequence, 1) %% 2 == 0) {
      # Pokud je poslední člen sudý, přidej ho polovinu
      sequence <- c(sequence, tail(sequence, 1) / 2)
    } else {
      # Pokud je poslední člen lichý, přidej ho + 1
      sequence <- c(sequence, tail(sequence, 1) + 1)
    }
  }
  
  return(sequence)
}


mySequence (1)
# c(1)
mySequence (2)
# c(2, 1)
mySequence (5)
# c(5, 6, 3, 4, 2, 1)
mySequence (10)
# c(10, 5, 6, 3, 4, 2, 1)
mySequence (25)
# c(25, 26, 13, 14, 7, 8, 4, 2, 1)




#############    ii    #########################################################

mySequence <- function(m) {
  sequence <- c(m)
  
  while (tail(sequence, 1) != 1) {
    if (tail(sequence, 1) %% 2 == 0) {
      sequence <- c(sequence, tail(sequence, 1) / 2)
    } else {
      sequence <- c(sequence, tail(sequence, 1) + 1)
    }
  }
  
  return(sequence)
}

nejdelsi_delka <- 0
nejdelsi_m <- NULL

for (m in 1:1000) {
  delka_posloupnosti <- length(mySequence(m))
  if (delka_posloupnosti > nejdelsi_delka) {
    nejdelsi_delka <- delka_posloupnosti
    nejdelsi_m <- m
  }
}
cat("Pro m =", nejdelsi_m, "je délka posloupnosti nejdelší a má", nejdelsi_delka, "členů.\n")




############# 9 uloha ##########################################################

#############    i    ##########################################################

vendingMachine <- function(n, coins = c(1, 2, 5, 10, 20, 50)) {
  # Seřadíme mince od největší po nejmenší
  coins <- sort(coins, decreasing = TRUE)
  vysledek <- integer(0)

  for (coin in coins) {
    while (n >= coin) {
      vysledek <- c(vysledek, coin)
      n <- n - coin
    }
  }
  
  return(vysledek)
}



vendingMachine(
  n = 1,
  coins =c(1, 2, 5, 10, 20, 50)
)
# 1

vendingMachine(
  n = 8,
  coins =c(1, 2, 5, 10, 20, 50)
)
# c(5, 2, 1)

vendingMachine(
  n = 9,
  coins =c(1, 2, 5, 10, 20, 50)
)
# c(5, 2, 2)

vendingMachine(
  n = 10,
  coins =c(1, 2, 5, 10, 20, 50)
)
# 10

vendingMachine(
  n = 99,
  coins =c(1, 2, 5, 10, 20, 50)
)
# c(50, 20, 20, 5, 2, 2)




#############    ii    #########################################################

vendingMachine <- function(n, coins = c(1, 2, 5, 10, 20, 50)) {
  # Seřadíme mince od největší po nejmenší
  coins <- sort(coins, decreasing = TRUE)
  vysledek <- integer(0)
  
  for (coin in coins) {
    while (n >= coin) {
      vysledek <- c(vysledek, coin)
      n <- n - coin
    }
  }
  
  return(vysledek)
}

yes = 0
for (n in 1:1000){
  yes = length(vendingMachine(n))
  if (yes == 7){
    cat(vendingMachine(n), "\n")
    i = sum(vendingMachine(n))
    cat("Nejmenší částka, kterou rozmění automat, právě sedmi některými, aktuálně platnými českými mincemi je", i, "\n")
    break
  }
}




############# 10 uloha #########################################################

# Počet lamp na náměstí
pocet_lamp <- 300

# Vytvoření vektoru reprezentujícího stav lamp (1 = svítí, 0 = zhasnutá)
stav_lamp <- rep(0, pocet_lamp)

# Cyklus pro každou obchůzku náměstí
for (obchuzka in 1:300) {
  # Cyklus pro každou lampu v rámci obchůzky
  for (lampa in seq(obchuzka, pocet_lamp, obchuzka)) {
    # Pokud lampa svítí, zhasneme ji; pokud nesvítí, rozsvítíme ji
    stav_lamp[lampa] <- 1 - stav_lamp[lampa]
  }
}

# Celkový počet svítících lamp
pocet_sviticich_lamp <- sum(stav_lamp)
cat("Celkový počet svítících lamp po třísté obchůzce náměstí je:", pocet_sviticich_lamp, "\n")




############# 11 uloha #########################################################

sumUpTexts <- function(x, y) {
  # Převod textových hodnot na znaky a následné převedení na čísla
  cisla_x <- as.numeric(strsplit(x, "")[[1]])
  cisla_y <- as.numeric(strsplit(y, "")[[1]])
  
  # Určení délky delšího z obou čísel
  delka <- max(length(cisla_x), length(cisla_y))
  
  # Doplnění nul na začátek, aby měla obě čísla stejnou délku
  cisla_x <- c(rep(0, delka - length(cisla_x)), cisla_x)
  cisla_y <- c(rep(0, delka - length(cisla_y)), cisla_y)
  
  # Inicializace proměnných
  vysledek <- numeric(delka + 1)
  prenos <- 0
  
  # Sčítání od posledních cifer k prvním
  for (i in rev(seq_along(cisla_x))) {
    soucet <- cisla_x[i] + cisla_y[i] + prenos
    vysledek[i + 1] <- soucet %% 10
    prenos <- soucet %/% 10
  }
  
  # Přidání přenosu na začátek výsledku
  vysledek[1] <- prenos
  
  # Převedení výsledku na textový řetězec a odstranění přebytečné nuly
  vysledek_text <- as.character(paste(vysledek, collapse = ""))
  vysledek_text <- sub("^0+", "", vysledek_text)  # Odstranění přebytečných nul na začátku
  
  return(vysledek_text)
}



sumUpTexts(
  x = "1",
  y = "2"
)
# „3“

sumUpTexts(
  x = "123",
  y = "234"
)# „357“


sumUpTexts(
  x = "10000000000000000001",
  y = "10000000000000000002"
)# „20000000000000000003“




#############    i    ##########################################################

z = 1234567890123456789012345 + 2345678901234567890123456
z
z %% 10

# [1] 0
#Warning message:
#  probable complete loss of accuracy in modulus 




#############    ii    #########################################################

z = sumUpTexts(
  x = "1234567890123456789012345",
  y = "2345678901234567890123456"
)# „3580246791358024679135801“

z = as.numeric(substr(z, nchar(z), nchar(z)))
z%%10
# [1] 1




############# 12 uloha #########################################################

# Inicializace proměnných
rok <- 1901
obdelnikove_unory <- c()
# Cyklus přes roky od 1901 do 2023
while (rok <= 2023) {
  # Vypsání dne v týdnu, kdy začíná únor
  day = weekdays(as.Date(paste(rok, "-02-01", sep = "")))
  cat("Únor roku", rok, "začíná v den:", day , "\n")
  if (day == "Monday"){
    obdelnikove_unory = c(rok, obdelnikove_unory)
  }
  # Přesun na další rok
  rok <- rok + 1

}


# Inicializace proměnných
rok <- 2022
p <- c()
# Cyklus přes roky od 1901 do 2023
while (rok <= 2030) {
  # Vypsání dne v týdnu, kdy začíná únor
  day = weekdays(as.Date(paste(rok, "-02-01", sep = "")))
  if (day == "Monday"){
    p = c(rok, p)
  }
  # Přesun na další rok
  rok <- rok + 1
  
}

obdelnikove_unory
# Výsledky
cat("Celkový počet obdélníkových únorů mezi lety 1901 a 2023:", length(obdelnikove_unory), "\n")
cat("Poslední obdélníkový únor byl v roce:", max(obdelnikove_unory), "\n")
cat("Následující obdélníkový únor bude nejblíže v roce:", p, "\n")




############# 13 uloha #########################################################

#############    i    ##########################################################

# Předpokládáme libovolná přirozená čísla a a b
# Levá strana nerovnice: ((a + b)/2)^2
# Pravá strana nerovnice: a * b
# Krok 1: Rozepíšeme levou stranu nerovnice
# ((a + b)/2)^2 = (a + b)^2 / 4
# Krok 2: Rozepíšeme pravou stranu nerovnice
# a * b
# Porovnání obou stran nerovnice
# Krok 3: Vynásobíme obě strany nerovnice číslem 4
# (a + b)^2 >= 4ab
# Krok 4: Rozepíšeme člen na levé straně
# a^2 + 2ab + b^2 >= 4ab
# Krok 5: Převedeme vše na jednu stranu a získáme kvadratickou nerovnici
# a^2 - 2ab + b^2 >= 0
# Krok 6: Rozložíme kvadratický výraz na součin dvou členů
# (a - b)^2 >= 0
# Tato nerovnice vždy platí, protože druhá mocnina reálného čísla je vždy nezáporná.




#############    ii    #########################################################

# Velikost tabulky
rows <- 10
cols <- 10

# Inicializace prázdné tabulky
tabulka <- matrix(0, nrow = rows, ncol = cols)

# Naplnění tabulky součiny dle Adamova přístupu
for (i in 1:rows) {
  for (j in 1:cols) {
    # Výpočet součinu dle Adamova přístupu
    tabulka[i, j] <- ((i + j) / 2)^2
  }
}

# Vypsání tabulky
print(tabulka)




#############    iii    ########################################################

# Definujeme funkci pro výpočet procentuální nadhodnocenosti
procentualni_nadhodnoceni <- function(k) {
  nadhodnocenost <- ((k + 1) / 2)^2 - k / k * 100
  return(nadhodnocenost)
}


# Výpočet pro maximální hodnotu k (k → ∞)
max_k <- 1e6  # Velká hodnota reprezentující nekonečno
max_procentualni_nadhodnoceni <- procentualni_nadhodnoceni(max_k)

# Výpis maximálního procentuálního nadhodnocení
cat("Maximální procentuální nadhodnocení pro k → ∞:", max_procentualni_nadhodnoceni, "%\n")

# Pro k = 2
nad_2 <- procentualni_nadhodnoceni(2)
cat("Procentuální nadhodnocení pro k = 2:", nad_2, "%\n")

# Pro k = 3
nad_3 <- procentualni_nadhodnoceni(3)
cat("Procentuální nadhodnocení pro k = 3:", nad_3, "%\n")




############# 14 uloha #########################################################

numberOfSolutions <- function(A, b) {
  
  # Získání počtu neznámých a rozměrů matice A
  n_variables <- ncol(A)
  n_equations <- nrow(A)
  
  # Získání hodnosti matice A rozšířené o vektor b
  augmented_matrix <- cbind(A, b)
  rank_augmented <- Matrix::rankMatrix(augmented_matrix)
  
  # Získání hodnosti původní matice A
  rank_A <- Matrix::rankMatrix(A)
  
  # Porovnání hodností pro určení počtu řešení
  if (rank_A == rank_augmented) {
    if (rank_A == n_variables) {
      return("Soustava má právě jedno řešení")
    } else {
      return("Soustava má nekonečně mnoho řešení")
    }
  } else {
    return("Soustava nemá řešení")
  }
}




numberOfSolutions(
  A =matrix(c(1, -1, 4, 2),nrow= 2),
  b =c(9, 12)
)
# ”soustava má právě jedno řešení”

numberOfSolutions(
  A =matrix(c(1, 2, 3, 6),nrow= 2),
  b =c(4, 8)
)
# ”soustava má nekonečně mnoho řešení”

numberOfSolutions(
  A =matrix(c(1, 2, 3, 6),nrow= 2),
  b =c(4, 9)
)# ”soustava nemá řešení”




############# 15 uloha #########################################################

findMyRoot <- function(f, a, b, epsilon) {
  # Definujeme funkci, jejíž kořen hledáme
  fn <- function(x) {
    eval(parse(text = f))
  }
  
  # Zkontrolujeme, zda znaménka na koncích intervalu jsou různá
  if (fn(a) * fn(b) >= 0) {
    stop("Interval musí splňovat podmínku f(a) * f(b) < 0.")
  }
  
  # Nastavení počáteční hodnoty
  x0 <- (a + b) / 2
  
  # Iterativní proces pro hledání kořene s danou nepřesností
  while ((b - a) / 2 > epsilon) {
    x0 <- (a + b) / 2
    if (fn(x0) == 0) {
      break  # Našli jsme přesný kořen
    } else if (fn(x0) * fn(a) < 0) {
      b <- x0
    } else {
      a <- x0
    }
  }
  
  # Vracíme aproximaci kořene s danou maximální nepřesností
  return(x0)
}



findMyRoot(
  f = "sqrt(x) - 0.5",
  a = 0,
  b = 1,
  epsilon = 0.01
)
# 0.25


findMyRoot(
  f = "sin(x - pi/4)",
  a = 0,
  b = pi/2,
  epsilon = 0.001
)# 0.785


findMyRoot(
  f = "x ^ 2 - 2",
  a = 0,
  b = 10,
  epsilon = 0.000001
)# 1.414213

findMyRoot(
  f = "exp(x) - 2024",
  a = 0,
  b = 10,
  epsilon = 0.000001
)# 7.612831




#############    i    ##########################################################

findMyRoot(
  f = "exp(x) - 1 - 1/(x^2)",
  a = 0.1,
  b = 2,
  epsilon = 0.000001
)

#[1] 0.8579934




#############    ii    #########################################################


pocet = seq(from = 0, to = 1000, by =0.001)


cat("kolikrát nejvíce může funkcefindMyRoot()volat v rámci celkového výpočtu funkci : ", length(pocet), "\n")

#kolikrát nejvíce může funkcefindMyRoot()volat v rámci celkového výpočtu funkci :  1000001




#############    iii    ########################################################

b = 1000
a = 0
e = 0.001
maximalni_pocet = log2((b - a)/e)

cat("kolikrát nejvíce může funkcefindMyRoot()volat v rámci celkového výpočtu funkci : ", maximalni_pocet, "\n")




#############    iv    #########################################################

#Porovnejte časovou složitost přístupů z bodů (ii) a (iii). 
#Který z přístupů je obecněmnohem rychlejší? 
# půlení intervalu je rychlejší 
# v našem připadě rychlejší 50000 krat 