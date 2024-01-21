#1 úloha

x = 1:1000
sum(x)
#[1] 500500





#2 úloha

x = 1:999
sum(x[x %% 2 != 0])
#[1] 250000





#3 úloha 

x = c(4, 6, 76, 35, 72, 28, 93, 352, 53, 63, 257, 35, 24, 34, 345, 37,263, 86, 178, 83, 53, 71, 59)
x[c(F, T, F, F, T)]
#[1]   6  72  93  63  35 345 263  83  71





#4 úloha 

# Odpověd': Čísla, která tento kód vrací, jsou všechny kladné celočíselné dělitele čísla 24.
print("Čísla, která tento kód vrací, jsou všechny kladné celočíselné dělitele čísla 24.")





#5 úloha 

n = 1206660
c(1:n)[n %% c(1:n) == 0]





#6 úloha

n = 19937
k = c(1:n)[n %% c(1:n) == 0]
print("Ano tohle je prvočíslo, protože celočíselně dělitelné jenom jedničkou a sebou:", k)
#[1]     1 19937





#7 uloha

n = 106
m = 159
k = 371

del = function(n) {
  x = c(1:n) [n %% c(1:n) == 0]
  x[1:(length(x)-1)]
  }

p = c(del(n), del(m), del(k))
p = unique(p)
p = prod(p)
print("nejmenší společný násobek:")
p

y = max(c(1:n) [k %% c(1:n) == 0 & m %% c(1:n) == 0 & n %% c(1:n) ==0])
print("největší společný dělitel:")
y


# nebo 


# nainstalovat balíček numbers
# install.packages("numbers")
# načíst balíček
library(numbers)
cisla = c(106, 159, 371)

nsd = mGCD(cisla)
print("největší společný dělitel:")
nsd
# největší společný dělitel: 53 

nsn = mLCM(cisla)
print("nejmenší společný násobek:")
nsn
# nejmenší společný násobek: 2226 





#8 uloha 

#Je dána množina čísel M={3,5,7,8,13,17,31}. Najděte pomocí R všechna čísla 
# nepřevyšující 1000000 taková, že nejsou dělitelná ani jedním z čísel množiny M.
nd = function(d){
  n = c(1:1000000)
  n %% d != 0
}
n = c(1:1000000)
n[nd(3) & nd(5) & nd(7) & nd(8) & nd(13) & nd(17) & nd(31)]




      
#9 uloha 

#Najděte pomocí R počet všech přirozených čísel n nepřevyšujících 1000 takových,
# že číslo ⌊3√n⌋ je dělitelem čísla n. Symbol ⌊x⌋ značí dolní celou část reálného
# čísla x, tedy nejvyšší celé číslo nepřevyšující číslo x; 
# formálně ⌊x⌋= max{z∈Z:z≤x}, kde x∈R

n = c(1:1000)
x = n[n*n*n <= 1000 & n %% 2 == 0]
y = length(x)
cat("Počet všech přirozených čísel n nepřevyšujících 1000 takových,
že číslo ⌊3√n⌋ je dělitelem čísla n:", y)

#Počet všech přirozených čísel n nepřevyšujících 1000 takových,že číslo ⌊3√n⌋ je dělitelem čísla n: 5





#10 uloha 

#Určete pomocí R počet všech navzájem různých obdélníků s obvodem rovným 
#200 takových, že mají všechny strany celočíselné a že jejich obsah je větší 
#než 1000, ale menší než 2000.
a = c(1:50)
b = 100 - a 
x = a[a*b<2000]
y = a[a*b>1000]
z = length(intersect(x, y))
cat("počet všech navzájem různých obdélníků:", z)




#11 uloha 

#Najděte pomocí R taková dvě přirozená čísla a a b, aby platilo a+b=2020 
#a současně součin √a·3√b2 byl největší možný.
a = c(0:2020)
b = 2020 - a 
max((a[b^(2/3)])*(a[a^(1/2)]))

#[1] 3080





#12 uloha

a = c(0:2020)
b = 2020 - a
x = a[a + a * b + a * b^2 + b^2 <= 2020^2]
y = 2020 - x
x
y





#13 uloha 

#i)Najděte všechna přirozená čísla nepřevyšující 1000, 
#která mají právě 24 kladných celýchdělitelů.
isEven = function(n){sum(n %% 1:n == 0) == 24}
c(1:1000)[unlist(lapply(c(1:1000), isEven))]



#(ii) Které přirozené číslo nepřevyšující 1000 
#má nejvíce kladných celých dělitelů?
isEven = function(n){sum(n %% 1:n == 0)}
x = c(1:1000)[unlist(lapply(c(1:1000), isEven))]
isEven = function(n){sum(n %% 1:n == 0) == max(x)}
c(1:1000)[unlist(lapply(c(1:1000), isEven))]





#14 uloha 

x = unlist(
  lapply(
    seq(
      as.Date("1900-01-01"),
      as.Date("1999-01-01"),
      by= "year"
      ),
    "weekdays"
    )
  )

table(x)





#15 uloha 

y = unlist(
  lapply(
    seq(
      as.Date("1910-01-01"),
      as.Date("2100-01-01"),
      by= "year"
    ),
    "weekdays"
  )
)

x = c(1910:2100)

my_table = data.frame(rok = x, den = y)

my_table = subset(my_table, rok %% 4 != 0)

my_table = subset(my_table, den == "Saturday")

my_table 

# nejblizsi rok 2033
