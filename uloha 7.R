# Najděte pomocí R největší společný dělitel a nejmenší společný násobek čísel
# 106,159 a 371.

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

# Vypsání výsledku
cat("nejmenší společný násobek:", p, "\n")

y = max(c(1:n) [k %% c(1:n) == 0 & m %% c(1:n) == 0 & n %% c(1:n) ==0])
cat("největší společný dělitel:", y , "\n") 

# nebo 



# nainstalovat balíček numbers
# install.packages("numbers")
# načíst balíček
library(numbers)
cisla = c(106, 159, 371)

gcd_value <- mGCD(cisla)
cat("největší společný dělitel:", gcd_value, "\n")
# největší společný dělitel: 53 

lcm_value <- mLCM(cisla)
cat("nejmenší společný násobek:", lcm_value, "\n")
# nejmenší společný násobek: 2226 




