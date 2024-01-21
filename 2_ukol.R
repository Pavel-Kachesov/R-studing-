#1 uloha 
#i
A = matrix(c(2, 4, 1, 3, 3, 0, -1, 2, -1), nrow = 3)

det_A = det(A)
cat("Determinant matice A:", det_A, "\n")

inv_A = solve(A)
cat("Inverzní matice k matici A:\n")
print(inv_A)

#ii

B = matrix(c(1, 2, 4, 1), nrow = 2)

eigenvalues = eigen(B)$values
cat("Vlastní čísla matice B:", eigenvalues, "\n")

B_trans = t(B)
cat("Transponovaná matice k matici B:\n")
print(B_trans)

#iii

C = matrix(c(2, 4, 1, 0, -2, 1), nrow = 2)
D = matrix(c(2, 3, 2, 3, 2, -1), nrow = 3)

#  C · D
CD = C %*% D
cat("Součin matic C · D:\n")
print(CD)

#  D · C
DC = D %*% C
cat("Součin matic D · C:\n")
print(DC)

# Hadamardův součin součin  C a  D
C_DT = C * t(D)
cat("Hadamardův součin C a D:\n")
print(C_DT)

#iv
# Definice matice soustavy rovnic
A = matrix(c(1, -2, 2, 3, 1, 1, -1, 2, 5), nrow = 3, byrow = TRUE)

# Pravá strana soustavy rovnic
B = c(-5, 4, -2)

solution = solve(A, B)

cat("Řešení soustavy rovnic:\n")
print(solution)

#v

E = matrix(c(2, 3, -1, 4, 3, 1, 1, 0, 1), nrow = 3, byrow = TRUE)
rank_E = qr(E)$rank
cat("Hodnost matice E:", rank_E, "\n")

ln = length(unique(qr(E)$pivot))
cat("Počet navzájem lineárně nezávislých sloupců matice E:", ln, "\n")



########################################################
#2 uloha

#i
A = matrix(letters[1:12] , nrow = 3, byrow = TRUE)
rownames(A) = c("r_1", "r_2", "r_3")
colnames(A) = c("c_1", "c_2", "c_3", "c_4")
print("Matice A:")
print(A)

# Odstranění názvů řádků a sloupců z matice A
rownames(A) = NULL
colnames(A) = NULL
print("Matice A bez názvů řádků a sloupců:")
print(A)

#ii

my_data = mtcars
my_data = rbind(my_data, rep(0, dim(my_data)[2]))
rownames(my_data)[nrow(my_data)] = "xyz"
my_data = data.frame(my_data, "posledni_sloupec" = rep(1, dim(my_data)[1]))
print("Nový dataset:")
print(my_data)


#iii

# Přetypování sloupce "am" na znakový typ
my_data$am = as.character(my_data$am)
my_data$am[my_data$am == "0"] = "automatic"
my_data$am[my_data$am == "1"] = "manual"
print("Nová tabulka:")
print(my_data)


#iv 


my_data = rbind(my_data, rep(NA, dim(my_data)[2]))
rownames(my_data)[nrow(my_data)] = "poslední_řádek"
my_data = my_data[rownames(my_data) != "xyz", ]
prumery = colMeans(my_data[, 1:7], na.rm = TRUE)
my_data
cat("Průměry prvních sedmi sloupců:", prumery, "\n")


#v
# Načtení vestavěného datasetu airquality
data(airquality)

# Průměrná kvalita ozonu pro květen
prumer_ozon_kveten = mean(airquality$Ozone[airquality$Month == 5], na.rm = TRUE)

# Průměrná kvalita ozonu pro září
prumer_ozon_zari = mean(airquality$Ozone[airquality$Month == 9], na.rm = TRUE)

# Průměrná teplota za prvních 14 dní července
prumer_teplota_cervenec = mean(airquality$Temp[airquality$Month == 7 & airquality$Day <= 14], na.rm = TRUE)

# Pearsonův korelační koeficient mezi kvalitou ozonu a teplotou pro celý dataset
korelace = cor(airquality$Ozone, airquality$Temp, use = "complete.obs")

# Výpis výsledků
cat("Průměrná kvalita ozonu v květnu:", prumer_ozon_kveten, "\n")
cat("Průměrná kvalita ozonu v září:", prumer_ozon_zari, "\n")
cat("Průměrná teplota za prvních 14 dní července:", prumer_teplota_cervenec, "\n")
cat("Pearsonův korelační koeficient mezi ozonem a teplotou:", korelace, "\n")




#############################################################################
#3 uloha 

m = 320
n = 120  
ph = 100  # požadovaná hodnost
# Generování matice s požadovanou hodností
M = matrix(0, nrow = m, ncol = n)
for (i in 1:min(m, n, ph)) {
  M[i, i] = 1
}
# Určení skutečné hodnosti matice
sh = qr(M)$rank
# Výpis informací
cat("Požadovaná hodnost matice M:", ph, "\n")
cat("Skutečná hodnost matice M:", sh, "\n")


