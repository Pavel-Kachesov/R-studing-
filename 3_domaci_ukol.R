#1 uloha  #################################################

#i #############

d = read.table("fidji_earthquakes.csv", sep = ";", header = TRUE)
d
#ii #############
png(file = "zemetreseni_fidji.png",
    width = 6,
    height = 6, 
    units = "in",
    res = 600
)

da = lapply(d, function(x) gsub(",", ".", x))

plot(
  formula = lat ~ long,
  data = da,
  xlab = "zeměpisná délka",
  ylab = "zeměpisná šířka",
  main = "diagram geografického rozložení zemětřesení v oblasti Fidji"
)

ox = da$long
oy = da$lat

library(maps)
par(mar = c(4.1, 4.1, 0.1, 0.1))
maps::map(
  xlim = c(105, 205), 
  ylim = c(-50, 10), 
  fill = TRUE,
  col = "lightgrey")

points(
  x = ox,
  y = oy,
  col = "red",
  pch = 20
)
axis(
  side = 1,
  at = seq(105, 195, 10),
  labels = seq(105, 195, 10)
)
axis(
  side = 2,
  at = seq(-50, 10, 10),
  labels = seq(-50, 10, 10)
)
mtext(
  text = "zeměpisná šířka [°]", side = 1,line = -1, adj = 0.90 , cex = 0.8)
mtext(
  text = "zeměpisná délka [°]", side = 2,
  line = -1,
  adj = 1.00,
  cex = 0.8
)

dev.off()



#iii ##########

d = subset(d, mag >= 5)
write.table(x = d, sep = ";", row.names = FALSE, file = "vyznamna_zemetreseni_fidji.csv"
)


#2 uloha #############################

#i ###########

cisla = scan("cisla.txt")

cisla

#ii###########


pd <- function(x) {
  delitele <- 1:x
  delitelnost <- x %% delitele
  pocet_lichych_delitelu <- sum(delitelnost == 0)
  return(pocet_lichych_delitelu)
}
pdc <- sapply(cisla, pd)
lichy <- cisla[pdc %% 2 == 1]
lichy
write.table(lichy, "cisla_s_lichym_poctem_delitelu.txt", col.names = FALSE)


#3 uloha ############################

#i###########

air = read.table("airquality.csv", sep = ";", header = TRUE)
air

#ii##########


air= subset(air, Temp >= 80)
air

write.table(x = air, sep = ";", row.names = FALSE, file = "kvalita_ovzdusi_pri_vyssich_teplotach.csv"
)