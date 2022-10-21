# 2
x = 0:3
p = c(0.064, 0.288, 0.432, 0.216)
suma = sum(p)
stredni_hodnota = sum(x * p) # stredni hodnota
rozptyl = sum(x*x*p) - (stredni_hodnota)^2 # Rozpyl
smerodatna_odchylka = sqrt(rozptyl) # smerodatna odchylka
# modus je nejvetsi hodnota

# 3

# Binom X~Bi(n,pi)
#  - pi je pravdepodobnos uspechu a n pocet pokusu
# -> pocet uspechu v nezavislych n pokusech
pbinom(4, 10, 1/6) # <=
dbinom(3, 10, 1/6) # =
pbinom(4, 10, 1/6, FALSE) # >
pbinom(150, 1000, 1/5) # <=
# E(X) stredni hodnota = N * pi = 1000 * 0.2 = 200
# D(X) = sqrt(D(x))

# Hypergeometricka rozdeleni X~H(N, M, n)
#  - n prvku z N, kde M prvku ma urcitou vlastnost
# -> pocet uspechu n na zavislych pokusech
phyper(149, 2000, 8000, 1000, FALSE)

# Dyk more tahas 4 piva z basi o 20 pivech
# kdy jenom 8 jsou radky a potrebujes 4 radky
dhyper(4, 12, 8, 4)

# Negativne binomicke X~NB(k, pi)
#  - k je pocet upsechu potrebny, pi je pravdepodobnost
# -> pocet pokusu do dosazeni k uspechu
#  ! v r se zadava jenom pocet neuspesnych pokusu
pnbinom(2, 1, 0.1)
pnbinom(3, 3, 0.87) - pnbinom(0, 3, 0.87)

# Podminena pravdepodobnost
# P(X >= 230 | X >= 215)
ppr = pbinom(229, 250, 0.9, FALSE)
ppo = pbinom(214, 250, 0.9, FALSE)
ppr / ppo

# pravdepodobnost ze se poroucha za 2 mesice nejvyse 1 stroj
# X~Po(^t=4)
ppois(1,4)

# stredni pocet porouchanych stroju za rok
# 12 * 2 = 24

# pocet mikroorganizmu v 10ml vzorku
# zobecnit
x = 0:40
y = dpois(x, 20)
plot(x,y)

# jaka je pravdepodobnost ze v 10ml je vie nez 14 organizmu
ppois(14,20,F)

# Nahodna velicina popisujici delku tehotenstvi ve dnech ma stredni hodnotu 266 dnu a odchylku 16
# Nacrtnete graf hustoty
# X~N(\mu=266, \omega^2 = 16^2)
# 3 sigma aby vetsina prvku byla na grafu
x = seq(266 - 3*16, 266 + 3*16, 0.01)
y = dnorm(x, 266, 16)
plot(x,y)

# P(X <= 246)
pnorm(246, 266, 16)

# P(250 < X < 282)
pnorm(282, 266, 16) - pnorm(250, 266, 16, T)

# Jaka delka tehotenstvi je prekrocena u maximalne 10 % zen
qnorm(0.9, 266, 16)

# Urcete horni a dolni kvartil
qnorm(0.25, 266, 16)
qnorm(0.75, 266, 16)

# Uvazujte nahodnout velicinu popisujici delku remise onk pacientu
# stredni hodnota je 1 rok
# \lambda = 1/1 = 1
x = seq(0, 4, 0.01)
y = pexp(x)
plot(x, y)

# Urcete median nahodne veliciny
qexp(0.5)

# Rychlost vetru na W rozdeleni tvar 2 a meritko 7
x = seq(0, 10, 0.01)
y = pweibull(x, 2, 7)
plot(x, y)

pweibull(10, 2, 7) - pweibull(4, 2, 7)


### Ukol 3
## 1
# A

# Poas, lambda=12

#B
ppois(9,12, F)

#C
ppois(13, 12, F) / ppois(10, 12, F)

## 2
# A
x = seq(0, 2000, 0.1)
y = pexp(x, 1/800)
plot(x, y)

x = seq(0, 2000, 0.1)
y = dexp(x, 1/800)
plot(x, y)

# B
pexp(900, 1/800, F)

#C
qexp(0.75, 1/800)
