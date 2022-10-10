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
