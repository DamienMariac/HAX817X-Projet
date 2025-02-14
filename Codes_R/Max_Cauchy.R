set.seed(42)  # Pour la reproductibilité
N <- 10000  # Nombre de simulations
n <- 1000  # Taille des échantillons

# Génération des maximums
M_n <- apply(matrix(rcauchy(n * N), nrow = n), 2, max)

# Normalisation avec a_n = n et b_n = πn
Y_n <- (M_n - pi * n) / n

# Tracé de l'histogramme avec comparaison à la loi limite
hist(Y_n, probability = TRUE, breaks = 50, col = "lightblue",
     main = "Max de 1000 lois de Cauchy (normalisé)",
     xlab = "Valeurs normalisées",ylim=c(0,0.2), xlim=c(0,100))

# Ajouter la densité limite (loi de Fréchet)
curve(exp(-x) * exp(-exp(-x)), col = "red", lwd = 2, add = TRUE)
