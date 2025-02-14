# Paramètres
n <- 1000        # Taille de l'échantillon pour la simulation des lois uniformes
N <- 10000       # Nombre de simulations pour le maximum

# Simulation des maxima de lois uniformes(0,1)
set.seed(123)    # fixation de l'aléa
M_n <- replicate(N, max(runif(n))) # M_n = max / X_n = runif

# Normalisation pour observer la convergence
Y_n <- n * (1 - M_n)

# Histogramme des valeurs transformées
hist(Y_n, breaks = 50, probability = TRUE, 
     col = "lightblue", border = "white",
     xlab = expression(n*(1 - M[n])))

# Densité théorique de la loi exponentielle (paramètre = 1)
curve(dexp(x, rate = 1), col = "red", lwd = 2, add = TRUE)

# Légende
legend("topright", legend = c("Simulation du max de n lois uniformes sur [0,1]", "Densité théorique loi Exp(1)"),
       fill = c("lightblue", NA), border = c("white", NA), 
       lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2))

