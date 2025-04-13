# Paramètres 
n <- 1000  # Taille de l'échantillon
N <- 10000 # Nombre de simulations pour le max

set.seed(123)
M_n <- replicate(N, max(rcauchy(n,0,1)))

# Normalisation correcte pour une loi de Cauchy
b_n <- n
a_n <- pi
Y_n <- M_n*a_n / b_n  # Centrage-réduction

# Histogramme
hist(Y_n, breaks = 50000, probability = TRUE, 
     col = "lightblue", border = "white", 
     main = paste("Max de", n, "lois de Cauchy"), ylab="Densité",
     xlab = "Y_n",ylim = c(0,0.6), xlim = c(0, 10))

# Densité limite de Fréchet (alpha = 1)
alpha <- 1
frechet <- function(x) alpha * exp(-x^(-alpha)) * x^(-1 - alpha)
curve(frechet, from = 0, to = 10, col = "red", lwd = 2, add = TRUE)

# Légende
legend("topright", legend = c("Simulation", "Densité théorique : Fréchet(1)"),
       fill = c("lightblue", NA), border = c("white", NA), 
       lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2))

