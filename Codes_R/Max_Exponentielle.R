# Paramètres 
n <- 1000 # taille de l'échantillon
N <- 10000 # nombre de simulations pour le max

set.seed(123)
M_n <- replicate(N, max(rexp(n, rate = 1)))

# Normalisation
b_n = log(n)
a_n = 1
Y_n <- (M_n - b_n) / a_n

hist(Y_n, breaks = 50, probability = TRUE, 
     col = "lightblue", border = "white", 
     main = paste("Max de", n, "lois exponentielles"), ylab="Densité",
     xlab = "Y_n", xlim = c(-2, 6))

gumbel <- function(x) exp(-(x + exp(-x)))  # Densité de Gumbel standard
curve(gumbel, from = -2, to = 6, col = "red", lwd = 2, add = TRUE)

# Légende
legend("topright", legend = c("Simulation", "Densité théorique : Gumbel(0,1)"),
       fill = c("lightblue", NA), border = c("white", NA), 
       lty = c(NA, 1), col = c(NA, "red"), lwd = c(NA, 2))
