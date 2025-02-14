# Paramètres 
n <- 1000 # taille de l'échantillon
N <- 10000 # nombre de simulations pour le max

set.seed(67983)
M_n <- replicate(N, max(rnorm(n, mean = 0, sd = 1)))

# Normalisation 
b_n <- sqrt(2 * log(n)) - (log(log(n)) + log(4 * pi)) / (2 * sqrt(2 * log(n)))
a_n <- 1/sqrt(2*log(n))

Y_n <- (M_n - b_n) / a_n

# Histogrammes
hist(Y_n, breaks = 50, probability = TRUE, 
     col = "lightblue", border = "white", 
     main = paste("Max de", n, "lois normales (centrées-réduites)"),
     xlab = "Valeurs centrées-réduites", ylim = c(0, 0.4))

gumbel <- function(x) exp(-(x + exp(-x)))  # Densité de Gumbel standard
curve(gumbel, col = "red", lwd = 2, add = TRUE)

