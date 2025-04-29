# PACKAGES
library(evd)
library(ggplot2)

# GPD SIMULEE
n <- 10000
gamma_true <- 0.2
data_gpd <- rgpd(n = n, loc = 0, scale = 1, shape = gamma_true) # "boite noire" du package evd qui génère des GPD

# ESTIMATEURS
# Hill
hill <- function(data_sorted, k) {
  n <- length(data_sorted)
  X <- data_sorted
  gamma_hat <- mean(log(X[(n-k+1):n]) - log(X[n-k]))
  return(gamma_hat)
}

# Pickands
pickands <- function(data_sorted, k) {
  n <- length(data_sorted)
  X <- data_sorted
  
  if (k/4 < 1) {
    stop("k est trop petit pour Pickands (k doit être au moins 4)")
  }
  
  x1 <- X[n - k + 1]      
  x2 <- X[n - k%/%2 + 1] 
  x4 <- X[n - k%/%4 + 1]   
  
  gamma_hat <- (1 / log(2)) * log((x2 - x1) / (x4 - x2))
  return(gamma_hat)
}

# CALCUL
data_sorted <- sort(data_gpd)
k_values <- 20:1000
hill_vals <- sapply(k_values, function(k) hill(data_sorted, k))
pick_vals <- sapply(k_values, function(k) pickands(data_sorted, k))

# DATAFRAME POUR PLOT
df <- data.frame(
  k = rep(k_values, 2),
  estimateur = factor(rep(c("Hill", "Pickands"), each = length(k_values))),
  gamma_hat = c(hill_vals, pick_vals)
)

# GRAPHE
ggplot(df, aes(x = k, y = gamma_hat, color = estimateur)) +
  geom_line() +
  geom_point(size = 1) +
  geom_hline(yintercept = gamma_true, linetype = "dashed", color = "black") +
  labs(title = "Estimation de Gamma en fonction de k",
       x = "k (nombre d'observations extrêmes)",
       y = "Estimation") +
  scale_color_manual(values = c("darkblue", "red")) +
  theme_minimal(base_size = 14) +
  theme(legend.title = element_blank())


# léger biais de Hill pour les petites valeurs de k puis s'écarte petit à petit
# mauvaise estimation générale de Pickands, quelle que soit la valeur de k
