library(evmix)
library(actuar)
set.seed(1234)

n <- 100000
valeurs_k <- 100:floor(n / 4)

lois <- list(
  "Pareto (forme = 2)" = list(
    donnees = function(n) actuar::rpareto(n, shape = 2, scale = 1),
    gamma = 0.5
  ),
  "Exponentielle" = list(
    donnees = function(n) rexp(n), 
    gamma = 0
  ),
  "Uniforme [0,1]" = list(
    donnees = function(n) runif(n), 
    gamma = -1
  ),
  "Cauchy" = list(
    donnees = function(n) rcauchy(n), 
    gamma = 1
  )
)

estimateur_pickands <- function(donnees, valeurs_k, titre, gamma_theorique) {
  donnees_tries <- sort(donnees, decreasing = TRUE)
  
  estimations <- sapply(valeurs_k, function(k) {
    if (4 * k <= length(donnees_tries)) {
      X_k <- donnees_tries[k]
      X_2k <- donnees_tries[2 * k]
      X_4k <- donnees_tries[4 * k]
      gamma_estime <- (1 / log(2)) * log((X_k - X_2k) / (X_2k - X_4k))
      return(gamma_estime)
    } else {
      return(NA)
    }
  })
  
  indices_valides <- which(!is.na(estimations) & is.finite(estimations))
  
  if (length(indices_valides) > 0) {
    k_valides <- valeurs_k[indices_valides]
    estimations_valides <- estimations[indices_valides]
    estimations_valides <- pmin(pmax(estimations_valides, -2), 2)
    
    plot(k_valides, estimations_valides, type = "l", 
         ylim = c(-1.5, 1.5),
         xlab = "k", ylab = "Estimateur de Pickands",
         main = titre, 
         cex.main = 0.9,
         col = "black", lwd = 2)
    
    abline(h = gamma_theorique, col = "red", lty = 2, lwd = 2)
    legend("topright", 
           legend = c("Estimateur de Pickands", paste("Valeur théorique (gamma =", gamma_theorique, ")")),
           col = c("black", "red"), 
           lty = c(1, 2), 
           lwd = c(2, 2),
           cex = 0.8)
  } else {
    plot(1, 1, type = "n", xlab = "k", ylab = "Estimateur de Pickands",
         main = titre, cex.main = 0.9)
    text(1, 1, "Pas assez de données valides")
  }
}

# affichage un par un
for (nom_loi in names(lois)) {
  loi <- lois[[nom_loi]]
  X <- loi$donnees(n)
  gamma_theo <- loi$gamma
  
  dev.new() # fenetre graphique
  estimateur_pickands(
    X, 
    valeurs_k, 
    paste("Évolution de l'estimateur de Pickands\nen fonction de k :", nom_loi), 
    gamma_theo
  )
}
