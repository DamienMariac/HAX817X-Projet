# Packages nécessaires
library(evd)
library(ggplot2)

# Génération de données : loi uniforme sur [0, 1]
n <- 10000
x <- runif(n, min = 0, max = 100)

# Choix du seuil élevé (par exemple 95e percentile)
u <- quantile(x, 0.95)

# Excès au-dessus du seuil
exces <- x[x > u] - u

# Ajustement GPD sur les excès
fit_gpd <- fpot(x, threshold = u, model = "gpd")

# Visualisation : histogramme des excès + densité théorique GPD
ggplot(data.frame(exces), aes(x = exces)) +
  geom_histogram(aes(y = ..density.., fill = "Simulation"),
                 bins = 30, color = "black", alpha = 0.6) +
  stat_function(aes(color = "Densité théorique"),
                fun = function(x) dgpd(x,
                                       loc = 0,
                                       scale = fit_gpd$estimate["scale"],
                                       shape = fit_gpd$estimate["shape"]),
                size = 1.2) + xlim(c(-2,7)) +
  scale_fill_manual("", values = c("Simulation" = "skyblue")) +
  scale_color_manual("", values = c("Densité théorique" = "red")) +
  labs(title = "Méthode du dépassement de seuil (POT) - Loi uniforme",
       x = "Excès au-dessus du seuil",
       y = "Densité") +
  theme_minimal() +
  theme(legend.position = "top")

