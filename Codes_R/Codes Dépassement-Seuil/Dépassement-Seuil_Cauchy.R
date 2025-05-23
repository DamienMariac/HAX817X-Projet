library(evd)
library(ggplot2)

# Simulation
set.seed(123)
n <- 10000

x <- rcauchy(10000, location = 0, scale = 1)
u <- quantile(x, 0.95)

# Sélection des excès au-dessus du seuil
exces <- x[x > u] - u  # on centre les données sur le seuil

# Ajustement de la loi de Pareto généralisée (GPD)
fit_gpd <- fpot(x, threshold = u, model = "gpd")

# Graphe : histogramme des excès + densité GPD
ggplot(data.frame(exces), aes(x = exces)) +
  geom_histogram(aes(y = ..density.., fill = "Simulation"), bins = 40,
                 color = "black", alpha = 0.6) +
  stat_function(aes(color = "Densité théorique"),
                fun = function(x) dgpd(x,
                                       loc = 0,  # car les excès sont centrés
                                       scale = fit_gpd$estimate["scale"],
                                       shape = fit_gpd$estimate["shape"]),
                size = 1.2) + xlim(c(-10,100)) +
  scale_fill_manual("", values = c("Simulation" = "skyblue")) +
  scale_color_manual("", values = c("Densité théorique" = "red")) +
  labs(title = "Méthode du dépassement de seuil (POT) pour une loi de Cauchy",
       x = "Excès au-dessus du seuil",
       y = "Densité") +
  theme_minimal() +
  theme(legend.position = "top")

