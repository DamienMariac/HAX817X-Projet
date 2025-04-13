# Chargement des packages
library(evd)
library(ggplot2)

# Génération de la loi de Cauchy
n <- 10000
x <- rcauchy(n)

# Taille des blocs
taille_blocs <- 50
nb_blocs <- floor(n / taille_blocs)

# Maxima par bloc
block_maxima <- sapply(1:nb_blocs, function(i) {
  start <- (i - 1) * taille_blocs + 1
  end <- i * taille_blocs
  max(x[start:end])
})

# Loi limite : Fréchet ici
fit_frechet <- fgev(block_maxima)

# Graphe avec ggplot2
ggplot(data.frame(block_maxima), aes(x = block_maxima), xlim=2000) +
  geom_histogram(aes(y = ..density.., fill = "Simulation"), bins = 60,
                 color = "black") +
  stat_function(aes(color = "Densité théorique"),
                fun = function(x) dgev(x,
                                       loc = fit_frechet$estimate["loc"],
                                       scale = fit_frechet$estimate["scale"],
                                       shape = fit_frechet$estimate["shape"]),
                size = 1.2) + xlim(0, 1000) +
  scale_fill_manual("", values = c("Simulation" = "skyblue")) +
  scale_color_manual("", values = c("Densité théorique" = "red")) +
  labs(title = "Méthode des maximas en bloc pour une loi de Cauchy",
       x = "Maxima par bloc",
       y = "Densité") +
  theme_minimal() +
  theme(legend.position = "top")
