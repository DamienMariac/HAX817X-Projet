# Chargement des packages
library(evd) 
library(ggplot2)

# Génération de la loi
n <- 10000
x <- rnorm(n) # on peut modifier les paramètres comme on le désire

# Taille des blocs
taille_blocs <- 50
nb_blocs <- floor(n / taille_blocs) # floor = partie entière inférieure

# Découpage des données en blocs et calcul du maximum de chaque bloc
block_maxima <- sapply(1:nb_blocs, function(i) {
  start <- (i - 1) * taille_blocs + 1
  end <- i *taille_blocs
  max(x[start:end])
})

# Loi limite : Gumbel issue du package evd
fit_gumbel <- fgev(block_maxima, shape = 0)

# Graphe ggplot
ggplot(data.frame(block_maxima), aes(x = block_maxima)) +
  geom_histogram(aes(y = ..density..), bins = 10, fill = "skyblue", color = "black") +
  stat_function(fun = function(x) dgumbel(x, loc = fit_gumbel$estimate["loc"],
                                          scale = fit_gumbel$estimate["scale"]),
                color = "red", size = 1.2) +
  scale_fill_manual("", values = c("Simulation" = "skyblue")) +
  scale_color_manual("", values = c("Densité théorique" = "red")) +
  labs(title = "Méthode des maximas en bloc pour une loi normale",
       x = "Blocs",
       y = "Densité") +
  theme_minimal()

