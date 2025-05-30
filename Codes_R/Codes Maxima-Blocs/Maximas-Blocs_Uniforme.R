# Chargement des packages
library(evd)
library(ggplot2)

# Génération d'une loi uniforme, on prend [0,100] pour que ce soit moins resserré 
n <- 10000
x <- runif(n, min = 0, max = 100)

# Taille des blocs
taille_blocs <- 50
nb_blocs <- floor(n / taille_blocs)

# Maxima par bloc
block_maxima <- sapply(1:nb_blocs, function(i) {
  start <- (i - 1) * taille_blocs + 1
  end <- i * taille_blocs
  max(x[start:end])
})

# Loi limite : Weibull
fit_weibull <- fgev(block_maxima, std.err = FALSE) # std.err=FALSE pour que la fonction arrête de chercher des erreurs

# Graphe
ggplot(data.frame(block_maxima), aes(x = block_maxima)) +
  geom_histogram(aes(y = ..density.., fill = "Simulation"), bins = 22,
                 color = "black") +
  stat_function(aes(color = "Densité théorique"),
                fun = function(x) dgev(x,
                                       loc = fit_weibull$estimate["loc"],
                                       scale = fit_weibull$estimate["scale"],
                                       shape = fit_weibull$estimate["shape"]),
                size = 1.2) +
  coord_cartesian(xlim = c(90, 102)) +
  scale_fill_manual("", values = c("Simulation" = "skyblue")) +
  scale_color_manual("", values = c("Densité théorique" = "red")) +
  labs(title = "Méthode des maximas en bloc pour une loi uniforme",
       x = "Maxima par bloc",
       y = "Densité") +
  theme_minimal() +
  theme(legend.position = "top")

