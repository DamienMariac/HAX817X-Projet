library(evir)
library(actuar)
library(VGAM)

lois <- list(
  Lévy = list(
    data = function(n) rlevy(n, location = 0, scale = 1),
    gamma = 0.5
  ),
  Exponentielle = list(
    data = function(n) rexp(n, rate = 1),
    gamma = 0
  ),
  Uniforme = list(
    data = function(n) runif(n),
    gamma = -1
  ),
  Cauchy = list(
    data = function(n) rcauchy(n),
    gamma = 1
  )
)

n <- 40000

for (nom in names(lois)) {
  loi <- lois[[nom]]
  x <- sort(loi$data(n), decreasing = TRUE)
  
  dev.new()
  
  hill(x,
       col = "black",
       lwd = 2,
       main = "")
  
  abline(h = loi$gamma,
         col = "red",
         lty = 2,
         lwd = 2)
  
  legend("topright",
         legend = c(paste("Estimateur de Hill –", nom),
                    paste0("Valeur théorique (gamma = ", loi$gamma, ")")),
         col = c("black", "red"),
         lty = c(1, 2),
         lwd = 2,
         bty = "n")
}
