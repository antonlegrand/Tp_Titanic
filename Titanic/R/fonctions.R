taux_survie <- function(data, variable, survie = "survived") {
  if (!variable %in% names(data)) stop("Variable introuvable : ", variable)
  if (!survie %in% names(data)) stop("Colonne de survie introuvable : ", survie)

  s <- data[[survie]]
  if (is.factor(s) || is.character(s)) {
    s <- tolower(as.character(s)) %in% c("yes", "oui", "1", "true")
  } else {
    s <- as.numeric(s) == 1
  }

  grp <- data[[variable]]
  n <- tapply(s, grp, length)
  surv <- tapply(s, grp, sum)
  res <- data.frame(modalite = names(n), n = as.integer(n),
                    survivants = as.integer(surv),
                    taux_survie = round(as.numeric(surv / n), 3))
  names(res)[1] <- variable
  res
}
