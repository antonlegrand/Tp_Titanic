#' Taux de survie selon une variable
#'
#' Calcule, pour chaque modalité d'une variable (sexe, classe...),
#' l'effectif, le nombre de survivants et le taux de survie.
#'
#' @param data Un data.frame contenant les données des passagers.
#' @param variable Nom de la colonne de regroupement (texte).
#' @param survie Nom de la colonne de survie. Par défaut "survived".
#'
#' @return Un data.frame avec la modalité, n, survivants et taux_survie.
#' @export
#'
#' @examples
#' titanic <- carData::TitanicSurvival
#' taux_survie(titanic, "sex")
taux_survie <- function(data, variable, survie = "survived") {

  # Vérification : les deux colonnes doivent exister dans les données
  if (!variable %in% names(data)) stop("Variable introuvable : ", variable)
  if (!survie %in% names(data)) stop("Colonne de survie introuvable : ", survie)

  # Conversion de la survie en vrai/faux, quel que soit son codage
  s <- data[[survie]]
  if (is.factor(s) || is.character(s)) {
    s <- tolower(as.character(s)) %in% c("yes", "oui", "1", "true")
  } else {
    s <- as.numeric(s) == 1
  }

  # Calcul par groupe : effectif, survivants, puis taux
  grp <- data[[variable]]
  n <- tapply(s, grp, length)
  surv <- tapply(s, grp, sum)

  # Assemblage du résultat, avec taux arrondi à 3 décimales
  res <- data.frame(modalite = names(n), n = as.integer(n),
                    survivants = as.integer(surv),
                    taux_survie = round(as.numeric(surv / n), 3))
  names(res)[1] <- variable
  res
}
