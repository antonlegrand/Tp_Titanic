#' Taux de survie selon une variable
#'
#' Calcule le taux de survie des passagers du Titanic pour chaque
#' modalité d'une variable (sexe, classe, etc.).
#'
#' @param data Un data.frame contenant une colonne de survie.
#' @param variable Nom de la variable de regroupement (chaîne de caractères).
#' @param survie Nom de la colonne de survie, codée 0/1. Par défaut "Survived".
#'
#' @return Un data.frame avec la modalité, l'effectif, le nombre de
#'   survivants et le taux de survie.
#' @export
#'
#' @examples
#' df <- data.frame(Survived = c(1, 0, 1, 0), Sex = c("F", "M", "F", "M"))
#' taux_survie(df, "Sex")
taux_survie <- function(data, variable, survie = "Survived") {
  if (!variable %in% names(data)) stop("Variable introuvable : ", variable)
  if (!survie %in% names(data)) stop("Colonne de survie introuvable : ", survie)

  agg <- aggregate(
    data[[survie]],
    by = list(modalite = data[[variable]]),
    FUN = function(x) c(n = length(x),
                        survivants = sum(x, na.rm = TRUE),
                        taux = mean(x, na.rm = TRUE))
  )
  res <- data.frame(agg$modalite, agg$x)
  names(res) <- c(variable, "n", "survivants", "taux_survie")
  res
}
