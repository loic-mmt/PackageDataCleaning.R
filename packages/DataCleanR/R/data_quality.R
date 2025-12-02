#' @title Valider les plages de valeurs
#'
#' @description
#' Vérifie que les données respectent les règles métier et **supprime** les lignes invalides.
#' Les règles incluent : salaire strictement positif, ratio de télétravail entre 0 et 100, et année de travail plausible.
#'
#' @details
#' Cette fonction applique un filtre strict. Une ligne est conservée uniquement si toutes les conditions suivantes sont vraies :
#' \itemize{
#'   \item `salary` > 0
#'   \item `remote_ratio` est compris entre 0 et 100 inclus.
#'   \item `work_year` est compris entre `min_year` et `max_year`.
#'   \item Aucune de ces valeurs n'est `NA`.
#' }
#' Un message est affiché dans la console si des lignes sont supprimées.
#'
#' @param data Le `data.frame` contenant les colonnes `salary`, `remote_ratio` et `work_year`.
#' @param min_year Entier. Année minimale acceptable (défaut 2000).
#' @param max_year Entier. Année maximale acceptable (défaut : année courante système).
#'
#' @return Un `data.frame` filtré (le nombre de lignes peut être inférieur à l'original).
#'
#' @family Qualité des Données
#' @examples
#' # Données avec une ligne invalide (salary négatif et remote_ratio > 100)
#' df <- data.frame(
#'   salary = c(50000, -100, 60000),
#'   remote_ratio = c(50, 150, 100),
#'   work_year = c(2022, 2022, 2023)
#' )
#'
#' # Application du filtre
#' df_clean <- validate_ranges(df)
#'
#' # Résultat : ne garde que les lignes valides
#' nrow(df_clean) # Devrait être 2
#' @export

validate_ranges <- function(data, min_year = 2000, max_year = as.integer(format(Sys.Date(), "%Y"))) {
  if (!all(c("salary", "remote_ratio", "work_year") %in% names(data))) {
    stop("Les colonnes 'salary', 'remote_ratio' et 'work_year' doivent exister.")
  }

  data$salary <- as.numeric(data$salary)
  data$remote_ratio <- as.numeric(data$remote_ratio)
  data$work_year <- as.integer(data$work_year)

  initial_rows <- nrow(data)

  # Filtrer les lignes valides
  valid_data <- data[
    data$salary > 0 &
      data$remote_ratio >= 0 &
      data$remote_ratio <= 100 &
      data$work_year >= min_year &
      data$work_year <= max_year &
      !is.na(data$salary) &
      !is.na(data$remote_ratio) &
      !is.na(data$work_year),
  ]

  removed_rows <- initial_rows - nrow(valid_data)

  if (removed_rows > 0) {
    message(sprintf("%d ligne(s) supprimée(s) car hors des plages valides.", removed_rows))
  }

  return(valid_data)
}



#' Cap salary outliers (quantile)
#'
#' @description
#' Traite les valeurs extrêmes (outliers) d'une colonne numérique en les remplaçant par les valeurs des quantiles limites.
#' C'est une technique de "Winsorisation".
#'
#' @details
#' La fonction calcule deux bornes :
#' \itemize{
#'   \item **L (Lower)** : Le quantile correspondant à la probabilité `lower` (ex: 1\%).
#'   \item **U (Upper)** : Le quantile correspondant à la probabilité `upper` (ex: 99\%).
#' }
#' Selon le paramètre `clip_side`, les valeurs inférieures à L sont remplacées par L, et les valeurs supérieures à U sont remplacées par U.
#'
#' @param data Le `data.frame` contenant les données.
#' @param col Chaîne de caractères. Nom de la colonne à traiter (défaut `"salary_in_usd"`).
#' @param lower Probabilité pour le quantile bas (0 à 1, défaut 0.01 pour 1\%).
#' @param upper Probabilité pour le quantile haut (0 à 1, défaut 0.99 pour 99\%).
#' @param clip_side Stratégie de plafonnement :
#'   \itemize{
#'     \item `"both"` (défaut) : Plafonne en bas et en haut.
#'     \item `"upper"` : Plafonne uniquement les valeurs hautes.
#'     \item `"lower"` : Plafonne uniquement les valeurs basses.
#'   }
#' @param na_rm Logique. Ignorer les `NA` lors du calcul des quantiles (défaut `TRUE`).
#' @param verbose Logique. Affiche un résumé des remplacements effectués.
#'
#' @return Le `data.frame` avec la colonne cible modifiée (les valeurs extrêmes sont "écrasées").
#'
#' @family Qualité des Données
#' @examples
#' # Jeu de données avec un salaire extrême (1 million)
#' df <- data.frame(
#'   salary_in_usd = c(50000, 55000, 60000, 45000, 1000000)
#' )
#'
#' # On plafonne les 10% supérieurs (Winsorisation)
#' df_capped <- cap_outliers_salary(df, lower = 0.1, upper = 0.9, clip_side = "upper")
#'
#' # La valeur 1,000,000 a été remplacée par le 90ème percentile
#' print(df_capped)
#' @export
cap_outliers_salary <- function(data,
                                col = "salary_in_usd",
                                lower = 0.01,
                                upper = 0.99,
                                clip_side = c("both", "upper", "lower"),
                                na_rm = TRUE,
                                verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!col %in% names(data)) stop(sprintf("Colonne '%s' introuvable", col))
  if (!is.numeric(data[[col]])) stop(sprintf("'%s' doit être numérique", col))

  clip_side <- match.arg(clip_side)

  caps_quantile <- function(x) {
    L <- as.numeric(stats::quantile(x, probs = lower, na.rm = na_rm, type = 7))
    U <- as.numeric(stats::quantile(x, probs = upper, na.rm = na_rm, type = 7))
    c(lower = L, upper = U)
  }
  apply_caps <- function(x, caps) {
    L <- caps[["lower"]]; U <- caps[["upper"]]
    xi <- x
    idx_low <- !is.na(xi) & xi < L
    idx_up  <- !is.na(xi) & xi > U
    if (clip_side %in% c("both","lower")) xi[idx_low] <- L
    if (clip_side %in% c("both","upper")) xi[idx_up]  <- U
    list(x = xi, n_low = sum(idx_low), n_up = sum(idx_up))
  }

  x <- data[[col]]
  caps <- caps_quantile(x)
  res  <- apply_caps(x, caps)
  data[[col]] <- res$x

  if (verbose) message(sprintf(
    "cap_outliers_salary: %d bas / %d haut  (caps [%g, %g])",
    res$n_low, res$n_up, caps[["lower"]], caps[["upper"]]))

  data
}
