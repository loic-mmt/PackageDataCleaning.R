#' @title Convertir les salaires en USD
#'
#' @description
#' Convertit les montants des salaires locaux vers le dollar américain (USD).
#' Cette fonction prend en compte la variation des taux de change selon l'année de travail (`work_year`).
#'
#' @details
#' La procédure de conversion est la suivante :
#' \enumerate{
#'   \item **Validation** : Vérifie la présence des colonnes `salary`, `salary_currency` et `work_year`.
#'   \item **Typage** : Assure que le salaire est numérique et l'année est un entier.
#'   \item **Jointure** : Fusionne les données avec la table de référence `exchange_rates_to_usd` sur la base de la devise et de l'année.
#'   \item **Calcul** : Applique le taux de change pour mettre à jour `salary_in_usd`.
#'   \item **Nettoyage** : Supprime les colonnes temporaires de taux (`rate`).
#' }
#'
#' @note
#' Cette fonction dépend de l'objet global `exchange_rates_to_usd` qui doit contenir les colonnes `currency`, `year` et `rate`.
#'
#' @param data Un `data.frame` contenant obligatoirement les colonnes :
#'   \itemize{
#'     \item `salary` : Le montant brut dans la devise locale.
#'     \item `salary_currency` : Le code ISO de la devise (ex: "EUR", "GBP").
#'     \item `work_year` : L'année fiscale de référence.
#'     \item `salary_in_usd` : (Optionnel, sera écrasé/créé).
#'   }
#'
#' @return Le `data.frame` enrichi avec la colonne `salary_in_usd` calculée.
#'
#' @family Fonctions Monétaires
#' @examples
#' # Pour que l'exemple fonctionne, on simule la table de taux de change
#' # Normalement, cet objet 'exchange_rates_to_usd' est chargé dans le package
#' exchange_rates_to_usd <- data.frame(
#'   currency = c("EUR", "GBP"),
#'   year = c(2023, 2023),
#'   rate = c(1.1, 1.25) # 1 EUR = 1.1 USD
#' )
#'
#' df <- data.frame(
#'   salary = c(50000, 40000),
#'   salary_currency = c("EUR", "GBP"),
#'   work_year = c(2023, 2023)
#' )
#'
#' # Conversion
#' convert_currency_to_usd(df)
#' @export
convert_currency_to_usd <- function(data) {
  if (!all(c("salary", "salary_currency", "salary_in_usd", "work_year") %in% names(data))) {
    stop("Les colonnes 'salary', 'salary_currency', 'salary_in_usd' et 'work_year' doivent exister.")
  }
  data$salary <- as.numeric(as.character(data$salary))
  data$salary_currency <- as.character(data$salary_currency)
  data$work_year <- as.integer(data$work_year)

  # Utilisation des données de mapping exportées
  data <- merge(data, exchange_rates_to_usd,
                by.x = c("salary_currency", "work_year"),
                by.y = c("currency", "year"),
                all.x = TRUE)

  data$salary_in_usd <- data$salary * data$rate
  data$calculated_usd <- NULL
  data$rate <- NULL

  return(data)
}