#' Normalize a vector to a factor. (use with "df$col <- normalize_to_factor(df$col, mapping, levels)")
#'
#' @description
#' Transforme un vecteur brut en facteur en appliquant une table de correspondance (mapping).
#' Les valeurs non trouvées ou manquantes sont remplacées par `"Unknown"`.
#'
#' @details
#' Cette fonction est utile pour standardiser des colonnes catégorielles mal formatées.
#' Elle effectue les opérations suivantes :
#' \enumerate{
#'   \item Remplace les valeurs du vecteur selon le `mapping` fourni.
#'   \item Remplace les `NA` par la chaîne `"Unknown"`.
#'   \item Convertit le résultat en `factor` avec les niveaux (`levels`) imposés.
#' }
#'
#' @param vector Le vecteur brut à normaliser.
#' @param mapping Un vecteur nommé (clé-valeur) indiquant la transformation (ex: `c("old_val" = "new_val")`).
#' @param levels_in_factor Un vecteur contenant tous les niveaux valides pour le facteur final.
#' @param ordered_factor Logique (défaut `FALSE`). Si `TRUE`, crée un facteur ordonné.
#'
#' @return Un vecteur de type `factor` normalisé.
#'
#' @family Fonctions de Normalisation
#' @examples
#' raw_data <- c("FR", "US", "UK", "FR", "XX")
#'
#' # Table de correspondance
#' map_countries <- c(
#'   "FR" = "France",
#'   "US" = "United States",
#'   "UK" = "United Kingdom"
#' )
#'
#' # Niveaux autorisés
#' valid_levels <- c("France", "United States", "United Kingdom", "Unknown")
#'
#' # Application
#' normalize_factor(raw_data, map_countries, valid_levels)
#' @export
normalize_factor <- function(vector, mapping, levels_in_factor, ordered_factor = FALSE) {
  normalized <- mapping[vector]
  normalized[is.na(normalized)] <- "Unknown"
  normalized <- factor(normalized, levels = levels_in_factor, ordered = ordered_factor)
  return(normalized)
}

#' Normaliser la colonne 'remote_ratio'
#'
#' @description
#' Nettoie et standardise la colonne `remote_ratio` d'un data.frame.
#' Assure que les valeurs sont numériques et comprises entre 0 et 100.
#'
#' @details
#' Le traitement inclut :
#' \itemize{
#'   \item Conversion en numérique (les erreurs deviennent `NA`).
#'   \item Bornage des valeurs (`pmin`/`pmax`) : tout ce qui est < 0 devient 0, tout ce qui est > 100 devient 100.
#'   \item (Optionnel) Binarisation : Si `binary=TRUE`, les valeurs deviennent soit 0 soit 100 selon le `threshold`.
#' }
#'
#' @param data Le data.frame contenant la colonne `remote_ratio`.
#' @param binary Logique (défaut `FALSE`). Active le mode binaire (0 ou 100 uniquement).
#' @param threshold Numérique (défaut 50). Seuil pour la binarisation.
#'   \itemize{
#'     \item Si valeur >= threshold : devient 100.
#'     \item Si valeur < threshold : devient 0.
#'   }
#'
#' @return Le `data.frame` avec la colonne `remote_ratio` corrigée.
#'
#' @family Fonctions de Normalisation
#' @examples
#' df <- data.frame(remote_ratio = c("0", "100", "50", "30", "-5", "abc"))
#'
#' # Normalisation standard (numérique + bornage 0-100)
#' normalize_remote_ratio(df)
#'
#' # Normalisation binaire (Tout ce qui est >= 50 devient 100, le reste 0)
#' normalize_remote_ratio(df, binary = TRUE, threshold = 50)
#' @export
normalize_remote_ratio <- function(data, binary = FALSE, threshold = 50) {
  if (!"remote_ratio" %in% names(data)) {
    stop("La colonne 'remote_ratio' n'existe pas dans le dataframe.")
  }
  # convert en numeric (les non-convertibles donneront NA)
  data$remote_ratio <- suppressWarnings(as.numeric(as.character(data$remote_ratio)))
  # bornes
  data$remote_ratio <- pmin(pmax(data$remote_ratio, 0), 100)
  if (binary) {
    data$remote_ratio <- ifelse(is.na(data$remote_ratio), NA, ifelse(data$remote_ratio >= threshold, 100, 0))
  }
  return(data)
}


#' Normalize everything of an salary_tbl
#'
#' @description
#' Fonction "wrapper" (orchestrateur) qui applique séquentiellement toutes les règles de normalisation
#' spécifiques au jeu de données des salaires (`salary_tbl`).
#'
#' @details
#' Cette fonction transforme les colonnes suivantes :
#' \itemize{
#'   \item **Localisation** (`company_location`, `employee_residence`) : Normalisation ISO2 et création de groupements régionaux.
#'   \item **Poste** (`job_title`) : Standardisation des intitulés.
#'   \item **Télétravail** (`remote_ratio`) : Nettoyage numérique.
#'   \item **Taille** (`company_size`) : Conversion en facteur ordonné.
#'   \item **Type d'emploi** (`employment_type`) : Standardisation.
#'   \item **Expérience** (`experience_level`) : Conversion en facteur ordonné.
#' }
#'
#' @note
#' Cette fonction dépend de variables globales de mapping (ex: `mapping_total`, `levels_iso2`, `size_mapping`, etc.)
#' qui doivent être chargées dans l'environnement avant exécution.
#'
#' @param data Le data.frame (`salary_tbl`) à normaliser.
#'
#' @return Le `data.frame` entièrement normalisé prêt pour l'analyse.
#'
#' @family Fonctions de Normalisation
#' @seealso \code{\link{normalize_factor}}, \code{\link{normalize_remote_ratio}}
#' @examples
#' \dontrun{
#' # Cet exemple ne tourne pas automatiquement car il nécessite
#' # que les objets de mapping (mapping_total, levels_iso2, etc.)
#' # soient présents dans l'environnement global.
#'
#' df_clean <- normalize_all(salary_tbl_raw)
#' }
#' @export
normalize_all <- function(data) {
  #Normalize company location and creation of a regionnal grouping.
  data$company_location <- normalize_factor(data$company_location, mapping_total, levels_iso2)
  data$company_grouping <- normalize_factor(data$company_location, region_map, regions_levels)

  #Normalize employee residence and creation of a regionnal grouping.
  data$employee_residence <- normalize_factor(data$employee_residence, mapping_total, levels_iso2)
  data$employee_grouping <- normalize_factor(data$employee_residence, region_map, regions_levels)

  #Normalize job titles
  data$job_title <- normalize_factor(data$job_title, mapping_job_title, levels_job_title)

  #Normalize remote ratios
  data <- normalize_remote_ratio(data)

  #Normalize company sizes
  data$company_size <- normalize_factor(data$company_size, size_mapping, size_levels, TRUE)

  #Normalize employement types
  data$employment_type <- normalize_factor(data$employment_type, mapping_employement_type, levels_employement_type)

  #Normalize experience levels
  data$experience_level <- normalize_factor(data$experience_level, experience_mapping, experience_labels_ordered, TRUE)
  return(data)
}


