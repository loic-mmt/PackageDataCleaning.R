#' Exporter les données en CSV
#'
#' @description
#' Sauvegarde un `data.frame` dans un fichier CSV sur le disque.
#' Cette fonction gère automatiquement la création du dossier cible, l'ajout de l'extension `.csv` et le choix du séparateur.
#'
#' @details
#' La fonction simplifie l'exportation en effectuant les étapes suivantes :
#' \enumerate{
#'   \item Vérifie que l'objet `data` est bien un data.frame.
#'   \item Crée le dossier `path` récursivement s'il n'existe pas encore.
#'   \item Ajoute l'extension `.csv` au nom du fichier si elle est absente.
#'   \item Choisit la fonction d'écriture optimisée selon le séparateur :
#'     \itemize{
#'       \item Pour `sep = ";"` : utilise `utils::write.csv2` (format européen).
#'       \item Pour `sep = ","` : utilise `utils::write.csv` (format US).
#'       \item Autres : utilise `utils::write.table`.
#'     }
#' }
#'
#' @param data Le `data.frame` à exporter.
#' @param path Chemin du dossier de destination (défaut `"exports"`). Le dossier sera créé s'il n'existe pas.
#' @param filename Nom du fichier. L'extension `.csv` est ajoutée automatiquement si nécessaire (défaut `"cleaned_data"`).
#' @param sep Caractère séparateur de colonnes (défaut `";"`).
#' @param row.names Logique. Faut-il inclure les noms de lignes dans le fichier ? (défaut `FALSE`).
#' @param overwrite Logique. Si `FALSE`, lève une erreur si le fichier existe déjà. Si `TRUE` (défaut), l'écrase.
#' @param verbose Logique. Affiche un message avec le chemin complet du fichier après écriture (défaut `TRUE`).
#'
#' @return Renvoie de manière invisible (`invisible()`) le chemin complet du fichier créé.
#'
#' @family Fonctions d'Export
#' @examples
#' # Données de test
#' df_test <- data.frame(
#'   id = 1:3,
#'   nom = c("Alice", "Bob", "Charlie"),
#'   score = c(10.5, 15.2, 8.0)
#' )
#'
#' # Création d'un dossier temporaire pour ne pas polluer votre disque
#' tmp_folder <- tempfile() # On utilise un nom aléatoire
#'
#' # 1. Export simple (point-virgule par défaut)
#' path_file <- export_csv(df_test, path = tmp_folder, filename = "test_export")
#'
#' # Vérification que le fichier existe
#' file.exists(path_file)
#'
#' # 2. Export avec virgule et sans écraser (devrait générer une erreur si on relance)
#' try({
#'   export_csv(df_test, path = tmp_folder, filename = "test_export", sep = ",", overwrite = FALSE)
#' })
#'
#' # Nettoyage du dossier temporaire
#' unlink(tmp_folder, recursive = TRUE)
#' @export
export_csv <- function(data,
                       path = "exports",
                       filename = "cleaned_data",
                       sep = ";",
                       row.names = FALSE,
                       overwrite = TRUE,
                       verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")
  if (!dir.exists(path)) dir.create(path, recursive = TRUE)

  if (!grepl("\\.csv$", filename, ignore.case = TRUE)) filename <- paste0(filename, ".csv")
  filepath <- file.path(path, filename)

  if (!overwrite && file.exists(filepath)) stop(sprintf("Le fichier existe déjà : %s", filepath))

  if (sep == ";") {
    utils::write.csv2(data, file = filepath, row.names = row.names)
  } else if (sep == ",") {
    utils::write.csv(data, file = filepath, row.names = row.names)
  } else {
    utils::write.table(data, file = filepath, sep = sep, dec = ".", row.names = row.names, col.names = TRUE, qmethod = "double")
  }

  if (verbose) message(sprintf("Fichier écrit: %s", normalizePath(filepath)))
  invisible(filepath)
}