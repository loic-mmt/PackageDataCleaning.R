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


#' write a cleaning report to a text file
#'
#' @description
#' Crée un fichier texte résumant l'état du jeu de données après nettoyage.
#' Il compare (optionnellement) les dimensions avant/après et inscrit les statistiques fournies.
#'
#' @details
#' Le rapport contient :
#' * La date et l'heure du rapport.
#' * Les dimensions du jeu de données final.
#' * Le nombre de valeurs manquantes (NA) restantes par colonne.
#' * Une section "Logs Opérationnels" affichant les compteurs fournis dans `stats_list`.
#'
#' @param data Le `data.frame` nettoyé (final).
#' @param original_data (Optionnel) Le `data.frame` brut (avant nettoyage) pour comparer les lignes supprimées.
#' @param file Chemin du fichier de sortie (ex: "reports/cleaning_log.txt").
#' @param stats_list Une liste nommée contenant des compteurs ou messages (ex: `list(outliers_capped = 12, rows_removed = 5)`).
#'
#' @return Renvoie le chemin du fichier créé (invisiblement).
#' @family Export
#' @examples
#' # Données brutes
#' df_raw <- data.frame(id = 1:5, val = c(10, NA, 30, 1000, 50))
#'
#' # Données nettoyées (fictives)
#' df_clean <- data.frame(id = c(1,3,4,5), val = c(10, 30, 100, 50))
#'
#' # Liste des opérations effectuées (recueillies pendant le script)
#' my_stats <- list(
#'   "NA imputés" = 1,
#'   "Outliers plafonnés" = 1,
#'   "Lignes supprimées" = 1
#' )
#'
#' # Génération du rapport
#' tmp_file <- tempfile(fileext = ".txt")
#' write_cleaning_report(df_clean, original_data = df_raw, file = tmp_file, stats_list = my_stats)
#'
#' # Lecture du résultat
#' cat(readLines(tmp_file), sep = "\n")
#' @export
write_cleaning_report <- function(data,
                                  original_data = NULL,
                                  file = "cleaning_report.txt",
                                  stats_list = NULL) {

  if (!is.data.frame(data)) stop("data doit être un data.frame")

  # Création du dossier parent si besoin
  dir_name <- dirname(file)
  if (dir_name != "." && !dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)

  # Ouverture du fichier en écriture
  sink(file)
  on.exit(sink())

  cat("   RAPPORT  \n")

  cat("Date :", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n\n")

  # 1. Dimensions et Comparaison
  cat("--- DIMENSIONS ---\n")
  n_rows <- nrow(data)
  n_cols <- ncol(data)
  cat(sprintf("Lignes finales   : %d\n", n_rows))
  cat(sprintf("Colonnes finales : %d\n", n_cols))

  if (!is.null(original_data)) {
    n_orig <- nrow(original_data)
    diff_rows <- n_orig - n_rows
    cat(sprintf("Lignes initiales : %d\n", n_orig))
    cat(sprintf("Lignes supprimées: %d (%.2f%%)\n", diff_rows, (diff_rows/n_orig)*100))
  }
  cat("\n")

  # 2. Vérification des NA restants
  cat("--- VALEURS MANQUANTES (NA) RESTANTES ---\n")
  na_counts <- colSums(is.na(data))
  na_cols <- na_counts[na_counts > 0]

  if (length(na_cols) == 0) {
    cat("Aucune valeur manquante (Jeu de données complet).\n")
  } else {
    for (col in names(na_cols)) {
      cat(sprintf("- %s : %d NA\n", col, na_cols[col]))
    }
  }
  cat("\n")

  # 3. Logs personnalisés (Outliers, etc.)
  if (!is.null(stats_list) && length(stats_list) > 0) {
    for (name in names(stats_list)) {
      cat(sprintf("- %s : %s\n", name, as.character(stats_list[[name]])))
    }
    cat("\n")
  }
  cat("Fin du rapport.\n")

  invisible(normalizePath(file, mustWork = FALSE))
}