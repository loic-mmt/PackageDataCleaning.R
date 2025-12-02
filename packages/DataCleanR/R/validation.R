#' Charger un fichier CSV brut
#'
#' @description
#' Lit un fichier CSV depuis le disque et le charge dans un `data.frame`.
#' Cette fonction impose des séparateurs "point-virgule" et ne convertit pas les chaînes en facteurs.
#'
#' @param file_path Une chaîne de caractères indiquant le chemin vers le fichier CSV.
#'
#' @return Un `data.frame` contenant les données brutes.
#'
#' @family Fonctions validation
#' #' @examples
#' # Création d'un fichier CSV temporaire pour l'exemple
#' tf <- tempfile(fileext = ".csv")
#' writeLines("col1;col2\n1;a\n2;b", tf)
#'
#' # Utilisation de la fonction
#' df <- read_raw_csv(tf)
#' print(df)
#'
#' # Nettoyage
#' unlink(tf)
#' @export
read_raw_csv <- function(file_path) {
  data <- read.csv(file_path, sep = ";", stringsAsFactors = FALSE)
  return(data)
}


#' Valider la présence des colonnes requises
#'
#' @description
#' Vérifie si un `data.frame` contient bien toutes les colonnes nécessaires à la suite du traitement.
#'
#' @details
#' Cette fonction permet deux modes de retour via l'argument `boolean_form` :
#' \itemize{
#'   \item **Mode booléen** : Renvoie `TRUE` ou `FALSE`. Utile pour les conditions `if`.
#'   \item **Mode verbeux** (par défaut) : Renvoie un message textuel indiquant explicitement quelles colonnes sont manquantes.
#' }
#'
#' @param dataframe Le `data.frame` à tester.
#' @param required_columns Un vecteur contenant les noms des colonnes attendues.
#' @param boollean_form Logique (`TRUE`/`FALSE`). Si `TRUE`, renvoie un booléen simple. Si `FALSE`, renvoie un message détaillé.
#'
#' @return
#' \itemize{
#'   \item Si `boollean_form = TRUE` : Un booléen.
#'   \item Si `boollean_form = FALSE` : Une chaîne de caractères (message de succès ou d'erreur).
#' }
#'
#' @family Fonctions validation
#'
#' #' @examples
#' df <- data.frame(id = 1:3, salary = c(100, 200, 300))
#'
#' # Cas Succès (Message)
#' validate_schema(df, c("id", "salary"))
#'
#' # Cas Échec (Message)
#' validate_schema(df, c("id", "salary", "age"))
#'
#' # Cas Succès (Booléen)
#' if(validate_schema(df, c("id"), boolean_form = TRUE)) {
#'   print("Tout est OK !")
#' }
#' @export
validate_schema <- function(dataframe, required_columns, boolean_form = FALSE) {
  not_commun <- required_columns[!required_columns %in% names(dataframe)]
  if (boolean_form) {
    presence <- (length(not_commun) == 0)
  }
  else {
    presence <- ""
    if (length(not_commun) == 0) {
        presence <- "All required columns are present in the dataframe"
    }
    else {
        presence <- paste("The dataframe in not complete and it's missing", paste(not_commun, collapse = ", "))
    }
  }
  return(presence)
}

#' Standardiser les noms de colonnes
#'
#' @description
#' Convertit un vecteur de chaînes de caractères (noms de colonnes) en format standard `snake_case`.
#'
#' @details
#' La normalisation applique les règles suivantes :
#' \enumerate{
#'   \item Remplacement de tout caractère non-alphanumérique par `_`.
#'   \item Gestion du CamelCase (insertion d'un `_` entre minuscule et majuscule).
#'   \item Passage en minuscules.
#'   \item Suppression des `_` multiples et nettoyage des extrémités.
#' }
#'
#' @param data Un vecteur de chaînes de caractères (ex: `names(df)`).
#'
#' @return Un vecteur de chaînes de caractères nettoyé.
#'
#' @family Fonctions validation
#' #' @examples
#' dirty_names <- c("First Name", "salary(USD)", "IsRemote?", "jobTitle")
#' clean_names <- standardize_colnames(dirty_names)
#' print(clean_names)
#' # Résultat attendu : "first_name", "salary_usd", "is_remote", "job_title"
#' @export
standardize_colnames <- function(data) {
  data <- gsub("[^A-Za-z0-9]+", "_", data)
  data <- gsub("([a-z0-9])([A-Z])", "\\1_\\2", data)
  data <- tolower(data)
  data <- gsub("_+", "_", data)
  data <- gsub("^_+|_+$", "", data)
  return(data)
}

#' Enforce simple column types
#'
#' @description
#' Analyse chaque colonne d'un `data.frame` et tente de convertir les types `character`
#' en types plus appropriés (`numeric`, `integer` ou `factor`).
#'
#' @details
#' L'algorithme procède colonne par colonne :
#' \itemize{
#'   \item Ignore les colonnes déjà numériques ou dates.
#'   \item Nettoie les espaces (`trimws`).
#'   \item **Conversion Numérique** : Si le ratio de valeurs convertibles dépasse `num_threshold`, la colonne devient numérique (ou entier si possible).
#'   \item **Conversion Facteur** : Sinon, si le nombre de valeurs uniques est inférieur à `max_factor_levels`, la colonne devient un facteur.
#'   \item Sinon, la colonne reste en texte.
#' }
#'
#' @param data Le `data.frame` en entrée.
#' @param num_threshold Proportion (0 à 1). Seuil de valeurs valides nécessaires pour convertir en numérique (défaut 0.9).
#' @param max_factor_levels Entier. Nombre maximum de modalités pour convertir en facteur (défaut 20).
#'
#' @return Un nouveau `data.frame` avec les types optimisés.
#'
#' @family Fonctions validation
#' @seealso \code{\link{standardize_colnames}}
#' @examples
#' df <- data.frame(
#'   id = c("1", "2", "3"),              # Devrait devenir integer
#'   cat = c("A", "A", "B"),             # Devrait devenir factor
#'   text = c("Unique1", "Unique2", "Unique3"), # Reste character
#'   stringsAsFactors = FALSE
#' )
#'
#' str(df) # Tout est character au début
#'
#' df_typed <- enforce_types(df, max_factor_levels = 2)
#' str(df_typed) # Types corrigés
#' @export
enforce_types <- function(data, num_threshold = 0.9, max_factor_levels = 20) {
  out <- data

  for (col in names(out)) {
    x <- out[[col]]

    # Ignorer si déjà au bon type
    if (is.numeric(x) || is.factor(x) || inherits(x, "Date")) {
      next
    }

    # Nettoyer les espaces
    if (is.character(x)) {
      x <- trimws(x)
    }

    # Calculer le nombre de valeurs non-NA
    valid_values <- x[!is.na(x) & x != ""]
    n_valid <- length(valid_values)

    if (n_valid == 0) {
      next  # Colonne vide
    }

    #  conversion numérique
    x_numeric <- suppressWarnings(as.numeric(x))
    n_numeric_valid <- sum(!is.na(x_numeric[!is.na(x) & x != ""]))

    # Si au moins num_threshold% des valeurs sont convertibles en numérique
    if (n_numeric_valid / n_valid >= num_threshold) {
      # Vérifier si ce sont des entiers
      if (all(x_numeric[!is.na(x_numeric)] == floor(x_numeric[!is.na(x_numeric)]))) {
        out[[col]] <- as.integer(x_numeric)
      } else {
        out[[col]] <- x_numeric
      }
      next
    }
    # Vérifier si c'est un facteur potentiel
    n_unique <- length(unique(valid_values))

    # Convertir en facteur SEULEMENT si <= max_factor_levels valeurs uniques
    if (n_unique <= max_factor_levels) {
      out[[col]] <- as.factor(x)
      next
    }

    # Sinon garder comme character
    out[[col]] <- as.character(x)
  }
  return(out)
}


#' Deduplicate data
#'
#' @description
#' Supprime les doublons dans un `data.frame` en se basant sur une clé composée de colonnes spécifiques.
#'
#' @param data Le `data.frame` à dédupliquer.
#' @param keys Vecteur de noms de colonnes ou `NULL`.
#'   \itemize{
#'     \item Si `NULL` (défaut) : Toutes les colonnes sont utilisées pour identifier les doublons.
#'     \item Si Vecteur : Seules ces colonnes définissent l'unicité (ex: `c("ID", "Date")`).
#'   }
#' @param keep Chaîne de caractères indiquant quelle ligne garder en cas de doublon :
#'   \itemize{
#'     \item `"first"` : Garde la première occurrence trouvée.
#'     \item `"last"` : Garde la dernière occurrence trouvée.
#'   }
#'
#' @return Le `data.frame` sans doublons.
#'   L'objet retourné possède un attribut `"n_removed"` indiquant le nombre de lignes supprimées.
#'
#' @family Fonctions validation
#' @examples
#' df <- data.frame(
#'   id = c(1, 1, 2, 3),
#'   val = c("a", "a", "b", "c")
#' )
#'
#' # Supprime le doublon exact (ligne 2)
#' deduplicate_rows(df)
#'
#' # Déduplication basée uniquement sur l'ID (garde la dernière occurrence)
#' df2 <- data.frame(id = c(1, 1), val = c("a", "b"))
#' deduplicate_rows(df2, keys = "id", keep = "last")
#' @export
deduplicate_rows <- function(data, keys = NULL, keep = c("first", "last")) {
  keep <- match.arg(keep)

  if (!is.data.frame(data)) {
    stop("'data' doit être un data.frame")
  }
  if (is.null(keys)) {
    keys <- names(data)
  } else {
    inconnues <- setdiff(keys, names(data))
    if (length(inconnues) > 0) {
      stop("Clés inconnues: ", paste(inconnues, collapse = ", "))
    }
  }
  from_last <- identical(keep, "last")
  dup <- duplicated(data[keys], fromLast = from_last)

  out <- data[!dup, , drop = FALSE]

  attr(out, "n_removed") <- sum(dup)
  return(out)
}