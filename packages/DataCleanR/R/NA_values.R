#' Impute missing values for numeric and categorical columns
#'
#' @description
#' Remplace les valeurs manquantes (`NA`) dans un `data.frame` colonne par colonne,
#' en appliquant des stratégies simples et indépendantes (sans dépendances externes complexes).
#'
#' @details
#' Les stratégies appliquées dépendent du type de la colonne :
#' \itemize{
#'   \item **Numérique** (`numeric`, `integer`) :
#'     \itemize{
#'       \item `"median"` (défaut) : Remplace par la médiane (robuste aux outliers).
#'       \item `"mean"` : Remplace par la moyenne.
#'       \item `"constant"` : Remplace par la valeur fournie dans `num_constant` (ex: 0).
#'     }
#'   \item **Catégorielle** (`character`, `factor`) :
#'     \itemize{
#'       \item `"mode"` (défaut) : Remplace par la valeur la plus fréquente. En cas d'égalité, l'ordre alphabétique tranche.
#'       \item `"constant"` ou `"new_level"` : Remplace par la valeur `cat_constant` (par défaut "Missing").
#'       \item *Note* : Si la colonne est un facteur, les niveaux (levels) sont automatiquement mis à jour pour inclure la nouvelle valeur si nécessaire.
#'     }
#'   \item **Logique** (`logical`) : Remplace par la valeur majoritaire (`TRUE` ou `FALSE`).
#' }
#'
#' @param data Le `data.frame` contenant des valeurs manquantes.
#' @param cols Vecteur de noms de colonnes à traiter (optionnel). Si `NULL` (défaut), toutes les colonnes sont traitées.
#' @param exclude Vecteur de noms de colonnes à exclure du traitement (optionnel).
#' @param num_method Méthode pour les numériques : `"median"`, `"mean"` ou `"constant"`.
#' @param cat_method Méthode pour les textes/facteurs : `"mode"`, `"constant"` ou `"new_level"`.
#' @param num_constant Valeur utilisée si `num_method = "constant"` (défaut `0`).
#' @param cat_constant Valeur utilisée si `cat_method = "constant"` (défaut `"Missing"`).
#' @param verbose Logique. Si `TRUE` (défaut), affiche un message dans la console pour chaque colonne imputée.
#'
#' @return Le `data.frame` avec les valeurs manquantes imputées.
#'
#' @family Gestion des NA
#' @export
impute_missing <- function(data,
                           cols = NULL,
                           exclude = NULL,
                           num_method = c("median", "mean", "constant"),
                           cat_method = c("mode", "constant", "new_level"),
                           num_constant = 0,
                           cat_constant = "Missing",
                           verbose = TRUE) {
  if (!is.data.frame(data)) stop("data doit être un data.frame")

  num_method <- match.arg(num_method)
  cat_method <- match.arg(cat_method)

  # mode (sur character/factor), égalité -> ordre alphabétique
  mode_char <- function(x) {
    x <- x[!is.na(x)]
    if (!length(x)) return(NA_character_)
    tb <- sort(table(x), decreasing = TRUE)
    max_count <- tb[1]
    candidates <- names(tb)[tb == max_count]
    sort(candidates)[1]
  }

  cols_all <- names(data)
  cols_use <- if (is.null(cols)) cols_all else intersect(cols, cols_all)
  if (!is.null(exclude)) cols_use <- setdiff(cols_use, exclude)
  if (!length(cols_use)) return(data)

  for (nm in cols_use) {
    x <- data[[nm]]
    n_na <- sum(is.na(x))
    if (!n_na) {
      if (verbose) message(sprintf("impute_missing: '%s' -> 0 NA", nm))
      next
    }

    # Numeric
    if (is.numeric(x)) {
      repl <- switch(
        num_method,
        median   = if (sum(!is.na(x)) > 0) stats::median(x, na.rm = TRUE)  else num_constant,
        mean     = if (sum(!is.na(x)) > 0) base::mean(x, na.rm = TRUE)   else num_constant,
        constant = num_constant
      )
      x[is.na(x)] <- repl
      data[[nm]] <- x
      if (verbose) message(sprintf("impute_missing: '%s' (numeric) -> %d NA imputés (%s%s%g)",
                                   nm, n_na, num_method,
                                   if (num_method == "constant") "=" else ": ", repl))
      next
    }

    # Logical -> majorité
    if (is.logical(x)) {
      if (sum(!is.na(x)) == 0) {
        repl_log <- FALSE
      } else {
        count_true  <- sum(x, na.rm = TRUE)
        count_false <- sum(!x, na.rm = TRUE)
        repl_log <- count_true >= count_false
      }
      x[is.na(x)] <- repl_log
      data[[nm]] <- x
      if (verbose) message(sprintf("impute_missing: '%s' (logical) -> %d NA imputés (%s)", nm, n_na, repl_log))
      next
    }

    # Character / Factor -> catégorielle
    is_fac <- is.factor(x)
    lev <- if (is_fac) levels(x) else NULL
    x_chr <- as.character(x)

    repl_chr <- switch(
      cat_method,
      mode = {
        m <- mode_char(x_chr)
        if (is.na(m)) cat_constant else m
      },
      constant  = cat_constant,
      new_level = cat_constant
    )

    x_chr[is.na(x_chr)] <- repl_chr

    if (is_fac) {
      new_levels <- lev
      if (!(repl_chr %in% new_levels)) new_levels <- c(new_levels, repl_chr)
      data[[nm]] <- factor(x_chr, levels = new_levels)
    } else {
      data[[nm]] <- x_chr
    }

    if (verbose) message(sprintf("impute_missing: '%s' (categorical) -> %d NA imputés (%s%s%s)",
                                 nm, n_na, cat_method,
                                 if (cat_method == "mode") ": " else "=",
                                 repl_chr))
  }

  data
}