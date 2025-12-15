DataCleanR — pipelines de nettoyage de données salaires
=======================================================

DataCleanR fournit des fonctions simples et des pipelines orchestrés pour :

- vérifier et standardiser vos colonnes (schéma, types, doublons) ;
- normaliser les valeurs métier (pays, devises, tailles d’entreprise, intitulés de poste) ;
- traiter les valeurs manquantes et les outliers salariaux ;
- convertir les salaires en USD et exporter les jeux de données nettoyés.

Installation
------------

```r
# depuis le dépôt GitHub
devtools::install_git("https://github.com/loic-mmt/DataCleanR.git")
```

Usage rapide
------------

```r
library(DataCleanR)

# Pipeline prêt pour la modélisation (standardisation, déduplication,
# winsorisation salaire, imputation, normalisation, devise, finalisation)
clean <- pipeline_ml_ready(
  data = "data/salaries.csv",
  required_columns = c("work_year", "salary", "salary_currency",
                       "job_title", "employee_residence",
                       "company_location", "company_size",
                       "experience_level", "remote_ratio"),
  finalize = TRUE
)

# Export et rapport
export_pipeline(
  in_path = "data/salaries.csv",
  out_path = "data/salaries_clean.csv",
  report_path = "data/cleaning_report.txt",
  mode = "ml_ready"
)
```

Fonctionnalités clés
--------------------

- Validation : `read_raw_csv()`, `validate_schema()`, `validate_ranges()`,
  `standardize_colnames()`, `enforce_types()`, `deduplicate_rows()`.
- Normalisation : `normalize_all()` (pays, taille d’entreprise, emploi, télétravail),
  tables de référence (`mapping_*`).
- Qualité : `cap_outliers_salary()` pour limiter les extrêmes salariaux.
- Valeurs manquantes : `impute_missing()` avec stratégies num/cat.
- Devises : `convert_currency_to_usd()` basé sur `exchange_rates_to_usd`.
- Pipelines : de `pipeline_minimal()` à `pipeline_ml_ready()` plus `export_pipeline()`.

Documentation
-------------

- Référence : `packages/DataCleanR/docs/reference/index.html`
- Accueil : `packages/DataCleanR/docs/index.html`
