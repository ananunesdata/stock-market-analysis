# 01 Data
# Data directory
BASE <- "data"

# Local data file paths
path_msci       <- file.path(BASE, "MCSI europe.csv")
path_pib        <- file.path(BASE, "PIB todos.xlsx")
path_pib_pt_ie  <- file.path(BASE, "PIB Irlanda Portugal.xlsx")
path_rnb_pt_ie  <- file.path(BASE, "RNB Irlanda Portugal.xlsx")
path_pib_ze     <- file.path(BASE, "PIB Euro.xlsx")
path_rnb_ze     <- file.path(BASE, "RNB Euro.xlsx")
path_pop        <- file.path(BASE, "population annualy.xlsx")
path_cci        <- file.path(BASE, "consumer confidence monthly.xlsx")
path_desemprego <- file.path(BASE, "unemployment rate monthly.xlsx")
path_gini       <- file.path(BASE, "GINI.xlsx")
path_gini_real  <- file.path(BASE, "eurozone gini.xlsx")
path_crime      <- file.path(BASE, "homicide.csv")
path_corrupcao  <- file.path(BASE, "political-corruption-index.csv")
path_emissions  <- file.path(BASE, "co-emissions-per-capita.csv")
path_inflacao   <- file.path(BASE, "inflação percentagem.csv")
path_juros_10a  <- file.path(BASE, "long term rates.xlsx")
path_juros_3m   <- file.path(BASE, "short term rates.xlsx")


# Raw data import

dados_raw <- read.csv(
  path_msci,
  header = FALSE
)

dados_pib <- read_excel(
  path_pib_pt_ie,
  sheet = 2,
  col_names = FALSE
)

dados_rnb <- read_excel(
  path_rnb_pt_ie,
  sheet = 2,
  col_names = FALSE
)

dados_pib_ze <- read_excel(
  path_pib_ze,
  sheet = 2,
  col_names = FALSE
)

dados_rnb_ze <- read_excel(
  path_rnb_ze,
  sheet = 2,
  col_names = FALSE
)

df_cci_raw <- read_excel(
  path_cci,
  sheet = 1,
  skip = 6
)

df_desemprego_raw <- read_excel(
  path_desemprego,
  sheet = 1,
  skip = 6
)

corrupcao_raw <- read_csv(
  path_corrupcao
)

emissions_raw <- read.csv(
  path_emissions,
  skip = 1,
  header = FALSE,
  col.names = c(
    "country",
    "code",
    "year",
    "emissions_pc",
    "region"
  )
)

df_10a <- read_excel(
  path_juros_10a,
  sheet = 2
)

df_3m <- read_excel(
  path_juros_3m,
  sheet = 2
)

dados_mensais_raw <- read.csv(
  path_inflacao
)


# GDP and population data with a consistent layout

pib_long <- ler_bloco_excel(
  path_pib,
  sheet = 2,
  linha_anos = c(9, 32),
  linhas_paises = 3:23
) %>%
  rename(
    Entity = entity,
    Year = year,
    pib = value
  ) %>%
  filter(!is.na(pib))


pop_long <- ler_bloco_excel(
  path_pop,
  sheet = 2,
  linha_anos = c(10, 32),
  linhas_paises = 4:23
) %>%
  rename(
    Entity = entity,
    Year = year,
    population = value
  ) %>%
  filter(!is.na(population))


# Shared constants

paises_zona_euro <- c(
  "Austria", "Belgium", "Croatia", "Cyprus", "Estonia",
  "Finland", "France", "Germany", "Greece", "Ireland",
  "Italy", "Latvia", "Lithuania", "Luxembourg", "Malta",
  "Netherlands", "Portugal", "Slovakia", "Slovenia", "Spain"
)

paises_gini <- c(
  "Germany", "Greece", "Spain", "France", "Italy", "Portugal",
  "Belgium", "Ireland", "Netherlands", "Austria", "Finland"
)