# 02 Data Processing

# 1. MSCI — prices and returns

df_msci <- dados_raw %>%
  rename(Data = V1, MSCI = V2) %>%
  filter(Data != "Date") %>%
  mutate(
    MSCI = as.numeric(MSCI),
    Data = as.Date(paste0("01/", Data), format = "%d/%m/%Y")
  ) %>%
  arrange(Data) %>%
  filter(Data >= as.Date("1997-01-01"))

df_retornos <- df_msci %>%
  mutate(
    Retorno_t = (MSCI - lag(MSCI)) / lag(MSCI)
  ) %>%
  filter(!is.na(Retorno_t))

df_volatilidade <- dados_raw %>%
  slice(-1) %>%
  rename(date = V1, preco = V2) %>%
  mutate(
    date = as.Date(
      parse_date_time(
        date,
        orders = c("dmy", "ymd", "mdy")
      )
    ),
    preco = as.numeric(
      gsub(",", ".", preco)
    )
  ) %>%
  filter(!is.na(date), !is.na(preco)) %>%
  arrange(date) %>%
  mutate(
    retorno_mensal = log(preco / lag(preco)),
    Volatilidade_Mensal = rollapplyr(
      retorno_mensal,
      width = 12,
      FUN = sd,
      fill = NA
    )
  ) %>%
  filter(
    !is.na(Volatilidade_Mensal),
    date >= as.Date("1997-01-01")
  ) %>%
  select(
    date,
    preco,
    retorno_mensal,
    Volatilidade_Mensal
  )


# 2. GDP and GNI — Portugal and Ireland

# Internal function: extracts vectors by row and column from the PT/IE files

extrair_serie <- function(
    raw,
    linha_anos_idx,
    linha_dados_ie,
    linha_dados_pt,
    filtro_min = 1997,
    filtro_max = 2024
) {
  
  linha_anos <- as.character(
    raw[linha_anos_idx, ]
  )
  
  # Identify columns containing valid periods
  # (annual "1975" or quarterly "1975-Q1")
  
  cols <- which(
    grepl("^\\d{4}", linha_anos)
  )
  
  periodos <- linha_anos[cols]
  
  # Extract the year for filtering
  
  anos_num <- as.integer(
    substr(periodos, 1, 4)
  )
  
  idx <- which(
    !is.na(anos_num) &
      anos_num >= filtro_min &
      anos_num <= filtro_max
  )
  
  ie_vals <- suppressWarnings(
    as.numeric(
      raw[linha_dados_ie, cols[idx]]
    )
  )
  
  pt_vals <- suppressWarnings(
    as.numeric(
      raw[linha_dados_pt, cols[idx]]
    )
  )
  
  list(
    periodos = periodos[idx],
    ireland = ie_vals,
    portugal = pt_vals
  )
}

pib_info <- extrair_serie(
  dados_pib,
  linha_anos_idx = 9,
  linha_dados_ie = 11,
  linha_dados_pt = 12
)

rnb_info <- extrair_serie(
  dados_rnb,
  linha_anos_idx = 11,
  linha_dados_ie = 13,
  linha_dados_pt = 14
)

df_total <- bind_rows(
  data.frame(
    periodo = pib_info$periodos,
    ireland = pib_info$ireland,
    portugal = pib_info$portugal,
    tipo = "PIB"
  ),
  data.frame(
    periodo = rnb_info$periodos,
    ireland = rnb_info$ireland,
    portugal = rnb_info$portugal,
    tipo = "RNB"
  )
) %>%
  pivot_longer(
    c(ireland, portugal),
    names_to = "pais",
    values_to = "valor"
  ) %>%
  mutate(
    serie = paste0(pais, "_", tipo),
    data = periodo_para_data(periodo)
  )


# 3. GDP and GNI — Euro Area

extrair_ze <- function(
    raw,
    linha_anos_idx,
    linha_dados,
    filtro_max = "2024-Q4"
) {
  
  linha_anos <- as.character(
    raw[linha_anos_idx, ]
  )
  
  # Same approach as extrair_serie — supports "1975-Q1" and "1975"
  
  cols <- which(
    grepl("^\\d{4}", linha_anos)
  )
  
  periodos <- linha_anos[cols]
  
  anos_num <- as.integer(
    substr(periodos, 1, 4)
  )
  
  idx <- which(
    !is.na(anos_num) &
      anos_num >= 1997 &
      periodos <= filtro_max
  )
  
  list(
    periodos = periodos[idx],
    valores = as.numeric(
      raw[linha_dados, cols[idx]]
    )
  )
}

ze_pib_raw <- extrair_ze(
  dados_pib_ze,
  linha_anos_idx = 9,
  linha_dados = 11
)

ze_rnb_raw <- extrair_ze(
  dados_rnb_ze,
  linha_anos_idx = 11,
  linha_dados = 13
)

df_ze <- bind_rows(
  data.frame(
    periodo = ze_pib_raw$periodos,
    valor = ze_pib_raw$valores,
    serie = "ZonaEuro_PIB"
  ),
  data.frame(
    periodo = ze_rnb_raw$periodos,
    valor = ze_rnb_raw$valores,
    serie = "ZonaEuro_RNB"
  )
) %>%
  mutate(
    data = periodo_para_data(periodo)
  )


# Annual GDP series and annual average GNI for regression

df_ze_anual <- df_ze %>%
  filter(
    serie == "ZonaEuro_PIB"
  ) %>%
  mutate(
    ano = as.integer(
      format(data, "%Y")
    )
  ) %>%
  select(
    ano,
    pib = valor
  ) %>%
  inner_join(
    df_ze %>%
      filter(
        serie == "ZonaEuro_RNB"
      ) %>%
      mutate(
        ano = as.integer(
          format(data, "%Y")
        )
      ) %>%
      group_by(ano) %>%
      summarise(
        rnb = mean(valor, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "ano"
  ) %>%
  arrange(ano) %>%
  bind_cols(
    hp_decompose(
      .$pib,
      freq = 100
    ) %>%
      rename(
        ciclo_pib = ciclo,
        tend_pib = tendencia
      )
  ) %>%
  bind_cols(
    hp_decompose(
      .$rnb,
      freq = 100
    ) %>%
      rename(
        ciclo_rnb = ciclo,
        tend_rnb = tendencia
      )
  )


# Monthly Euro Area series
# Annual-to-monthly / quarterly-to-monthly expansion

df_ze_mensal <- bind_cols(
  df_ze %>%
    filter(
      serie == "ZonaEuro_PIB"
    ) %>%
    mutate(
      data = as.Date(
        paste0(
          substr(periodo, 1, 4),
          "-01-01"
        )
      )
    ) %>%
    rowwise() %>%
    mutate(
      Data = list(
        seq(
          data,
          by = "month",
          length.out = 12
        )
      )
    ) %>%
    unnest(Data) %>%
    select(
      Data,
      PIB_ZE = valor
    ),
  
  df_ze %>%
    filter(
      serie == "ZonaEuro_RNB"
    ) %>%
    rowwise() %>%
    mutate(
      Data = list(
        seq(
          data,
          by = "month",
          length.out = 3
        )
      )
    ) %>%
    unnest(Data) %>%
    select(
      RNB_ZE = valor
    )
) %>%
  filter(
    Data >= as.Date("1997-01-01"),
    Data <= as.Date("2024-12-01")
  )


# Common y-axis limits for Portugal/Ireland vs. Euro Area charts

y_lim_comum <- range(
  c(
    df_total$valor,
    df_ze$valor
  ),
  na.rm = TRUE
)


# 4. Unemployment and Consumer Confidence Index (CCI)

df_cci <- df_cci_raw %>%
  select(
    date = 1,
    CCI = 3
  ) %>%
  mutate(
    date = ymd(
      paste0(date, "-01")
    ),
    CCI = as.numeric(
      gsub(",", ".", CCI)
    )
  ) %>%
  filter(
    !is.na(date),
    !is.na(CCI)
  )

df_desemprego <- df_desemprego_raw %>%
  select(
    date = 1,
    Taxa_Desemprego = 3
  ) %>%
  mutate(
    date = ymd(
      paste0(
        sub(
          ".*(\\d{4})-(\\d{2}).*",
          "\\1-\\2",
          date
        ),
        "-01"
      )
    ),
    Taxa_Desemprego = as.numeric(
      gsub(
        ",",
        ".",
        trimws(
          gsub(
            "E",
            "",
            Taxa_Desemprego
          )
        )
      )
    )
  ) %>%
  filter(
    !is.na(date),
    !is.na(Taxa_Desemprego)
  )

df_economia <- inner_join(
  df_desemprego,
  df_cci,
  by = "date"
) %>%
  filter(
    date >= as.Date("1997-01-01"),
    date <= as.Date("2024-12-31")
  ) %>%
  arrange(date) %>%
  bind_cols(
    hp_decompose(
      .$Taxa_Desemprego,
      freq = 14400
    ) %>%
      rename(
        tendencia_desemprego = tendencia,
        ciclo_desemprego = ciclo
      ),
    
    hp_decompose(
      .$CCI,
      freq = 14400
    ) %>%
      rename(
        tendencia_cci = tendencia,
        ciclo_cci = ciclo
      )
  )

df_economia_mensal <- df_economia %>%
  rename(
    Data = date
  ) %>%
  dplyr::select(
    Data,
    Taxa_Desemprego,
    CCI
  )


# 5. GDP and population

pib_longo <- pib_long %>%
  rename(
    pais = Entity,
    ano = Year
  ) %>%
  filter(
    pais != "Euro area – 20 countries (2023-2025)"
  )


# Country share of total GDP

pib_plot <- pib_long %>%
  filter(
    Year == max(Year),
    Entity != "Euro area – 20 countries (2023-2025)"
  ) %>%
  rename(
    pais = Entity
  ) %>%
  mutate(
    percentagem = pib / sum(pib, na.rm = TRUE) * 100
  ) %>%
  arrange(
    desc(percentagem)
  ) %>%
  mutate(
    pais_grupo = ifelse(
      row_number() > 10,
      "Other",
      pais
    )
  ) %>%
  group_by(
    pais_grupo
  ) %>%
  summarise(
    percentagem = sum(percentagem),
    .groups = "drop"
  ) %>%
  arrange(
    desc(percentagem)
  ) %>%
  rename(
    pais = pais_grupo
  ) %>%
  mutate(
    label_legenda = paste0(
      pais,
      " (",
      round(percentagem, 1),
      "%)"
    )
  )


# Country share of total population

pop_2024 <- pop_long %>%
  filter(
    Year == 2024,
    Entity != "Euro area – 20 countries (from 2023)"
  ) %>%
  rename(
    pais = Entity
  ) %>%
  mutate(
    percentagem = population /
      sum(population, na.rm = TRUE) * 100
  ) %>%
  arrange(
    desc(percentagem)
  ) %>%
  mutate(
    pais_grupo = ifelse(
      row_number() > 10,
      "Other",
      pais
    )
  ) %>%
  group_by(
    pais_grupo
  ) %>%
  summarise(
    percentagem = sum(percentagem),
    .groups = "drop"
  ) %>%
  arrange(
    desc(percentagem)
  ) %>%
  rename(
    pais = pais_grupo
  ) %>%
  mutate(
    label_legenda = paste0(
      pais,
      " (",
      round(percentagem, 1),
      "%)"
    )
  )


# GDP boxplots for selected years

pib_anos <- pib_longo %>%
  filter(
    ano %in% c(1997, 2011, 2024)
  ) %>%
  mutate(
    ano = as.factor(ano)
  )

pib_anos_out <- identify_outliers(
  pib_anos,
  ano,
  pib
)

pop_anos <- pop_long %>%
  filter(
    Year %in% c(1997, 2011, 2024),
    Entity != "Euro area – 20 countries (from 2023)"
  ) %>%
  rename(
    country = Entity,
    year = Year
  ) %>%
  mutate(
    year = as.factor(year)
  )

pop_anos_out <- identify_outliers(
  pop_anos,
  year,
  population
)


# GDP and population coefficients of variation

cv_pib <- cv(
  pib_longo,
  ano,
  pib
) %>%
  mutate(
    ano_int = as.integer(
      as.character(ano)
    )
  ) %>%
  select(
    ano_int,
    cv_pib = cv
  )

cv_pop <- cv(
  pop_long %>%
    rename(
      country = Entity,
      year = Year
    ) %>%
    filter(
      country != "Euro area – 20 countries (from 2023)"
    ),
  year,
  population
) %>%
  rename(
    ano_int = year,
    cv_pop = cv
  )

cv_comparacao <- left_join(
  cv_pib,
  cv_pop,
  by = "ano_int"
) %>%
  arrange(ano_int) %>%
  mutate(
    var_cv_pib = cv_pib - lag(cv_pib),
    var_cv_pop = cv_pop - lag(cv_pop)
  )


# 6. Social factors

# Total CO2 emissions in the Euro Area

emissions_euro <- emissions_raw %>%
  filter(
    country %in% paises_zona_euro,
    year >= 1997
  ) %>%
  left_join(
    pop_long %>%
      rename(
        country = Entity,
        year = Year
      ),
    by = c("country", "year")
  ) %>%
  group_by(year) %>%
  summarise(
    emissions_total = sum(
      emissions_pc * population,
      na.rm = TRUE
    ),
    .groups = "drop"
  )


# Monthly date base

datas_mensais <- tibble(
  Data = seq(
    as.Date("1997-01-01"),
    as.Date("2024-12-01"),
    by = "month"
  )
)


# Monthly CO2 emissions

emissions_mensal <- emissions_euro %>%
  mutate(
    Data = as.Date(
      paste0(year, "-01-01")
    )
  ) %>%
  select(
    Data,
    emissions_total
  ) %>%
  right_join(
    datas_mensais,
    by = "Data"
  ) %>%
  arrange(Data) %>%
  mutate(
    emissions_interp = zoo::na.approx(
      emissions_total,
      na.rm = FALSE
    )
  ) %>%
  fill(
    emissions_interp,
    .direction = "down"
  ) %>%
  fill(
    emissions_total,
    .direction = "down"
  ) %>%
  rename(
    emissions_constante = emissions_total
  )


# Corruption

corrupcao_ze <- corrupcao_raw %>%
  filter(
    Entity %in% paises_zona_euro,
    Year >= 1997,
    Year <= 2024
  ) %>%
  select(
    Entity,
    Code,
    Year,
    `Political Corruption Index`
  )

corrupcao_mensal <- corrupcao_ze %>%
  rename(
    year = Year,
    corrupcao = `Political Corruption Index`
  ) %>%
  group_by(year) %>%
  summarise(
    corrupcao_euro = mean(
      corrupcao,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) %>%
  mutate(
    Data = as.Date(
      paste0(year, "-01-01")
    )
  ) %>%
  select(
    Data,
    corrupcao_euro
  ) %>%
  right_join(
    datas_mensais,
    by = "Data"
  ) %>%
  arrange(Data) %>%
  mutate(
    corrupcao_interp = zoo::na.approx(
      corrupcao_euro,
      na.rm = FALSE
    )
  ) %>%
  fill(
    corrupcao_interp,
    .direction = "down"
  ) %>%
  fill(
    corrupcao_euro,
    .direction = "down"
  ) %>%
  rename(
    corrupcao_constante = corrupcao_euro
  )


# Corruption averages

base_pond <- corrupcao_ze %>%
  inner_join(
    pop_long,
    by = c(
      "Entity" = "Entity",
      "Year" = "Year"
    )
  ) %>%
  inner_join(
    pib_long,
    by = c(
      "Entity" = "Entity",
      "Year" = "Year"
    )
  )

media_simples <- corrupcao_ze %>%
  group_by(Year) %>%
  summarise(
    media_simples = mean(
      `Political Corruption Index`,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

media_pop <- base_pond %>%
  group_by(Year) %>%
  summarise(
    media_pop = weighted.mean(
      `Political Corruption Index`,
      w = population,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

media_pib_corr <- base_pond %>%
  group_by(Year) %>%
  summarise(
    media_pib = weighted.mean(
      `Political Corruption Index`,
      w = pib,
      na.rm = TRUE
    ),
    .groups = "drop"
  )

medias_long <- media_simples %>%
  left_join(
    media_pop,
    by = "Year"
  ) %>%
  left_join(
    media_pib_corr,
    by = "Year"
  ) %>%
  pivot_longer(
    -Year,
    names_to = "tipo",
    values_to = "valor"
  ) %>%
  mutate(
    tipo = recode(
      tipo,
      "media_simples" = "Simple Average",
      "media_pop" = "Population-Weighted",
      "media_pib" = "GDP-Weighted"
    )
  )


# GDP-weighted Gini coefficient

euro_gdp <- pib_long %>%
  filter(
    Entity == "Euro area – 20 countries (2023-2025)"
  ) %>%
  select(
    year = Year,
    euro_area = pib
  )

shares_wide <- pib_long %>%
  filter(
    Entity %in% paises_gini
  ) %>%
  left_join(
    euro_gdp,
    by = c("Year" = "year")
  ) %>%
  mutate(
    share_pct = pib / euro_area * 100
  ) %>%
  select(
    country = Entity,
    year = Year,
    share_pct
  ) %>%
  pivot_wider(
    names_from = country,
    values_from = share_pct,
    names_glue = "{country}_share"
  )

resultado_gini <- read_excel(
  path_gini,
  sheet = 1
) %>%
  rename(
    year = Date
  ) %>%
  filter(
    year >= 1997,
    year <= 2024
  ) %>%
  left_join(
    shares_wide,
    by = "year"
  ) %>%
  mutate(
    across(
      all_of(paises_gini),
      ~ . * get(
        paste0(
          cur_column(),
          "_share"
        )
      ) / 100,
      .names = "{.col}_w"
    ),
    peso_total = rowSums(
      across(ends_with("_share")),
      na.rm = TRUE
    ),
    gini_euro = rowSums(
      across(ends_with("_w")),
      na.rm = TRUE
    ) / peso_total * 100
  ) %>%
  select(
    year,
    ends_with("_w"),
    peso_total,
    gini_euro
  )

gini_real <- read_excel(
  path_gini_real,
  sheet = 1,
  col_names = FALSE
) %>%
  setNames(
    c(
      "year",
      "gini_real"
    )
  ) %>%
  mutate(
    year = as.integer(year)
  ) %>%
  filter(
    year >= 1997,
    year <= 2024
  )

grafico_gini <- resultado_gini %>%
  select(
    year,
    gini_euro
  ) %>%
  left_join(
    gini_real,
    by = "year"
  ) %>%
  arrange(year)


# Monthly Gini series

gini_mensal_constante <- grafico_gini %>%
  mutate(
    Data = as.Date(
      paste0(
        year,
        "-01-01"
      )
    )
  ) %>%
  select(
    Data,
    gini_euro
  ) %>%
  right_join(
    datas_mensais,
    by = "Data"
  ) %>%
  arrange(Data) %>%
  fill(
    gini_euro,
    .direction = "down"
  ) %>%
  rename(
    gini_constante = gini_euro
  )

gini_mensal_interp <- grafico_gini %>%
  mutate(
    Data = as.Date(
      paste0(
        year,
        "-01-01"
      )
    )
  ) %>%
  select(
    Data,
    gini_euro
  ) %>%
  right_join(
    datas_mensais,
    by = "Data"
  ) %>%
  arrange(Data) %>%
  mutate(
    gini_interp = zoo::na.approx(
      gini_euro,
      na.rm = FALSE
    )
  ) %>%
  fill(
    gini_interp,
    .direction = "down"
  ) %>%
  select(
    Data,
    gini_interp
  )

gini_mensal <- gini_mensal_constante %>%
  left_join(
    gini_mensal_interp,
    by = "Data"
  )


# Crime — population-weighted homicide rates

pop_crime <- pop_long %>%
  filter(
    Entity %in% paises_gini
  ) %>%
  rename(
    country = Entity,
    year = Year
  )

crime_euro_weighted <- read.csv(
  path_crime,
  skip = 1,
  header = FALSE,
  col.names = c(
    "country",
    "code",
    "year",
    "homicide_rate",
    "region"
  )
) %>%
  filter(
    country %in% paises_gini,
    year >= 1997,
    year <= 2024
  ) %>%
  select(
    country,
    year,
    homicide_rate
  ) %>%
  left_join(
    pop_crime,
    by = c(
      "country",
      "year"
    )
  ) %>%
  filter(
    !is.na(population),
    !is.na(homicide_rate)
  ) %>%
  group_by(year) %>%
  summarise(
    homicide_euro =
      sum(
        homicide_rate * population,
        na.rm = TRUE
      ) /
      sum(
        population,
        na.rm = TRUE
      ),
    .groups = "drop"
  )

crime_mensal_constante <- crime_euro_weighted %>%
  mutate(
    Data = as.Date(
      paste0(
        year,
        "-01-01"
      )
    )
  ) %>%
  select(
    Data,
    homicide_euro
  ) %>%
  right_join(
    datas_mensais,
    by = "Data"
  ) %>%
  arrange(Data) %>%
  fill(
    homicide_euro,
    .direction = "down"
  ) %>%
  rename(
    crime_constante = homicide_euro
  )

crime_mensal_interp <- crime_euro_weighted %>%
  mutate(
    Data = as.Date(
      paste0(
        year,
        "-01-01"
      )
    )
  ) %>%
  select(
    Data,
    homicide_euro
  ) %>%
  right_join(
    datas_mensais,
    by = "Data"
  ) %>%
  arrange(Data) %>%
  mutate(
    crime_interp = zoo::na.approx(
      homicide_euro,
      na.rm = FALSE
    )
  ) %>%
  fill(
    crime_interp,
    .direction = "down"
  ) %>%
  select(
    Data,
    crime_interp
  )

crime_mensal <- crime_mensal_constante %>%
  left_join(
    crime_mensal_interp,
    by = "Data"
  )


# HP cycles for crime vs. corruption regression

dados_ciclos <- crime_euro_weighted %>%
  inner_join(
    media_pib_corr %>%
      rename(
        year = Year
      ),
    by = "year"
  ) %>%
  arrange(year) %>%
  bind_cols(
    hp_decompose(
      .$homicide_euro,
      freq = 100
    ) %>%
      rename(
        ciclo_crime = ciclo,
        tend_crime = tendencia
      ),
    
    hp_decompose(
      .$media_pib,
      freq = 100
    ) %>%
      rename(
        ciclo_corrupcao = ciclo,
        tend_corr = tendencia
      )
  )


# Gini validation — residuals from linear detrending

dados_validacao_gini <- resultado_gini %>%
  select(
    year,
    gini_euro
  ) %>%
  inner_join(
    gini_real,
    by = "year"
  ) %>%
  arrange(year) %>%
  mutate(
    resid_euro = residuals(
      lm(
        gini_euro ~ year,
        data = .
      )
    ),
    resid_real = residuals(
      lm(
        gini_real ~ year,
        data = .
      )
    )
  )


# 7. Yield curve

colnames(df_10a) <- c(
  "date",
  "juros_10a"
)

colnames(df_3m) <- c(
  "date",
  "juros_3m"
)

df_curva <- inner_join(
  df_10a,
  df_3m,
  by = "date"
) %>%
  mutate(
    date = as.Date(date),
    spread = juros_10a - juros_3m,
    spread_neg = ifelse(
      spread < 0,
      spread,
      0
    )
  ) %>%
  filter(
    date >= as.Date("1997-01-01"),
    date <= as.Date("2024-12-31")
  ) %>%
  arrange(date) %>%
  bind_cols(
    hp_decompose(
      .$spread,
      freq = 129600
    ) %>%
      rename(
        tendencia = tendencia,
        ciclo = ciclo
      )
  )


# 8. Inflation

dados_mensais <- dados_mensais_raw %>%
  rename(
    Data = DATE,
    Taxa_Inflacao = 3
  ) %>%
  mutate(
    Data = as.Date(
      Data,
      format = "%Y-%m-%d"
    ),
    Taxa_Inflacao = as.numeric(
      Taxa_Inflacao
    )
  ) %>%
  filter(
    Data >= as.Date("1997-01-01"),
    Data <= as.Date("2024-12-31"),
    !is.na(Taxa_Inflacao)
  ) %>%
  bind_cols(
    hp_decompose(
      .$Taxa_Inflacao,
      freq = 129600
    ) %>%
      rename(
        tendencia = tendencia,
        ciclo = ciclo
      )
  ) %>%
  mutate(
    is_min = Taxa_Inflacao ==
      min(
        Taxa_Inflacao,
        na.rm = TRUE
      ),
    is_max = Taxa_Inflacao ==
      max(
        Taxa_Inflacao,
        na.rm = TRUE
      )
  )

limites_y <- range(
  c(
    dados_mensais$Taxa_Inflacao,
    dados_mensais$tendencia,
    dados_mensais$ciclo
  ),
  na.rm = TRUE
)