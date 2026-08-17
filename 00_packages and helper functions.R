# 00 Packages
pacotes <- c(
  "readxl", "readr",
  "dplyr", "tidyr", "tidyverse",
  "ggplot2", "ggrepel",
  "lubridate",
  "zoo",
  "mFilter",
  "lmtest", "sandwich",
  "tseries", "urca",
  "patchwork",
  "depmixS4"
)

novos <- pacotes[!pacotes %in% installed.packages()[, "Package"]]
if (length(novos)) install.packages(novos)

invisible(lapply(pacotes, library, character.only = TRUE))

select <- dplyr::select
filter <- dplyr::filter


# Utility functions shared across all modules


# Converts strings such as "2003Q1" / "2003" into dates
periodo_para_data <- function(periodo) {
  periodo <- gsub("-Q", "Q", periodo)
  
  case_when(
    grepl("Q1", periodo) ~ as.Date(paste0(substr(periodo, 1, 4), "-01-01")),
    grepl("Q2", periodo) ~ as.Date(paste0(substr(periodo, 1, 4), "-04-01")),
    grepl("Q3", periodo) ~ as.Date(paste0(substr(periodo, 1, 4), "-07-01")),
    grepl("Q4", periodo) ~ as.Date(paste0(substr(periodo, 1, 4), "-10-01")),
    TRUE                 ~ as.Date(paste0(periodo, "-07-01"))
  )
}


# Detects outliers using the IQR method within groups
identify_outliers <- function(df, group_var, value_var) {
  df %>%
    group_by({{ group_var }}) %>%
    mutate(
      q1         = quantile({{ value_var }}, 0.25, na.rm = TRUE),
      q3         = quantile({{ value_var }}, 0.75, na.rm = TRUE),
      iqr        = q3 - q1,
      limite_inf = q1 - 1.5 * iqr,
      limite_sup = q3 + 1.5 * iqr,
      outlier    = {{ value_var }} < limite_inf | {{ value_var }} > limite_sup
    ) %>%
    ungroup()
}


# Applies the HP filter and returns a tibble with the cycle and trend
hp_decompose <- function(serie, freq) {
  hp <- hpfilter(serie, freq = freq, type = "lambda")
  tibble(
    tendencia = as.numeric(hp$trend),
    ciclo = as.numeric(hp$cycle)
  )
}


# Reads a country/year block from an Excel sheet with a fixed layout
ler_bloco_excel <- function(path, sheet, linha_anos, linhas_paises,
                            colunas_step = 2, ano_min = 1997, ano_max = 2024) {
  raw           <- read_excel(path, sheet = sheet, col_names = FALSE)
  colunas_dados <- c(1, seq(2, ncol(raw), by = colunas_step))
  sub           <- raw[linha_anos[1]:linha_anos[2], colunas_dados]
  anos          <- as.integer(unlist(sub[1, -1]))
  idx           <- which(anos >= ano_min & anos <= ano_max)
  
  sub[linhas_paises, c(1, idx + 1)] %>%
    setNames(c("entity", as.character(anos[idx]))) %>%
    pivot_longer(-entity, names_to = "year", values_to = "value") %>%
    mutate(
      year = as.integer(year),
      value = as.numeric(value)
    )
}


# Calculates the coefficient of variation by group
cv <- function(df, grupo, variavel) {
  df %>%
    group_by({{ grupo }}) %>%
    summarise(
      cv = sd({{ variavel }}, na.rm = TRUE) / mean({{ variavel }}, na.rm = TRUE),
      .groups = "drop"
    )
}


# Shared visualization helpers

tema <- theme_minimal() +
  theme(
    axis.text.x      = element_text(angle = 45, hjust = 1),
    legend.position  = "top",
    panel.grid.minor = element_blank()
  )

escala_x <- scale_x_date(
  date_breaks = "2 years",
  date_labels = "%Y"
)

cores_hp <- scale_colour_manual(
  values = c("Observed" = "blue", "HP Trend" = "red")
)


# Generic HP cycle plot (green/red ribbons)
plot_ciclo_hp <- function(df, x, ciclo, y_lab) {
  ggplot(df, aes(x = {{ x }}, y = {{ ciclo }})) +
    geom_ribbon(
      aes(
        ymin = 0,
        ymax = pmax({{ ciclo }}, 0)
      ),
      fill = "green",
      alpha = 0.2
    ) +
    geom_ribbon(
      aes(
        ymin = pmin({{ ciclo }}, 0),
        ymax = 0
      ),
      fill = "red",
      alpha = 0.2
    ) +
    geom_line(
      colour = "darkgreen",
      linewidth = 1
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey50"
    ) +
    escala_x +
    labs(
      x = "Year",
      y = y_lab
    ) +
    tema
}


# Generic CCF plot
plot_ccf <- function(x, y, lag = 12, ...) {
  ccf(
    x,
    y,
    lag.max = lag,
    main = "",
    ylab = "Correlation",
    xlab = "Lag (months)",
    ...
  )
}