# 03 Graphs

cores_pt_ie <- c(
  "ireland_PIB" = "darkred",
  "ireland_RNB" = "salmon",
  "portugal_PIB" = "darkblue",
  "portugal_RNB" = "lightblue"
)

cores_ze <- c(
  "ZonaEuro_PIB" = "red",
  "ZonaEuro_RNB" = "blue"
)

cores_pib_paises <- c(
  "Germany" = "#1f4e79",
  "France" = "#2e75b6",
  "Italy" = "#70ad47",
  "Spain" = "#ffc000",
  "Netherlands" = "#ed7d31",
  "Ireland" = "#00b050",
  "Belgium" = "#7030a0",
  "Austria" = "#ff0000",
  "Finland" = "#00b0f0",
  "Portugal" = "#92d050",
  "Outros" = "#808080"
)

cores_pop_paises <- c(
  "Germany" = "#1f4e79",
  "France" = "#2e75b6",
  "Italy" = "#70ad47",
  "Spain" = "#ffc000",
  "Outros" = "#808080",
  "Netherlands" = "#ed7d31",
  "Belgium" = "#7030a0",
  "Portugal" = "#ff0000",
  "Greece" = "#00b0f0",
  "Austria" = "#92d050",
  "Finland" = "#00b050"
)

# 1. MSCI — market index

ggplot(df_msci, aes(x = Data, y = MSCI)) +
  geom_line(color = "blue") +
  escala_x +
  labs(
    title = "MSCI Europe Index",
    x = "Date",
    y = "MSCI"
  ) +
  tema

# 2. MSCI — autocorrelation of returns

acf(
  df_retornos$Retorno_t,
  lag.max = 24,
  main = "Autocorrelation of MSCI Returns",
  xlim = c(0.5, 24.5),
  ylim = c(-0.15, 0.15),
  xaxs = "i",
  axes = FALSE
)

axis(1, at = 1:24, labels = 1:24, cex.axis = 0.6, las = 1)
axis(2)
box()
abline(h = 0, col = "black")

# 3. MSCI — 12-month rolling volatility

ggplot(df_volatilidade, aes(x = date, y = Volatilidade_Mensal)) +
  geom_line(color = "blue", linewidth = 0.8) +
  escala_x +
  labs(
    title = "12-Month Rolling Volatility of MSCI Returns",
    x = "Year",
    y = "Volatility (standard deviation of returns)"
  ) +
  tema

# 4. GDP and GNI — Portugal vs Ireland

ggplot(df_total, aes(x = data, y = valor, colour = serie, group = serie)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = cores_pt_ie,
    labels = c(
      "ireland_PIB" = "Ireland — GDP",
      "ireland_RNB" = "Ireland — GNI",
      "portugal_PIB" = "Portugal — GDP",
      "portugal_RNB" = "Portugal — GNI"
    )
  ) +
  escala_x +
  scale_y_continuous(limits = y_lim_comum) +
  labs(
    title = "GDP and GNI: Portugal vs Ireland",
    x = "Year",
    y = "Index (2010 = 100)",
    colour = NULL
  ) +
  tema

# 5. GDP and GNI — Euro Area

ggplot(df_ze, aes(x = data, y = valor, colour = serie, group = serie)) +
  geom_line(linewidth = 0.8) +
  scale_colour_manual(
    values = cores_ze,
    labels = c(
      "ZonaEuro_PIB" = "GDP",
      "ZonaEuro_RNB" = "GNI"
    )
  ) +
  escala_x +
  scale_y_continuous(limits = y_lim_comum) +
  labs(
    title = "GDP and GNI: Euro Area",
    x = "Year",
    y = "Index (2010 = 100)",
    colour = NULL
  ) +
  tema

# 6. GDP — country shares

ggplot(pib_plot, aes(x = "", y = percentagem, fill = pais)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "white",
    linewidth = 0.5
  ) +
  coord_polar("y") +
  scale_fill_manual(
    values = cores_pib_paises,
    labels = setNames(pib_plot$label_legenda, pib_plot$pais)
  ) +
  geom_text(
    aes(
      label = ifelse(
        percentagem >= 5,
        paste0(round(percentagem, 1), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  labs(
    title = "Euro Area GDP Share by Country",
    fill = "Country"
  ) +
  theme_void() +
  theme(legend.position = "right")

# 7. GDP — boxplot for 1997 / 2011 / 2024

ggplot(pib_anos_out, aes(x = ano, y = pib / 1000, fill = ano)) +
  geom_boxplot(
    show.legend = FALSE,
    outlier.shape = NA
  ) +
  geom_jitter(
    data = filter(pib_anos_out, !outlier),
    width = 0.15,
    alpha = 0.3,
    size = 1.5,
    show.legend = FALSE
  ) +
  geom_point(
    data = filter(pib_anos_out, outlier),
    color = "red",
    size = 2,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = filter(pib_anos_out, outlier),
    aes(label = pais),
    size = 3,
    fontface = "bold",
    max.overlaps = Inf
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 0)),
    vjust = -0.8,
    size = 3.2,
    color = "black"
  ) +
  scale_y_continuous(
    breaks = seq(0, 5000, by = 250)
  ) +
  labs(
    title = "GDP Distribution Across Euro Area Countries",
    x = "Year",
    y = "GDP (€ billions)"
  ) +
  tema

# 8. Population — country shares

ggplot(pop_2024, aes(x = "", y = percentagem, fill = pais)) +
  geom_bar(
    stat = "identity",
    width = 1,
    color = "white",
    linewidth = 0.5
  ) +
  coord_polar("y") +
  scale_fill_manual(
    values = cores_pop_paises,
    labels = setNames(pop_2024$label_legenda, pop_2024$pais)
  ) +
  geom_text(
    aes(
      label = ifelse(
        percentagem >= 5,
        paste0(round(percentagem, 1), "%"),
        ""
      )
    ),
    position = position_stack(vjust = 0.5),
    size = 3.5,
    fontface = "bold",
    color = "white"
  ) +
  labs(
    title = "Euro Area Population Share by Country",
    fill = "Country"
  ) +
  theme_void() +
  theme(legend.position = "right")

# 9. Population — boxplot for 1997 / 2011 / 2024

ggplot(pop_anos_out, aes(x = year, y = population / 1e6, fill = year)) +
  geom_boxplot(
    show.legend = FALSE,
    outlier.shape = NA
  ) +
  geom_jitter(
    data = filter(pop_anos_out, !outlier),
    width = 0.15,
    alpha = 0.3,
    size = 1.5,
    show.legend = FALSE
  ) +
  geom_point(
    data = filter(pop_anos_out, outlier),
    color = "red",
    size = 2,
    show.legend = FALSE
  ) +
  geom_text_repel(
    data = filter(pop_anos_out, outlier),
    aes(label = country),
    size = 3,
    fontface = "bold",
    max.overlaps = Inf
  ) +
  stat_summary(
    fun = median,
    geom = "text",
    aes(label = round(after_stat(y), 1)),
    vjust = -0.8,
    size = 3.2,
    color = "black"
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, by = 5)
  ) +
  labs(
    title = "Population Distribution Across Euro Area Countries",
    x = "Year",
    y = "Population (millions)"
  ) +
  tema

# 10. Unemployment — series and HP trend

ggplot(df_economia, aes(x = date)) +
  geom_line(
    aes(y = Taxa_Desemprego, colour = "Observed"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = tendencia_desemprego, colour = "HP Trend"),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  cores_hp +
  escala_x +
  labs(
    title = "Unemployment Rate: Observed and HP Trend",
    x = "Year",
    y = "Unemployment Rate (%)",
    colour = ""
  ) +
  tema

# Unemployment — HP cycle

plot_ciclo_hp(
  df_economia,
  date,
  ciclo_desemprego,
  "Cyclical Deviation (%)"
) +
  ggtitle("Unemployment Rate — HP Cycle")

# 11. Consumer Confidence — series and HP trend

ggplot(df_economia, aes(x = date)) +
  geom_line(
    aes(y = CCI, colour = "Observed"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = tendencia_cci, colour = "HP Trend"),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  cores_hp +
  escala_x +
  labs(
    title = "Consumer Confidence Index: Observed and HP Trend",
    x = "Year",
    y = "Consumer Confidence Index",
    colour = ""
  ) +
  tema

# Consumer Confidence — HP cycle

plot_ciclo_hp(
  df_economia,
  date,
  ciclo_cci,
  "Cyclical Deviation (%)"
) +
  ggtitle("Consumer Confidence Index — HP Cycle")

# 12. Yield curve — spread and HP trend

ggplot(df_curva, aes(x = date)) +
  geom_area(
    aes(
      y = spread_neg,
      fill = "Inverted Yield Curve (< 0)"
    ),
    alpha = 0.4
  ) +
  geom_line(
    aes(
      y = spread,
      color = "Yield Spread (10Y – 3M)"
    ),
    linewidth = 1
  ) +
  geom_line(
    aes(
      y = tendencia,
      color = "HP Trend"
    ),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  geom_hline(
    yintercept = 0,
    color = "red",
    linetype = "solid",
    alpha = 0.6
  ) +
  scale_color_manual(
    name = NULL,
    values = c(
      "Yield Spread (10Y – 3M)" = "blue",
      "HP Trend" = "red"
    )
  ) +
  scale_fill_manual(
    name = NULL,
    values = c(
      "Inverted Yield Curve (< 0)" = "#d9534f"
    )
  ) +
  escala_x +
  labs(
    title = "Yield Curve Spread: 10-Year vs 3-Month Interest Rates",
    x = "Year",
    y = "Spread (percentage points)"
  ) +
  tema

# Yield curve — HP cycle

plot_ciclo_hp(
  df_curva,
  date,
  ciclo,
  "Cyclical Deviation (pp)"
) +
  ggtitle("Yield Curve Spread — HP Cycle")

# 13. Inflation — series and HP trend

ggplot(dados_mensais, aes(x = Data)) +
  geom_line(
    aes(y = Taxa_Inflacao, colour = "Observed"),
    linewidth = 1
  ) +
  geom_line(
    aes(y = tendencia, colour = "HP Trend"),
    linewidth = 1.2,
    linetype = "dashed"
  ) +
  cores_hp +
  escala_x +
  scale_y_continuous(limits = limites_y) +
  geom_hline(
    yintercept = 0,
    linetype = "dashed",
    color = "gray50"
  ) +
  labs(
    title = "Inflation Rate: Observed and HP Trend",
    x = "Year",
    y = "Inflation (%)",
    colour = ""
  ) +
  tema

# Inflation — HP cycle

plot_ciclo_hp(
  dados_mensais,
  Data,
  ciclo,
  "Cyclical Deviation (%)"
) +
  scale_y_continuous(limits = limites_y) +
  ggtitle("Inflation Rate — HP Cycle")

# 14. CCF — yield curve vs inflation

plot_ccf(
  df_curva$ciclo,
  dados_mensais$ciclo,
  lag = 24
)

# 15. CCF — unemployment vs consumer confidence

plot_ccf(
  df_economia$ciclo_desemprego,
  df_economia$ciclo_cci
)

# 16. GDP-weighted Gini vs actual Gini

ggplot(grafico_gini) +
  geom_line(
    aes(
      x = year,
      y = gini_euro,
      color = "GDP-weighted Gini"
    ),
    linewidth = 1
  ) +
  geom_point(
    aes(
      x = year,
      y = gini_euro,
      color = "GDP-weighted Gini"
    ),
    size = 2
  ) +
  geom_line(
    aes(
      x = year,
      y = gini_real,
      color = "Actual Gini"
    ),
    linewidth = 1,
    na.rm = TRUE
  ) +
  geom_point(
    aes(
      x = year,
      y = gini_real,
      color = "Actual Gini"
    ),
    size = 2,
    na.rm = TRUE
  ) +
  scale_color_manual(
    values = c(
      "GDP-weighted Gini" = "blue",
      "Actual Gini" = "red"
    )
  ) +
  scale_x_continuous(
    breaks = seq(1997, 2024, by = 2)
  ) +
  labs(
    title = "GDP-Weighted vs Actual Gini Index",
    x = "Year",
    y = "Gini Index",
    color = ""
  ) +
  theme_minimal()

# 17. Crime — Euro Area homicide rate

ggplot(
  crime_euro_weighted,
  aes(x = year, y = homicide_euro)
) +
  geom_line(
    color = "blue",
    linewidth = 1
  ) +
  geom_point(
    color = "blue",
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      1997,
      max(crime_euro_weighted$year),
      by = 2
    )
  ) +
  labs(
    title = "Homicide Rate — Euro Area",
    x = "Year",
    y = "Homicides per 100,000 inhabitants"
  ) +
  theme_minimal()

# 18. Corruption — Euro Area averages

ggplot(medias_long, aes(x = Year, y = valor, colour = tipo)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  scale_colour_manual(
    values = c(
      "Simple Average" = "blue",
      "Population-Weighted" = "red",
      "GDP-Weighted" = "green"
    )
  ) +
  labs(
    title = "Political Corruption Index — Euro Area Averages",
    x = "Year",
    y = "Political Corruption Index",
    colour = NULL
  ) +
  ylim(0, 0.3) +
  tema

# Selected countries

corrupcao_ze %>%
  filter(
    Entity %in% c(
      "France",
      "Italy",
      "Portugal",
      "Malta",
      "Finland"
    )
  ) %>%
  ggplot(
    aes(
      x = Year,
      y = `Political Corruption Index`,
      colour = Entity
    )
  ) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 1.5) +
  labs(
    title = "Political Corruption Index — Selected Countries",
    x = "Year",
    y = "Political Corruption Index",
    colour = NULL
  ) +
  ylim(0, 0.3) +
  tema

# 19. CCF — crime vs corruption

plot_ccf(
  dados_ciclos$ciclo_crime,
  dados_ciclos$ciclo_corrupcao,
  lag = 5
)


# 21. CO2 emissions — Euro Area total

ggplot(
  emissions_euro,
  aes(x = year, y = emissions_total)
) +
  geom_line(
    color = "blue",
    linewidth = 1
  ) +
  geom_point(
    color = "blue",
    size = 2
  ) +
  scale_x_continuous(
    breaks = seq(
      1997,
      max(emissions_euro$year),
      by = 2
    )
  ) +
  labs(
    title = "CO2 Emissions — Euro Area",
    x = "Year",
    y = "Emissions (tonnes)"
  ) +
  theme_minimal()