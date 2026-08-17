# 04 tests
# 1. MSCI — return stationarity

cat("\n══ 1. MSCI — ADF on returns ══\n")
summary(ur.df(df_retornos$Retorno_t, type = "drift", lags = 1))


# 2. GDP vs GNI — Euro Area (HAC regression + R²)

cat("\n══ 2. GDP vs GNI Euro Area — HAC regression ══\n")
modelo_ze     <- lm(ciclo_pib ~ ciclo_rnb, data = df_ze_anual)
resultado_hac <- coeftest(modelo_ze, vcov = NeweyWest(modelo_ze))
print(resultado_hac)
cat("R²:", round(summary(modelo_ze)$r.squared * 100, 2), "%\n")


# 3. Unemployment and Consumer Confidence Index — CCF

cat("\n══ 3. CCF unemployment vs CCI ══\n")
plot_ccf(df_economia$ciclo_desemprego, df_economia$ciclo_cci)


# 4. Yield curve vs inflation — CCF

cat("\n══ 4. CCF spread vs inflation ══\n")
plot_ccf(df_curva$ciclo, dados_mensais$ciclo, lag = 24)


# 5. Gini — stationarity and HAC regression

cat("\n══ 5. Weighted Gini — ADF on residuals (linear detrending) ══\n")
print(adf.test(dados_validacao_gini$resid_euro))

cat("\n══ 5. Real Gini — ADF on residuals (linear detrending) ══\n")
print(adf.test(dados_validacao_gini$resid_real))

cat("\n══ 5. HAC: Real Gini ~ Weighted Gini (residuals) ══\n")
modelo_gini <- lm(resid_real ~ resid_euro, data = dados_validacao_gini)
print(coeftest(modelo_gini, vcov = vcovHAC(modelo_gini)))
cat("R²:", round(summary(modelo_gini)$r.squared * 100, 2), "%\n")
cat("N:", nrow(dados_validacao_gini), "observations\n")


# 6. Crime vs corruption — ADF, KPSS and CCF
cat("\n══ 6. Crime — KPSS on cycle ══\n")
print(kpss.test(dados_ciclos$ciclo_crime, null = "Level"))

cat("\n══ 6. Corruption — KPSS on cycle ══\n")
print(kpss.test(dados_ciclos$ciclo_corrupcao, null = "Level"))

cat("\n══ 6. CCF crime vs corruption ══\n")
ccf_resultado <- plot_ccf(dados_ciclos$ciclo_crime, dados_ciclos$ciclo_corrupcao, lag = 5)
idx_ccf   <- which.max(abs(ccf_resultado$acf))
lag_otimo <- ccf_resultado$lag[idx_ccf]
cor_otima <- ccf_resultado$acf[idx_ccf]
cat(sprintf("Maximum correlation at lag = %d (r = %.3f)\n", lag_otimo, cor_otima))


# 7. Descriptive statistics

cat("\n══ 7. Inflation — minimum and maximum ══\n")
cat("Minimum:", min(dados_mensais$Taxa_Inflacao, na.rm = TRUE), "%",
    "- Date:", format(dados_mensais$Data[which.min(dados_mensais$Taxa_Inflacao)], "%B %Y"), "\n")
cat("Maximum:", max(dados_mensais$Taxa_Inflacao, na.rm = TRUE), "%",
    "- Date:", format(dados_mensais$Data[which.max(dados_mensais$Taxa_Inflacao)], "%B %Y"), "\n")

cat("\n══ 7. Weighted Gini — minimum and maximum ══\n")
cat("Maximum:", max(resultado_gini$gini_euro, na.rm = TRUE),
    "| year:", resultado_gini$year[which.max(resultado_gini$gini_euro)], "\n")
cat("Minimum:", min(resultado_gini$gini_euro, na.rm = TRUE),
    "| year:", resultado_gini$year[which.min(resultado_gini$gini_euro)], "\n")

cat("\n══ 7. Homicide — minimum and maximum ══\n")
cat("Maximum:", max(crime_euro_weighted$homicide_euro, na.rm = TRUE),
    "| year:", crime_euro_weighted$year[which.max(crime_euro_weighted$homicide_euro)], "\n")
cat("Minimum:", min(crime_euro_weighted$homicide_euro, na.rm = TRUE),
    "| year:", crime_euro_weighted$year[which.min(crime_euro_weighted$homicide_euro)], "\n")

cat("\n══ 7. Emissions — minimum and maximum ══\n")
cat("Maximum:", max(emissions_euro$emissions_total),
    "| year:", emissions_euro$year[which.max(emissions_euro$emissions_total)], "\n")
cat("Minimum:", min(emissions_euro$emissions_total),
    "| year:", emissions_euro$year[which.min(emissions_euro$emissions_total)], "\n")

cat("\n══ 7. Ireland vs Portugal crossovers ══\n")
crossover <- function(tipo_filtro) {
  df_total %>%
    filter(tipo == tipo_filtro) %>%
    select(periodo, pais, valor) %>%
    pivot_wider(names_from = pais, values_from = valor) %>%
    mutate(diff = ireland - portugal) %>%
    filter(diff >= 0) %>%
    slice(1) %>%
    pull(periodo)
}
cat("GDP — Ireland overtakes Portugal in:", crossover("PIB"), "\n")
cat("GNI — Ireland overtakes Portugal in:", crossover("RNB"), "\n")

cat("\n══ 7. Coefficient of variation — total change in GDP vs population ══\n")
variacao_total <- cv_comparacao %>%
  summarise(var_total_cv_pib = last(cv_pib) - first(cv_pib),
            var_total_cv_pop = last(cv_pop) - first(cv_pop))
print(variacao_total)

if (variacao_total$var_total_cv_pib > variacao_total$var_total_cv_pop) {
  cat("Economic disparity (GDP CV) increased more than demographic disparity between",
      first(cv_comparacao$ano_int), "and", last(cv_comparacao$ano_int), ".\n")
} else {
  cat("Demographic disparity (population CV) increased more (or equally) between",
      first(cv_comparacao$ano_int), "and", last(cv_comparacao$ano_int), ".\n")
}

cat("\n══ 7. GDP medians in key years ══\n")
pib_anos %>%
  group_by(ano) %>%
  summarise(mediana_bilhoes = median(pib, na.rm = TRUE) / 1000) %>%
  print()

cat("\n══ 7. Population medians in key years ══\n")
pop_anos %>%
  group_by(year) %>%
  summarise(mediana_milhoes = median(population, na.rm = TRUE) / 1e6) %>%
  print()

cat("\n══ 7. GDP outliers ══\n")
pib_anos_out %>%
  filter(outlier) %>%
  select(pais, ano, pib) %>%
  arrange(ano, desc(pib)) %>%
  print()

cat("\n══ 7. Population outliers ══\n")
pop_anos_out %>%
  filter(outlier) %>%
  select(country, year, population) %>%
  arrange(year, desc(population)) %>%
  print()