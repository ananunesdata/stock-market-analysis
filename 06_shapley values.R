#06 shapley values

# SHAPLEY VALUES FOR THE COMPLETE NH-HMM
# The complete model includes all available covariates.

library(tidyverse)
library(ggplot2)
library(ggbeeswarm)

modelo <- modelo1_todas

STATE_LABELS <- c("Bear", "Bull")

covariavel_labels <- c(
  CCI        = "ICC",
  corrupcao  = "Corrupção",
  crime      = "Criminalidade",
  desemprego = "Desemprego",
  emissions  = "Emissões",
  gini       = "Índice de Gini",
  inflacao   = "Inflação",
  juros_3m   = "Juros a 3 meses",
  RNB_ZE     = "RNB",
  spread     = "Curva de juros"
)

aplicar_labels <- function(df) {
  df %>%
    mutate(covariavel = recode(covariavel, !!!covariavel_labels))
}

get_player_cols <- function(X) {
  setdiff(colnames(X), c("intercept", "t"))
}

softmax_local <- function(x) {
  x <- x - max(x)
  exp(x) / sum(exp(x))
}

build_subset_table <- function(players) {
  M <- length(players)
  all_subsets <- list()
  
  for (m in 0:M) {
    if (m == 0) {
      all_subsets[[length(all_subsets) + 1]] <- character(0)
    } else {
      all_subsets <- c(
        all_subsets,
        combn(players, m, simplify = FALSE)
      )
    }
  }
  
  all_subsets
}

# Calculate Shapley values for the state transition probabilities.

compute_shapley_transitions <- function(model,
                                        t_idx = NULL,
                                        verbose = TRUE) {
  
  beta_mat <- model$beta_mat
  X_trans  <- model$X_trans
  df       <- model$df
  
  N <- dim(beta_mat)[2]
  
  players <- get_player_cols(X_trans)
  M <- length(players)
  
  if (is.null(t_idx)) {
    t_idx <- seq_len(nrow(X_trans))
  }
  
  Xsub <- X_trans[t_idx, , drop = FALSE]
  Tn <- length(t_idx)
  
  subset_matrices <- list()
  weight_vectors <- list()
  
  for (k in players) {
    
    others <- setdiff(players, k)
    
    subs <- build_subset_table(others)
    
    Smat <- matrix(
      0,
      nrow = length(subs),
      ncol = length(players),
      dimnames = list(NULL, players)
    )
    
    for (r in seq_along(subs)) {
      
      if (length(subs[[r]]) > 0) {
        Smat[r, subs[[r]]] <- 1
      }
    }
    
    sizes <- rowSums(Smat)
    
    weights <- (
      factorial(sizes) *
        factorial(M - sizes - 1)
    ) / factorial(M)
    
    subset_matrices[[k]] <- Smat
    weight_vectors[[k]] <- weights
  }
  
  # Calculate transition probabilities for a given subset of covariates.
  
  transition_probability <- function(i, mask_vec) {
    
    Xm <- Xsub
    
    excluded <- players[mask_vec == 0]
    
    if (length(excluded) > 0) {
      Xm[, excluded] <- 0
    }
    
    eta <- matrix(
      0,
      nrow = Tn,
      ncol = N
    )
    
    for (j in seq_len(N)) {
      eta[, j] <- Xm %*% beta_mat[i, j, ]
    }
    
    eta <- eta - apply(eta, 1, max)
    
    exp_eta <- exp(eta)
    
    exp_eta / rowSums(exp_eta)
  }
  
  results <- vector("list", N * M)
  idx <- 1
  
  for (i in seq_len(N)) {
    
    for (k in players) {
      
      Smat <- subset_matrices[[k]]
      weights <- weight_vectors[[k]]
      
      n_subsets <- nrow(Smat)
      
      contribution <- matrix(
        0,
        nrow = Tn,
        ncol = N
      )
      
      for (r in seq_len(n_subsets)) {
        
        mask_S <- setNames(
          Smat[r, ],
          players
        )
        
        mask_Sk <- mask_S
        mask_Sk[k] <- 1
        
        prob_S <- transition_probability(
          i,
          mask_S
        )
        
        prob_Sk <- transition_probability(
          i,
          mask_Sk
        )
        
        contribution <- contribution +
          weights[r] *
          (prob_Sk - prob_S)
      }
      
      for (j in seq_len(N)) {
        
        results[[idx]] <- data.frame(
          Data = df$Data[t_idx],
          t_idx = t_idx,
          from_state = STATE_LABELS[i],
          to_state = STATE_LABELS[j],
          covariavel = k,
          phi = contribution[, j]
        )
        
        idx <- idx + 1
      }
      
      if (verbose) {
        cat(
          sprintf(
            "Origin = %s | Covariate = %s OK\n",
            STATE_LABELS[i],
            k
          )
        )
      }
    }
  }
  
  bind_rows(results)
}

# Check the Shapley efficiency property.

check_efficiency <- function(shap_df, model) {
  
  beta_mat <- model$beta_mat
  X_trans <- model$X_trans
  
  players <- get_player_cols(X_trans)
  
  check <- shap_df %>%
    group_by(
      Data,
      t_idx,
      from_state,
      to_state
    ) %>%
    summarise(
      soma_phi = sum(phi),
      .groups = "drop"
    ) %>%
    mutate(
      i = match(from_state, STATE_LABELS),
      j = match(to_state, STATE_LABELS)
    )
  
  errors <- numeric(nrow(check))
  
  for (r in seq_len(nrow(check))) {
    
    X_row <- setNames(
      X_trans[check$t_idx[r], ],
      colnames(X_trans)
    )
    
    eta_full <- sapply(
      seq_len(dim(beta_mat)[2]),
      function(jj) {
        sum(
          beta_mat[
            check$i[r],
            jj,
          ] * X_row
        )
      }
    )
    
    X_base <- X_row
    X_base[players] <- 0
    
    eta_base <- sapply(
      seq_len(dim(beta_mat)[2]),
      function(jj) {
        sum(
          beta_mat[
            check$i[r],
            jj,
          ] * X_base
        )
      }
    )
    
    prob_full <- softmax_local(eta_full)[check$j[r]]
    prob_base <- softmax_local(eta_base)[check$j[r]]
    
    errors[r] <- abs(
      check$soma_phi[r] -
        (prob_full - prob_base)
    )
  }
  
  cat(
    sprintf(
      "Maximum efficiency error: %.2e\n",
      max(errors)
    )
  )
  
  check$erro <- errors
  
  check
}

# Aggregate Shapley values over time.

aggregate_shapley_transitions <- function(
    shap_df,
    fun = mean) {
  
  shap_df %>%
    mutate(
      transicao = paste0(
        from_state,
        " -> ",
        to_state
      )
    ) %>%
    group_by(
      transicao,
      covariavel
    ) %>%
    summarise(
      phi_agg = fun(phi),
      .groups = "drop"
    )
}

# Produce descriptive statistics for the Shapley values.

summary_shapley_transitions <- function(shap_df) {
  
  shap_df %>%
    mutate(
      transicao = paste0(
        from_state,
        " -> ",
        to_state
      )
    ) %>%
    group_by(
      transicao,
      covariavel
    ) %>%
    summarise(
      mediana = median(phi),
      q25 = quantile(phi, 0.25),
      q75 = quantile(phi, 0.75),
      iqr = q75 - q25,
      media_abs = mean(abs(phi)),
      min = min(phi),
      max = max(phi),
      .groups = "drop"
    )
}

# Plot average Shapley contributions for each transition.

plot_shapley_transitions_bar <- function(shap_agg) {
  
  shap_agg <- aplicar_labels(shap_agg)
  
  ggplot(
    shap_agg,
    aes(
      x = transicao,
      y = phi_agg,
      fill = covariavel
    )
  ) +
    geom_col(
      position = "stack",
      width = 0.7
    ) +
    geom_hline(
      yintercept = 0,
      colour = "black"
    ) +
    labs(
      x = NULL,
      y = "Shapley Value (mean over time)",
      fill = "Covariate"
    ) +
    theme_minimal(base_size = 13) +
    theme(
      axis.text.x = element_text(
        angle = 20,
        hjust = 1
      )
    )
}

# Plot the distribution of Shapley values for each transition.

plot_shapley_boxplot <- function(shap_df) {
  
  df_plot <- shap_df %>%
    mutate(
      transicao = paste0(
        from_state,
        " -> ",
        to_state
      )
    ) %>%
    aplicar_labels()
  
  ggplot(
    df_plot,
    aes(
      x = covariavel,
      y = phi,
      fill = covariavel
    )
  ) +
    geom_boxplot(
      outlier.alpha = 0.3,
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey40"
    ) +
    facet_wrap(
      ~ transicao,
      scales = "free_y"
    ) +
    labs(
      x = NULL,
      y = "Shapley Value (distribution over time)"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 30,
        hjust = 1
      )
    )
}

# Plot individual Shapley values over time using a beeswarm representation.

plot_shapley_beeswarm <- function(shap_df) {
  
  df_plot <- shap_df %>%
    mutate(
      transicao = paste0(
        from_state,
        " -> ",
        to_state
      )
    ) %>%
    aplicar_labels()
  
  ggplot(
    df_plot,
    aes(
      x = covariavel,
      y = phi,
      colour = covariavel
    )
  ) +
    geom_beeswarm(
      cex = 0.3,
      alpha = 0.4,
      size = 0.8
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey40"
    ) +
    facet_wrap(
      ~ transicao,
      scales = "free_y"
    ) +
    labs(
      x = NULL,
      y = "Shapley Value"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 40,
        hjust = 1,
        size = 8
      )
    )
}

# Separate Shapley values according to high and low levels of each covariate.

plot_shapley_box_split <- function(
    shap_df,
    model) {
  
  X_trans <- model$X_trans
  
  players <- get_player_cols(X_trans)
  
  X_long <- as.data.frame(
    X_trans[, players, drop = FALSE]
  ) %>%
    mutate(
      t_idx = seq_len(n())
    ) %>%
    pivot_longer(
      cols = all_of(players),
      names_to = "covariavel",
      values_to = "valor_real"
    )
  
  df_plot <- shap_df %>%
    mutate(
      transicao = paste0(
        from_state,
        " -> ",
        to_state
      )
    ) %>%
    left_join(
      X_long,
      by = c(
        "t_idx",
        "covariavel"
      )
    ) %>%
    group_by(covariavel) %>%
    mutate(
      nivel = ifelse(
        valor_real >
          median(valor_real, na.rm = TRUE),
        "High",
        "Low"
      )
    ) %>%
    ungroup() %>%
    aplicar_labels()
  
  ggplot(
    df_plot,
    aes(
      x = covariavel,
      y = phi,
      fill = nivel
    )
  ) +
    geom_boxplot(
      outlier.alpha = 0.2,
      position = position_dodge(
        width = 0.8
      )
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey40"
    ) +
    facet_wrap(
      ~ transicao,
      scales = "free_y"
    ) +
    labs(
      x = NULL,
      y = "Shapley Value",
      fill = "Covariate level"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.x = element_text(
        angle = 30,
        hjust = 1
      )
    )
}

# Calculate Shapley values for the state-dependent emission equation.

compute_shapley_emission <- function(
    model,
    t_idx = NULL) {
  
  phi_mat <- model$phi_mat
  X_emis <- model$X_emis
  df <- model$df
  
  N <- nrow(phi_mat)
  
  all_cols <- colnames(X_emis)
  players <- get_player_cols(X_emis)
  
  if (is.null(t_idx)) {
    t_idx <- seq_len(nrow(X_emis))
  }
  
  Xsub <- X_emis[
    t_idx,
    ,
    drop = FALSE
  ]
  
  results <- vector(
    "list",
    N * length(players)
  )
  
  idx <- 1
  
  for (i in seq_len(N)) {
    
    for (k in players) {
      
      k_pos <- match(
        k,
        all_cols
      )
      
      coefficient <- phi_mat[
        i,
        k_pos
      ]
      
      covariate_value <- Xsub[
        ,
        k_pos
      ]
      
      results[[idx]] <- data.frame(
        Data = df$Data[t_idx],
        t_idx = t_idx,
        estado = STATE_LABELS[i],
        covariavel = k,
        phi = coefficient *
          covariate_value
      )
      
      idx <- idx + 1
    }
  }
  
  bind_rows(results)
}

# Check the efficiency property for the emission Shapley values.

check_efficiency_emission <- function(
    shap_emis_df,
    model) {
  
  phi_mat <- model$phi_mat
  X_emis <- model$X_emis
  
  players <- get_player_cols(X_emis)
  
  check <- shap_emis_df %>%
    group_by(
      Data,
      t_idx,
      estado
    ) %>%
    summarise(
      soma_phi = sum(phi),
      .groups = "drop"
    ) %>%
    mutate(
      i = match(
        estado,
        STATE_LABELS
      )
    )
  
  errors <- numeric(
    nrow(check)
  )
  
  for (r in seq_len(nrow(check))) {
    
    X_row <- setNames(
      X_emis[
        check$t_idx[r],
      ],
      colnames(X_emis)
    )
    
    mu_full <- sum(
      phi_mat[
        check$i[r],
      ] * X_row
    )
    
    X_base <- X_row
    X_base[players] <- 0
    
    mu_base <- sum(
      phi_mat[
        check$i[r],
      ] * X_base
    )
    
    errors[r] <- abs(
      check$soma_phi[r] -
        (mu_full - mu_base)
    )
  }
  
  cat(
    sprintf(
      "Maximum emission efficiency error: %.2e\n",
      max(errors)
    )
  )
  
  check$erro <- errors
  
  check
}

# Produce descriptive statistics for emission Shapley values.

summary_shapley_emission <- function(
    shap_emis_df) {
  
  shap_emis_df %>%
    group_by(
      estado,
      covariavel
    ) %>%
    summarise(
      mediana = median(phi),
      q25 = quantile(phi, 0.25),
      q75 = quantile(phi, 0.75),
      iqr = q75 - q25,
      media_abs = mean(abs(phi)),
      min = min(phi),
      max = max(phi),
      .groups = "drop"
    )
}

# Plot emission Shapley values using a beeswarm representation.

plot_shapley_beeswarm_emission <- function(
    shap_emis_df) {
  
  df_plot <- aplicar_labels(
    shap_emis_df
  )
  
  ggplot(
    df_plot,
    aes(
      x = covariavel,
      y = phi,
      colour = covariavel
    )
  ) +
    geom_beeswarm(
      cex = 0.3,
      alpha = 0.4,
      size = 0.8
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey40"
    ) +
    facet_wrap(
      ~ estado,
      scales = "free_y"
    ) +
    labs(
      x = NULL,
      y = "Shapley Value"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 40,
        hjust = 1,
        size = 8
      )
    )
}

# Plot the distribution of emission Shapley values.

plot_shapley_boxplot_emission <- function(
    shap_emis_df) {
  
  df_plot <- aplicar_labels(
    shap_emis_df
  )
  
  ggplot(
    df_plot,
    aes(
      x = covariavel,
      y = phi,
      fill = covariavel
    )
  ) +
    geom_boxplot(
      outlier.alpha = 0.3,
      alpha = 0.8
    ) +
    geom_hline(
      yintercept = 0,
      linetype = "dashed",
      colour = "grey40"
    ) +
    facet_wrap(
      ~ estado,
      scales = "free_y"
    ) +
    labs(
      x = NULL,
      y = "Shapley Value"
    ) +
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(
        angle = 30,
        hjust = 1
      )
    )
}

# Calculate transition Shapley values.

shap_df <- compute_shapley_transitions(
  modelo,
  verbose = TRUE
)

# Verify the Shapley efficiency condition.

check_transitions <- check_efficiency(
  shap_df,
  modelo
)

print(
  check_transitions,
  n = 20
)

# Keep only regime-switching transitions.

shap_df_mudancas <- shap_df %>%
  filter(
    paste0(
      from_state,
      " -> ",
      to_state
    ) %in%
      c(
        "Bear -> Bull",
        "Bull -> Bear"
      )
  )

# Generate descriptive statistics for transition Shapley values.

resumo_shapley <- summary_shapley_transitions(
  shap_df_mudancas
)

print(
  resumo_shapley,
  n = Inf
)

# Calculate average Shapley contributions over time.

shap_agg <- aggregate_shapley_transitions(
  shap_df_mudancas,
  fun = mean
)

print(shap_agg)

# Plot transition Shapley values.

print(
  plot_shapley_transitions_bar(
    shap_agg
  )
)

print(
  plot_shapley_boxplot(
    shap_df_mudancas
  )
)

print(
  plot_shapley_beeswarm(
    shap_df_mudancas
  )
)

print(
  plot_shapley_box_split(
    shap_df_mudancas,
    modelo
  )
)

# Calculate Shapley values for the emission equation.

shap_emis_df <- compute_shapley_emission(
  modelo
)

# Verify the emission Shapley efficiency condition.

check_emission <- check_efficiency_emission(
  shap_emis_df,
  modelo
)

print(
  check_emission,
  n = 20
)

# Generate descriptive statistics for emission Shapley values.

resumo_shapley_emissao <- summary_shapley_emission(
  shap_emis_df
)

print(
  resumo_shapley_emissao,
  n = Inf
)

# Plot emission Shapley values.

print(
  plot_shapley_beeswarm_emission(
    shap_emis_df
  )
)

print(
  plot_shapley_boxplot_emission(
    shap_emis_df
  )
)