# 05 HMM

library(tidyverse)
library(numDeriv)
library(Matrix)
library(ggcorrplot)
library(scales)

set.seed(42)

# Global configuration
# Thresholds used to define the rule-based Bear/Bull states.

# Global configuration
# Thresholds used to define the rule-based Bear/Bull states.

thresh_retorno <- -0.01
thresh_vol     <-  0.04

# Make sure the date variable has the correct format.

df_retornos <- df_retornos %>%
  mutate(Data = as.Date(Data))

# Calculate monthly volatility from the daily MSCI returns.

df_volatilidade <- df_retornos %>%
  mutate(ano_mes = format(Data, "%Y-%m")) %>%
  group_by(ano_mes) %>%
  summarise(
    Volatilidade_Mensal = sd(Retorno_t, na.rm = TRUE),
    .groups = "drop"
  )

# Create the Bear/Bull classification using monthly returns
# and monthly volatility.

df_estados <- df_retornos %>%
  mutate(
    Data = as.Date(Data),
    ano_mes = format(Data, "%Y-%m")
  ) %>%
  inner_join(
    df_volatilidade,
    by = "ano_mes"
  ) %>%
  mutate(
    estado_raw = case_when(
      Retorno_t < thresh_retorno &
        Volatilidade_Mensal > thresh_vol ~ "Bear",
      TRUE ~ "Bull"
    ),
    estado = factor(
      estado_raw,
      levels = c("Bull", "Bear")
    )
  ) %>%
  arrange(Data) %>%
  select(-ano_mes) %>%
  filter(
    Data >= as.Date("1997-01-01"),
    Data <= as.Date("2024-12-01")
  )

# Merge the MSCI returns with the macroeconomic, financial,
# institutional, environmental and inequality variables.

df_covariaveis <- df_estados %>%
  mutate(
    ano_mes = format(Data, "%Y-%m"),
    Ano = as.numeric(format(Data, "%Y"))
  ) %>%
  
  left_join(
    df_curva %>%
      mutate(ano_mes = format(date, "%Y-%m")) %>%
      select(ano_mes, spread, juros_3m),
    by = "ano_mes"
  ) %>%
  
  left_join(
    df_ze_mensal %>%
      mutate(ano_mes = format(Data, "%Y-%m")) %>%
      select(ano_mes, RNB_ZE),
    by = "ano_mes"
  ) %>%
  
  left_join(
    dados_mensais %>%
      mutate(ano_mes = format(Data, "%Y-%m")) %>%
      select(ano_mes, Taxa_Inflacao),
    by = "ano_mes"
  ) %>%
  
  left_join(
    df_economia_mensal %>%
      mutate(ano_mes = format(Data, "%Y-%m")) %>%
      select(ano_mes, Taxa_Desemprego, CCI),
    by = "ano_mes"
  ) %>%
  
  left_join(
    crime_mensal %>%
      mutate(ano_mes = format(Data, "%Y-%m")) %>%
      select(ano_mes, crime_interp),
    by = "ano_mes"
  ) %>%
  
  left_join(
    media_pib_corr %>%
      rename(corrupcao = media_pib),
    by = c("Ano" = "Year")
  ) %>%
  
  left_join(
    emissions_mensal %>%
      mutate(ano_mes = format(Data, "%Y-%m")) %>%
      select(ano_mes, emissions_interp),
    by = "ano_mes"
  ) %>%
  
  left_join(
    gini_mensal_constante %>%
      mutate(ano_mes = format(Data, "%Y-%m")) %>%
      select(ano_mes, gini_constante),
    by = "ano_mes"
  ) %>%
  
  select(
    Data,
    estado,
    Retorno_t,
    spread,
    juros_3m,
    RNB_ZE,
    Taxa_Inflacao,
    Taxa_Desemprego,
    CCI,
    crime_interp,
    corrupcao,
    emissions_interp,
    gini_constante
  ) %>%
  
  arrange(Data)


# Model settings
# Two latent states are estimated: Bear and Bull.

N_STATES     <- 2
STATE_LABELS <- c("Bear", "Bull")
MAX_ITER     <- 200
TOL          <- 1e-6
NR_ITER      <- 20
NR_TOL       <- 1e-8


# Auxiliary functions for the non-homogeneous Hidden Markov Model.

prep_covariaveis <- function(df) {
  
  df <- df %>%
    arrange(Data) %>%
    mutate(
      t_raw = row_number(),
      t = (t_raw - 1) / (n() - 1)
    )
  
  covs <- c(
    "spread",
    "juros_3m",
    "RNB_ZE",
    "Taxa_Inflacao",
    "Taxa_Desemprego",
    "CCI",
    "crime_interp",
    "corrupcao",
    "emissions_interp",
    "gini_constante"
  )
  
  df %>%
    mutate(
      across(
        all_of(covs),
        ~ as.numeric(scale(.)),
        .names = "{.col}_z"
      )
    )
}


# Build the design matrix for the state transition probabilities.

make_X_trans <- function(df, covs_trans) {
  
  base <- cbind(
    intercept = 1,
    t = df$t
  )
  
  map_cols <- c(
    spread     = "spread_z",
    juros_3m   = "juros_3m_z",
    RNB_ZE     = "RNB_ZE_z",
    inflacao   = "Taxa_Inflacao_z",
    desemprego = "Taxa_Desemprego_z",
    CCI        = "CCI_z",
    crime      = "crime_interp_z",
    corrupcao  = "corrupcao_z",
    emissions  = "emissions_interp_z",
    gini       = "gini_constante_z"
  )
  
  if (length(covs_trans) > 0) {
    
    extra <- df[, map_cols[covs_trans], drop = FALSE]
    
    colnames(extra) <- covs_trans
    
    base <- cbind(
      base,
      as.matrix(extra)
    )
  }
  
  base
}


# Build the design matrix for the state-dependent return equation.

make_X_emis <- function(df, covs_emis) {
  
  base <- cbind(
    intercept = 1,
    t = df$t
  )
  
  map_cols <- c(
    spread     = "spread_z",
    juros_3m   = "juros_3m_z",
    RNB_ZE     = "RNB_ZE_z",
    inflacao   = "Taxa_Inflacao_z",
    desemprego = "Taxa_Desemprego_z",
    CCI        = "CCI_z",
    crime      = "crime_interp_z",
    corrupcao  = "corrupcao_z",
    emissions  = "emissions_interp_z",
    gini       = "gini_constante_z"
  )
  
  if (length(covs_emis) > 0) {
    
    extra <- df[, map_cols[covs_emis], drop = FALSE]
    
    colnames(extra) <- covs_emis
    
    base <- cbind(
      base,
      as.matrix(extra)
    )
  }
  
  base
}


# Convert the transition equations into probabilities.

softmax <- function(x) {
  x <- x - max(x)
  exp(x) / sum(exp(x))
}


# Calculate time-varying transition probabilities between the two states.

compute_A_t <- function(beta_mat, X_trans) {
  
  T <- nrow(X_trans)
  N <- dim(beta_mat)[1]
  
  A <- array(
    0,
    dim = c(T, N, N)
  )
  
  for (i in seq_len(N)) {
    
    eta <- X_trans %*% t(beta_mat[i, , ])
    
    for (tt in seq_len(T)) {
      A[tt, i, ] <- softmax(eta[tt, ])
    }
  }
  
  A
}


# Calculate the probability of each observed return under each latent state.

compute_emission_prob <- function(
    phi_mat,
    sigma_vec,
    X_emis,
    obs
) {
  
  T <- length(obs)
  N <- nrow(phi_mat)
  
  B <- matrix(
    0,
    nrow = T,
    ncol = N
  )
  
  for (i in seq_len(N)) {
    
    B[, i] <- dnorm(
      obs,
      mean = X_emis %*% phi_mat[i, ],
      sd = sigma_vec[i]
    )
  }
  
  B
}


# Forward algorithm used to calculate the likelihood.

forward_pass <- function(pi_vec, A_t, B_mat) {
  
  T <- nrow(B_mat)
  N <- ncol(B_mat)
  
  alpha <- matrix(
    0,
    T,
    N
  )
  
  scale_c <- numeric(T)
  
  alpha[1, ] <- pi_vec * B_mat[1, ]
  
  scale_c[1] <- max(
    sum(alpha[1, ]),
    1e-300
  )
  
  alpha[1, ] <- alpha[1, ] / scale_c[1]
  
  for (tt in 2:T) {
    
    for (j in seq_len(N)) {
      
      alpha[tt, j] <-
        sum(
          alpha[tt - 1, ] *
            A_t[tt - 1, , j]
        ) *
        B_mat[tt, j]
    }
    
    scale_c[tt] <- max(
      sum(alpha[tt, ]),
      1e-300
    )
    
    alpha[tt, ] <- alpha[tt, ] / scale_c[tt]
  }
  
  list(
    alpha = alpha,
    scale_c = scale_c,
    log_lik = sum(log(scale_c))
  )
}


# Backward algorithm used together with the forward probabilities.

backward_pass <- function(A_t, B_mat, scale_c) {
  
  T <- nrow(B_mat)
  N <- ncol(B_mat)
  
  beta <- matrix(
    0,
    T,
    N
  )
  
  beta[T, ] <- 1
  
  if (T > 1) {
    
    for (tt in (T - 1):1) {
      
      for (i in seq_len(N)) {
        
        beta[tt, i] <-
          sum(
            A_t[tt, i, ] *
              B_mat[tt + 1, ] *
              beta[tt + 1, ]
          )
      }
      
      beta[tt, ] <-
        beta[tt, ] /
        scale_c[tt + 1]
    }
  }
  
  beta
}


# Estimate the smoothed probability of being in each state
# and the probability of transitioning between states.

e_step <- function(alpha, beta, A_t, B_mat) {
  
  T <- nrow(alpha)
  N <- ncol(alpha)
  
  gamma <- alpha * beta
  
  rs <- rowSums(gamma)
  rs[rs == 0] <- 1e-300
  
  gamma <- gamma / rs
  
  xi <- array(
    0,
    dim = c(T - 1, N, N)
  )
  
  if (T > 1) {
    
    for (tt in seq_len(T - 1)) {
      
      for (i in seq_len(N)) {
        
        for (j in seq_len(N)) {
          
          xi[tt, i, j] <-
            alpha[tt, i] *
            A_t[tt, i, j] *
            B_mat[tt + 1, j] *
            beta[tt + 1, j]
        }
      }
      
      s <- sum(xi[tt, , ])
      
      if (s > 0) {
        xi[tt, , ] <- xi[tt, , ] / s
      }
    }
  }
  
  list(
    gamma = gamma,
    xi = xi
  )
}


# Newton-Raphson update for the transition parameters.

update_beta_NR <- function(
    beta_mat,
    X_trans,
    xi,
    i
) {
  
  T <- dim(xi)[1]
  N <- dim(beta_mat)[2]
  K <- ncol(X_trans)
  
  X <- X_trans[
    seq_len(T),
    ,
    drop = FALSE
  ]
  
  for (j in seq_len(N - 1)) {
    
    beta_cur <- beta_mat[i, j, ]
    
    for (nr in seq_len(NR_ITER)) {
      
      eta_mat <- matrix(
        0,
        nrow = T,
        ncol = N
      )
      
      for (k in seq_len(N - 1)) {
        
        eta_mat[, k] <-
          X %*%
          beta_mat[i, k, ]
      }
      
      p_mat <- t(
        apply(
          eta_mat,
          1,
          softmax
        )
      )
      
      w_j <- xi[, i, j]
      
      w_total <-
        rowSums(
          xi[, i, , drop = FALSE]
        )
      
      r <-
        w_j -
        w_total * p_mat[, j]
      
      g <-
        as.vector(
          t(X) %*% r
        )
      
      v <-
        w_total *
        p_mat[, j] *
        (1 - p_mat[, j])
      
      H <-
        -t(X) %*%
        (X * v)
      
      H_reg <-
        H -
        diag(1e-4, K)
      
      delta <- tryCatch(
        solve(
          -H_reg,
          g
        ),
        error = function(e) {
          rep(
            NA_real_,
            K
          )
        }
      )
      
      if (
        anyNA(delta) ||
        any(!is.finite(delta))
      ) {
        break
      }
      
      obj <- function(b) {
        
        eta_try <- eta_mat
        
        eta_try[, j] <-
          X %*% b
        
        p_try <- t(
          apply(
            eta_try,
            1,
            softmax
          )
        )
        
        sum(
          xi[, i, ] *
            log(
              pmax(
                p_try,
                1e-15
              )
            )
        )
      }
      
      step <- 1
      
      obj_cur <-
        obj(beta_cur)
      
      b_cand <- beta_cur
      
      for (ls in seq_len(15)) {
        
        b_try <-
          pmax(
            pmin(
              beta_cur + step * delta,
              8
            ),
            -8
          )
        
        if (
          obj(b_try) >=
          obj_cur - 1e-10
        ) {
          
          b_cand <- b_try
          break
        }
        
        step <- step * 0.5
      }
      
      if (
        max(
          abs(
            b_cand -
            beta_cur
          )
        ) < NR_TOL
      ) {
        
        beta_cur <- b_cand
        break
      }
      
      beta_cur <- b_cand
      
      beta_mat[i, j, ] <-
        beta_cur
    }
    
    beta_mat[i, j, ] <-
      beta_cur
  }
  
  beta_mat
}


# Update the state-specific return regressions and volatility parameters.

update_emission_NR <- function(
    phi_mat,
    sigma_vec,
    X_emis,
    obs,
    gamma
) {
  
  for (i in seq_len(nrow(phi_mat))) {
    
    w <- gamma[, i]
    
    W <- diag(w)
    
    XtW <-
      t(X_emis) %*% W
    
    XtWX <-
      XtW %*%
      X_emis +
      diag(
        1e-6,
        ncol(X_emis)
      )
    
    phi_mat[i, ] <- tryCatch(
      solve(
        XtWX,
        XtW %*% obs
      ),
      error = function(e) {
        phi_mat[i, ]
      }
    )
    
    resid <-
      obs -
      X_emis %*%
      phi_mat[i, ]
    
    sigma_vec[i] <-
      max(
        sqrt(
          sum(
            w *
              resid^2
          ) /
            max(
              sum(w),
              1e-10
            )
        ),
        1e-4
      )
  }
  
  list(
    phi_mat = phi_mat,
    sigma_vec = sigma_vec
  )
}


update_pi <- function(gamma) {
  
  pv <- gamma[1, ]
  
  pv / sum(pv)
}


# Estimate the non-homogeneous Hidden Markov Model using Baum-Welch.

baum_welch_NH <- function(
    df,
    verbose = FALSE,
    beta_init = NULL,
    pi_init = NULL,
    covs_trans,
    covs_emis
) {
  
  df <- prep_covariaveis(df)
  
  obs <- df$Retorno_t
  
  T <- nrow(df)
  N <- N_STATES
  
  X_trans <-
    make_X_trans(
      df,
      covs_trans
    )
  
  X_emis <-
    make_X_emis(
      df,
      covs_emis
    )
  
  K_trans <- ncol(X_trans)
  K_emis  <- ncol(X_emis)
  
  pi_vec <-
    if (is.null(pi_init)) {
      rep(
        1 / N,
        N
      )
    } else {
      pi_init /
        sum(pi_init)
    }
  
  if (is.null(beta_init)) {
    
    beta_mat <-
      array(
        0,
        dim = c(
          N,
          N,
          K_trans
        )
      )
    
    beta_mat[1, 1, 1] <- 0.5
    beta_mat[2, 1, 1] <- -0.5
    
  } else {
    
    beta_mat <- beta_init
    
    beta_mat[, N, ] <- 0
  }
  
  phi_mat <-
    matrix(
      0,
      nrow = N,
      ncol = K_emis
    )
  
  phi_mat[1, 1] <- -0.01
  phi_mat[2, 1] <- 0.01
  
  sigma_vec <- c(
    0.04,
    0.02
  )
  
  log_lik_prev <- -Inf
  
  log_lik_hist <-
    numeric(MAX_ITER)
  
  log_lik <- NA_real_
  
  for (iter in seq_len(MAX_ITER)) {
    
    A_t <-
      compute_A_t(
        beta_mat,
        X_trans
      )
    
    B_mat <-
      compute_emission_prob(
        phi_mat,
        sigma_vec,
        X_emis,
        obs
      )
    
    B_mat[
      B_mat < 1e-300
    ] <- 1e-300
    
    fwd <-
      forward_pass(
        pi_vec,
        A_t,
        B_mat
      )
    
    log_lik <- fwd$log_lik
    
    log_lik_hist[iter] <-
      log_lik
    
    if (
      abs(
        log_lik -
        log_lik_prev
      ) < TOL &&
      iter > 1
    ) {
      break
    }
    
    log_lik_prev <- log_lik
    
    bwd <-
      backward_pass(
        A_t,
        B_mat,
        fwd$scale_c
      )
    
    E <-
      e_step(
        fwd$alpha,
        bwd,
        A_t,
        B_mat
      )
    
    gamma <- E$gamma
    xi    <- E$xi
    
    pi_vec <-
      update_pi(gamma)
    
    for (i in seq_len(N)) {
      
      beta_mat <-
        update_beta_NR(
          beta_mat,
          X_trans,
          xi,
          i
        )
    }
    
    emis_upd <-
      update_emission_NR(
        phi_mat,
        sigma_vec,
        X_emis,
        obs,
        gamma
      )
    
    phi_mat <-
      emis_upd$phi_mat
    
    sigma_vec <-
      emis_upd$sigma_vec
  }
  
  list(
    pi = pi_vec,
    beta_mat = beta_mat,
    phi_mat = phi_mat,
    sigma_vec = sigma_vec,
    log_lik = log_lik,
    log_lik_hist =
      log_lik_hist[
        seq_len(iter)
      ],
    gamma = gamma,
    df = df,
    X_trans = X_trans,
    X_emis = X_emis,
    covs_trans = covs_trans,
    covs_emis = covs_emis
  )
}


# Run the model several times with different random starting values.
# The specification with the highest log-likelihood is retained.

baum_welch_multi <- function(
    df,
    n_runs = 10,
    covs_trans,
    covs_emis
) {
  
  best_ll <- -Inf
  best_model <- NULL
  
  df_tmp <-
    prep_covariaveis(df)
  
  K_trans <-
    ncol(
      make_X_trans(
        df_tmp,
        covs_trans
      )
    )
  
  N <- N_STATES
  
  for (run in seq_len(n_runs)) {
    
    cat(
      sprintf(
        "Run %d/%d ...",
        run,
        n_runs
      )
    )
    
    set.seed(
      run * 7
    )
    
    beta_init <-
      array(
        0,
        dim = c(
          N,
          N,
          K_trans
        )
      )
    
    for (i in seq_len(N)) {
      
      for (j in seq_len(N - 1)) {
        
        beta_init[i, j, ] <-
          rnorm(
            K_trans,
            0,
            0.5
          )
      }
    }
    
    pi_init <-
      runif(N)
    
    pi_init <-
      pi_init /
      sum(pi_init)
    
    model <- tryCatch(
      
      baum_welch_NH(
        df,
        beta_init = beta_init,
        pi_init = pi_init,
        covs_trans = covs_trans,
        covs_emis = covs_emis
      ),
      
      error = function(e) {
        cat(
          " ERRO:",
          conditionMessage(e),
          "\n"
        )
        NULL
      }
    )
    
    if (
      !is.null(model) &&
      is.finite(model$log_lik)
    ) {
      
      cat(
        sprintf(
          " log-lik = %.4f\n",
          model$log_lik
        )
      )
      
      if (
        model$log_lik >
        best_ll
      ) {
        
        best_ll <-
          model$log_lik
        
        best_model <-
          model
      }
      
    } else {
      
      cat(
        " falhou\n"
      )
    }
  }
  
  cat(
    sprintf(
      "Melhor log-lik: %.4f\n",
      best_ll
    )
  )
  
  if (
    is.null(best_model)
  ) {
    stop(
      "Todas as runs falharam."
    )
  }
  
  best_model
}


# Decode the most likely sequence of latent states using Viterbi.

viterbi_NH <- function(model) {
  
  df <- model$df
  
  obs <- df$Retorno_t
  
  T <- nrow(df)
  N <- N_STATES
  
  A_t <-
    compute_A_t(
      model$beta_mat,
      model$X_trans
    )
  
  B_mat <-
    compute_emission_prob(
      model$phi_mat,
      model$sigma_vec,
      model$X_emis,
      obs
    )
  
  B_mat[
    B_mat < 1e-300
  ] <- 1e-300
  
  log_A <-
    log(
      A_t + 1e-300
    )
  
  log_B <-
    log(B_mat)
  
  log_pi <-
    log(
      model$pi + 1e-300
    )
  
  delta <-
    matrix(
      -Inf,
      T,
      N
    )
  
  psi <-
    matrix(
      0L,
      T,
      N
    )
  
  delta[1, ] <-
    log_pi +
    log_B[1, ]
  
  for (tt in 2:T) {
    
    for (j in seq_len(N)) {
      
      scores <-
        delta[tt - 1, ] +
        log_A[tt - 1, , j]
      
      psi[tt, j] <-
        which.max(scores)
      
      delta[tt, j] <-
        max(scores) +
        log_B[tt, j]
    }
  }
  
  states <-
    integer(T)
  
  states[T] <-
    which.max(
      delta[T, ]
    )
  
  if (T > 1) {
    
    for (tt in (T - 1):1) {
      
      states[tt] <-
        psi[
          tt + 1,
          states[tt + 1]
        ]
    }
  }
  
  states
}


# Calculate AIC and BIC to compare model fit while penalising complexity.

calc_aic_bic <- function(model) {
  
  T <- nrow(model$df)
  N <- N_STATES
  
  K_trans <-
    ncol(model$X_trans)
  
  K_emis <-
    ncol(model$X_emis)
  
  k <-
    N * (N - 1) * K_trans +
    N * K_emis +
    N +
    (N - 1)
  
  list(
    log_lik = model$log_lik,
    k = k,
    T = T,
    AIC =
      -2 * model$log_lik +
      2 * k,
    BIC =
      -2 * model$log_lik +
      k * log(T)
  )
}


# Prepare model results for plotting and comparison.

make_df_plot <- function(
    model,
    states,
    label
) {
  
  model$df %>%
    mutate(
      Estado =
        factor(
          states,
          levels = 1:2,
          labels = STATE_LABELS
        ),
      Prob_Bear =
        model$gamma[, 1],
      Modelo = label
    )
}


# Correlation matrix between MSCI returns and explanatory variables.
# This provides a descriptive check before estimating the HMM.

covs_raw <- c(
  "Retorno_t",
  "spread",
  "juros_3m",
  "RNB_ZE",
  "Taxa_Inflacao",
  "Taxa_Desemprego",
  "CCI",
  "crime_interp",
  "corrupcao",
  "emissions_interp",
  "gini_constante"
)

label_map <- c(
  Retorno_t        = "MSCI (Retorno)",
  spread           = "Spread",
  juros_3m         = "Juros 3M",
  RNB_ZE           = "RNB Zona Euro",
  Taxa_Inflacao    = "Inflação",
  Taxa_Desemprego  = "Desemprego",
  CCI              = "ICC",
  crime_interp     = "Criminalidade",
  corrupcao        = "Corrupção",
  emissions_interp = "Emissões",
  gini_constante   = "Gini"
)

cor_mat <-
  cor(
    df_covariaveis[, covs_raw],
    use = "complete.obs"
  )

rownames(cor_mat) <-
  colnames(cor_mat) <-
  unname(
    label_map[covs_raw]
  )

ggcorrplot(
  cor_mat,
  method = "square",
  type = "lower",
  lab = TRUE,
  lab_size = 3,
  colors = c(
    "#1a9850",
    "white",
    "#d73027"
  ),
  title = "",
  ggtheme =
    theme_minimal(
      base_size = 11
    )
)


# Estimate the four competing model specifications.

COVS_ALL <- c(
  "spread",
  "juros_3m",
  "RNB_ZE",
  "inflacao",
  "desemprego",
  "CCI",
  "crime",
  "corrupcao",
  "emissions",
  "gini"
)

covs_modelo1 <- COVS_ALL

covs_modelo2 <- c(
  "emissions",
  "juros_3m",
  "corrupcao"
)

covs_modelo3 <- c(
  "emissions",
  "juros_3m",
  "corrupcao",
  "crime",
  "gini"
)

covs_modelo4 <- c(
  "emissions",
  "juros_3m",
  "corrupcao",
  "spread",
  "CCI"
)

n_runs_robusto <- 10


# Model 1: all explanatory variables.

cat(
  "Modelo 1: Todas as variáveis\n"
)

modelo1_todas <-
  baum_welch_multi(
    df_covariaveis,
    n_runs = n_runs_robusto,
    covs_trans = covs_modelo1,
    covs_emis = covs_modelo1
  )


# Model 2: reduced baseline specification.

cat(
  "\nModelo 2: Emissões, Juros 3m, Corrupção\n"
)

modelo2_base <-
  baum_welch_multi(
    df_covariaveis,
    n_runs = n_runs_robusto,
    covs_trans = covs_modelo2,
    covs_emis = covs_modelo2
  )


# Model 3: extended specification with crime and inequality.

cat(
  "\nModelo 3: Emissões, Juros 3m, Corrupção, Criminalidade, Gini\n"
)

modelo3_alargado <-
  baum_welch_multi(
    df_covariaveis,
    n_runs = n_runs_robusto,
    covs_trans = covs_modelo3,
    covs_emis = covs_modelo3
  )


# Model 4: final specification including the yield spread and consumer confidence.

cat(
  "\nModelo 4: Emissões, Juros 3m, Corrupção, Spread, ICC\n"
)

modelo4_final <-
  baum_welch_multi(
    df_covariaveis,
    n_runs = n_runs_robusto,
    covs_trans = covs_modelo4,
    covs_emis = covs_modelo4
  )


# Compare the four specifications using AIC and BIC.
# Lower values indicate a better balance between fit and complexity.

aicbic_1 <- calc_aic_bic(modelo1_todas)
aicbic_2 <- calc_aic_bic(modelo2_base)
aicbic_3 <- calc_aic_bic(modelo3_alargado)
aicbic_4 <- calc_aic_bic(modelo4_final)

LABEL_M1 <-
  "Todas as variáveis"

LABEL_M2 <-
  "Emissões, Juros 3m, Corrupção"

LABEL_M3 <-
  "Emissões, Juros 3m, Corrupção, Criminalidade, Gini"

LABEL_M4 <-
  "Emissões, Juros 3m, Corrupção, Spread, ICC"


tabela_comparacao <-
  tibble(
    modelo = c(
      LABEL_M1,
      LABEL_M2,
      LABEL_M3,
      LABEL_M4
    ),
    log_lik = c(
      aicbic_1$log_lik,
      aicbic_2$log_lik,
      aicbic_3$log_lik,
      aicbic_4$log_lik
    ),
    k = c(
      aicbic_1$k,
      aicbic_2$k,
      aicbic_3$k,
      aicbic_4$k
    ),
    AIC = c(
      aicbic_1$AIC,
      aicbic_2$AIC,
      aicbic_3$AIC,
      aicbic_4$AIC
    ),
    BIC = c(
      aicbic_1$BIC,
      aicbic_2$BIC,
      aicbic_3$BIC,
      aicbic_4$BIC
    )
  ) %>%
  mutate(
    log_lik = round(
      log_lik,
      10
    ),
    AIC = round(
      AIC,
      6
    ),
    BIC = round(
      BIC,
      6
    )
  )

options(
  pillar.sigfig = 10
)

cat(
  "\nModel comparison\n"
)

print(
  tabela_comparacao
)

cat(
  "\nBest by AIC:",
  tabela_comparacao$modelo[
    which.min(
      tabela_comparacao$AIC
    )
  ],
  "\n"
)

cat(
  "Best by BIC:",
  tabela_comparacao$modelo[
    which.min(
      tabela_comparacao$BIC
    )
  ],
  "\n"
)


# Rule-based states used as a benchmark for the HMM classifications.

thresh_retorno_regra <- -0.01
thresh_vol_regra <- 0.04

df_regra <-
  df_estados %>%
  mutate(
    ano_mes =
      format(
        Data,
        "%Y-%m"
      )
  ) %>%
  select(
    Data,
    ano_mes,
    Retorno_t
  ) %>%
  left_join(
    df_volatilidade,
    by = "ano_mes"
  ) %>%
  mutate(
    Estado = case_when(
      Retorno_t < thresh_retorno_regra &
        Volatilidade_Mensal > thresh_vol_regra ~
        "Bear",
      TRUE ~
        "Bull"
    ),
    Estado =
      factor(
        Estado,
        levels = c(
          "Bear",
          "Bull"
        )
      ),
    Periodo =
      "Regra (sem modelo)"
  ) %>%
  select(
    -ano_mes
  )

LABEL_REGRA <- "Regra"

df_regra_plot <-
  df_regra %>%
  mutate(
    Modelo = LABEL_REGRA
  ) %>%
  select(
    Data,
    Retorno_t,
    Estado,
    Modelo
  )


# Comparative panel showing the classifications from Models 1–4
# alongside the rule-based benchmark.

df_todos <-
  bind_rows(
    
    make_df_plot(
      modelo1_todas,
      viterbi_NH(modelo1_todas),
      LABEL_M1
    ),
    
    make_df_plot(
      modelo2_base,
      viterbi_NH(modelo2_base),
      LABEL_M2
    ),
    
    make_df_plot(
      modelo3_alargado,
      viterbi_NH(modelo3_alargado),
      LABEL_M3
    ),
    
    make_df_plot(
      modelo4_final,
      viterbi_NH(modelo4_final),
      LABEL_M4
    ),
    
    df_regra_plot %>%
      mutate(
        Prob_Bear = NA_real_
      )
  ) %>%
  mutate(
    Modelo =
      factor(
        Modelo,
        levels = c(
          LABEL_M1,
          LABEL_M2,
          LABEL_M3,
          LABEL_M4,
          LABEL_REGRA
        )
      )
  )


# Identify continuous Bear periods for the shaded areas.

df_bear_todos <-
  df_todos %>%
  filter(
    Estado == "Bear"
  ) %>%
  arrange(
    Modelo,
    Data
  ) %>%
  group_by(
    Modelo
  ) %>%
  mutate(
    grupo =
      cumsum(
        c(
          1,
          diff(
            as.numeric(Data)
          ) > 40
        )
      )
  ) %>%
  group_by(
    Modelo,
    grupo
  ) %>%
  summarise(
    inicio = min(Data),
    fim = max(Data),
    .groups = "drop"
  )


# Comparative plot of the five classifications.

ggplot(
  df_todos,
  aes(x = Data)
) +
  
  geom_rect(
    data = df_bear_todos,
    aes(
      xmin = inicio - 15,
      xmax = fim + 15,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d73027",
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    aes(y = Retorno_t),
    linewidth = 0.35,
    colour = "grey55"
  ) +
  
  geom_point(
    aes(
      y = Retorno_t,
      colour = Estado
    ),
    size = 0.9
  ) +
  
  scale_colour_manual(
    values = c(
      "Bear" = "#d73027",
      "Bull" = "#1a9850"
    ),
    name = NULL
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 3)
  ) +
  
  labs(
    x = NULL,
    y = "Retorno log"
  ) +
  
  facet_wrap(
    ~ Modelo,
    ncol = 1
  ) +
  
  theme_minimal(
    base_size = 11
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      ),
    strip.text =
      element_text(
        face = "bold"
      )
  )


# Plot Model 1 separately to examine its classification.

df_m1 <-
  df_todos %>%
  filter(
    Modelo == LABEL_M1
  )

df_bear_m1 <-
  df_bear_todos %>%
  filter(
    Modelo == LABEL_M1
  )

ggplot(
  df_m1,
  aes(x = Data)
) +
  
  geom_rect(
    data = df_bear_m1,
    aes(
      xmin = inicio - 15,
      xmax = fim + 15,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d73027",
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    aes(y = Retorno_t),
    linewidth = 0.35,
    colour = "grey55"
  ) +
  
  geom_point(
    aes(
      y = Retorno_t,
      colour = Estado
    ),
    size = 1.3
  ) +
  
  scale_colour_manual(
    values = c(
      "Bear" = "#d73027",
      "Bull" = "#1a9850"
    ),
    name = NULL
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 3)
  ) +
  
  labs(
    x = NULL,
    y = "Retorno log"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      ),
    plot.title =
      element_text(
        face = "bold",
        size = 14
      )
  )


# Plot Model 2 separately to examine its classification.

df_m2 <-
  df_todos %>%
  filter(
    Modelo == LABEL_M2
  )

df_bear_m2 <-
  df_bear_todos %>%
  filter(
    Modelo == LABEL_M2
  )

ggplot(
  df_m2,
  aes(x = Data)
) +
  
  geom_rect(
    data = df_bear_m2,
    aes(
      xmin = inicio - 15,
      xmax = fim + 15,
      ymin = -Inf,
      ymax = Inf
    ),
    fill = "#d73027",
    alpha = 0.18,
    inherit.aes = FALSE
  ) +
  
  geom_line(
    aes(y = Retorno_t),
    linewidth = 0.35,
    colour = "grey55"
  ) +
  
  geom_point(
    aes(
      y = Retorno_t,
      colour = Estado
    ),
    size = 1.3
  ) +
  
  scale_colour_manual(
    values = c(
      "Bear" = "#d73027",
      "Bull" = "#1a9850"
    ),
    name = NULL
  ) +
  
  scale_x_date(
    date_breaks = "2 years",
    date_labels = "%Y"
  ) +
  
  scale_y_continuous(
    breaks = pretty_breaks(n = 3)
  ) +
  
  labs(
    x = NULL,
    y = "Retorno log"
  ) +
  
  theme_minimal(
    base_size = 13
  ) +
  
  theme(
    panel.grid.minor = element_blank(),
    legend.position = "top",
    axis.text.x =
      element_text(
        angle = 45,
        hjust = 1
      ),
    plot.title =
      element_text(
        face = "bold",
        size = 14
      )
  )