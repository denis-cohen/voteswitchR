functions {
  /* compute correlated group-level effects
  * Args: 
    *   z: matrix of unscaled group-level effects
    *   SD: vector of standard deviation parameters
    *   L: cholesky factor correlation matrix
  * Returns: 
    *   matrix of scaled group-level effects
  */ 
  matrix scale_r_cor(matrix z, vector SD, matrix L) {
    // r is stored in another dimension order than z
    return transpose(diag_pre_multiply(SD, L) * z);
  }
}

data {
  // integers
  int<lower=2> ncat;   // number of categories
  int<lower=1> K;      // number of predictors at higher levels
  int<lower=1> J;      // number of units
  int<lower=1> D;      // number of varying coefficients at lowest level
  
  // multiplier for (weighted) outcomes at lower upper level
  matrix[J, ncat] Y; // aggregate number of (weighted) outcomes

  // predictors
  matrix[J,K] X; // upper level predictors (matrix at lowest upper level)
  
  // optional REs
  int<lower=1> num_parties;
  int<lower=1> party_id[J];
}

transformed data {
  int<lower=1> ncatm1 = ncat - 1;
  int<lower=2> Q = D * ncatm1;
  vector[ncat] ones_cats = rep_vector(1.0, ncat);
  vector[D] ones_D = rep_vector(1.0, D);
  vector[K-1] mean_X;
  vector[K] mean_X_full;
  matrix[K,J] X_transpose = X';
  matrix[J,D] X_varying = X[, 1:D];
  matrix[ncat, J] Y_transpose = Y';
  int zeroes[ncat, J];
  for (c in 1:ncat) {
    for (l in 1:J) {
      if (Y_transpose[c, l] == -1.0) {
        zeroes[c, l] = 1;
        Y_transpose[c, l] = 0.0;
      } else {
        zeroes[c, l] = 0;
      }
    }
  }
}

parameters {
  row_vector[K] beta[ncatm1];
  vector<lower=0>[Q] sigma;  // group-level standard deviations
  matrix[Q,J] nu_raw;        // standardized group-level effects
  cholesky_factor_corr[Q] L; // cholesky factor of correlation matrix
  vector<lower=0>[ncatm1] sigma_parties;
  matrix[ncatm1,num_parties] nu_raw_parties;
  cholesky_factor_corr[ncatm1] L_parties;
}

transformed parameters {
  matrix[J,Q] nu;
  matrix[num_parties,ncatm1] nu_parties;
  
  nu = scale_r_cor(nu_raw, sigma, L);
  nu_parties = scale_r_cor(nu_raw_parties, sigma_parties, L_parties);
}

model {
  int start = 1;
  int end = D;
  matrix[ncat, J] log_prob;

  // linear predictions, incl. random intercepts
  log_prob[ncat, ] = rep_row_vector(0.0, J);
  for (c in 1:ncatm1) {
    log_prob[c, ] =  beta[c] * X_transpose + 
      ((X_varying .* nu[, start:end]) * ones_D)';
      start = start + D;
      end = end + D;
      
      for (j in 1:J) {
        log_prob[c,j] = log_prob[c,j] + nu_parties[party_id[j],c];
      }
      
  }

  // transform to log probabilities
  for (l in 1:J) {
    for (c in 1:ncat) {
      if (zeroes[c, l] == 1) {
        log_prob[c, l] = -1000.0; // de facto minus_infinity()
      }
    }
    log_prob[, l] = log_softmax(log_prob[, l]);
  }
  
  // priors 
  for (c in 1:ncatm1) {
    // fixed effects
    target += normal_lpdf(beta[c] | 0, 2.5);
  }
  // random effects
  target += std_normal_lpdf(to_vector(nu_raw));
  target += student_t_lpdf(sigma | 3, 0, 2.5)
    - 4 * student_t_lccdf(0 | 3, 0, 2.5);
  target += lkj_corr_cholesky_lpdf(L | 2);
  
  target += std_normal_lpdf(to_vector(nu_raw_parties));
  target += student_t_lpdf(sigma_parties | 3, 0, 2.5)
    - 4 * student_t_lccdf(0 | 3, 0, 2.5);
  target += lkj_corr_cholesky_lpdf(L_parties | 2);

  // likelihood
  target += sum(log_prob .* Y_transpose);
}

generated quantities {
  matrix[Q,Q] Sigma = quad_form_diag(tcrossprod(L), sigma);
  matrix[ncatm1,ncatm1] Sigma_parties = quad_form_diag(tcrossprod(L_parties), sigma_parties);
}
