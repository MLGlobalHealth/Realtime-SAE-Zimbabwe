## Helper functions
# Get info needed to fit ICAR and BYM model from adjacency matrix
nb2mat_to_nodes <- function(adj_mat) {
  # input: adjacency matrix
  # output: list of two vectors, the i-th element of these vectors should form a neibouring pair
  N_A <- nrow(adj_mat)
  N_edges <- sum(adj_mat != 0) / 2
  n1 <- vector(mode="numeric", length = N_edges)
  n2 <- vector(mode="numeric", length = N_edges)
  k <- 1
  for (i in 1:N_A) {
    for (j in i:N_A) {
      if (adj_mat[i, j] != 0) {
        n1[k] <- i
        n2[k] <- j
        k <- k + 1
      }
    }
  }
  return(list(n1 = n1, n2 = n2))
}
prepare_bym2 <- function(adj_mat) {
  # input: adjacency matrix
  # output: list of 
  ## - two vectors, the i-th element of these vectors should form a neibouring pair
  ## - the geometric mean of the variances of ICAR variance-covariance matrix used for BYM2 model
  nodes <- nb2mat_to_nodes(adj_mat)
  inla_adj <- sparseMatrix(i = nodes$n1, j = nodes$n2,
                           x = 1, symmetric = T)
  # ICAR precision matrix
  Q <- Diagonal(nrow(adj_mat), Matrix::rowSums(inla_adj)) - inla_adj
  Q_jit = Q + Diagonal(nrow(adj_mat)) * max(diag(Q)) * sqrt(.Machine$double.eps)
  
  Q_inv = inla.qinv(Q_jit, constr=list(A = matrix(1, 1, nrow(adj_mat)), e=0))
  
  #Compute the geometric mean of the variances, which are on the diagonal of Q.inv
  scl = exp(mean(log(diag(Q_inv))))
  return(list(n1 = nodes$n1, n2 = nodes$n2, scaling_factor = scl))
}

# Helper functions for post-stratification and prediction
# Helper functions
post_pred_each <- function(b0,betas,phi,theta,psi,nu,xi,
                           Xnew, adm2_index, adm1_index, time_index,
                           method = 'MR',
                           weights = NULL,
                           response = 'pr'){
  #Input
  #- parameters: b0,betas,phi,theta,psi, note psi to be in matrix form
  #- New data: Xnew, admin2_index, admin1_index, time_index,
  #- method: either SAE or MRP
  #- weights: weights to be used in aggregation if not given, equal weights are used
  #- response: the type of response for prediction at unit level pr for probability, binary for 0/1, binomial [only available for MRP with zimvac pop frame] 
  # ----  for binary case, both types of estimates will be returned
  #Output
  # prevalence for each admin2 X times 
  
  Nnew = length(adm1_index)
  if (is.null(weights)) weights = rep(1, Nnew) 
  
  psi_vec = vector(mode = "numeric",length=Nnew)
  for (i in 1:Nnew){
    psi_vec[i] = psi[time_index[i],adm1_index[i]]
  }
  eta = b0 + Xnew%*%betas + phi[adm2_index] + theta[adm2_index] + psi_vec + nu[time_index] + xi[time_index]
  pvec = 1/(1+exp(-eta))
  
  df = data.frame(adm2_index=adm2_index, 
                  time_index=time_index, 
                  phat = pvec,
                  w = weights)
  if (response == 'binomial'){
    phat = rbinom(length(pvec),weights,pvec)/weights
    df = data.frame(adm2_index=adm2_index, 
                    time_index=time_index, 
                    phat = phat,
                    w = weights)
  }
  
  df_res = df |>
    group_by(adm2_index,time_index) |>
    summarise(prev = weighted.mean(phat, w),.groups="rowwise") |> ungroup()
  
  if (response=='binary'){
    yvec = rbernoulli(length(pvec),pvec)
    df$yhat = yvec
    df_res$prev2 = (df |>
                      group_by(adm2_index,time_index) |>
                      summarise(prev2 = weighted.mean(yhat, w),.groups="rowwise"))$prev2
  } 
  return(df_res)
}

post_pred_sample <- function(vec_b0, df_betas, df_phi, df_theta, df_psi, df_nu,df_xi,
                             Xnew, adm2_index, adm1_index, time_index,
                             N_new,
                             response,
                             weights){
  N_draw = length(vec_b0)
  df_post_pred_sample = (matrix(NA,N_new,(N_draw)))
  if (response == 'binary') df_post_pred_sample2 = df_post_pred_sample
  for (j in 1:N_draw){
    if (j%%100 == 0) {cat("iteration:",j,"\n")}
    
    df_tmp <- post_pred_each(b0 = vec_b0[j],
                             betas = c(t(df_betas[j,])),
                             phi = c(t(df_phi[j,])),
                             theta = c(t(df_theta[j,])),
                             psi = matrix(unlist(df_psi[j,]), nrow = length(unique(df_model$month_idx)), 
                                          ncol = length(unique(df_model$adm1_index))),
                             nu =  c(t(df_nu[j,])),
                             xi = c(t(df_xi[j,])),
                             Xnew = Xnew,
                             adm2_index = adm2_index,
                             adm1_index = adm1_index,
                             time_index = time_index,
                             response = response,
                             weights = weights)
    if(j == 1) {
      df_index = df_tmp[,1:2]
    }
    df_post_pred_sample[,j] = df_tmp$prev
    if (response == 'binary') df_post_pred_sample2[,j] = df_tmp$prev2
  }
  
  df_post_pred_sample <- cbind(df_index,as.data.frame(df_post_pred_sample))
  if (response == 'binary'){
    df_post_pred_sample2 <- cbind(df_index,as.data.frame(df_post_pred_sample2))
    df_post_pred_sample <- list(df_post_pred_sample, df_post_pred_sample2)
  } 
  return(df_post_pred_sample)
}


# Function that takes cmdstanfit object & model_names and produce posterior predictive sample 
## For MR estimate
MR_post_samples <- function(fit, new_data, variables.toinclude, response = 'pr', weights = F){
  if (weights){
    smp_weights = new_data$dmg_weight_ust
  } else {
    smp_weights = NULL
  }
  vec_b0  = fit$draws(format = "df", variables=c('b0'),  inc_warmup = F) |> 
    select(-.chain,-.iteration,-.draw) |> unlist() |> unname()
  df_betas  = fit$draws(format = "df", variables=c('betas'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_phi = fit$draws(format = "df", variables=c('phi'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_theta = fit$draws(format = "df", variables=c('theta'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_psi= fit$draws(format = "df", variables=c('psi'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_nu= fit$draws(format = "df", variables=c('nu'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_xi = fit$draws(format = "df", variables=c('xi'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  
  df_ps = post_pred_sample(vec_b0, df_betas, df_phi, df_theta, df_psi,df_nu,df_xi,
                           Xnew = as.matrix(new_data[,variables.toinclude]),
                           adm2_index = new_data$adm2_index, 
                           adm1_index = new_data$adm1_index,
                           time_index = new_data$month_idx,
                           N_new = ((new_data |> group_by(adm2_name, month_idx) |> summarise(n()) |> nrow())),
                           response = response,
                           weights = smp_weights
  )
  return(df_ps)
}
## For MRP/jMRP estimate
MRP_post_samples <- function(fit, new_data, month_idx_list, variables.toinclude, response = 'pr'){
  
  vec_b0  = fit$draws(format = "df", variables=c('b0'),  inc_warmup = F) |> 
    select(-.chain,-.iteration,-.draw) |> unlist() |> unname()
  df_betas  = fit$draws(format = "df", variables=c('betas'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_phi = fit$draws(format = "df", variables=c('phi'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_theta = fit$draws(format = "df", variables=c('theta'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_psi= fit$draws(format = "df", variables=c('psi'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_nu= fit$draws(format = "df", variables=c('nu'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  df_xi = fit$draws(format = "df", variables=c('xi'),  inc_warmup = F) |> select(-.chain,-.iteration,-.draw)
  
  df_ps =  as.data.frame(matrix(NA,0,(length(vec_b0)+2)))
  if (response=='binary') df_ps2 = df_ps
  for (i in 1:length(month_idx_list)){
    print(month_idx_list[i])
    df_to_pred <- new_data |> 
      mutate(month_idx = month_idx_list[i]
      ) |> select(-week_idx)
    df_ps_tmp = post_pred_sample(vec_b0, df_betas, df_phi, df_theta, df_psi,df_nu,df_xi,
                                 Xnew = as.matrix(df_to_pred[,variables.toinclude]),
                                 adm2_index = df_to_pred$adm2_index, 
                                 adm1_index = df_to_pred$adm1_index,
                                 time_index = df_to_pred$month_idx,
                                 N_new = ((df_to_pred |> group_by(adm2_name, month_idx) |> summarise(n()) |> nrow())),
                                 response = response,
                                 weights = df_to_pred$pop_weight)
    if(response=='binary'){
      df_ps = rbind(df_ps,df_ps_tmp[[1]])
      df_ps2 = rbind(df_ps2, df_ps_tmp[[2]])
    } else {
      df_ps = rbind(df_ps,df_ps_tmp)
    }
  }
  if (response=='binary') return(list(df_ps,df_ps2)) else return(df_ps)
}


dummy_guide <- function(
    labels = NULL,  
    ..., 
    title = NULL, 
    key   = draw_key_point,
    guide_args = list()
) {
  # Capture arguments
  aesthetics <- list(...)
  n <- max(lengths(aesthetics), 0)
  labels <- labels %||%  seq_len(n)
  
  # Overrule the alpha = 0 that we use to hide the points
  aesthetics$alpha <- aesthetics$alpha %||% rep(1, n)
  
  # Construct guide
  guide_args$override.aes <- guide_args$override.aes %||% aesthetics
  guide <- do.call(guide_legend, guide_args)
  
  # Allow dummy aesthetic
  update_geom_defaults("point", list(dummy = "x"))
  
  dummy_geom <- geom_point(
    data = data.frame(x = rep(Inf, n), y = rep(Inf, n), 
                      dummy = factor(labels)),
    aes(x, y, dummy = dummy), alpha = 0, key_glyph = key
  )
  dummy_scale <- discrete_scale(
    "dummy", "dummy_scale", palette = scales::identity_pal(), name = title,
    guide = guide
  )
  list(dummy_geom, dummy_scale)
}


# some helper functions
wilson_lower <- function(phat, n, alpha = 0.05, z = NULL){
  # compute lower bound of wilson score interval 
  # phat : weighted sample mean (proportion)
  # weights : weights vector 
  # alpha : confidence level 
  if (is.null(z))   z <-  qnorm(1-alpha/2)
  if (phat==0) {
    return(0)
  } else if (phat==1) {
    return(n/(n+z^2))
  } else{
    return(
      (1/(1+(1/n)*(z^2)))*(phat + (z^2)/(2*n) - ((z)/(2*n))*sqrt(4*n*phat*(1-phat) +z^2))
    ) 
  }
}
wilson_lower <- Vectorize(wilson_lower)
wilson_upper <- function(phat, n,  alpha = 0.05, z = NULL){
  # compute upper bound of wilson score interval 
  # phat : (weighted) sample mean (proportion)
  # n: sample size 
  # alpha : confidence level 
  if (is.null(z))   z <-  qnorm(1-alpha/2)
  if (phat==0) {
    return(z^2 / (n+z^2))
  } else if (phat==1) {
    return(1)
  } else{
    return(
      (1/(1+(1/n)*(z^2)))*(phat + (z^2)/(2*n) + ((z)/(2*n))*sqrt(4*n*phat*(1-phat) + z^2))
    ) 
  }
}
wilson_upper <- Vectorize(wilson_upper)

compute_wald <- function(est,se, z){
  return(est + z*se)
}
compute_wilson <- function(est,n,z){
  if (z<0) wilson_lower(phat=est,n=n,z=-z) else wilson_upper(phat=est, n=n, z=z)
}

# from dataset containing posterior sample of post-stratified prevalance, get posterior mean and quarantines
process_ps <- function(df_ps, coverage_level = c(0.8,0.9)){
  df_ps <- df_ps |> mutate(month_idx = factor(time_index)) |> select(-time_index)
  quantiles = c()
  q_names = c()
  for (q in coverage_level){
    alpha <- (1-q)
    quantiles = c(quantiles, c(alpha/2, 1-alpha/2))
    q_names = c(q_names, paste0(c('low','high'), as.character(q*100)))
  }
  df_est <- bind_cols(
    (df_ps |> select(adm2_index,month_idx)),
    (df_ps |> select(-adm2_index,-month_idx) |> rowMeans() |> bind_cols()),
    (df_ps |> select(-adm2_index,-month_idx) |> pmap_df(~as.list(quantile(c(...), quantiles))) |> bind_cols())
  )
  colnames(df_est)[c(-1,-2)] = c('y_est', q_names)
  df_est <- df_est |>
    left_join(df_month_unique , by = 'month_idx') |> 
    left_join(df_region_unique, by = "adm2_index")|> 
    select(-adm2_index, -month_idx, - adm1_index)
  return(df_est)
}
process_direct <- function(df, coverage_level = c(0.8,0.9)){
  q_names_wald =q_names_wilson = c()
  z_scores  = c()
  for (q in coverage_level){
    alpha <- (1-q)
    z_scores = c(z_scores, qnorm(c(alpha/2, 1-alpha/2)))
    q_names_wald = c(q_names_wald, paste0(c('wald_low','wald_high'), as.character(q*100)))
    q_names_wilson = c(q_names_wilson, paste0(c('wilson_low','wilson_high'), as.character(q*100)))
  }
  
  wald_cis <- lapply(z_scores, compute_wald, est=df$y_est, se=df$y_est_se) |> bind_cols()
  colnames(wald_cis) = q_names_wald
  wilson_cis <- lapply(z_scores, compute_wilson, est=df$y_est, n=df$n_w) |> bind_cols()
  colnames(wilson_cis) = q_names_wilson
  
  df <- df |> bind_cols(
    wald_cis,
    wilson_cis
  )
  return(df)
}
process_zimvac <- function(df, coverage_level = c(0.8,0.9)){
  q_names_wald =q_names_wilson = c()
  z_scores  = c()
  for (q in coverage_level){
    alpha <- (1-q)
    z_scores = c(z_scores, qnorm(c(alpha/2, 1-alpha/2)))
    q_names_wald = c(q_names_wald, paste0(c('zv_wald_low','zv_wald_high'), as.character(q*100)))
    q_names_wilson = c(q_names_wilson, paste0(c('zv_wilson_low','zv_wilson_high'), as.character(q*100)))
  }
  
  wald_cis <- lapply(z_scores, compute_wald, est=df$zimvac_est, se=df$zimvac_est_se) |> bind_cols()
  colnames(wald_cis) = q_names_wald
  wilson_cis <- lapply(z_scores, compute_wilson, est=df$zimvac_est, n=df$n) |> bind_cols()
  colnames(wilson_cis) = q_names_wilson
  
  df <- df |> bind_cols(
    wald_cis,
    wilson_cis
  )
  return(df)
}

