save_results <- T
if(interactive()) {
  cfg <-
    yaml::yaml.load_file("Code/configs/observation-density.yaml")[['debug']]
  n_cores <- parallelly::availableCores()
  list2env(cfg, envir = environment())
  observation_density <- 0.9
  init_est_proportion <- 1
  num_queues <- 1
  save_results <- F
  trial_path <- 'TEST'
}

trial_path <- gen_results_path(res_dir = trial_path, filename=paste('replication',replication,sep = "-"), ext = ".RData")
cat(
    "========================================\n",
    "Observation Density:",observation_density,"\n",
    "Replication: ", replication, "\n",
    "Perceived Trajectory Initial \u03BB estimate:",init_est_proportion,"\n",
    "Number of Queues: ", num_queues,"\n",
    "Results Directory:",trial_path, "\n",
    "--  --  --  --  --  --  --\n"
  )
if(file.exists(trial_path)){
  load(trial_path)
  cat('Trial has already been run\n',
      "========================================\n")

} else {
  cat('TRIAL IS RUNNING')
  sim_output <- lapply(
    X = seq(num_queues),
    FUN = function(i) {
      lambda <-
        cfg$`observation-queues`[[paste('queue', i, sep = "-")]]$lambda
      mu <-
        cfg$`observation-queues`[[paste('queue', i, sep = "-")]]$mu
      servers <-
        cfg$`observation-queues`[[paste('queue', i, sep = "-")]]$servers
      K <-
        cfg$`observation-queues`[[paste('queue', i, sep = "-")]]$queue_cap
      system_cap <- servers + K
      
      cat(sprintf("Queue-%d:\n  lambda = %f\n  mu = %f\n  servers = %d\n  K = %d\n  system_cap = %d\n",
                i, lambda, mu, servers, K, system_cap))
      cat("--  --  --  --  --  --  --\n")
                      
      traj <- gen_sample_queue_states(
        num_reps = 1,
        lambda = lambda,
        mu = mu,
        c = servers,
        K = K,
        warmup = warmup,
        sim_length = sim_length
      )
      
      generated_states <- traj[[1]]$states
      generated_times <- traj[[1]]$times
      num_obs <- observation_density * length(generated_states)
      obs_idx <-
        c(1,
          2 + sample(x = length(generated_states) - 2, size = num_obs),
          length(generated_states))
      thetas <- rep(theta_i_max, length(generated_states))
      thetas[obs_idx] <- 0
      return(
        list(
          data_seq = generated_states,
          time_seq = generated_times,
          theta = thetas,
          delta = delta,
          lambda = lambda * init_est_proportion,
          mu = mu,
          c = servers,
          K = system_cap,
          report = TRUE
        )
      )
      
    }
    # mc.cores = n_cores
  )

  sw_repair <- mclapply(
    X = sim_output,
    FUN = function(inp_list)
      do.call(repair_prob_dp, inp_list),
    mc.cores = n_cores
  )

  queue_df <-
    lapply(seq(num_queues), function(i)
      eval(parse(
        text = paste0(
          "queue_1 <- data.table(queue_",
          i,
          "_states = sim_output[[",
          i,
          "]]$data_seq, queue_",
          i,
          "_perceived_states = sw_repair[[",
          i,
          "]]$trajectory, times = cumsum(c(0,sim_output[[",
          i,
          "]]$time_seq)))"
        )
      )))
  names(queue_df) <-
    paste('queue', seq(num_queues), "df", sep = "_")
  queue_df <- Reduce(function(x, y)
    merge(x, y, by = "times", all = T), queue_df)
  queue_df[, (setdiff(names(queue_df), 'times')) := lapply(.SD, na.locf, na.rm = FALSE), .SDcols = setdiff(names(queue_df), 'times')]
  states_cols <- grep(paste0("^queue_[1-", num_queues, "]_states$"),
                      names(queue_df),
                      value = TRUE)
  perceived_cols <- grep(
    paste0("^queue_[1-", num_queues, "]_perceived_states$"),
    names(queue_df),
    value = TRUE
  )

  queue_df[, network_states := rowSums(.SD), .SDcols = states_cols]
  queue_df[, perceived_states := rowSums(.SD), .SDcols = perceived_cols]

  results <- list(
    "nu_pi_c" = calc_nu(
      perceived_states = queue_df$perceived_states,
      generated_times = c(na.omit(
        queue_df$time - data.table::shift(queue_df$time, n = 1, type = 'lag')
      ), 0),
      generated_states = queue_df$network_states,
      c = sum(sapply(sim_output, function(i) i$c)),
      nu_bar = FALSE),
    
    "nu_L_bar" = calc_nu(
      perceived_states = queue_df$perceived_states,
      generated_times = c(na.omit(
        queue_df$time - data.table::shift(queue_df$time, n = 1, type = 'lag')
      ), 0),
      generated_states = queue_df$network_states,
      c = sum(sapply(sim_output, function(i) i$c))
    )
  )

  if(save_results){
    save.image(trial_path)
  }
  cat(
    "\u03BD-{I}: ",
    results$nu_pi_c,
    "\n",
    "\u03BD-bar: ",
    results$nu_L_bar,
    "\n",
    "========================================\n",
    "\n"
  )
}

