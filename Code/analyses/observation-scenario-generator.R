if(interactive()) {
  cfg_name <- "debug"
  n_cores <- parallelly::availableCores()
  list2env(cfg, envir = environment())
  save_trial_results <- F
  trial_path <- 'TEST'
  cfg <- yaml::yaml.load_file("Code/configs/repair-budget-SA.yaml")[[cfg_name]]
}

lambda <-
      cfg$`observation-queues`[["queue-1"]]$lambda
    mu <-
      cfg$`observation-queues`[["queue-1"]]$mu
    servers <-
      cfg$`observation-queues`[["queue-1"]]$servers
    K <-
      cfg$`observation-queues`[["queue-1"]]$queue_cap
    system_cap <- servers + K
    
    cat(sprintf("Queue-%d:\n  lambda = %f\n  mu = %f\n  servers = %d\n  K = %d\n  system_cap = %d\n",
               1, lambda, mu, servers, K, system_cap))
    cat("--  --  --  --  --  --  --\n")

sim_output <- mclapply(
  X = seq(num_replications),
  FUN = function(i) {             
    
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
    obs_idx <-
      c(1,
        2 + sample(x = length(generated_states) - 2, size = num_obs),
        length(generated_states))
    thetas <- rep(theta_i_max, length(generated_states))
    thetas[obs_idx] <- 0

    seq_likelihood <- calc_subtraj_prob(
                state_vector = generated_states,
                times_vector = generated_times,
                lambda = lambda,
                mu = mu,
                c =servers,
                K = system_cap,
                cumm_endpoint_dwell = T
            )
    cat('Observation Scenario',i,'L(S): ', seq_likelihood,"\n")

    return(
      list(
        data_seq = generated_states,
        time_seq = generated_times,
        theta = thetas,
        lambda = lambda,
        mu = mu,
        c = servers,
        K = system_cap,
        seq_likelihood = seq_likelihood,
        report = FALSE
      )
    )
    
  },
  mc.cores = n_cores
)

sim_output <- jsonlite::toJSON(sim_output, pretty = TRUE)
write(sim_output, file = paste0("Results/repair_budget_SA/",cfg_name,"/replication_info.json"))