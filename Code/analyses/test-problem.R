source(".Rprofile")

job_type <- commandArgs(trailingOnly = TRUE)
config <- yaml.load_file('Code/configs/test_problem.yaml')
list2env(config, envir = environment())

file_name <-
  gsub('\\.',
       '_',
       paste(
         'lambda',
         lambda,
         'mu',
         mu,
         'servers',
         servers,
         `if`(
           partially_observable,
           'partially_observable',
           'corrupted_data'
         ),
         sep = '-'
       ))
results_directory <- paste(results_directory, sep = "_")
if (!dir.exists(results_directory)) {
  dir.create(results_directory, recursive = TRUE)
}

system_cap <- queue_cap + servers
sample_trajs <- gen_sample_queue_states(
  num_reps = 1,
  lambda = lambda,
  mu = mu,
  c = servers,
  K = system_cap,
  warmup = warmup,
  sim_length = sim_length
)

if (length(sample_trajs) == 1) {
  sample_trajs <- sample_trajs[[1]]
}
generated_states <- sample_trajs$states
num_data_pts <- min(length(generated_states), num_data_pts)
generated_states <-
  sample_trajs$states[seq(min(length(sample_trajs$states), num_data_pts))]
if (!partially_observable) {
  corrupted_states <- generated_states
}
generated_times <-
  sample_trajs$times[seq(min(length(sample_trajs$states), num_data_pts) - 1)]

cat(
  'L(S): ',
  calc_subtraj_prob(
    generated_states,
    times_vector = generated_times,
    lambda = lambda,
    mu = mu,
    c = servers,
    K = system_cap
  )
)

# Endpoints of the sampled trajectory are assumed to be correct
if (!partially_observable) {
  max_error_bound <- floor(servers / 2)
  thetas <- rep(0, length(corrupted_states))
  for (i in seq(2, length(corrupted_states) - 1)) {
    if (runif(1) < data_error_rate) {
      corrupted_states[i] <-
        min(system_cap , max(0, corrupted_states[i] + sample(
          seq(-max_error_bound, max_error_bound), 1
        )))
      thetas[i] <- max_error_bound
    }
  }
  
} else {
  # Create a sequence in log space
  obs_idx <-
    c(1,
      2 + sample(x = length(generated_states) - 2, size = num_obs),
      length(generated_states))
  thetas <- rep(max_error_bound, length(generated_states))
  thetas[obs_idx] <- 0
}
delta <- floor(delta_proportion * sum(thetas))

repair_results <- repair_prob_dp(
  data_seq = `if`(partially_observable, generated_states, corrupted_states),
  time_seq = generated_times,
  theta = thetas,
  delta = delta,
  lambda = lambda,
  mu = mu,
  c = servers,
  K = system_cap,
  report = TRUE,
  heuristic_cutoff = FALSE
)


fixed_traj <- repair_results$trajectory
remaining_budget <- repair_results$budget
runtime <- repair_results$runtime
plot_x_axis <- c(0, cumsum(generated_times))

plot_trajectories(generated_states = generated_states,
                  fixed_traj = fixed_traj,
                  time = plot_x_axis,
                  path = gen_results_path(
                    res_dir = results_directory,
                    filename = file_name,
                    ext = '.jpg'
                  ))

save.image(gen_results_path(
  res_dir = results_directory,
  filename = file_name,
  ext = '.Rdata'
))
