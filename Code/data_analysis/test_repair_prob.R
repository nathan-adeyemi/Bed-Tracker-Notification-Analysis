test_lambda  <- 0.75
test_mu <- 0.15
test_servers <- 15
test_queue_cap <- 0
test_system_cap <- test_queue_cap + test_servers
data_error_rate <- 0.25

plot_ylim <- 150
corrupted_data <- FALSE

sample_trajs <- gen_sample_queue_states(
   num_reps = 1,
   lambda = test_lambda,
   mu = test_mu,
   c = test_servers,
   K = test_queue_cap,
   warmup=10,
   sim_length = 60
)

if (length(sample_trajs) == 1) {
   sample_trajs <- sample_trajs[[1]]
}
generated_states <- sample_trajs$states
plot_ylim <- min(length(generated_states),plot_ylim)
generated_states <- corrupted_states <- generated_states
generated_times <- sample_trajs$times

# Endpoints of the sampled trajectory are assumed to be correct
if (corrupted_data){
   max_error_bound <- floor(test_servers/2)
   thetas <- rep(0,length(corrupted_states))
   for (i in seq(2, length(corrupted_states)-1)){
      if (runif(1) < data_error_rate) {
         corrupted_states[i] <- min(test_system_cap , max(0, corrupted_states[i] + sample(seq(-max_error_bound,max_error_bound),1)))
         thetas[i] <- max_error_bound
      }
   }
   test_delta <- floor(0.75*sum(thetas))
} else {
   max_error_bound <- 3
   thetas <- c(0,rep(max_error_bound,length(generated_states)-2),0)
   test_delta <- 15
}

repair_results <- repair_prob_dp(
   data_seq = `if`(!corrupted_data, generated_states, corrupted_states),
   time_seq = generated_times,
   theta = thetas,
   delta = 10,
   lambda = test_lambda,
   mu = test_mu,
   c = test_servers,
   K = test_system_cap
)


fixed_traj <- repair_results$trajectory
remaining_budget <- repair_results$budget
runtime <- repair_results$runtime
plot_x_axis <- c(0, cumsum(generated_times))
# plot_x_axis <- seq_along(generated_states)
plot(
   x = plot_x_axis[seq(plot_ylim)],
   y = generated_states[seq(plot_ylim)],
   # y = fixed_traj,
   type = 's',
   col = 'black',
   lty = 3,
   lwd = 5,
   xlab = 'time',
   ylab = 'Number in System'
)

lines(x = plot_x_axis[seq(plot_ylim)],
      y = fixed_traj[seq(plot_ylim)],
      type = 's',
      col = 'blue',
      lwd = 3,
      lty = 1)

abline(v = plot_x_axis[which(corrupted_states != generated_states)])

legend(
   'bottom',
   legend = c(
      'Original State Trajectory',
      # 'Corrupted Trajectory',
      'Repaired State Trajectory'
   ),
   col = c('black',
           # 'red',
           'blue'),
   lty=c(2,1),
   lwd = c(2,2)
)

traj_likelihoods <-
   lapply(
      X = list(generated_states,
               # corrupted_states,
               fixed_traj),
      FUN = calc_subtraj_prob,
      times_vector = generated_times,
      lambda = test_lambda,
      mu = test_mu,
      c = test_servers,
      K = test_system_cap
   )

cat('The original simulated trajectory has a likelihood of ',
    traj_likelihoods[[1]],
    '\n',
    'The repaired trajectory had a likelihood of ',traj_likelihoods[[2]],
    '\n',
    'There was a remaining budget of',remaining_budget,'and solving took ',run_time,'seconds.')

print(table(fixed_traj - shift(fixed_traj,type= 'lag',n=1)))
