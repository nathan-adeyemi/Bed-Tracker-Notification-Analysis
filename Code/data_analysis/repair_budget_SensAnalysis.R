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
   warmup=25,
   sim_length = 100
)

if (length(sample_trajs) == 1) {
   sample_trajs <- sample_trajs[[1]]
}
generated_states <- sample_trajs$states
generated_states <- corrupted_states <- generated_states
generated_times <- sample_trajs$times

# Endpoints of the sampled trajectory are assumed to be correct
# thetas <- c(0,rep(2, length(sample_trajs$states)-1)) # Ensures the first point cant be changed

max_error_bound <- 3
thetas <- c(0,rep(max_error_bound,length(generated_states)-2),0)
budgets <- c(seq(5,25,5),seq(30,150,10))
# budgets <- c(5,15,30,50)

repair_results <- mclapply(
   X = budgets,
   FUN = repair_prob_dp,
   data_seq = generated_states,
   time_seq = generated_times,
   theta = thetas,
   lambda = test_lambda,
   mu = test_mu,
   c = test_servers,
   K = test_system_cap,
   mc.cores = availableCores()
)

traj_likelihoods <-
   sapply(
      X = lapply(X = repair_results, FUN = function(res) res$trajectory),
      FUN = calc_subtraj_prob,
      times_vector = generated_times,
      lambda = test_lambda,
      mu = test_mu,
      c = test_servers,
      K = test_system_cap
   )
df <- data.table(`Repair Budget` = budgets, `Trajectory Log-Likelihood` = traj_likelihoods, `Solution Runtime` = sapply(X = repair_results, FUN = function(res) res$runtime))
# Scale factor for the secondary y-axis
scale_factor <- df[,min(`Trajectory Log-Likelihood`)] / df[,max(`Solution Runtime`)]

# Two Line plots: the Likelihood of the returned Trajectory and the solution time
ggplot(df, aes(x = `Repair Budget`)) +
   geom_line(aes(y = `Trajectory Log-Likelihood`), color = "blue") +  # Primary y-axis
   geom_line(aes(y = (max(df$`Solution Runtime`) - `Solution Runtime`) * scale_factor), color = "red") +  # Secondary y-axis transformed and reversed
   scale_y_continuous(
      name = "Log-Likelihood",  # Label for the primary y-axis
      sec.axis = sec_axis(~ max(df$`Solution Runtime`) - (. / scale_factor), name = "Runtime (in seconds)")  # Secondary y-axis with reversed transformation
   ) +
   theme_linedraw()

save.image(file = file.path('Results','numerical_experiments','repair_budget_sensitivity_analysis',paste('lambda',test_lambda,'mu',test_mu,'seervers',test_servers,'.Rdata',sep = '_')))

