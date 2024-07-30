source(".Rprofile")

gen_results_path <- function(res_dir,filename,ext = '.rds'){
   if(!grepl('\\.',ext)){
      ext <- paste0("\\.",ext)
   }
   filename <- paste0(filename,ext)
   path <- file.path(res_dir,filename)
   return(path)
}
job_name <- commandArgs(trailingOnly = TRUE)

if(length(job_name) ==  0){
   job_name <- "small_test"
}

args <- yaml.load_file('Code/configs/estimate_sensitivity_analysis.yaml')[[job_name]]
list2env(args,envir = environment())

system_cap <- queue_cap + servers
file_name <- gsub('\\.','_',paste('lambda',lambda$true_param,'mu',mu$true_param,'servers',servers,sep = '-'))
results_directory <- paste(results_directory,sep = "_")

if(!dir.exists(results_directory)){
   dir.create(results_directory,recursive = TRUE)
   print(paste0('Results directory created at: ',results_directory))
} else {
   print(paste0('Results stored at: ',results_directory))
   
}

sample_trajs <- gen_sample_queue_states(
   num_reps = 1,
   lambda = lambda$true_param,
   mu = mu$true_param,
   c = servers,
   K = queue_cap,
   warmup=warmup,
   sim_length = sim_length
)

if (length(sample_trajs) == 1) {
   sample_trajs <- sample_trajs[[1]]
}

generated_states <- sample_trajs$states[seq(min(length(sample_trajs$states),num_data_pts))]
generated_times <- sample_trajs$times[seq(min(length(sample_trajs$states),num_data_pts)-1)]

cat('L(S):', calc_subtraj_prob(
                state_vector = generated_states,
                times_vector = generated_times,
                lambda = lambda$true_param,
                mu = mu$true_param,
                c =servers,
                K = system_cap,
                cumm_endpoint_dwell = T
            ))

# Create trajectory observation points
obs_idx <- c(1,2 + sample(x = length(generated_states)-2,size = num_obs),length(generated_states))
thetas <- rep(theta_i_max,length(generated_states))
thetas[obs_idx] <- 0

# Create a sequence of budgets in log_2 space
lambda_estimates <- seq(lambda$min_estimate,lambda$max_estimate,length.out = lambda$num_samples)
mu_estimates <- seq(mu$min_estimate, mu$max_estimate, length.out = mu$num_samples)
budget <- floor(0.5 * sum(thetas))

# Recreate the observation trajectories with each different budget
lambda_sensitivity_results <- mclapply(
   X = seq_along(lambda_estimates),
   FUN = function(idx){ 
      repair_prob_dp(
         data_seq = generated_states,
         time_seq = generated_times,
         delta = budget,
         theta = thetas,
         lambda = lambda_estimates[idx],
         mu = mu$true_param,
         c = servers,
         K = system_cap,
         report = TRUE)
   },
    mc.cores = availableCores()
)

save.image(file = gen_results_path(res_dir = results_directory, filename = file_name , ext = '.RData'))
mu_sensitivity_results <- mclapply(
   X = seq_along(lambda_estimates),
   FUN = function(idx) repair_prob_dp(
      data_seq = generated_states,
      time_seq = generated_times,
      delta = budget,
      theta = thetas,
      lambda = lambda$true_param,
      mu = mu_estimates[idx],
      c = servers,
      K = system_cap,
      report = TRUE
      ),
   mc.cores = availableCores()
)
save.image(file = gen_results_path(res_dir = results_directory, filename = file_name , ext = '.RData'))

lambda_estimate_trajectories <- lapply(X = lambda_sensitivity_results, FUN = function(res) res$trajectory)
mu_estimate_trajectories <- lapply(X = mu_sensitivity_results, FUN = function(res) res$trajectory)
plot_x_axis <- c(0, cumsum(generated_times))
save.image(file = gen_results_path(res_dir = results_directory, filename = file_name , ext = '.RData'))

# Plot the varipus generated trajectories when varying mu

plot_data <- data.table(Time = plot_x_axis)

for (trajectory in seq_along(lambda_estimate_trajectories)){
  plot_data <- eval(parse(text = paste0("cbind(plot_data, `",as.character(lambda_estimates[trajectory]),"` = lambda_estimate_trajectories[[trajectory]])")))
}

plot <- ggplot() + 
  geom_step(data = melt(plot_data, id.vars = 'Time', variable.name = 'Repair Budget', value.name = 'State'), aes(x=Time,color=`Repair Budget`,y=State), alpha = 0.8) +
  geom_step(aes(x = plot_x_axis, y=generated_states), linetype = 'dashed',alpha = 0.4) + 
  geom_point(aes(x = plot_x_axis[obs_idx],
                 y=generated_states[obs_idx]),
             color = 'red',
             size = 5,
             shape = 20)
ggsave(filename = gen_results_path(res_dir = results_directory, filename =  paste(file_name,'lambda-estimate-trajectory-plots',sep = "-"), ext = '.png'),
       plot = plot,
       height = 6,
       width = 8,
       units = 'in',
       dpi = 500)
# Plot the various mu trajectories
plot_data <- data.table(Time = plot_x_axis)

for (trajectory in seq_along(mu_estimate_trajectories)){
  plot_data <- eval(parse(text = paste0("cbind(plot_data, `",as.character(mu_estimates[trajectory]),"` = mu_estimate_trajectories[[trajectory]])")))
}

plot <- ggplot() + 
  geom_step(data = melt(plot_data, id.vars = 'Time', variable.name = 'Repair Budget', value.name = 'State'), aes(x=Time,color=`Repair Budget`,y=State), alpha = 0.8) +
  geom_step(aes(x = plot_x_axis, y=generated_states), linetype = 'dashed',alpha = 0.4) + 
  geom_point(aes(x = plot_x_axis[obs_idx],
                 y=generated_states[obs_idx]),
             color = 'red',
             size = 5,
             shape = 20)
ggsave(filename = gen_results_path(res_dir = results_directory, filename =  paste(file_name,'mu-estimate-trajectory-plots',sep = "-"), ext = '.png'),
       plot = plot,
       height = 6,
       width = 8,
       units = 'in',
       dpi = 500)