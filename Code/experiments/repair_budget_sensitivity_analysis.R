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

if(length(args)> 0){
   args <- yaml.load_file('Code/configs/repair_budget_sensitivity_analysis.yaml')[[job_name]]
   list2env(args,envir = environment())
}else{
   lambda  <- 1.5
   mu <- 0.15
   servers <- 25
   queue_cap <- 0
   theta_i_max <-  8
   budget_min <- 1       # Start of the sequence
   budget_max <- 100      # End of the sequence
   n_budgets <- 40   # Number of points in the sequence
   budget_log <- 2 # Controls spacing of the evaluated budgets
   num_obs <- 10
   job_name <- 'test'
   warmup <- 10
   sim_length <- 100
}

system_cap <- queue_cap + servers
file_name <- gsub('\\.','_',paste('lambda',lambda,'mu',mu,'servers',servers,sep = '-'))
results_directory <- paste(results_directory,sep = "_")

if(!dir.exists(results_directory)){
   dir.create(results_directory,recursive = TRUE)
   print(paste0('Results directory created at: ',results_directory))
}

sample_trajs <- gen_sample_queue_states(
   num_reps = 1,
   lambda = lambda,
   mu = mu,
   c = servers,
   K = queue_cap,
   warmup=warmup,
   sim_length = sim_length
)

if (length(sample_trajs) == 1) {
   sample_trajs <- sample_trajs[[1]]
}
generated_states <- sample_trajs$states
generated_times <- sample_trajs$times

# Create trajectory observation points
obs_idx <- c(1,2 + sample(x = length(generated_states)-2,size = num_obs),length(generated_states))
thetas <- rep(theta_i_max,length(generated_states))
thetas[obs_idx] <- 0

# Create a sequence of budgets in log_2 space
budgets <- seq(log(budget_min,base = budget_log), log(budget_max, base = budget_log), length.out = n_budgets)
budgets <- floor(budget_log ** budgets)

# Recreate the observation trajectories with each different budget
repair_results <- mclapply(
   X = budgets,
   FUN = repair_prob_dp,
   data_seq = generated_states,
   time_seq = generated_times,
   theta = thetas,
   lambda = lambda,
   mu = mu,
   c = servers,
   K = system_cap,
   mc.cores = availableCores()
)

save.image(file = gen_results_path(res_dir = results_directory, filename = file_name , ext = '.RData'))

traj_likelihoods <- lapply(X = repair_results, FUN = function(res) res$likelihood)

df <- data.table(`Repair Budget` = budgets,
                 `Trajectory Log-Likelihood` = traj_likelihoods,
                 `Solution Runtime` = sapply(X = repair_results,
                                             FUN = function(res) res$runtime))
# Scale factor for the secondary y-axis
scale_factor <- df[,min(`Trajectory Log-Likelihood`)] / df[,max(`Solution Runtime`)]

# Two Line plots: the Likelihood of the returned Trajectory and the solution time
plot <- ggplot(df, aes(x = `Repair Budget`)) +
   geom_point(aes(y = `Trajectory Log-Likelihood`), color = "black", shape = 3) +  # Primary y-axis
   geom_line(aes(y = `Trajectory Log-Likelihood`), color = "blue") +  # Primary y-axis
   geom_point(aes(y = (max(df$`Solution Runtime`) - `Solution Runtime`) * scale_factor), color = "black", shape = 4) +
   geom_line(aes(y = (max(df$`Solution Runtime`) - `Solution Runtime`) * scale_factor), color = "red") +  # Secondary y-axis transformed and reversed
   scale_y_continuous(
      name = "Log-Likelihood",  # Label for the primary y-axis
      sec.axis = sec_axis(~ max(df$`Solution Runtime`) - (. / scale_factor), name = "Runtime (sec.)")  # Secondary y-axis with reversed transformation
   ) +
   theme_linedraw() +
   guides(
  color = guide_legend(
    override.aes = list(
      linetype = c(1, 1),
      color = c("blue", "red")
    ),
    title = "Metric",
    labels = c("Log-Likelihood of the Repaired Trajectory", "Execution Time")
  )
)

ggsave(gen_results_path(res_dir = results_directory, filename = file_name, ext = '.png'),
       plot = plot,
       height = 6,
       width = 8,
       units = 'in',
       dpi = 500)
save.image(file = gen_results_path(res_dir = results_directory, filename = file_name , ext = '.RData'))

