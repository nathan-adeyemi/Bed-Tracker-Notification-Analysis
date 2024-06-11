gen_sample_queue_states <- function(num_reps, lambda, mu, c = 1, K = 0, warmup = 50, sim_length = 100) {
   test_traj <-
      simmer::trajectory('arrival_path') %>% simmer::seize('queue_resource', 1)  %>% simmer::timeout(function()
         rexp(1, rate = mu)) %>% simmer::release('queue_resource')
   test_env  <-
      function(i) {
         simmer('test_env') %>% add_resource('queue_resource',
                                             capacity = c,
                                             queue_size = K) %>% add_generator('arrival', test_traj, function()
                                                rexp(1, rate = lambda)) %>% run(until = warmup + sim_length) %>% wrap()
      }
   test_res <- mclapply(X = seq(num_reps), FUN = test_env,mc.cores = 4)
   sample_state_trajs  <- split(x = data.table(simmer::get_mon_resources(test_res))[time > warmup, list(system, time, replication)][, time := time - data.table::shift(time, n = 1, type = 'lag'), by = replication], by = 'replication')
   sample_state_trajs  <- lapply(
      sample_state_trajs,
      FUN = function(dt) {
         return(list(states = dt$system,
                     times = as.numeric(na.omit(dt$time))))
      }
   )
   return(sample_state_trajs)
}