if(interactive()){
  port_val <- NULL
  trial_path <- 'TEST'
  cfg_path <- "Code/configs/bed-registry-analysis.yaml"
  cfg_name <- "full"
  arg_list <- list(delta = 100, init_lambda_prop = 1.0, replication = 1)
} else {
  port_val <- get_sys_env_var(var_name = 'port', var_class = 'numeric')
  trial_path <-get_sys_env_var(var_name = 'trial_path', var_class = 'character')
  cfg_name <- get_sys_env_var(var_name = 'cfg_type', var_class = 'character')
  cfg_path <- get_sys_env_var(var_name = 'cfg_path', var_class = 'character')
  trial_path <- file.path(trial_path,cfg_name)
}
if(cfg_name != 'None'){
  cfg <- yaml.load_file(cfg_path)[[cfg_name]]
  list2env(cfg, envir = environment())
}

if(!dir.exists(trial_path)){
  dir.create(trial_path, recursive = TRUE)
}

n_cores <- availableCores()
analysis_path <- gsub("configs","analyses",gsub("yaml","R",cfg_path))
if (!is.null(port_val)) {
  client_socket <-
    make.socket(
      host = 'localhost',
      port = as.numeric(port_val),
      server = F,
      fail = T
    )
} else {
  client_socket <- NULL
}
if(!interactive()){
  arg_list <- fromJSON(read_json_con(client_socket))
}
cat("Beginning analysis trial with argument: \n")
# print(arg_list)
list2env(arg_list,environment())
source(analysis_path)

if(!interactive()){

  if(!exists('results')){
    results = list("result" = "None")
  }

  transmit_results(results = results, receiver = client_socket)
}