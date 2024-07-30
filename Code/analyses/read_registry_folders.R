process_file <- function(file){
  
  # Extract the delta value
  delta <- regexpr("(?<=init_lambda_prop-)[0-9_]+", file, perl = TRUE)
  delta <- as.numeric(delta)
  
  # Extract the init_lambda_prop value
  init_lambda_prop <- regexpr("(?<=init_lambda_prop-)[0-9_]+", file, perl = TRUE)
  init_lambda_prop <- as.numeric(gsub("_", ".", init_lambda_prop))
  
  load_env <- environment()
  if("workspace.RData" %in% list.files(file.path(file,"full"))){
  load(file.path(file,"full",'workspace.RData'), envir = load_env)
  if(is.null(names(load_env$perceived_ip_trajs))){
    names(load_env$perceived_ip_trajs) <- sapply(load_env$observed_ip, function(x){
      test_path <- x$res_path
      return(gsub("\\.rds","",gsub("/"," ",substr(test_path, regexpr("full", test_path) + nchar("full/"), nchar(test_path)))))
    })  
  }
  # browser()
  } else {
    results = NULL
  }
  return(results)
}


list_leaf_dirs <- function(root_dir) {
  # List all directories
  all_dirs <- list.dirs(root_dir, recursive = TRUE, full.names = TRUE)
  
  # Function to check if a directory is a leaf (has no subdirectories)
  is_leaf_dir <- function(dir) {
    subdirs <- list.dirs(dir, recursive = FALSE, full.names = TRUE)
    length(subdirs) == 1  # Only the directory itself
  }
  
  # Filter to get only leaf directories
  leaf_dirs <- Filter(is_leaf_dir, all_dirs)
  
  return(leaf_dirs)
}
results_list <- lapply(
  X = list_leaf_dirs("/scratch/adeyemi.n/bed-registry-analysis"),
  FUN = function(x){
      res <- process_file(x)
      browser()
      return(res)
    }
)

names(results_list) <- sapply(results_list, function(x) paste("delta", x$delta, "init_lambda_prop", x$init_lambda_prop, sep = "_")) 

