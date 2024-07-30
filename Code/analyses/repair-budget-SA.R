if(interactive()) {
  cfg <-
    yaml::yaml.load_file("Code/configs/repair-budget-SA.yaml")[['debug']]
  n_cores <- parallelly::availableCores()
  list2env(cfg, envir = environment())
  replication <- 1
  save_trial_results <- F
  trial_path <- 'Results/repair_budget_SA/debug'
  budget <- 10
}

cat(
  "========================================\n",
  "Repair Budget",budget,"\n",
  "Replication #:", replication, "\n",
  "Results Directory:",trial_path, "\n",
  "--  --  --  --  --  --  --\n"
  )

replication_info <- conjoin_list_elements(jsonlite::fromJSON(paste0("Results/repair_budget_SA/",cfg_name,"/replication_info.json"), simplifyVector = F)[[replication]])
seq_likelihood <- replication_info$seq_likelihood
replication_info$seq_likelihood <- NULL
replication_info$delta <- budget
# Recreate the observation trajectories with each different budget
repair_results <-do.call(repair_prob_dp,replication_info)

results <- list(
   "$\\Delta_{remaining}$" = repair_results$budget,
   'L(S\')' = repair_results$likelihood,
   'Runtime (sec.)' = repair_results$runtime
)

print_list(results)

cat(
    "========================================\n"
)
   
results[["Original Sequence Likelihood"]] <- seq_likelihood

if(save_trial_results){
  save.image(
    gen_results_path(res_dir = trial_path,
      filename=paste('replication',replication,sep = "-"),
      ext = ".RData"
    )
  )
}