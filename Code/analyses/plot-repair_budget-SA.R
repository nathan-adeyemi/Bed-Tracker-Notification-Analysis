file_results <- list()

 # Define the main function
 lapply(
   X = list.dirs(
       file.path('/scratch', 'adeyemi.n', 'repair-budget-SA'),
       full.names = TRUE,
       recursive = FALSE
   ),
   FUN = function(dir) {
     lapply(
       X = list.files(dir, full.names = TRUE),
       FUN = function(rep_file) {
         load(rep_file)
          # Assuming repair_results and other variables are loaded correctly from the file
         results$budget <- budget
         results$replication <- replication
         file_results <<- c(file_results, list(as.data.table(results)))
         return(NULL)
       }
     )
   }
 )

results_df <- rbindlist(file_results, fill = T, use.names = T)
# results_df <- fread("Results/repair_budget_SA/full/results_df.csv")[,V1 := NULL][!is.na(`Runtime (sec.)`),]
results_df = results_df[!is.na(`L(S')`),][, `Percent Change` := `L(S')`/`Original Sequence Likelihood`][,.(`% of Original Sequence L(S')` = mean(`Percent Change`), `Runtime (sec.)` = mean(`Runtime (sec.)`)), by = list(budget)]

scale_factor <-  results_df[, min(`% of Original Sequence L(S')`) / max(`Runtime (sec.)`)]
ggplot(results_df, aes(x = `budget`)) +
  geom_point(aes(y = `% of Original Sequence L(S')`), color = "black", shape = 3) +  # Primary y-axis
  geom_line(aes(y = `% of Original Sequence L(S')`, color =  "`% of Original Sequence L(S')`")) +  # Primary y-axis
  geom_point(aes(y = (
    max(results_df$`Runtime (sec.)`) - `Runtime (sec.)`
  ) * scale_factor), color = "black", shape = 4) +
  geom_line(aes(
    y = (max(results_df$`Runtime (sec.)`) - `Runtime (sec.)`) * scale_factor,
    color = "Runtime (sec.)"
  )) +  # Secondary y-axis transformed and reversed
  scale_y_reverse(name = "`% of Original Sequence L(S')`",
                    labels = scales::percent_format(),
                     # Label for the primary y-axis
                     # limits = c(0.8,1),
                     sec.axis = sec_axis( ~ max(results_df$`Runtime (sec.)`) - (. / scale_factor), 
                                         name = "Runtime (sec.)")) + # Secondary y-axis with reversed transformation) +
                     theme_linedraw() +
                       theme(legend.position = 'bottom') +
                       scale_color_manual(
                         name = "Metric",
                         values = c("`% of Original Sequence L(S')`" = "blue", "Runtime (sec.)" = "red"),
                         labels = c("`% of L(S)`", "Execution Time")
                       ) 
ggsave(filename = gen_results_path(res_dir = "Results/repair_budget_SA/full/",
                                  filename = "results-plot-test",
                                  ext = '.png'),
       plot = plot,
       height = 6,
       width = 8,
       units = 'in',
       dpi = 500)
