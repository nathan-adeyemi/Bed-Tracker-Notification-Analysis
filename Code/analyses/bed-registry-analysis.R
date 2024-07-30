# Begin Analysis -----------------------------------
observed_ip <- readRDS(file.path("Data","notification-observations.rds"))

if(cfg_name == 'debug'){
  observed_ip <- lapply(X = observed_ip,
                        FUN = function(i){ 
                          for( attr in c('states',"dwell_times",'timestamps')){
                            i[[attr]] <- i[[attr]][seq(num_pts)]
                          }
                          return(i)
                          })
}
observed_ip <- Map(function(list){list$delta <- delta; return(list)},observed_ip)
observed_ip <- lapply(
  X = names(observed_ip),
  FUN = function(name){
    observed_ip[[name]]$timestamps <- observed_ip[[name]]$timestamps[observed_ip[[name]]$timestamps < as.Date('2018-05-01')]
    num_pts <- length(observed_ip[[name]]$timestamps)
    observed_ip[[name]]$res_path <- file.path(trial_path,paste0(name,'.rds'))
    observed_ip[[name]]$states <- observed_ip[[name]]$states[seq(num_pts)]
    observed_ip[[name]]$dwell_times <- observed_ip[[name]]$dwell_times[seq(num_pts)]
    observed_ip[[name]]$theta <- observed_ip[[name]]$theta[seq(num_pts)]
    
    return(observed_ip[[name]])
  }
)
perceived_ip_trajs <-
  mclapply(X = observed_ip,
           FUN =  function(x) {
              tryCatch({
                # Attempt to run the error-prone function
                result <- repair_notif_trajectory(x)
                return(result)
              }, error = function(e) {
                # Handle the error and return a message or default value
                message <- paste("Error:", e$message)
                message(message)  # Print the error message
                
                return(NULL)  # Return NA or any other default value
              })
            },
           mc.cores = availableCores())
print("Perceived Trajectories Generated")
checkpoint_workspace(trial_path = trial_path)

results <- age_util_analysis(perceived_ip_trajs)
checkpoint_workspace(trial_path = trial_path)

# Code for generating the initial data for each facility
# data  <-
#   readRDS('Data/combined_notifications.rds'
#   )[, Available_Capacity := as.numeric(Available_Capacity)]
# 
# ml <- data[grepl('Mille Lacs', Facility_Name),
# ][,Facility_Name := "Mille Lacs Health System"
# ][siteInfo, `:=`(Bed_Group = i.Bed_Group, total_beds = i.total_beds), on = c("Facility_Name" = "Facility_name")]
# 
# others <- data[grepl('Albert Lea|New Ulm|Carris', Facility_Name)
# ][, `:=`(Bed_Group = na.omit(unique(Bed_Group)),
#          total_beds = na.omit(unique(total_beds))), by = Facility_Name]
# 
# data <- rbindlist(list(ml,data[!grepl('Mille Lacs|Albert Lea|New Ulm|Carris', Facility_Name)], others), use.names = T)
# data <- data[is.na(Bed_Group), Bed_Group := Facility_Name]
# data  <- split(data, by = 'Bed_Group')
# 
# observed_ip <-
#   mclapply(X = data,
#            FUN = build_notif_trajectory,
#            mc.cores = availableCores())
# 
# observed_ip <- Filter(function(list) "timestamps" %in% names(list), observed_ip)
# checkpoint_workspace(trial_path = trial_path)
