siteInfo  <- readRDS(file = 'Data/function_requirements/ip_facilities_info.rds')


data  <- readRDS('Data/combined_notifications.rds')[,Available_Capacity := as.numeric(Available_Capacity)]
data[is.na(Bed_Group),Bed_Group := Facility_Name]
data  <- split(data, by = 'Bed_Group')

results_df  <-
        mclapply(X=data,
                FUN=calc_params,
                mc.cores = availableCores())

results_df <- rbindlist(results_df[which(!unlist(lapply(results_df,is.null)))])[order(Hospital,Bed_Group)]

write.csv(results_df,file = file.path('Data','Results','queueing_params_Results.csv'))
