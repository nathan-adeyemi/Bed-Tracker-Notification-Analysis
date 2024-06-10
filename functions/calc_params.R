calc_params <- function(notif_subset){
   tryCatch(
      expr={
         inSiteInfo  <- notif_subset[,unique(Bed_Group)] %in% siteInfo$Bed_Group
         if(notif_subset[,.N] > 2 & inSiteInfo){
            notif_subset  <- tag_dups_for_remove(dt = notif_subset, dup_tolerance = 180)[dupl == FALSE,]
            # [,`:=`(dupl = NULL, time_diff = NULL)]

            new_rows <- rbindlist(lapply(X = seq(2,nrow(notif_subset)),FUN = gen_inputed_data,dt = notif_subset),use.names = TRUE, fill = TRUE)

            browser(expr = unique(notif_subset[,Bed_Group]) == 'Mayo Rochester Pediatric/Adolescent')

            notif_subset  <- rbind(notif_subset,new_rows,use.names = TRUE, fill = TRUE
            )[order(date_time),
            ][, `:=`(
               # Calculate instantaneous occuaqpnacy rate
               occupancy = (total_beds-Available_Capacity)/total_beds,

               # Determine the amount of time spent at that occupancy %
               time_diff = abs(as.numeric(difftime(date_time,shift(date_time,type = 'lead',n=1),units = 'hours'))),

               # Identify when arrivals/departures occur
               cap_shift = Available_Capacity - shift(Available_Capacity,type = 'lag',n=1))]


            lambda  <- copy(notif_subset)[cap_shift > 0,
            ][,`:=`(date_no = as.Date(date_time))
            ][,.N,by = date_no
            ][CJ(date_no = seq(min(date_no),max(date_no),by = 1)),on = .(date_no)
            ][is.na(N),N:=0][,.(mean(N))]
            lambda <- as.numeric(lambda)

            rho <- copy(notif_subset)[!is.na(time_diff),weighted.mean(x = occupancy, w = time_diff, na.rm=TRUE)]
            rho <- as.numeric(rho)

            server_count  <- unique(notif_subset$total_beds)
            model  <- queueing::NewInput.MMCK(lambda=lambda,
                                              mu = lambda/(server_count * rho),
                                              c = server_count,
                                              k = server_count)
            model  <- queueing::QueueingModel(model)

            return(cbind(data.table(Hospital = unique(notif_subset$Facility_Name),
                                    Bed_Group = unique(notif_subset$Bed_Group),
                                    avg_interarrival = (lambda^-1)*24,
                                    LOS = (server_count * rho)/lambda * 24),
                         data.table(queueing::summary.o_MM1KK(model)[1]$el)))
         }
      },
      error = function(e){
         print(e)
      })
}