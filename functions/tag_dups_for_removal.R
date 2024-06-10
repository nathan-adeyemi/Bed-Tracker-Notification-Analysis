tag_dups_for_remove <- function(dt, dup_tolerance = 30){
   dt <- dt[,dupl:=FALSE]
   dt <- dt[order(date_time, -Available_Capacity)
   ][,`:=`(imputed = FALSE,
           time_diff = as.numeric(difftime(date_time, shift(date_time,type = 'lag',n=1), units='min')))]
   for (row_number in seq(2,dt[,.N])){
      if(dt[row_number, time_diff] <= dup_tolerance){
         dt[row_number,Available_Capacity := max(dt[seq(row_number-1,row_number),Available_Capacity])]
         dt[row_number-1,dupl := TRUE]
      }
   }
   return(dt)
}
