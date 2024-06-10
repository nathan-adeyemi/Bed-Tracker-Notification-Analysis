gen_inputed_data  <-  function(dt,row_number){

   cap_diff  <- dt[row_number, Available_Capacity] - dt[row_number-1, Available_Capacity]

   if(cap_diff <= 0){
      new_caps  <- c(seq( dt[row_number-1,Available_Capacity]-1,0),
                     seq(0,dt[row_number,Available_Capacity] - 1))
      new_caps  <- as.numeric(unlist(str_split(gsub("0,0","0",paste(new_caps,collapse = ',')),',')))
   } else if(cap_diff > 1) {
      new_caps <- seq(dt[row_number-1,Available_Capacity]+1,dt[row_number,Available_Capacity]-1)
   } else {
      new_caps  <- NULL
   }


   if(!is.null(new_caps)){
      new_rows  <- rbindlist(rep(list(dt[row_number,]),length(new_caps)))
      new_rows[,`:=`(Available_Capacity = new_caps)]

      new_times <- abs(as.numeric(difftime(dt[row_number-1,date_time],
                                           dt[row_number,date_time],
                                           units='secs')))/(1 + new_rows[,.N]) * seq(new_rows[,.N]) + dt[row_number-1,date_time]

      new_rows[,`:=`(date_time = new_times, imputed = TRUE)]

      return(new_rows)
   } else {
      return(c())
   }
}