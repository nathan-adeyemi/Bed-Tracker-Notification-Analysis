calc_nu <- function(perceived_states,
                    generated_times,
                    generated_states,
                    c,
                    avail_cap_state = NA,
                    nu_bar = TRUE) {
  if (is.na(avail_cap_state) & nu_bar == FALSE) {
    avail_cap_state <- max(c - 1, 1, na.rm = T)
  }
  
  ret <-
    data.table(
      perceived_states = perceived_states,
      true_states = generated_states,
      times = c(generated_times, 0)
    )[, .(perceived_time = sum(.SD[perceived_states == true_states, times]),
          true_time = sum(times)), by = list(states = perceived_states)]
  
  nu <- as.numeric(if (nu_bar) {
    ret[, .(nu = sum(perceived_time) / sum(true_time))]
  } else {
    ret[states < avail_cap_state, .(nu = sum(perceived_time) / sum(true_time))]
  })
  
  return(nu)
}

calc_params <- function(notif_subset) {
  tryCatch(
    expr = {
      inSiteInfo  <-
        notif_subset[, unique(Bed_Group)] %in% siteInfo$Bed_Group
      if (notif_subset[, .N] > 2 & inSiteInfo) {
        notif_subset  <-
          tag_dups_for_remove(dt = notif_subset, dup_tolerance = 180)[dupl == FALSE, ]
        # [,`:=`(dupl = NULL, time_diff = NULL)]
        
        new_rows <-
          rbindlist(
            lapply(
              X = seq(2, nrow(notif_subset)),
              FUN = gen_inputed_data,
              dt = notif_subset
            ),
            use.names = TRUE,
            fill = TRUE
          )
        
        # browser(expr = unique(notif_subset[,Bed_Group]) == 'Mayo Rochester Pediatric/Adolescent')
        
        notif_subset  <-
          rbind(notif_subset,
                new_rows,
                use.names = TRUE,
                fill = TRUE)[order(date_time),][, `:=`(
                  # Calculate instantaneous occuaqpnacy rate
                  occupancy = (total_beds - Available_Capacity) / total_beds,
                  
                  # Determine the amount of time spent at that occupancy %
                  time_diff = abs(as.numeric(
                    difftime(
                      date_time,
                      data.table::shift(date_time, type = 'lead', n = 1),
                      units = 'hours'
                    )
                  )),
                  
                  # Identify when arrivals/departures occur
                  cap_shift = Available_Capacity - data.table::shift(Available_Capacity, type = 'lag', n =
                                                                       1)
                )]
        
        
        lambda  <- copy(notif_subset)[cap_shift > 0,][, `:=`(date_no = as.Date(date_time))][, .N, by = date_no][CJ(date_no = seq(min(date_no), max(date_no), by = 1)), on = .(date_no)][is.na(N), N :=
                                                                                                                                                                                          0][, .(mean(N))]
        lambda <- as.numeric(lambda)
        
        rho <-
          copy(notif_subset)[!is.na(time_diff), weighted.mean(x = occupancy, w = time_diff, na.rm =
                                                                TRUE)]
        rho <- as.numeric(rho)
        
        server_count  <- unique(notif_subset$total_beds)
        model  <- queueing::NewInput.MMCK(
          lambda = lambda,
          mu = lambda / (server_count * rho),
          c = server_count,
          k = server_count
        )
        model  <- queueing::QueueingModel(model)
        
        return(cbind(
          data.table(
            Hospital = unique(notif_subset$Facility_Name),
            Bed_Group = unique(notif_subset$Bed_Group),
            avg_interarrival = (lambda ^ -1) * 24,
            LOS = (server_count * rho) / lambda * 24
          ),
          data.table(queueing::summary.o_MM1KK(model)[1]$el)
        ))
      }
    },
    error = function(e) {
      print(e)
    }
  )
}

calc_subtraj_prob  <- function(state_vector,
                               times_vector,
                               lambda,
                               mu,
                               c,
                               K = NA,
                               log_odds = TRUE,
                               epsilon = 0.01,
                               cumm_endpoint_dwell = T) {
  calc_dwell_prob <-
    function(time,
             state,
             cumulative = TRUE,
             delta_t = epsilon) {
      if (!is.na(state)) {
        if (state == K) {
          rate  <- c * mu
        } else if (state == 0) {
          rate  <- lambda
        } else{
          rate  <- lambda + min(c, state) * mu
        }
        
        if (cumulative) {
          res <- pexp(time, rate, lower.tail = FALSE, log.p = T)
        } else{
          while (TRUE) {
            res <-
              log(pexp(time + delta_t * time, rate) - pexp(time - delta_t * time, rate))
            if (is.infinite(res)) {
              delta_t <- delta_t * 2
            } else{
              break
            }
          }
          
        }
      } else {
        res <- 0
      }
      
      return(res)
    }
  
  if (cumm_endpoint_dwell) {
    
  if(length(K) == 0){
    print(K)
  }
      if (!is.na(K)) {
        model  <-  queueing::NewInput.MMCK(
          lambda = lambda,
          mu = mu,
          c = c,
          k = K
        )
        
      } else {
        model  <-queueing::NewInput.MMC(lambda = lambda,
                               mu = mu,
                               c = c)
      }
    traj_probs <-
      c(log(queueing::Pn(queueing::QueueingModel(model))[state_vector[1] + 1]))
  } else {
    traj_probs <- c()
  }
  
  for (index in seq(length(times_vector))) {
    # Probability the system remains in s_i for t_i
    if (!is.na(times_vector[index])) {
      dwell_prob  <-
        calc_dwell_prob(
          time = times_vector[index],
          state = state_vector[index],
          
          # Take the Pr{x >= t_dwell} when the state is a trajectory endpoint or there are no remaining states in the trajectory
          # ignored when function is called while building a sub-trajectory of a longer state-trajectory (e.g. in the repair problem DP)
          cumulative = ((index == 1 |
                           is.na(
                             state_vector[index + 1]
                           )) & cumm_endpoint_dwell)
        )
      
      if (is.na(state_vector[index])) {
        transition_prob <- 0
      } else if ((is.na(state_vector[index + 1])) |
                 # if the state is the last state of the trajectory (e.g. no remaining transitions) or the state remains unchanged
                 (state_vector[index + 1] == state_vector[index])) {
        transition_prob  <-  1
      } else if (state_vector[index + 1] == state_vector[index] + 1 &
                 state_vector[index + 1] <= K) {
        # if the next state is 1 greater than the current state
        transition_prob  <-
          lambda / (lambda + (min(state_vector[index], K) * mu))
      } else if (state_vector[index + 1] == state_vector[index] - 1) {
        transition_prob  <-
          (min(state_vector[index], K) * mu) / (lambda + min(state_vector[index], K) * mu)
      } else {
        transition_prob <- 0
      }
      
      traj_probs  <-
        c(traj_probs, dwell_prob, log(transition_prob))
    }
    
  }
  return(sum(traj_probs))
}

conjoin_list_elements <- function(lst) {
  unique_names <- unique(names(lst))
  result <- list()
  
  for (name in unique_names) {
    result[[name]] <- unlist(lst[names(lst) == name])
  }
  
  return(result)
}

gen_inputed_data  <-  function(dt, row_number) {
  cap_diff  <-
    dt[row_number, Available_Capacity] - dt[row_number - 1, Available_Capacity]
  
  if (cap_diff <= 0) {
    new_caps  <- c(seq(dt[row_number - 1, Available_Capacity] - 1, 0),
                   seq(0, dt[row_number, Available_Capacity] - 1))
    new_caps  <-
      as.numeric(unlist(strsplit(
        x = gsub("0,0", "0", paste(new_caps, collapse = ',')), split = ','
      )))
  } else if (cap_diff > 1) {
    new_caps <-
      seq(dt[row_number - 1, Available_Capacity] + 1, dt[row_number, Available_Capacity] -
            1)
  } else {
    new_caps  <- NULL
  }
  
  
  if (!is.null(new_caps)) {
    new_rows  <- rbindlist(rep(list(dt[row_number, ]), length(new_caps)))
    new_rows[, `:=`(Available_Capacity = new_caps)]
    
    new_times <-
      abs(as.numeric(difftime(dt[row_number - 1, date_time],
                              dt[row_number, date_time],
                              units = 'secs'))) / (1 + new_rows[, .N]) * seq(new_rows[, .N]) + dt[row_number -
                                                                                                    1, date_time]
    
    new_rows[, `:=`(date_time = new_times, imputed = TRUE)]
    
    return(new_rows)
  } else {
    return(c())
  }
}

gen_results_path <- function(res_dir, filename, ext = '.rds') {
  if (!grepl('\\.', ext)) {
    ext <- paste0("\\.", ext)
  }
  filename <- paste0(filename, ext)
  path <- file.path(res_dir, filename)
  return(path)
}

gen_sample_queue_states <-
  function(num_reps,
           lambda,
           mu,
           c = 1,
           K = 0,
           warmup = 50,
           sim_length = 100) {
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
    test_res <-
      mclapply(X = seq(num_reps),
               FUN = test_env,
               mc.cores = 4)
    sample_state_trajs  <-
      split(x = data.table(simmer::get_mon_resources(test_res))[time > warmup, list(system, time, replication)][, time := time - data.table::shift(time, n = 1, type = 'lag'), by = replication], by = 'replication')
    sample_state_trajs  <- lapply(
      sample_state_trajs,
      FUN = function(dt) {
        return(list(states = dt$system,
                    times = as.numeric(na.omit(dt$time))))
      }
    )
    return(sample_state_trajs)
  }

get_sys_env_var <- function(var_name, var_class = 'numeric') {
  # Function that grabs relecant environment variables from the encompassing shell session.
  # Environment variables are added by the parent python process
  
  res <- Sys.getenv(var_name)
  if (nchar(res) == 0) {
    return(NULL)
  } else if (var_class == 'numeric') {
    return(as.numeric(res))
  } else if (grepl('datatable|dataframe', var_class)) {
    print(res)
    return(data.table(fromJSON(res)))
  } else if (grepl("bool|logic", var_class)) {
    return(as.logical(res))
  } else {
    return(as.character(res))
  }
}

plot_trajectories <- function(generated_states,
                              times,
                              fixed_traj = NA,
                              path = NA_character_,
                              partially_observable = TRUE) {
  orig_seq_lwd <- `if`(!partially_observable, 5, 1.4)
  df <-
    data.table(
      timestamp = seq(length(generated_states)),
      `N(t)` = generated_states,
      `N'(t)` = fixed_traj,
      service_times = times
    )
  
  plot <-
    ggplot() + geom_step(
      data = df,
      mapping = aes(x = service_times, y = `N(t)`),
      linetype = 2
    ) +
    xlab('Time') +
    ylab('Number in the System')
  
  if (!all(is.na(fixed_traj))) {
    plot <- plot + geom_step(
      data = df,
      mapping = aes(x = service_times, y = `N'(t)`),
      color = 'blue',
      linetype = 1
    )
  }
  
  if (!partially_observable) {
    plot <-
      plot + geom_vline(xintercept = times[which(corrupted_states != generated_states)],
                        linetype = 2,
                        alpha = 0.15)
    
  } else {
    plot <-
      plot + geom_point(
        mapping = aes(x = times[obs_idx], y = generated_states[obs_idx]),
        fill = 'red',
        shape = 23,
        size = 2
      )
  }
  
  if (!is.na(path)) {
    ggsave(
      filename = path,
      plot = plot,
      height = 5,
      width = 7.5
    )
  } else {
    plot
  }
}

print_list <- function(results) {
  for (name in names(results)) {
    cat(name, ": ", results[[name]], "\n", sep = "")
  }
}

source("src/repair_problem_dyn_prog.R")

source_with_args <-
  function(file, ...)
    system(paste("Rscript", file, ...))

tag_dups_for_remove <- function(dt, dup_tolerance = 30) {
  dt <- dt[, dupl := FALSE]
  dt <- dt[order(date_time,-Available_Capacity)][, `:=`(imputed = FALSE,
                                                        time_diff = as.numeric(difftime(
                                                          date_time,
                                                          data.table::shift(date_time, type = 'lag', n = 1),
                                                          units = 'min'
                                                        )))]
  for (row_number in seq(2, dt[, .N])) {
    if (dt[row_number, time_diff] <= dup_tolerance) {
      dt[row_number, Available_Capacity := max(dt[seq(row_number - 1, row_number), Available_Capacity])]
      dt[row_number - 1, dupl := TRUE]
    }
  }
  return(dt)
}

transmit_results <-  function(results, receiver) {
  write.socket(receiver, jsonlite::toJSON(results, auto_unbox = TRUE))
}

read_json_con <- function(socket, max_length = 256) {
  json_string <- ""
  
  while (TRUE) {
    new_piece <- read.socket(socket)
    json_string <- paste0(json_string, new_piece)
    if (nchar(new_piece) < max_length) {
      break
    }
  }
  return(json_string)
}

build_notif_trajectory <- function(test_data) {
  test_data  <-
    tag_dups_for_remove(dt = test_data, dup_tolerance = 180)[dupl == FALSE, ]
  if (!is.na(unique(test_data$total_beds)) & test_data[, .N] > 1) {
    new_rows <-
      rbindlist(
        lapply(
          X = seq(2, nrow(test_data)),
          FUN = gen_inputed_data,
          dt = test_data
        ),
        use.names = TRUE,
        fill = TRUE
      )
    test_data  <-
      rbind(test_data,
            new_rows,
            fill = TRUE,
            use.names = TRUE)[order(date_time)][, states := total_beds - Available_Capacity][, dwell_times := difftime(date_time,
                                                                                                                       data.table::shift(date_time, type = 'lag', n = 1),
                                                                                                                       units = 'h')]
    if (!(grepl("all", num_pts))) {
      test_data <- test_data[seq(min(.N, num_pts))]
    }
    
    obs_idx  <-
      which(!test_data$imputed) # Indices where actual observations occurred
    # browser()
    # Define the states and dwell times
    states <- test_data$states
    timestamps <- test_data$date_time
    dwell_times  <- test_data$dwell_times
    
    dwell_times <- na.omit(as.numeric(dwell_times))
    age_beds <-
      `if`(grepl('Adult|Geriatric', unique(test_data$Bed_Group)),
           'Adult_Admissions',
           'Pediatric_Admissions')
    
    # TO DO: Adjust to grab the proper HHCIS estimate for arrival rate and LoS based off patient Ages
    lambda  <-
      as.numeric(hccis[hccis_id == test_data[, unique(Facility_Name)], ..age_beds] /
                   (365 * 24))
    mu  <-
      as.numeric(hccis[hccis_id == test_data[, unique(Facility_Name)], ..age_beds]) /
      (24 * as.numeric(hccis[hccis_id == test_data[, unique(Facility_Name)], ..age_beds]))
    server_count <-
      unique(siteInfo[Bed_Group == test_data[, unique(Bed_Group)], total_beds])
    theta <- rep(theta_i_max, length(states))
    theta[obs_idx] <- 0
    
    return(
      list(
        states = states,
        dwell_times = dwell_times,
        theta = theta,
        delta = delta,
        lambda = lambda,
        mu = mu,
        server_count = server_count,
        timestamps = timestamps
      )
    )
    
  } else{
    return(NULL)
  }
}

repair_notif_trajectory <- function(input) {
  if (length(input) > 0) {
    #  browser()
    list2env(input, environment())
    
    fitted_traj_likelihood <-
      calc_subtraj_prob(
        states,
        times_vector = dwell_times,
        lambda = lambda,
        mu = mu,
        c = server_count,
        K = server_count
      )
    
    repair_results <- repair_prob_dp(
      data_seq = states,
      time_seq = dwell_times,
      theta = theta,
      delta = delta,
      lambda = lambda * init_lambda_prop,
      res_path = res_path,
      mu = mu,
      c = server_count,
      K = server_count,
      heuristic_cutoff = FALSE
    )
    repair_results$timestamps <- timestamps
    repair_results$servers <- server_count
    
    return(repair_results)
  } else {
    return(NULL)
  }
}

checkpoint_workspace <- function(trial_path) {
  if (!interactive()) {
    save.image(gen_results_path(
      res_dir = trial_path,
      filename = 'workspace',
      ext = ".RData"
    ))
  }
}

age_util_analysis <- function(perceived_ip_trajs){
  perceived_ip_trajs <- 
    Filter(is.list, perceived_ip_trajs)
  browser()
  units_df <- lapply(
    # X = names(perceived_ip_trajs),
    X = names(perceived_ip_trajs),
    FUN = function(name) {
      dt <-  data.table(
        states = perceived_ip_trajs[[name]]$trajectory,
        timestamps = perceived_ip_trajs[[name]]$timestamps,
        beds = perceived_ip_trajs[[name]]$servers,
        facility = name
      )
      # browser()
      dcast(dt[!is.na(states)][order(timestamps)],
            timestamps ~ facility ,
            value.var = c("states", "beds"))[, facility := NULL]
      
    }
  )
  units_df <-
    Reduce(function(x, y)
      merge(x, y, by = 'timestamps', all = T),
      Filter(Negate(is.null), units_df))
  setDT(units_df)
  units_df <-
    units_df[, (setdiff(names(units_df), 'timestamps')) := lapply(.SD, na.locf, na.rm = FALSE), .SDcols = setdiff(names(units_df), 'timestamps')]
  units_df <- 
    units_df[which(sapply(units_df[,rowSums(.SD), .SDcols = setdiff(names(units_df),"timestamps")],Negate(is.na))),]
  treatment_ages <-
    c('Adult',
      'Adolescent', 
      # 'Pediatric',
      'Geriatric')
  results <-
    lapply(
      X = treatment_ages,
      FUN = function(age) {
        perceived_states_cols <-
          colnames(units_df)[sapply(names(units_df), function(text)
            all(sapply(c('states', age), function(patt)
              grepl(pattern = patt, text))))]
        beds_cols <-
          colnames(units_df)[sapply(names(units_df), function(text)
            all(sapply(c('beds', age), function(patt)
              grepl(pattern = patt, text))))]
        combined_cols <-
          c('timestamps', perceived_states_cols, beds_cols)
        age_df <-
          units_df[, ..combined_cols][, `:=`(perceived_states = rowSums(.SD, na.rm = T)), .SDcols = perceived_states_cols][, `:=`(total_beds = rowSums(.SD, na.rm = T)), .SDcols = beds_cols]
        return(as.list(age_df[, `:=`(
          dwell_time = as.numeric(difftime(
            timestamps,
            data.table::shift(timestamps, type = 'lag'),
            units = 'h'
          )),
          Occupancy = perceived_states / total_beds * 100,
          available_beds = total_beds - perceived_states
        )][!is.na(perceived_states) & !is.na(dwell_time)][, .(
          Occupancy = weighted.mean(Occupancy, dwell_time, na.rm = T),
          `Avg-Available-Beds` = weighted.mean(available_beds, dwell_time, na.rm = T)
        )]))
      }
    )
  names(results) <- treatment_ages
  results <- unlist(results)
  results$`observed-queues` <- length(perceived_ip_trajs)
  return(results)
}

