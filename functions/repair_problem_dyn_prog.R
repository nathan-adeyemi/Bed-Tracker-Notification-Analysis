repair_prob_dp  <-
    function(data_seq,
             time_seq,
             theta,
             delta,
             lambda,
             mu,
             c = 1,
             K = NA,
             state_cutoff = NA) {
        orig_seq_likelihood <- calc_subtraj_prob(
            state_vector = data_seq,
            times_vector = time_seq,
            lambda = lambda,
            mu = mu,
            c = c,
            K = K,
            cumm_endpoint_dwell = T
        )

        check_infeasible <- function(struc, index) {
            return(all(sapply(
                struc[[index]],
                FUN = function(sub)
                    all(sapply(sub, function(x)
                        all(
                            is.infinite(x)
                        )))
            )))
        }

        calc_lambda_eff <- function(curr_state) {
            if (curr_state == 0) {
                lambda_eff <- lambda
            } else if (curr_state == K) {
                lambda_eff <- mu
            } else {
                lambda_eff <- (min(c, curr_state) * mu) + lambda
            }
            return(lambda_eff)
        }


        calc_p_cap_change_max <- function() {
            probs <- sapply(
                c(lambda, lambda + seq(c) * mu,  lambda + c * mu),
                FUN = function(lam)
                    lam * dexp(lam ^ -1, rate = lam)
            )

            for (current_state in seq(0, K)) {
                if (current_state == 0) {
                    probs <-  c(probs,
                                dexp(
                                    calc_lambda_eff(current_state + 1) ^ -1,
                                    rate = calc_lambda_eff(curr_state = current_state + 1)
                                )) # Maximum dwell time probability given empty system
                } else if (current_state == K) {
                    probs <- c(probs,
                               dexp(
                                   calc_lambda_eff(current_state - 1) ^ -1,
                                   rate = calc_lambda_eff(curr_state = current_state - 1)
                               )) # Maximum dwell time prob given full system capacity
                } else {
                    probs <- c(
                        probs,
                        prod(
                            # Probability of transitioning from n in the system to n+1 in the system
                            lambda / calc_lambda_eff(curr_state = current_state),

                            # Maximum dwell time probability given (n+1) in the system
                            dexp(
                                calc_lambda_eff(current_state + 1) ^ -1,
                                rate = calc_lambda_eff(curr_state = current_state + 1)
                            )
                        ),


                        prod(
                            # Probability of transitioning from n in the system to n-1 in the system
                            (min(K, current_state) * mu) / calc_lambda_eff(curr_state = current_state),

                            # Maximum dwell time probability given (n-1) in the system
                            dexp(
                                calc_lambda_eff(current_state - 1) ^ -1,
                                rate = calc_lambda_eff(curr_state = current_state - 1)
                            )
                        )
                    )
                }

            }

            return(log(max(probs)))
        }

        calc_repair_budget <- function(budget,
                                       original_state,
                                       repair_state) {
            budget <-

                if (is.na(original_state)) {
                    budget - 1 # Fixing an empty data point only decreases budget by 1
                } else if (repair_state != original_state) {
                    # Reset the number of available repairs to search in the correct subtree of the dp_struc
                    budget - abs(original_state - as.numeric(repair_state))
                } else {
                    budget
                }


            return(budget)
        }

        find_prev_repair_index <-
            function(budget, idx, data_pts, current_state) {
                likelihoods_per_prev_repair <-
                    dp_struc[[idx]][[budget + 1]]

                prev_repairs_per_curr_state <-
                    sapply(
                        X = likelihoods_per_prev_repair,
                        FUN = function(vec)
                            vec[as.character(current_state)]
                    )

                prev_repair_state <-
                    gsub(paste0('.', current_state), '', names(which.max(prev_repairs_per_curr_state)))

                # Find the repair budget of the preceeding stage given the current stage repair
                prev_repair_budget <<-
                    calc_repair_budget(
                        budget = budget,
                        original_state = data_pts[idx-1],
                        repair_state = prev_repair_state
                    )

                # A vector of the names of possible previous stage repaired states (from the previous stages' element in dp_struc)
                prev_repair_states <-
                    sapply(
                        X = dp_struc[[idx - 1]][[prev_repair_budget + 1]],
                        FUN = function(vec)
                            vec[as.character(prev_repair_state)]
                    )

                # browser()
                # Return where to grab the current state in the preceeding stage's dp_struc element
                res <- c(which.max(prev_repair_states),
                         which(names(dp_struc[[idx - 1]][[prev_repair_budget +
                                                              1]][[which.max(prev_repair_states)]]) == prev_repair_state))
                return(list(indices = res,
                            budget = prev_repair_budget))
            }


        if (is.na(state_cutoff)) {
            if (is.na(K)) {
                state_cutoff <- c + 30
            }  else {
                state_cutoff <- K
            }
        }

        # Initialize repair problem the dynamic programming structure
        dp_struc  <-
            vector('list', length = length(data_seq)) # One element per data point

        for (i in seq_along(dp_struc)) {
            dp_struc[[i]] <-
                vector('list', length = delta + 1)
            if (i == 1) {
                for (j in seq_along(dp_struc[[i]])) {
                    dp_struc[[1]][[j]] <- list('start' = c(0))
                    names(dp_struc[[1]][[j]][['start']]) = as.character(data_seq[1])
                }
            }
        }

        runtime <- system.time(expr = {
        # Loop through the DP structure
        for (i in 2:length(dp_struc)) {
            # Loop through repair budgets available at data_seq[i]
            for (j in seq(length(dp_struc[[i]]))) {
                c_i <- j - 1

                prev_repairs  <-
                    if (!is.na(data_seq[i - 1])) {
                        seq(max(0, data_seq[i - 1] - theta[i - 1]),
                            min(K, data_seq[i - 1] + theta[i - 1])) # list the possible repairs (considering either available repair capacity and points repair)
                    } else {
                        seq(state_cutoff)
                    }
                dp_struc[[i]][[j]] <-
                    vector('list', length(prev_repairs))
                names(dp_struc[[i]][[j]]) <- names(prev_repairs)

                poss_repairs <-
                    # List the possible repairs at this stage
                    if (!is.na(data_seq[i])) {
                        seq(max(0, data_seq[i] - min(c_i, theta[i])),
                            min(K, data_seq[i] + min(c_i, theta[i]))) # list the possible repairs
                    } else {
                        seq(state_cutoff)
                    }

                for (prev_repair in seq_along(prev_repairs)) {
                    # Loop through possible repairs

                    c_prev  <-
                        if (!is.na(data_seq[i - 1])) {
                            c_i - abs(data_seq[i - 1] - prev_repairs[prev_repair]) # The minimum remaining repair budget at stage i is 0
                        } else {
                            c_prev # Replacing a missing value does not affect the budget
                        }

                    c_prev_ind  <-
                        c_prev + 1 # ensures the budget vector starts at c_i = 0


                    if (c_prev > 0 & c_prev <= delta) {
                        # browser(expr = i == length(data_seq) & c_i == delta)

                        prev_stage_likelihood = max(unlist(lapply(dp_struc[[i - 1]][[c_prev_ind]], function(sublist)
                            sublist[which(names(sublist) == prev_repairs[prev_repair])])))
                        prev_repair_likes <- sapply(
                            X = seq_along(poss_repairs),
                            FUN = function(k) {
                                ret_val <- -Inf
                                if (prev_stage_likelihood > -Inf) {
                                    ret_val <-
                                        # l = the maximum log-likelihood of repaired sub-trajectory up to point (i-1)
                                        prev_stage_likelihood + calc_subtraj_prob(
                                            # state_vector = c(data_seq[i - 1], poss_repairs[repair]),
                                            state_vector = c(prev_repairs[prev_repair],
                                                             poss_repairs[k]),
                                            times_vector = c(time_seq[i - 1]),
                                            lambda = lambda,
                                            mu = mu,
                                            c = c,
                                            K = K,
                                            cumm_endpoint_dwell = FALSE
                                        )
                                }
                                return(ret_val)
                            }
                        )
                    } else {
                        prev_repair_likes <- rep(-Inf, length(poss_repairs))
                    }
                    prev_repair_likes <-
                        sapply(
                            X = seq_along(poss_repairs),
                            FUN = function(repair) {
                                # Find out the set of possible repairs in the next stage (constrained by repair budget and theta_i)
                                # and set l to -Inf if the {repair} is infeasible given the next stage repairs
                                # (e.g. if the repaired state is 2 and the smallest feasible future repair state is 4)

                                # 1. List out the possible repairs for the next stage
                                # - find what c_fut (i.e. the budget of the next stage)
                                # - list all possible repairs to the data_seq[i+1] given c_fut
                                poss_future_repairs  <-
                                    if (!is.na(data_seq[i + 1])) {
                                        seq(max(0, data_seq[i + 1] - min(
                                            delta - c_i, theta[i + 1]
                                        )),
                                        min(K, data_seq[i + 1] + min(
                                            delta - c_i, theta[i + 1]
                                        )))

                                        # seq(max(0, data_seq[i + 1] - theta[i + 1]),min(K, data_seq[i + 1] + theta[i + 1]))

                                    } else {
                                        seq(state_cutoff)
                                    }


                                # To-Do: Fix the likelihood bounds pruning
                                #   - determine how to calculate the upper_bound for
                                p_max <- max(sapply(
                                    X = poss_future_repairs,
                                    FUN = function(fut_repair) {
                                        calc_subtraj_prob(
                                            state_vector = c(poss_repairs[repair], fut_repair),
                                            times_vector = c(time_seq[i]),
                                            lambda = lambda,
                                            mu = mu,
                                            c = c,
                                            K = K,
                                            cumm_endpoint_dwell = FALSE
                                        )
                                    }
                                ))

                                if (prev_repair_likes[repair] + p_max  <= orig_seq_likelihood) {
                                    return(-Inf)
                                } else {
                                    return(prev_repair_likes[repair])
                                }
                            }
                        )
                    names(prev_repair_likes) <-
                        as.character(poss_repairs)
                    dp_struc[[i]][[j]][[prev_repair]] <-
                        prev_repair_likes
                }
                names(dp_struc[[i]][[j]]) <- prev_repairs
            }
            tryCatch(
                expr = check_infeasible(dp_struc, i),
                error = function(e) {
                    if (!check_infeasible(dp_struc, i - 1)) {
                        e <-
                            'Delta is not high enough! Please increase the repair budget and try again.'
                    }
                    print(e)
                }
            )
        }
        })

        # browser()
        # Initiate delta, delta index and the repaired trajectory vector
        curr_delta <- delta
        repaired_traj <- c()

        for (index in seq(length(data_seq), 1)) {
            # Find the name of the repaired state
            repaired_state <-
                if (index == length(data_seq)) {
                    as.numeric(names(which.max(dp_struc[[i]][[delta]][[which.max(dp_struc[[i]][[delta]])]])))
                } else{
                    as.numeric(names(dp_struc[[index]][[curr_delta + 1]][[repair_ind[1]]])[repair_ind[2]])
                }


            # Append the repaired tgrajectory with the repaired state
            repaired_traj <- c(repaired_state, repaired_traj)

            if (index > 1) {
                # Find the index of the previous stage's repair
                repair_ind <-
                    find_prev_repair_index(
                        budget = curr_delta,
                        idx = index ,
                        data_pts = data_seq,
                        current_state = repaired_state
                    )

                # Set the previous stage's repair budget
                curr_delta <-repair_ind$budget
                repair_ind <- repair_ind$indices
            }


        }

        return(list(trajectory = repaired_traj,
                    budget = curr_delta,
                    runtime = runtime[3]))
    }