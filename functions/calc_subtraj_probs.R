calc_subtraj_prob  <-
   function(state_vector,
            times_vector,
            lambda,
            mu,
            c,
            K = Inf,
            log_odds = TRUE,
            cumm_endpoint_dwell = T) {
      calc_dwell_prob <- function(time, state, cumulative = TRUE) {
         if (!is.na(state)) {
            if (state == K) {
               rate  <- c * mu
            } else if (state == 0) {
               rate  <- lambda
            } else{
               rate  <- lambda + min(c, state) * mu
            }

            if (cumulative) {
               return(pexp(time, rate, lower.tail = FALSE))
            } else{
               return(dexp(time,rate))
            }
         } else {
            return(0)
         }
      }

      if (cumm_endpoint_dwell) {
         model  <-
            if (is.infinite(K)) {
               queueing::NewInput.MMC(lambda = lambda,
                                      mu = mu,
                                      c = c)
            } else {
               queueing::NewInput.MMCK(
                  lambda = lambda,
                  mu = mu,
                  c = c,
                  k = K
               )
            }
         traj_probs <-
            c(queueing::Pn(queueing::QueueingModel(model))[state_vector[1] + 1])
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
               c(traj_probs, dwell_prob * transition_prob)
         }
      }
      res <- prod(traj_probs)
      return(ifelse(log_odds, log(res), res))
   }