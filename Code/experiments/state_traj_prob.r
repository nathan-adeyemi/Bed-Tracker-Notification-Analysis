data  <-
    readRDS('Data/combined_notifications.rds')[, Available_Capacity := as.numeric(Available_Capacity)]
data[is.na(Bed_Group), Bed_Group := Facility_Name]
data  <- split(data, by = 'Bed_Group')

test_data  <-
    data[['Mayo Rochester Pediatric/Adolescent']]
test_data  <-
    tag_dups_for_remove(dt = test_data, dup_tolerance = 180)[dupl == FALSE, ]
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
    rbind(test_data, new_rows, fill = TRUE, use.names = TRUE)[order(date_time)][, states := total_beds - Available_Capacity]
obs_idx  <-
    which(!test_data$imputed) # Indices where actual observations occurred

# Define the states and dwell times
states <- test_data$states
times  <- test_data$date_time

# TO DO: Adjust to grab the proper HHCIS estimate for arrival rate and LoS based off patient Ages
lambda  <-
    as.numeric(hccis[hccis_id == test_data[, unique(Facility_Name)]][, 'Pediatric_Admissions'] /
                   (365 * 24))
mu  <-
    as.numeric(hccis[hccis_id == test_data[, unique(Facility_Name)]][, 'Pediatric_Admissions']) /
    (24 * as.numeric(hccis[hccis_id == 'Mayo Clinic Hospital - Rochester'][, 'Pediatric_Days']))
server_count <-
    unique(siteInfo[Bed_Group == test_data[, unique(Bed_Group)], total_beds])

# Create the queueing model to obtain initial state probability
model  <-
    queueing::NewInput.MMCK(lambda = lambda,
                            mu = mu,
                            c = server_count,
                            k = server_count)
model  <- queueing::QueueingModel(model)

sub_traj_probs  <-
    log(c(queueing::Pn(model)[states[1] + 1])) # Initial probability is the probability of the system starting in the initial state
for (i in seq(2, length(obs_idx))) {
    subtraj_states  <- states[obs_idx[i - 1]:obs_idx[i]]
    subtraj_time_vec <- times[obs_idx[i - 1]:obs_idx[i]]
    subtraj_time_vec  <-
        sapply(
            subtraj_time_vec,
            FUN = function(x)
                as.numeric(difftime(x, subtraj_time_vec[1], units = 'hours'))
        )
    subtraj_time_vec  <- subtraj_time_vec[2:length(subtraj_time_vec)]
    sub_traj_probs <-
        c(
            sub_traj_probs,
            calc_subtraj_prob(
                state_vector = subtraj_states,
                times_vector = subtraj_time_vec,
                lambda = lambda,
                mu = mu,
                c = server_count
            )
        )
}
traj_likelihood <- round(sum(sub_traj_probs),digits = 6)

print(paste('The log-likelihood of the imputed state-trajectory is',traj_likelihood))
