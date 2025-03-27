


library(TrialSimulator)

simulate_example1 <- function(n = 1, seed = NULL){
  #' median PFS: 60 months in control
  pfs <- Endpoint$new(name = 'pfs', type = 'tte', 
                      generator = rexp, rate = log(2) / 60)
  
  #' define placebo arm
  pbo <- Arm$new(name = 'control')
  pbo$add_endpoints(pfs)
  
  #' hazard ratio: 0.74
  pfs <- Endpoint$new(name = 'pfs', type = 'tte', 
                      generator = rexp, rate = log(2) / 60 * .74)
  
  #' define treatment arm
  trt <- Arm$new(name = 'treatment')
  trt$add_endpoints(pfs)
  
  #' recruitment
  accrual_rate <- data.frame(end_time = c(1:6, Inf), 
                             piecewise_rate = seq(6, 42, by = 6))
  
  #' create a trial
  trial <- Trial$new(
    name = 'Trial-1234', n_patients = 1200, duration = 100, 
    enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
    dropout = rexp, rate = -log(1 - .025)/12
  )
  
  trial$add_arms(sample_ratio = c(1, 1), pbo, trt)
  
  interim_action <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    
    trial$bind(fitCoxph(endpoint = 'pfs', 
                        placebo = 'control', 
                        data = locked_data), 
               name = 'stats')
    NULL
  }
  
  interim <- Event$new(name = 'interim analysis', 
                       trigger_condition = eventNumber(endpoint = 'pfs', 
                                                       n = 232), 
                       action = interim_action)
  
  final_action <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    
    trial$bind(fitCoxph(endpoint = 'pfs', 
                        placebo = 'control', 
                        data = locked_data), 
               name = 'stats')
    
    gst <- GroupSequentialTest$new(
      alpha = .025, 
      alpha_spending = 'asOF', 
      planned_max_info = 351
    )
    
    stats <- trial$get_custom_data('stats')
    
    gst$test(
      observed_info = stats$info, 
      is_final = c(FALSE, TRUE), 
      p_values = stats$p
    )
    
    ret <- gst$get_trajectory()$decision
    if(ret[1] == 'reject'){
      ret[2] <- 'reject'
    }
    
    trial$save(ret[1], name = 'decision_<interim>')
    trial$save(ret[2], name = 'decision_<final>')
    NULL
    
  }
  
  final <- Event$new(name = 'final analysis', 
                     trigger_condition = eventNumber(endpoint = 'pfs', 
                                                     n = 351), 
                     action = final_action)
  
  
  listener <- Listener$new()
  listener$add_events(interim, final)
  
  controller <- Controller$new(trial, listener)
  controller$run(n = n, plot_event = FALSE, silent = TRUE)
  
  controller$get_output()
  
}

