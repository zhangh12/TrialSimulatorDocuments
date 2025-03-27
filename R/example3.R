

library(TrialSimulator)

simulate_example3 <- function(n = 1, seed = NULL){
  pfs <- Endpoint$new(name = 'pfs', type = 'tte', 
                      generator = rexp, rate = -log(1-.2)/12)
  pbo <- Arm$new(name = 'pbo')
  pbo$add_endpoints(pfs)
  
  pfs <- Endpoint$new(name = 'pfs', type = 'tte', 
                      generator = rexp, rate = -log(1-.2)/12 * .85)
  trt <- Arm$new(name = 'trt')
  trt$add_endpoints(pfs)
  
  trial <- Trial$new(name = 'trial', n_patients = 3000, duration = 100, 
                     enroller = StaggeredRecruiter, 
                     accrual_rate = data.frame(end_time = c(10, Inf), piecewise_rate = c(50, 100)), 
                     dropout = rexp, rate = -log(1-.1)/12)
  
  trial$add_arms(sample_ratio = c(1, 1), trt, pbo)
  
  action1 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    trial$bind(fitLogrank(endpoint = 'pfs', placebo = 'pbo', 
                          data = locked_data), 
               name = 'stats')
  }
  
  action2 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    trial$bind(fitLogrank(endpoint = 'pfs', placebo = 'pbo', 
                          data = locked_data), 
               name = 'stats')
  }
  
  action3 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    trial$bind(fitLogrank(endpoint = 'pfs', placebo = 'pbo', 
                          data = locked_data), 
               name = 'stats')
  }
  
  action4 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    trial$bind(fitLogrank(endpoint = 'pfs', placebo = 'pbo', 
                          data = locked_data), 
               name = 'stats')
    
    gst <- GroupSequentialTest$new(alpha = .025, alpha_spending = 'asOF', 
                                   planned_max_info = 1000)
    
    stats <- trial$get_custom_data('stats')
    
    gst$test(observed_info = stats$info, is_final = c(F,F,F,T), p_values = stats$p)
    
    ret <- gst$get_trajectory()$decision
    
    if(ret[1] == 'reject'){
      ret[-1] <- 'reject'
    }
    
    if(ret[2] == 'reject'){
      ret[3:4] <- 'reject'
    }
    
    if(ret[3] == 'reject'){
      ret[4] <- 'reject'
    }
    
    trial$save(ret[1], 'decision1')
    trial$save(ret[2], 'decision2')
    trial$save(ret[3], 'decision3')
    trial$save(ret[4], 'decision4')
  }
  
  interim1 <- Event$new(name = 'interim 1', 
                        trigger_condition = eventNumber(endpoint = 'pfs', n = 1000*.6), 
                        action = action1)
  
  interim2 <- Event$new(name = 'interim 2', 
                        trigger_condition = eventNumber(endpoint = 'pfs', n = 1000*.7), 
                        action = action2)
  
  interim3 <- Event$new(name = 'interim 3', 
                        trigger_condition = eventNumber(endpoint = 'pfs', n = 1000*.8), 
                        action = action3)
  
  final <- Event$new(name = 'final', 
                        trigger_condition = eventNumber(endpoint = 'pfs', n = 1000), 
                        action = action4)
  
  
  listener <- Listener$new()
  listener$add_events(interim1, interim2, interim3, final)
  
  controller <- Controller$new(trial, listener)
  controller$run(n = n, plot_event = FALSE, silent = TRUE)
  
  controller$get_output()
}

