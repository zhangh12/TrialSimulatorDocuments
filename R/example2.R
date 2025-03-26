

library(TrialSimulator)

simulate_example2 <- function(seed = NULL){
  #' endpoint in control arm
  failure <- Endpoint$new(name = 'failure', type = 'non-tte', 
                          generator = rbinom, size = 1, prob = .6, 
                          readout = c(failure = 0))
  
  pbo <- Arm$new(name = 'control')
  pbo$add_endpoints(failure)
  
  failure <- Endpoint$new(name = 'failure', type = 'non-tte', 
                          generator = rbinom, size = 1, prob = .4, 
                          readout = c(failure = 0))
  
  trt <- Arm$new(name = 'treatment')
  trt$add_endpoints(failure)
  
  #' set arbitrary recruitment rate
  accrual_rate <- data.frame(end_time = c(1, Inf), 
                             piecewise_rate = c(10, 20))
  
  trial <- Trial$new(name = 'Trial-1234', seed = seed, 
                     n_patients = 266, duration = 1000, 
                     enroller = StaggeredRecruiter, accrual_rate = accrual_rate, 
                     dropout = NULL)
  
  trial$add_arms(sample_ratio = c(1, 1), trt, pbo)
  
  action1 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    
    trial$bind(
      fitFarringtonManning(endpoint = 'failure', placebo = 'control', 
                  data = locked_data, alternative = 'less'), 
      name = 'stats'
    )
    
  }
  
  action2 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    
    trial$bind(
      fitFarringtonManning(endpoint = 'failure', placebo = 'control', 
                  data = locked_data, alternative = 'less'), 
      name = 'stats'
    )
    
    
  }
  
  action3 <- function(trial, event_name){
    locked_data <- trial$get_locked_data(event_name)
    
    trial$bind(
      fitFarringtonManning(endpoint = 'failure', placebo = 'control', 
                  data = locked_data, alternative = 'less'), 
      name = 'stats'
    )
    
    stats <- trial$get_custom_data('stats')
    trial$save(stats$z[1] > 0, name = 'futility_<interim1>')
    trial$save(stats$z[2] > .5, name = 'futility_<interim2>')
    
    browser()
    
    gst <- GroupSequentialTest$new(
      alpha = .025, 
      alpha_spending = 'asOF', 
      planned_max_info = 266
    )
    
    gst$test(
      observed_info = stats$info, 
      is_final = c(FALSE, FALSE, TRUE), 
      p_values = stats$p
    )
    
    ret <- gst$get_trajectory()$decision
    if(ret[1] == 'reject'){
      ret[-1] <- 'reject'
    }
    
    if(ret[2] == 'reject'){
      ret[3] <- 'reject'
    }
    
    trial$save(ret[1], name = 'decision_<interim1>')
    trial$save(ret[2], name = 'decision_<interim2>')
    trial$save(ret[3], name = 'decision_<final>')
    
    NULL
    
  }
  
  interim1 <- Event$new(name = 'interim 1', 
                        trigger_condition = enrollment(n = 133), 
                        action = action1)
  
  interim2 <- Event$new(name = 'interim 2', 
                        trigger_condition = enrollment(n = 200), 
                        action = action2)
  
  final <- Event$new(name = 'final', 
                        trigger_condition = enrollment(n = 266), 
                        action = action3)
  
  listener <- Listener$new()
  listener$add_events(interim1, interim2, final)
  
  controller <- Controller$new(trial, listener)
  controller$run(plot_event = FALSE)
  
  trial$get_output()
  
}






