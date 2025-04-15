#' define three arms
pbo <- Arm$new(name = 'placebo')
low <- Arm$new(name = 'low dose')
high <- Arm$new(name = 'high dose')

#' define endpoints in placebo
pfs <- Endpoint$new(name = 'pfs', type = 'tte',
                    generator = rexp, rate = log(2) / 5)

os <- Endpoint$new(name = 'os', type = 'tte',
                   generator = rexp, rate = log(2) / 14)

five_weeks <- 5 / 52 * 12 ## convert it in months
surrogate <- Endpoint$new(name = 'surrogate', type = 'non-tte',
                          readout = c(surrogate = five_weeks),
                          generator = rbinom, size = 1, prob = .05)
pbo$add_endpoints(pfs, os, surrogate)

#' define endpoints in low dose arm
pfs <- Endpoint$new(name = 'pfs', type = 'tte',
                    generator = rexp, rate = log(2) / 6.7)

os <- Endpoint$new(name = 'os', type = 'tte',
                   generator = rexp, rate = log(2) / 17.5)

surrogate <- Endpoint$new(name = 'surrogate', type = 'non-tte',
                          readout = c(surrogate = five_weeks),
                          generator = rbinom, size = 1, prob = .12)
low$add_endpoints(pfs, os, surrogate)

#' define endpoints in high dose arm
pfs <- Endpoint$new(name = 'pfs', type = 'tte',
                    generator = rexp, rate = log(2) / 7.1)

os <- Endpoint$new(name = 'os', type = 'tte',
                   generator = rexp, rate = log(2) / 18.2)

surrogate <- Endpoint$new(name = 'surrogate', type = 'non-tte',
                          readout = c(surrogate = five_weeks),
                          generator = rbinom, size = 1, prob = .13)
high$add_endpoints(pfs, os, surrogate)

accrual_rate <- data.frame(end_time = c(10, Inf),
                           piecewise_rate = c(30, 50))
trial <- Trial$new(
  name = 'Trial-3415', n_patients = 1000,
  seed = 1727811904, duration = 40,
  enroller = StaggeredRecruiter, accrual_rate = accrual_rate,
  dropout = rweibull, shape = 2.139, scale = 38.343
)

trial$add_arms(sample_ratio = c(1, 1, 1), low, high, pbo)


action1 <- function(trial, event_name){
  locked_data <- trial$get_locked_data(event_name)
  fit <- fitFarringtonManning(endpoint = 'surrogate', placebo = 'placebo',
                              data = locked_data, alternative = 'greater')
  # browser() ## if you want to see what does fit look like
  z_l <- fit$z[fit$arm == 'low dose']
  z_h <- fit$z[fit$arm == 'high dose']
  if(z_l > 1.28){
    trial$remove_arms('high dose')
    trial$save(value = 'low', name = 'kept_arm')
  }else if(z_h > 1.28){
    trial$remove_arms('low dose')
    trial$save(value = 'high', name = 'kept_arm')
  }else{
    trial$save(value = 'both', name = 'kept_arm')
  }
  
  invisible(NULL)
  
}


action2 <- function(trial, event_name){
  locked_data <- trial$get_locked_data(event_name)
  
  fit <- fitLogrank(endpoint = 'pfs', placebo = 'placebo',
                    data = locked_data)
  
  ## futility analysis
  if(max(fit$z) < .5){
    trial$save(value = 'negative', name = 'futility')
  }else{
    trial$save(value = 'positive', name = 'futility')
  }
  
  ## extend duration of a trial
  trial$set_duration(duration = 45)
  
  invisible(NULL)
  
}

action3 <- function(trial, event_name){
  locked_data <- trial$get_locked_data(event_name)
  
  ## extend duration of a trial
  ## calculate if duration should be extended or not
  trial$set_duration(duration = 45)
  
  invisible(NULL)
  
}

action4 <- function(trial, event_name){
  locked_data <- trial$get_locked_data(event_name)
  
  ## test PFS
  dt_pfs <- trial$dunnettTest(endpoint = 'pfs', placebo = 'placebo',
                              treatments = c('high dose', 'low dose'),
                              events = c('dose selection', 'duration', 'interim', 'final'),
                              planned_info = 'default')
  ct_pfs <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                             events = c('interim', 'final'),
                             alpha = .005, alpha_spending = 'asOF')
  
  ## test OS
  dt_os <- trial$dunnettTest(endpoint = 'os', placebo = 'placebo',
                             treatments = c('high dose', 'low dose'),
                             events = c('dose selection', 'duration', 'final'),
                             planned_info = 'default')
  ct_os <- trial$closedTest(dt_pfs, treatments = c('high dose', 'low dose'),
                            events = c('final'),
                            alpha = .02, alpha_spending = 'asOF')
  
  ## we only save testing decision here
  ## You can save whatever you want for summarizing things later, e.g. reject time
  trial$save(value = ct_pfs$decision[ct_pfs$arm == 'high dose'], 
             name = 'pfs_high_dose_decision')
  
  trial$save(value = ct_pfs$decision[ct_pfs$arm == 'low dose'], 
             name = 'pfs_low_dose_decision')
  trial$save(value = ct_os$decision[ct_os$arm == 'high dose'], 
             name = 'os_high_dose_decision')
  
  trial$save(value = ct_os$decision[ct_os$arm == 'low dose'], 
             name = 'os_low_dose_decision')
  
  invisible(NULL)
}

dose_selection <- Event$new(name = 'dose selection', action = action1,
                            trigger_condition = eventNumber(endpoint = 'surrogate',
                                                            n = 300))

interim <- Event$new(name = 'interim', action = action2,
                     trigger_condition = eventNumber(endpoint = 'pfs',
                                                     n = 300))

duration <- Event$new(name = 'interim', action = action3,
                     trigger_condition = eventNumber(endpoint = 'pfs',
                                                     n = 400))

final <- Event$new(name = 'final', action = action4,
                   trigger_condition =
                     enrollment(n = 1000, arms = c('placebo', 'low dose', 'high dose')) &
                     eventNumber(endpoint = 'os', n = 300) & (
                       calendarTime(time = 28) |
                         eventNumber(endpoint = 'pfs', n = 520)
                     )
)

listener <- Listener$new()
#' register events with listener
listener$add_events(
  dose_selection,
  interim,
  duration,
  final
)


controller <- Controller$new(trial, listener)
controller$run(plot_event = TRUE)







