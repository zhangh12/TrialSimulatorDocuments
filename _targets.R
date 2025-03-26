library(crew)
library(future)
library(targets)
library(tarchetypes)
library(tibble)

n_cores <- future::availableCores()

tar_option_set(
  packages = c('tidyverse', 'ggplot2', 'data.table', 
               'survival', 'rpact', 
               'TrialSimulator'),
  
  # Locally:
  controller = crew::crew_controller_local(workers = n_cores)
)

tar_source()

# n_sims <- n_cores * 10
n_sims <- 10000

list(
  tar_map_rep(
    name = example1,
    command = {
      simulate_example1()
    },
    #values = scenarios,
    batches = n_cores, 
    reps = n_sims / n_cores,
    deployment = 'worker'
  )
  ,
  
  tar_render(
    html1, 
    'docs/example1.Rmd'
  )
  ,
  tar_map_rep(
    name = example2,
    command = {
      simulate_example2()
    },
    #values = scenarios,
    batches = n_cores, 
    reps = n_sims / n_cores,
    deployment = 'worker'
  )
  ,
  
  tar_render(
    html2, 
    'docs/example2.Rmd'
  )
)
