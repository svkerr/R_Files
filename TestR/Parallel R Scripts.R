sfInit (parallel=TRUE , cpus=6)
result <- sfLapply(1:10, log)
sfStop ()