# Load the required libraries
library(doSNOW)
library(matilda)

# Create a cluster with two workers (you can adjust this number as needed)
clus <- makeCluster(7, type = "SOCK")
registerDoSNOW(clus)
clusterEvalQ(clus, library(matilda))

start <- Sys.time()
result_p <- foreach(evidence_name = names(ecs_samples), .combine = "rbind") %dopar% {
  # Extract data from ecs_samples using the evidence_name
  ecs_data <- ecs_samples[[evidence_name]]
  
  # Set param_values$ECS to the entire ecs_data
  param_values$ECS <- ecs_data
  
  # Establish core object
  ini <- system.file("input/hector_ssp245.ini", package = "hector")
  core_245 <- newcore(ini, name = "SSP2-4.5")
  
  # Run Matilda once for the entire ecs_data
  model <- matilda::iterate_model(
    core_245,
    params = param_values,
    save_vars = c(GMST(), CONCENTRATIONS_CO2())
  )
  
  # Add evidence_name to the result
  model$evidence <- evidence_name
  return(model)
}
print(Sys.time() - start)

# Stop the cluster
stopCluster(cluster)
