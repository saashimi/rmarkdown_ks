
### Initialize Application -------------------------------------------------------------------

# Load global variables
source(file.path("lib", "scripts", "_SYSTEM_VARIABLES.R"))
source(file.path("lib", "scripts", "_BASE_VARIABLES.R"))
source(file.path("lib", "scripts", "_SCENARIO_VARIABLES.R"))
source(file.path("lib", "scripts", "_USER_VARIABLES.R"))

# Load current rFreight installation
suppressWarnings(suppressMessages(library(rFreight, lib.loc = SYSTEM_PKGS_PATH)))

# Check for new rFreight version, load rFreight and other packages, create output folder
initializeApp(rFreight.path = SYSTEM_RFREIGHT_PATH,
              output.path = SCENARIO_OUTPUT_PATH,
              lib = SYSTEM_PKGS_PATH,
              packages = SYSTEM_PKGS,
              reload.rFreight = FALSE)

# Load any other scripts of general functions for use in the model
source(file.path("lib", "scripts", "omx.R"))

cat("Running the", SCENARIO_NAME, "scenario for", SCENARIO_YEAR)

### 1. Firm Synthesis ------------------------------------------------------------------------

if (SCENARIO_RUN_FIRMSYN) {
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "firm_sim.R"))
  
  # Process inputs
  firm_inputs <- new.env()
  EstablishmentsIndustryTAZ <- firm_sim_process_inputs(envir = firm_inputs)
  
  # Run simuation
  firm_sim_results <- suppressMessages(run_sim(FUN = firm_sim, data = EstablishmentsIndustryTAZ, packages = SYSTEM_PKGS,
                                               lib = SYSTEM_PKGS_PATH, inputEnv = firm_inputs))
  
  # Save inputs and results
  save(firm_sim_results, firm_inputs, file = file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
  rm(firm_sim_results, firm_inputs, EstablishmentsIndustryTAZ)
  gc(verbose = FALSE)
  
} 

### 2. Simulate Supply Chain ------------------------------------------------------------------------

if (SCENARIO_RUN_SCM) {
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "sc_sim.R"))
  
  # Process inputs
  sc_inputs <- new.env()
  FirmInputOutputPairs <- sc_sim_process_inputs(envir = sc_inputs)
  
  # Run simuation
  sc_sim_results <- suppressMessages(run_sim(FUN = sc_sim, data = FirmInputOutputPairs, packages = SYSTEM_PKGS, 
                                             lib = SYSTEM_PKGS_PATH, inputEnv = sc_inputs))
  
  # Save inputs and results
  save(sc_sim_results, sc_inputs, file = file.path(SCENARIO_OUTPUT_PATH, SYSTEM_SCM_OUTPUTNAME))
  rm(sc_sim_results, sc_inputs, FirmInputOutputPairs)
  gc(verbose = FALSE)
  
} 

### 3. Simulate Freight Truck Movements  ------------------------------------------------------------------------

if (SCENARIO_RUN_FTTM) {
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "ft_sim_process_inputs.R"))
  
  # Process inputs
  ft_inputs <- new.env()
  FreightShipments <- ft_sim_process_inputs(envir = ft_inputs)
  
  # Run simulation
  ft_sim_results <- suppressMessages(run_sim(FUN = ft_sim, data = FreightShipments, packages = SYSTEM_PKGS,
                                             lib = SYSTEM_PKGS_PATH, inputEnv = ft_inputs))
  
  # Save inputs and results
  save(ft_sim_results, ft_inputs, file = file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FTTM_OUTPUTNAME))
  rm(ft_sim_results, ft_inputs, FreightShipments)
  gc(verbose = FALSE)
  
}

### 4. Simulate Commercial Vehicle Movements -------------------------------------------------

if (SCENARIO_RUN_CVTM) {
  
  # Load executive functions (process inputs and simulation)
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim_process_inputs.R"))
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "cv_sim.R"))
  
  # Process inputs
  cv_inputs <- new.env()
  RegionalFirms <- cv_sim_process_inputs(envir = cv_inputs)
  
  # Run simuation
  cv_sim_results <- suppressMessages(run_sim(FUN = cv_sim, data = RegionalFirms,
                                             k = USER_PROCESSOR_CORES, packages = SYSTEM_PKGS, lib = SYSTEM_PKGS_PATH,
                                             inputEnv = cv_inputs, exclude = "BMC.shp"))
  cv_sim_results$cv_trips[, TourID := as.integer(factor(paste(BusID, Vehicle, TourID)))]
  
  # Save inputs and results
  save(cv_sim_results, cv_inputs, file = file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))
  rm(cv_sim_results, cv_inputs, RegionalFirms)
  gc(verbose = FALSE)
  
}

### 5. Produce Regional Trip Tables -------------------------------------------------------------------------

# Create trip tables for assignment
if (SCENARIO_RUN_TT) {
  
  source(file.path(SYSTEM_SCRIPTS_PATH, "trip_tables.R"))
  all_trips <- suppressMessages(trip_tables(triptables = TRUE))
  
  # Save inputs and results
  save(all_trips, file = file.path(SCENARIO_OUTPUT_PATH, SYSTEM_TT_OUTPUTNAME))
  rm(all_trips)
  gc(verbose = FALSE)
  
}

### 6. Produce Output Dashboard -------------------------------------------------------------------------

# Generate HTML Dashboard Reports
if (SCENARIO_RUN_DASHBOARD) {
  
  # Load functions
  source(file = file.path(SYSTEM_SCRIPTS_PATH, "dashboard_functions.R"))
  
  # Build the dashboards -- this will produce both the national and regional if SCENARIO_RUN_DASHBOARD_NAT and SCENARIO_RUN_DASHBOARD_REG are true
  suppressWarnings(suppressMessages(source(file = file.path(SYSTEM_SCRIPTS_PATH, "dashboard_build.R"))))
  
}
