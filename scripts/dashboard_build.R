
# Script to build the Portland dashboard including processing output

# What was the model run time?
SCENARIO_RUN_DURATION <- Sys.time() - SCENARIO_RUN_START

# Process inputs
loadPackages(SYSTEM_REPORT_PKGS, lib = SYSTEM_PKGS_PATH)
loadInputs(files = c(shp = file.path(SYSTEM_DATA_PATH, "TAZ.rds"),
                     ShipmentSizes = file.path(SYSTEM_DATA_PATH, "ShipmentSizes.rds")))

# Labeling objects
modefaf <- c("Truck", "Truck", "Air", "Rail","Multiple modes", "Multiple modes", "Water", "Multiple modes",
             "Multiple modes", "Multiple modes", "Multiple modes","Multiple modes", "Multiple modes",
             "Multiple modes", "Truck", "Truck", "Truck", "Truck")

c_taz2_ctyname <- data.table(shp@data[,c("TAZ","CountyState")])
setnames(c_taz2_ctyname, "TAZ", "TAZ2")

# Create objects required by dashboard from results tables
# Firm Synthesis Outputs
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FIRMSYN_OUTPUTNAME))
AllFirms <- firm_sim_results$AllFirms
AllFirmsHalo <- AllFirms[TAZ1 %in% BASE_TAZ1_HALO]
AllFirmsReg <- AllFirms[TAZ2 %in% BASE_TAZ2_INTERNAL]
rm(firm_inputs, firm_sim_results)

# Supply Chain Model Outputs
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_SCM_OUTPUTNAME))
AnnualShipments <- sc_sim_results$FreightShipments
rm(sc_inputs, sc_sim_results)
AnnualShipments[, Movement.Type := ODSegment]
AnnualShipments[, Mode := modefaf[ModePath]]
AnnualShipments[, ShipmentSize := ShipmentSizes$ShipmentSize[findInterval(ShipmentWeight, vec = ShipmentSizes$Lower)]]

# Freight Truck Touring Model Outputs
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_FTTM_OUTPUTNAME))

ft_trips <- ft_sim_results$ft_trips
ft_trips[, ExternalStation := ifelse(TAZ.Origin %in% BASE_TAZ2_EXTERNAL, TAZ.Origin, 
                                     ifelse(TAZ.Destination %in% BASE_TAZ2_EXTERNAL, TAZ.Destination, NA))]
ft_trips[c_taz2_ctyname[,.(TAZ.Origin = TAZ2, CountyState)],County.Origin := i.CountyState, on = "TAZ.Origin"]
ft_trips[c_taz2_ctyname[,.(TAZ.Destination = TAZ2, CountyState)],County.Destination := i.CountyState, on = "TAZ.Destination"]
ft_trips[is.na(County.Origin) & Movement.Type == "XI", County.Origin := "External"]
ft_trips[is.na(County.Destination) & Movement.Type == "IX", County.Destination := "External"]
ft_trips[c_taz2_ctyname[,.(TAZ.Anchor = TAZ2, CountyState)],County.Anchor := i.CountyState, on = "TAZ.Anchor"]
ft_trips[is.na(County.Anchor), County.Anchor:= "External"]

DailyShipments <- ft_sim_results$DailyShipments
DailyShipments[c_taz2_ctyname[,.(oTAZ2 = TAZ2, CountyState)],County.Origin := i.CountyState, on = "oTAZ2"]
DailyShipments[c_taz2_ctyname[,.(dTAZ2 = TAZ2, CountyState)],County.Destination := i.CountyState, on = "dTAZ2"]
DailyShipments[is.na(County.Origin), County.Origin := "External"]
DailyShipments[is.na(County.Destination), County.Destination := "External"]
DailyShipments[, Mode := modefaf[ModePath]]
DailyShipments[is.na(Mode) & Mode1 == "Air" & Mode2 == "Air", Mode := "Air"]
DailyShipments[is.na(Mode) & Mode1 == "Water" & Mode2 == "Water", Mode := "Water"]
DailyShipments[is.na(Mode) & Mode1 != Mode2, Mode := "Multiple modes"]
DailyShipments[, ShipmentSize := ShipmentSizes$ShipmentSize[findInterval(ShipmentWeight, vec = ShipmentSizes$Lower)]]

# Commercial Vehicle Touring Model Outputs
load(file.path(SCENARIO_OUTPUT_PATH, SYSTEM_CVTM_OUTPUTNAME))

cv_trips <- cv_sim_results$cv_trips
cv_trips[c_taz2_ctyname[,.(OTAZ = TAZ2, CountyState)],County.Origin := i.CountyState, on = "OTAZ"]
cv_trips[c_taz2_ctyname[,.(DTAZ = TAZ2, CountyState)],County.Destination := i.CountyState, on = "DTAZ"]
cv_trips[cv_trips[TripID == 1, .(TourID, TAZ.Start = OTAZ, County.Start = County.Origin)], 
         c("TAZ.Start", "County.Start") := .(i.TAZ.Start, i.County.Start), on = "TourID"]
skims <- cv_inputs[["skims_tod"]]
Facilities <- ft_inputs[["Facilities"]]

rm(cv_inputs, ft_inputs, cv_sim_results, ft_sim_results)
gc()

# Generate dashboard
rmarkdown::render(file.path(SYSTEM_TEMPLATES_PATH, "ReportDashboard.Rmd"),
                  output_dir = SCENARIO_OUTPUT_PATH,
                  intermediates_dir = SCENARIO_OUTPUT_PATH, quiet = TRUE)
# The following lines change the rendering engine from SVG to Canvas, which
# speeds up map rendering considerably.
ReportDashboard.html <- readLines(file.path(SCENARIO_OUTPUT_PATH, "ReportDashboard.html"))
idx <- which(ReportDashboard.html == "window.FlexDashboardComponents = [];")[1]
ReportDashboard.html <- append(ReportDashboard.html, "L_PREFER_CANVAS = true;", after = idx)
writeLines(ReportDashboard.html, file.path(SCENARIO_OUTPUT_PATH, "ReportDashboard.html"))
