# Detach all loaded packages and clean the environment
golem::detach_all_attached()
rm(list=ls(all.names = TRUE))

# Document and reload the package
golem::document_and_reload()

# schema_tables <- getInformationSchemaTables()

data_global <- list()

# Seection for "Area of interest" dropdown
data_global$available_study_areas <- getAvailableStudyAreas()

usethis::use_data(data_global, overwrite = TRUE)
