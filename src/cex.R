library(readxl)

patients_data_path <- "data/visites-cex-2022.xlsx"
patients_sheet_name <- "Detall_visites"

cex_patients <- read_xlsx(patients_data_path, sheet = patients_sheet_name) %>%
    rename(
        professional = "Agenda desc",
        nhc = "Pacient (NHC)",
        cip = "CIP (14d)",
        prestacion = "Prestació desc",
        dx_code = "Diagnòstic de l'ordre clínica codi",
        dx_desc = "Diagnòstic de l'ordre clínica desc",
        fecha_visita = "Data prestació"
    ) %>%
    mutate(
        nhc = as.integer(nhc),
        fecha_visita = as.Date(fecha_visita)
    ) %>%
    filter(prestacion == "Primera visita") %>%
    select(-prestacion)
