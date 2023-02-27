library(dplyr)
library(lubridate)
library(magrittr)
library(writexl)

source("src/ufmn.R")
source("src/cex.R")

als_patients_cex <- cex_patients %>%
    inner_join(ufmn_patients, by = "cip") %>%
    mutate(nhc = coalesce(nhc.x, nhc.y)) %>%
    select(pid, nhc, cip, fecha_visita) %>%
    arrange(fecha_visita)

als_patients_diagnosed <- ufmn_clinical %>%
    left_join(ufmn_patients, by = "pid") %>%
    filter(year(fecha_diagnostico) == 2022) %>%
    select(pid, nhc, cip, fecha_diagnostico) %>%
    arrange(fecha_diagnostico)

als_patients_firstvisits <- ufmn_followups %>%
    left_join(ufmn_patients, by = "pid") %>%
    group_by(pid) %>%
    slice_min(fecha_visita, n = 1) %>%
    ungroup() %>%
    filter(year(fecha_visita) == 2022) %>%
    select(pid, nhc, cip, fecha_visita) %>%
    arrange(fecha_visita)

write_xlsx(als_patients_cex, "cex-firsvisits-2022.xlsx")
write_xlsx(als_patients_diagnosed, "ufmn-diagnosed-2022.xlsx")
write_xlsx(als_patients_firstvisits, "ufmn-firstvisits-2022.xlsx")
