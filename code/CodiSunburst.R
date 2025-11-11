#########################################################
# 1.Codi sunburst                                       #
#########################################################

rm(list=ls())
.res <- paste0(.res0,"CodiSunburst/")
dir.create(.res,F,T)

#########################################################
# Objectiu: current treatment                           #
#########################################################
#Netejar tot l'enviroment i carregar dataset

rm(list=ls())
data <- readRDS(paste0(.dat,"currentTrt.0.rds"))
df <- data$dataset

#seleccionar variables i renombrar per treballar cómodament
subset_data <- df %>%
  select(any_of(c("patient_id", "treat_bio_type", "treat_bio_start"))) %>%
  rename(
    subject_id = patient_id,
    treatment_concept_name = treat_bio_type,
    start_date = treat_bio_start
  )

#donarli a la variable start_date un valor de data
subset_data$start_date <- as.Date(subset_data$start_date)

# FILTRAR PACIENTS
# Pacients amb algun tractament biològic
patients_with_treatment <- subset_data %>%
  filter(!is.na(treatment_concept_name) & treatment_concept_name != "") %>%
  pull(subject_id) %>%
  unique()

# Si el pacient té tractament → només mantenim les files amb tractament
# Si el pacient NO té cap tractament → mantenim una sola fila sense tractament
final_data <- subset_data %>%
  filter(
    # Manté les files amb tractament
    (!is.na(treatment_concept_name) & treatment_concept_name != "") |
      # O bé pacients sense tractament, sempre que no estiguin a la llista dels que sí en tenen
      !(subject_id %in% patients_with_treatment)
  ) %>%
  distinct(subject_id, treatment_concept_name, .keep_all = TRUE)

# Eliminar tots els altres objectes de l'entorn, excepte df i final_data
rm(list = setdiff(ls(), c("df", "final_data")))
saveRDS(final_data,paste0(.res,"currentTrt.processed.rds"))

#########################################################
# 2. Previous treatment                                 #
#########################################################
rm(list=ls())
data <- readRDS(paste0(.dat,"prevTrt.0.rds"))
df <- data$dataset

subset_data <- df %>%
  select(any_of(c("patient_id", "prev_treat_bio_type", "prev_treat_bio_start", "prev_treat_bio_end"))) %>%
  dplyr::rename(
    subject_id = patient_id,
    treatment_concept_name = prev_treat_bio_type,
    start_date = prev_treat_bio_start,
    end_date = prev_treat_bio_end
  )

# Filtrar para eliminar filas con tratamiento NA o vacío
final_data <- subset_data %>%
  filter(!is.na(treatment_concept_name) & treatment_concept_name != "") %>%
  distinct(subject_id, treatment_concept_name, .keep_all = TRUE)

# Limpiar entorno excepto df, subset_data y final_data
rm(list = setdiff(ls(), c("df", "subset_data", "final_data")))
##########################################################
# Dates pacients en dies     Prova trajectories erronies #
##########################################################

# Netejar dates buides o que tenen "NA"
df$prev_treat_bio_start[df$prev_treat_bio_start == "" | df$prev_treat_bio_start == "NA"] <- NA
df$prev_treat_bio_end[df$prev_treat_bio_end == "" | df$prev_treat_bio_end == "NA"] <- NA

# Convertir a Date (format "YYYY-MM-DD" estàndar)
df$prev_treat_bio_start <- as.Date(df$prev_treat_bio_start)
df$prev_treat_bio_end <- as.Date(df$prev_treat_bio_end)

final_data <- final_data %>%
  filter(!is.na(start_date) & !is.na(end_date)) %>%
  mutate(difference_days = as.numeric(end_date - start_date))


#### Modificació, 2025-10-13. Data de tractament verificada i corregida.####
s <- final_data$subject_id=="SE_EPI09" & final_data$treatment_concept_name == "Sarilumab"
final_data[s,"end_date"] <- "2024-02-20"

ss <- final_data$subject_id=="SE_YPI13" & final_data$treatment_concept_name == "Sarilumab"
final_data[ss,"end_date"] <- "2022-08-25"

saveRDS(final_data,paste0(.res,"prevTrt.processed.rds"))

#########################################################

selected_patients <- c("SE_EPI09", "SE_YPI13")

final_data_filtered <- final_data %>%
  filter(subject_id %in% selected_patients)

write.table(final_data_filtered,paste0(.res,"tractaments.csv"),sep=";")

###########################################################
# Juntar prevTrt.processed.rds i currentTrt.processed.rds #
###########################################################

rm(list=ls())
# Carregar els datasets
prevTrt <- readRDS(paste0(.res,"prevTrt.processed.rds"))
currentTrt <- readRDS(paste0(.res,"currentTrt.processed.rds"))

# Definim la funció per convertir labelled a Date
convert_to_date <- function(x) {
  if ("labelled" %in% class(x)) {
    # Convertir primero a character, luego a Date
    x <- as.character(x)
  }
  as.Date(x)
}

# Convertir columnes de dates en prevTrt
prevTrt <- prevTrt %>%
  mutate(
    start_date = convert_to_date(start_date),
    end_date = convert_to_date(end_date)
  )

# Convertir columnes de dates en currentTrt
currentTrt <- currentTrt %>%
  mutate(
    start_date = convert_to_date(start_date)
  )
currentTrt$end_date <- as.Date(NA)

# Unir datasets
all_treatments <- bind_rows(prevTrt, currentTrt)

# Eliminar duplicatss
all_treatments_unique <- all_treatments %>%
  distinct(subject_id, treatment_concept_name, start_date, end_date, .keep_all = TRUE)

# Eliminar tots els altres objectes de l'entorn
rm(list = setdiff(ls(), c("all_treatments_unique")))

saveRDS(all_treatments_unique,paste0(.res,"prev_current_processed.rds"))

#####################################################
# Crear sunburst general                            #
#####################################################

rm(list=ls())
df <- readRDS(paste0(.res,"prev_current_processed.rds"))

# Pas 1: convertir dates a Date (per si de cas)
df <- df %>%
  mutate(
    start_date = as.Date(start_date),
    end_date   = as.Date(end_date)
  )

# Pas 2: ordenar i crear rutes concatenades per pacient
df_paths <- df %>%
  arrange(subject_id, start_date) %>%
  group_by(subject_id) %>%
  summarise(path = paste(treatment_concept_name, collapse = " > "), .groups = "drop")

# Pas 3: contar rutes (nombre de pacients per ruta)
df_counts <- df_paths %>%
  count(path, name = "count")

# Pas 4: adaptar separador per sunburstR (sunburst espera paths separats per '-')
df_counts <- df_counts %>%
  mutate(path = gsub(" > ", "-", path))

# --- Crear sunburst ---
sb <- sunburst(df_counts, count = TRUE, width = 500, height = 500)

# Petita personalització CSS per la llegenda dins el widget (opcional)
sb <- htmlwidgets::onRender(
  sb,
  "
  function(el, x) {
    var style = document.createElement('style');
    style.innerHTML = `
      .sunburst-legend text { font-size: 12px !important; }
      .sunburst-legend rect { width: 150px !important; height: 35px !important; }
    `;
    document.head.appendChild(style);
  }
  "
)

##
df_drugs <- df_counts %>%
  separate_rows(path, sep = "-") %>%
  group_by(path) %>%
  summarise(Comptatge = sum(count), .groups = "drop") %>%
  mutate(Percentatge = round(100 * Comptatge / sum(Comptatge), 2)) %>%
  arrange(desc(Percentatge))
 
colnames(df_drugs) <- c("Fàrmac", "Comptatge", "Percentatge")

# Crear datatable (DT) amb opcions senzilles
dt <- datatable(
  df_drugs,
  options = list(
    paging = FALSE,
    searching = FALSE,
    info = FALSE,
    ordering = TRUE,
    columnDefs = list(list(className = 'dt-center', targets = 1:2)),
    dom = 't'
  ),
  rownames = FALSE,
  caption = htmltools::tags$caption(style = 'caption-side: top; text-align: left; font-weight: bold;', 
                                    'Fàrmacs — percentatge sobre el total de trajectòries')
)

# --- Construir pàgina HTML (igual que el primer que et funcionava) ---
page <- tags$html(
  tags$head(
    tags$title("Sunburst Fàrmacs Biològics"),
    tags$style(HTML("
  html, body {
    margin: 10;
    padding: 10;
    height: 100%;
    overflow: hidden;
    font-family: Arial, sans-serif;
  }

  .container {
    display: flex;
    flex-direction: row;
    align-items: flex-start;
    justify-content: center;
    gap: 30px;
    width: 100%;
    height: 100%;
  }

  .sunburst {
    flex: 2;
    width: 100%;
    height: 100%;
  }

  .tabla {
    flex: 1;
    width: 100%;
    height: 100%;
    overflow: hidden;
  }

  .dataTables_wrapper {
    width: 100%;
    height: 100%;
    overflow: auto;
  }
"))
  ),
  tags$body(
    tags$div(class = "container",
             tags$div(class = "sunburst", sb),
             tags$div(class = "tabla", dt)
    )
  )
)

# --- Guardar HTML (usar save_html que funciona bé amb tags$html + widgets integrats) ---
html_file <- paste0(.res, "sunburst_pathway_farmacologic.html")
htmltools::save_html(page, file = html_file)

# Mostrar ruta sense error
cat("Archivo HTML creado en:", normalizePath(html_file, mustWork = FALSE), "\n")



###################################################
# Agrupar fàrmacs en grups                        #
###################################################

rm(list=ls())
# Carregar dades
df <- readRDS(paste0(.res,"prev_current_processed.rds"))

# Taula de mapeig farmac -> grup
drug_groups <- tribble(
  ~treatment_concept_name, ~Subgrup,
  "Etanercept",            "anti TNF",
  "Adalimumab",            "anti TNF",
  "Infliximab",            "anti TNF",
  "Golimumab",             "anti TNF",
  "Certolizumab",          "anti TNF",
  "Tocilizumab",           "anti IL6",
  "Sarilumab",             "anti IL6",
  "Abatacept",             "anti CTLA4",
  "Rituximab",             "anti CD20",
  "Tofacitinib",           "JAK inhibitors",
  "Baricitinib",           "JAK inhibitors",
  "Upadacitinib",          "JAK inhibitors",
  "Filgotinib",            "JAK inhibitors",
  "NA",                    "NA"
)

# Preparar df amb grups
df <- df %>%
  mutate(treatment_concept_name = str_to_title(treatment_concept_name)) %>%
  left_join(drug_groups, by = "treatment_concept_name")

df_paths_groups <- df %>%
  arrange(subject_id, start_date) %>%
  group_by(subject_id) %>%
  summarise(path = paste(Subgrup, collapse = " > ")) %>%
  ungroup()

df_counts_groups <- df_paths_groups %>%
  count(path, name = "count") %>%
  mutate(path = gsub(" > ", "-", path))

tabla_grupos <- drug_groups %>%
  group_by(Subgrup) %>%
  summarise(Fàrmacs = paste(treatment_concept_name, collapse = ", ")) %>%
  filter(Subgrup != "NA")  # Opcional: quitar NA

# Crear sunburst
sb <- sunburst(df_counts_groups, count = TRUE, width = 500, height = 500)

# Injectar CSS per modificar la legenda dins del sunburst amb onRender
sb <- htmlwidgets::onRender(
  sb,
  "
  function(el, x) {
    var style = document.createElement('style');
    style.innerHTML = `
      .sunburst-legend text { font-size: 12px !important; }
      .sunburst-legend rect { width: 200px !important; height: 35px !important; }
    `;
    document.head.appendChild(style);
  }
  "
)

# Crear taula DT
dt <- datatable(tabla_grupos, options = list(
  paging = FALSE,
  lengthChange = FALSE,
  searching = FALSE,
  info = FALSE,
  ordering = FALSE
))

# Construir pàgina HTML
page <- tags$html(
  tags$head(
    tags$title("Subgrups fàrmacs biològics"),
    tags$style(HTML("
      html, body { margin:20; padding:20; height:100%; overflow:hidden; font-family:Arial,sans-serif; }
      .container { display:flex; gap:30px; width:100%; height:100%; align-items:flex-start; justify-content:center; }
      .sunburst { flex:2; width:100%; height:100%; }
      .tabla { flex:1; width:100%; height:100%; overflow:hidden; }
      .dataTables_wrapper { width:100%; height:100%; overflow:auto; }
    "))
  ),
  tags$body(
    tags$div(class="container",
             tags$div(class="sunburst", sb),
             tags$div(class="tabla", dt)
    )
  )
)

# Guardar HTML
html_file <- paste0(.res, "sunburst_subgrups_farmacs.html")
htmltools::save_html(page, file = html_file)

cat("Archivo HTML creado en:", normalizePath(html_file), "\n")


#########################################################################
#########################################################################
#########################################################################


##################################################
# Tractament de dades no biològics actuals       #
##################################################


rm(list=ls())
data <- readRDS(paste0(.dat,"currentTrt.0.rds"))
df <- data$dataset

#seleccionar variables i renombrar per treballar cómodament
subset_data <- df %>%
  select(any_of(c("patient_id", "treat_type", "treat_corti_start", "treat_famm_type", "treat_famm_start", "treat_bio_type", "treat_bio_start"))) %>%
  
  rename(
    subject_id = patient_id,
    treatment = treat_type,
    s_date_corti = treat_corti_start,
    famm_type = treat_famm_type,
    s_date_famm = treat_famm_start,
    bio_type = treat_bio_type,
    s_date_bio = treat_bio_start
    
  )

#Convertir a data els labels
subset_data$s_date_corti <- as.Date(subset_data$s_date_corti)
subset_data$s_date_famm <- as.Date(subset_data$s_date_famm)
subset_data$s_date_bio <- as.Date(subset_data$s_date_bio)

# Pacients amb algun tractament biològic
patients_with_treatment <- subset_data %>%
  filter(!is.na(treatment) & treatment != "") %>%
  pull(subject_id) %>%
  unique()

# Si el pacient té tractament → només mantenim les files amb tractament
# Si el pacient NO té cap tractament → mantenim una sola fila sense tractament
final_data <- subset_data %>%
  filter(
    # Manté les files amb tractament
    (!is.na(treatment) & treatment != "") |
      # O bé pacients sense tractament, sempre que no estiguin a la llista dels que sí en tenen
      !(subject_id %in% patients_with_treatment)
  ) %>%
  distinct(subject_id, treatment, .keep_all = TRUE)

#crear columna famms i borrar les altres
final_data <- final_data %>%
  mutate(
    famm = coalesce(famm_type, bio_type)
  )
final_data <- final_data %>%
  select(-famm_type, -bio_type)


#crear columna data inici i borrar les altres
final_data <- final_data %>%
  mutate(
    data_inici = coalesce(s_date_corti, s_date_famm, s_date_bio)
  )
final_data <- final_data %>%
  select(-s_date_corti, -s_date_famm, -s_date_bio)

saveRDS(final_data,paste0(.res,"currentTrt.alltreatments_processed.rds"))


###########################################
# Tractament de dades no biològics previs #
###########################################

rm(list=ls())
data <- readRDS(paste0(.dat,"prevTrt.0.rds"))
df <- data$dataset

#seleccionar variables i renombrar per treballar cómodament
subset_data <- df %>%
  select(any_of(c("patient_id", "prev_treat_type", "prev_treat_corti_start", 
                  "prev_treat_corti_end", "prev_treat_famm_type", 
                  "prev_treat_fame_start", "prev_treat_famm_end", 
                  "treat_famm_end_reason","treat_famm_end_reason_other", 
                  "prev_treat_bio_type", "prev_treat_bio_start", 
                  "prev_treat_bio_end", "treat_bio_end_reason", 
                  "treat_bio_end_reason_other"))) %>%
  
  rename(
    subject_id = patient_id,
    treatment = prev_treat_type,
    s_date_corti = prev_treat_corti_start,
    e_date_corti = prev_treat_corti_end,
    famm_type = prev_treat_famm_type,
    s_date_famm = prev_treat_fame_start,
    e_date_famm = prev_treat_famm_end,
    e_famm_reason = treat_famm_end_reason,
    e_famm_reason_o = treat_famm_end_reason_other,
    bio_type = prev_treat_bio_type,
    s_date_bio = prev_treat_bio_start,
    e_date_bio = prev_treat_bio_end,
    e_bio_reason = treat_bio_end_reason,
    e_bio_reason_o = treat_bio_end_reason_other
    
  )

# Primero, reemplazar "NI" por NA (asegurándonos que es carácter)
subset_data <- subset_data %>%
  mutate(across(c(s_date_corti, e_date_corti, s_date_famm, e_date_famm, 
                  s_date_bio, e_date_bio), 
                ~na_if(as.character(.), "NI"))) %>%
  mutate(across(c(s_date_corti, e_date_corti, s_date_famm, e_date_famm, 
                  s_date_bio, e_date_bio), as.Date))


# Pacients amb algun tractament biològic
patients_with_treatment <- subset_data %>%
  filter(!is.na(treatment) & treatment != "") %>%
  pull(subject_id) %>%
  unique()

# Si el pacient té tractament → només mantenim les files amb tractament
# Si el pacient NO té cap tractament → mantenim una sola fila sense tractament
final_data <- subset_data %>%
  filter(
    # Manté les files amb tractament
    (!is.na(treatment) & treatment != "") |
      # O bé pacients sense tractament, sempre que no estiguin a la llista dels que sí en tenen
      !(subject_id %in% patients_with_treatment)
  ) %>%
  distinct(subject_id, treatment, .keep_all = TRUE)

#crear columna famms i borrar les altres

  final_data <- final_data %>%
  mutate(famm = coalesce(famm_type, bio_type),
         data_inici = coalesce(s_date_corti, s_date_famm, s_date_bio),
         data_final = coalesce(e_date_corti, e_date_famm, e_date_bio),
         end_reason = coalesce(e_famm_reason, e_bio_reason),
         end_reason_other = coalesce(e_famm_reason_o, e_bio_reason_o))
  
  final_data <- final_data %>% 
    select(-famm_type, -bio_type,
           -s_date_corti, -s_date_famm, -s_date_bio,
           -e_date_corti, -e_date_famm, -e_date_bio,
           -e_famm_reason, -e_bio_reason,
           -e_famm_reason_o, -e_bio_reason_o)

saveRDS(final_data,paste0(.res,"prevTrt.alltreatments_processed.rds"))
  

#################################

rm(list=ls())
# Carregar els datasets
prevTrt <- readRDS(paste0(.res,"prevTrt.alltreatments_processed.rds"))
currentTrt <- readRDS(paste0(.res,"currentTrt.alltreatments_processed.rds"))

# Definim la funció per convertir labelled a Date
convert_to_date <- function(x) {
  if ("labelled" %in% class(x)) {
    # Convertir primero a character, luego a Date
    x <- as.character(x)
  }
  as.Date(x)
}

# Convertir columnes de dates en prevTrt
prevTrt <- prevTrt %>%
  mutate(
    data_inici = convert_to_date(data_inici),
    data_final = convert_to_date(data_final)
  )

# Convertir columnes de dates en currentTrt
currentTrt <- currentTrt %>%
  mutate(
    data_inici = convert_to_date(data_inici)
  )
currentTrt$data_final <- as.Date(NA)

# Asegurar tipos compatibles
if ("end_reason_other" %in% names(prevTrt)) {
  prevTrt$end_reason_other <- as.character(prevTrt$end_reason_other)
}
if (!"end_reason_other" %in% names(currentTrt)) {
  currentTrt$end_reason_other <- NA_character_
}

# Unir datasets
all_treatments <- bind_rows(prevTrt, currentTrt)

all_treatments <- all_treatments %>%
  mutate(across(where(is.character), ~ na_if(trimws(.x), "")))

all_treatments <- all_treatments %>%
  filter(!(is.na(data_inici) & is.na(data_final)))

# Eliminar duplicatss
all_treatments_processed <- all_treatments %>%
  distinct(subject_id, treatment, famm, data_inici, data_final, end_reason, end_reason_other, .keep_all = TRUE)

# Eliminar tots els altres objectes de l'entorn
rm(list = setdiff(ls(), c("all_treatments_processed")))

saveRDS(all_treatments_processed,paste0(.res,"prev_current_alltreatments_processed.rds"))

###################################
###################################
###################################







rm(list=ls())
# Carregar els datasets
df <- readRDS(paste0(.res, "prev_current_alltreatments_processed.rds"))

df <- df %>%
  mutate(famm = as.character(famm)) %>% 
  mutate(famm = ifelse(is.na(famm), "Corticoides", famm))

# Convertir dates correctament
df <- df %>%
  mutate(
    data_inici = as.Date(data_inici),
    data_final = as.Date(data_final)
  )

# Ordenar per pacient i data d'inici
df_ordered <- df %>%
  arrange(subject_id, data_inici)

# Crear camins (paths) per pacient
df_paths <- df_ordered %>%
  group_by(subject_id) %>%
  summarise(path = paste(famm, collapse = " > ")) %>%
  ungroup()

# Comptar trajectòries úniques
df_counts <- df_paths %>%
  count(path, name = "count")

# Adaptar separador per sunburstR
df_counts <- df_counts %>%
  mutate(path = gsub(" > ", "-", path))

# Crear gràfic sunburst
sb <- sunburst(df_counts, count = TRUE, width = 450, height = 450)

# Injectar CSS per millorar l'estètica
sb <- htmlwidgets::onRender(
  sb,
  "
  function(el, x) {
    var style = document.createElement('style');
    style.innerHTML = `
      .sunburst-legend text { font-size: 12px !important; }
      .sunburst-legend rect { width: 150px !important; height: 35px !important; }
    `;
    document.head.appendChild(style);
  }
  "
)

# Crear pàgina HTML
page <- tags$html(
  tags$head(
    tags$title("SunburstR - Tots els tractaments"),
    tags$style(HTML("
  body { font-family: Arial, sans-serif; margin: 20px; }
  .container { display: flex; justify-content: flex-start; gap: 30px; }
  .sunburst { flex: 1; }
"))
  ),
  tags$body(
    tags$div(class = "container",
             tags$div(class = "sunburst", sb)
    )
  )
)

# Guardar HTML
html_file <- paste0(.res, "sunburst_alltreatments.html")
htmltools::save_html(page, file = html_file)


#############################################
#############################################
#############################################

##############################################################
# Crear combinacions de tractaments per pacient i període    #
# amb tractaments actius i eliminant NA no informatius       #
##############################################################

rm(list = ls())


# ─── 0️⃣ Dataset ────────────────────────────────────────────────
df <- readRDS(paste0(.res, "prev_current_alltreatments_processed.rds"))

df <- df %>%
  mutate(
    data_inici = as.Date(data_inici),
    data_final = as.Date(data_final)
  ) %>%
  filter(!is.na(data_inici)) %>%
  mutate(data_final = ifelse(is.na(data_final), as.Date("2026-12-31"), data_final)) %>%
  filter(data_final >= data_inici)

df <- df %>%
  group_by(subject_id) %>%
  filter(sum(!is.na(data_inici)) > 0) %>%
  ungroup()

df <- df %>%
  mutate(
    categoria = case_when(
      str_detect(tolower(treatment), "corti") ~ "Corticoides",
      famm %in% c(
        "Etanercept", "Adalimumab", "Infliximab", "Golimumab", "Certolizumab",
        "Tocilizumab", "Sarilumab", "Abatacept", "Rituximab",
        "Tofacitinib", "Baricitinib", "Upadacitinib", "Filgotinib"
      ) ~ "FAMM avançat",
      !is.na(famm) & famm != "" ~ "FAMM convencional",
      TRUE ~ "Altres"
    )
  ) %>%
  select(subject_id, categoria, data_inici, data_final)

# ─── 3️⃣ Funció per combinar tractaments ────────────────────────
get_combinations <- function(patient_data) {
  if (nrow(patient_data) == 0) return(NULL)
  
  events <- data.frame(
    date = c(patient_data$data_inici, patient_data$data_final),
    type = rep(c("start", "end"), each = nrow(patient_data)),
    categoria = rep(patient_data$categoria, 2),
    stringsAsFactors = FALSE
  )
  
  events <- events[order(events$date), ]
  active <- character()
  result <- list()
  last_date <- events$date[1]
  
  for (i in seq_len(nrow(events))) {
    current_date <- events$date[i]
    if (length(active) > 0 && current_date > last_date) {
      combo <- paste(sort(unique(active)), collapse = " + ")
      result[[length(result) + 1]] <- data.frame(
        subject_id = unique(patient_data$subject_id)[1],
        start = last_date,
        end = current_date,
        combo = combo,
        stringsAsFactors = FALSE
      )
    }
    if (events$type[i] == "start") active <- c(active, events$categoria[i])
    else active <- setdiff(active, events$categoria[i])
    last_date <- current_date
  }
  
  if (length(result) == 0) {
    return(data.frame(
      subject_id = unique(patient_data$subject_id)[1],
      start = min(patient_data$data_inici, na.rm = TRUE),
      end = max(patient_data$data_final, na.rm = TRUE),
      combo = unique(patient_data$categoria),
      stringsAsFactors = FALSE
    ))
  }
  
  bind_rows(result)
}

# ─── 4️⃣ Aplicar i comptar ─────────────────────────────────────
combinations_df <- df %>%
  group_split(subject_id) %>%
  lapply(get_combinations) %>%
  purrr::compact() %>%
  bind_rows()

df_paths <- combinations_df %>%
  arrange(subject_id, start) %>%
  group_by(subject_id) %>%
  summarise(path = paste(combo, collapse = " > ")) %>%
  ungroup()

df_counts <- df_paths %>%
  count(path, name = "count") %>%
  mutate(path = gsub(" > ", "-", path))

# ─── 5️⃣ Crear gràfic ─────────────────────────────────────────
sb <- sunburst(df_counts, count = TRUE, width = 720, height = 605, 
               legend = list(w = 300))

# ─── Moure la llegenda a la dreta ───────────────────────
sb <- htmlwidgets::onRender(
  sb,
  "
  function(el, x) {
    setTimeout(function() {
      var legend = el.querySelector('.sunburst-legend');
      if (legend) {
        // Mou la llegenda a la dreta del gràfic
        legend.setAttribute('transform', 'translate(750, 120)');
      }

      // Ajusta estil de llegenda
      var style = document.createElement('style');
      style.innerHTML = `
        .sunburst-legend text { font-size: 13px !important; }
        .sunburst-legend rect { width: 300px !important; height: 30px !important; }
      `;
      document.head.appendChild(style);
    }, 500);
  }
  "
)

# ─── 6️⃣ CSS per estil i alineació ────────────────────────────

# Construir pàgina HTML
page <- tags$html(
  tags$head(
    tags$title("Subgrups fàrmacs biològics"),
    tags$style(HTML("
      html, body { margin:20; padding:20; height:100%; overflow:hidden; font-family:Arial,sans-serif; }
      .container { display:flex; gap:30px; width:100%; height:100%; align-items:flex-start; justify-content:center; }
      .sunburst { flex:2; width:100%; height:100%; }
      .tabla { flex:1; width:100%; height:100%; overflow:hidden; }
      .dataTables_wrapper { width:100%; height:100%; overflow:auto; }
    "))
  ),
  tags$body(
    tags$div(class="container",
             tags$div(class="sunburst", sb)
    )
  )
)

# ─── 7️⃣ Guardar HTML ─────────────────────────────────────────

html_file <- paste0(.res, "sunburst_alltreatments_combinat.html")
htmltools::save_html(page, file = html_file)
cat("✅ HTML generat correctament a:", html_file, "\n")








###################################################################







###################################################################







###################################################################



#ShinyApp

rm(list = ls())

# ─────────────────────────────────────────────
# 🖥️ INTERFÍCIE (UI)
# ─────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      body {
        font-family: 'Segoe UI', Arial, sans-serif;
        margin: 10px;
        background-color: #f7f8fa;
      h4 {
        margin-top: 10px;
        color: #2a3d66;
      }
      iframe {
        width: 100%;
        min-height: 80vh;
        border: none;
        transition: opacity 0.3s ease;
      }
      iframe.hidden {
        opacity: 0;
      }
      footer {
        margin-top: 15px;
        text-align: center;
        font-size: 11px;
        color: grey;
      }
    ")),
    # JavaScript per forçar redibuix en canviar mida
    tags$script(HTML("
      window.addEventListener('resize', function() {
        document.querySelectorAll('iframe').forEach(function(iframe) {
          iframe.classList.add('hidden');
          setTimeout(() => iframe.classList.remove('hidden'), 300);
        });
      });
    "))
  ),
  
  titlePanel("Sunburst dels tractaments SERHA 🦴"),
  
  tabsetPanel(
    tabPanel("Pathway farmacològic biològic",
             tags$h4("Trajectòries farmacològiques per fàrmac biològic"),
             htmlOutput("sunburst1")),
    tabPanel("Subgrups de fàrmac biològics",
             tags$h4("Trajectòries per subgrup terapèutic biològic"),
             htmlOutput("sunburst2")),
    tabPanel("Tots els tractaments",
             tags$h4("Tots els tractaments per període"),
             htmlOutput("sunburst3")),
    tabPanel("Combinacions de tractaments",
             tags$h4("Combinacions de tractaments per període"),
             htmlOutput("sunburst4"))
  ),
  
  tags$footer(
    tags$p("App creada per Dídac Segarra — © 2025")
  )
)

# ─────────────────────────────────────────────
# ⚙️ SERVIDOR
# ─────────────────────────────────────────────
server <- function(input, output, session) {
  
  addResourcePath("res", .res)
  
  renderSunburstFrame <- function(file_name) {
    renderUI({
      file_path <- paste0(.res, file_name)
      print(paste("Buscant fitxer:", file_path, "->", file.exists(file_path)))
      if (!file.exists(file_path)) {
        tags$p(paste("⚠️ No s'ha trobat l'arxiu:", basename(file_path)),
               style = "color:red;")
      } else {
        tags$iframe(
          src = paste0("res/", file_name, "?v=", as.numeric(Sys.time())),
          style = "width:100%; height:80vh; border:none; min-height:600px;"
        )
      }
    })
  }
  
  output$sunburst1 <- renderSunburstFrame("sunburst_pathway_farmacologic.html")
  output$sunburst2 <- renderSunburstFrame("sunburst_subgrups_farmacs.html")
  output$sunburst3 <- renderSunburstFrame("sunburst_alltreatments.html")
  output$sunburst4 <- renderSunburstFrame("sunburst_alltreatments_combinat.html")
}

# ─────────────────────────────────────────────
# 🚀 EXECUTAR APP
# ─────────────────────────────────────────────
shinyApp(ui, server)


