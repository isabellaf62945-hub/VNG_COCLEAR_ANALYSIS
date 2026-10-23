# VNG and audiometry analysis in cochlear implant candidates
# Tables 1-8 and Figures 1-4: one Word report, PNG figures and CSV tables.
# Sheet 1 = clinical data | sheet 2 = VNG (pre/post) | sheet 3 = pre-operative audiometry
# Run from the repository root:  source("R/analysis_tables_figures.R")
# See data/README.md for the expected input format.

packages <- c("readxl", "dplyr", "stringr", "stringi", "tibble", "flextable",
              "officer", "ggplot2", "patchwork", "ragg", "systemfonts")
new_packages <- packages[!packages %in% rownames(installed.packages())]
if (length(new_packages) > 0) install.packages(new_packages)
invisible(lapply(packages, library, character.only = TRUE))

# ---- Paths ----
file_path <- "data/example_synthetic_data.xlsx"   # replace with your own file (keep real data out of Git)
output_folder <- "outputs"
dir.create(output_folder, recursive = TRUE, showWarnings = FALSE)
if (!file.exists(file_path)) stop("Excel file not found: ", file_path, "\nRun the script from the repository root or set file_path to the full path (use / in the path).", call. = FALSE)

# ---- Vestibular classification protocol (SPV in deg/s) ----
pl_cutoff <- 19                  # labyrinthine predominance (unilateral weakness), %
dp_cutoff <- 17                  # directional preponderance, %
hypo_unilateral_cutoff <- 5      # hyporeflexia: SPV sum (cold + warm) of one ear
hypo_bilateral_cutoff <- 12      # hyporeflexia: SPV sum of the four tests
hyper_unilateral_cutoff <- 62    # hyperreflexia: one ear
hyper_bilateral_cutoff <- 122    # hyperreflexia: four tests
weakness_criterion <- "classification"  # "classification": PL or hyporeflexia | "threshold": PL only

# ---- Samples ----
table4_sample <- "table5"        # "table5": same patients as Table 5 | "all_vng": every VNG
complete_case_only <- TRUE       # Table 5: exclude patients with missing audiometry
required_measures <- NULL        # NULL = all measures, or e.g. c("PTA (dB)")

# ---- Audiometric screening (Figures 1-2, Table 6) ----
figure_hz <- 250                 # 250, 500, 1000, 2000, 4000 or 8000
cutoff_db <- 45
cutoff_db_2 <- 60
lpta_cutoff_db <- 50
screening_common_sample <- TRUE  # Table 6: same patients in every column
figure_use_table_sample <- FALSE # Figures 1-2: only the patients of Table 5
figure_col_manual <- NULL        # exact column of sheet 3 if automatic detection fails

# ---- Surgical factors (Table 7, Figure 4) ----
surgical_factors <- c("electrode", "approach", "brand")
surgical_figure_factor <- "electrode"

# ---- Report ----
combined_docx_name <- "Report_VNG_audiology.docx"
combined_order <- c("table1", "table2", "table3", "table4", "table5", "figure1",
                    "table6", "figure2", "figure3", "table7", "figure4", "table8")
page_break_between_parts <- TRUE
raw_table_id <- "sequential"     # Table 8 ID: "sequential" or "code"
if (!exists("write_combined_report")) write_combined_report <- TRUE

# ---- Style ----
table_font  <- "NewsGotT"
cor_verde   <- "#AC8ABB"
cor_azul    <- "#B2BB8A"
cor_roxo    <- "#AC8ABB"
cor_amarelo <- "#F2CB5C"

strong_border  <- officer::fp_border(color = "black", width = 1)
thin_border    <- officer::fp_border(color = "black", width = 0.5)
title_fp       <- officer::fp_text(bold = TRUE, font.size = 10, font.family = table_font)
description_fp <- officer::fp_text(font.size = 10, font.family = table_font)
note_fp        <- officer::fp_text(font.size = 8, font.family = table_font)
fig_title_fp   <- officer::fp_text(bold = TRUE, font.size = 9, font.family = table_font)
fig_text_fp    <- officer::fp_text(font.size = 9, font.family = table_font)
fig_note_fp    <- officer::fp_text(font.size = 8, font.family = table_font)

color_name <- function(hex) {
  nm <- c("#B2BB8A" = "green", "#8A99BB" = "blue", "#AC8ABB" = "purple", "#F2CB5C" = "yellow")[toupper(hex)]
  ifelse(is.na(nm), "colored", unname(nm))
}
fonts_available <- unique(systemfonts::system_fonts()$family)
fig_font <- if (table_font %in% fonts_available) table_font else {
  warning("Font '", table_font, "' not found; figures use the default sans font.")
  "sans"
}

definition_text <- if (weakness_criterion == "threshold") {
  paste0("Vestibular weakness was defined as labyrinthine predominance (unilateral ",
         "caloric weakness) above ", pl_cutoff, "% on preoperative VNG testing.")
} else {
  paste0("Vestibular weakness was defined as labyrinthine predominance above ",
         pl_cutoff, "%, unilateral hyporeflexia (SPV sum of one ear below ",
         hypo_unilateral_cutoff, " \u00B0/s) or bilateral hyporeflexia (SPV sum of ",
         "the four tests below ", hypo_bilateral_cutoff,
         " \u00B0/s) on preoperative VNG testing.")
}

report_parts <- list()
add_part <- function(doc, part) {
  if (part$type == "table") {
    doc %>%
      officer::body_add_fpar(part$title) %>%
      flextable::body_add_flextable(part$ft) %>%
      officer::body_add_fpar(part$note)
  } else {
    doc %>%
      officer::body_add_img(src = part$png, width = part$w, height = part$h) %>%
      officer::body_add_fpar(part$caption) %>%
      officer::body_add_fpar(part$note)
  }
}

# ---- Helpers ----
norm <- function(x) {
  stringi::stri_trans_general(tolower(stringr::str_squish(as.character(x))), "Latin-ASCII")
}
norm_col <- function(x) stringr::str_replace_all(norm(x), "[^a-z0-9]", "")

find_col <- function(df, pattern, required = TRUE) {
  idx <- which(stringr::str_detect(norm(names(df)), pattern))
  if (!length(idx)) {
    if (required) {
      cat("Available columns:\n"); print(names(df))
      stop("No column matched pattern: ", pattern)
    }
    return(NA_character_)
  }
  names(df)[idx[1]]
}

find_repeated_cols <- function(df, pattern, required_n = 2) {
  idx <- which(stringr::str_detect(norm_col(names(df)), pattern))
  if (length(idx) < required_n) {
    cat("Columns:\n"); print(names(df))
    stop("Expected at least ", required_n, " columns matching: ", pattern,
         "; found ", length(idx), ".")
  }
  names(df)[idx]
}

col_or_na <- function(df, col) if (is.na(col)) rep(NA, nrow(df)) else df[[col]]

to_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- stringr::str_replace_all(stringr::str_squish(as.character(x)), "%", "")
  suppressWarnings(as.numeric(stringr::str_replace(x, ",", ".")))
}
to_logical <- function(x) {
  norm(x) %in% c("1", "sim", "s", "x", "yes", "y", "true", "positivo", "presente")
}
to_date <- function(x) {
  if (inherits(x, c("Date", "POSIXt"))) return(as.Date(x))
  if (is.numeric(x)) return(as.Date(x, origin = "1899-12-30"))
  tryCatch(as.Date(as.character(x)), error = function(e) as.Date(rep(NA, length(x))))
}
is_true <- function(x) x %in% TRUE

translate <- function(x, dic) {
  original <- stringr::str_squish(as.character(x))
  new <- unname(dic[norm(original)])
  ifelse(is.na(new), original, new)
}
dic_sex <- c(feminino = "Female", f = "Female", masculino = "Male", m = "Male",
             female = "Female", male = "Male")

dic_approach <- c("janela redonda" = "Round window", "cocleostomia" = "Cochleostomy",
                  "round window" = "Round window", "cochleostomy" = "Cochleostomy")

dic_group <- c("Infecciosa" = "Infectious", "Degenerativa" = "Degenerative",
               "Traumática" = "Traumatic", "Tóxica/Iatrogénica" = "Iatrogenic",
               "Genética/Metabólica" = "Genetic", "Idiopática" = "Idiopathic",
               "Outros" = "Other")
group_order <- unname(dic_group)

dic_etiologia <- c(
  "lab pneumococos" = "Pneumococcal labyrinthitis",
  "lab borrelia" = "Borrelia labyrinthitis",
  "labirintite" = "Labyrinthitis",
  "meningite" = "Meningitis",
  "omc" = "Chronic otitis media",
  "otosclerose" = "Otosclerosis",
  "meniere" = "Meniere disease",
  "deiscencia csc sup" = "Superior semicircular canal dehiscence",
  "trauma" = "Trauma",
  "ototoxicidade" = "Ototoxicity",
  "iatrogenica (pos petrosectomia)" = "Iatrogenic (after petrosectomy)",
  "defice da biotinidase" = "Biotinidase deficiency",
  "sindrome evans" = "Evans syndrome",
  "surdez ns progressiva" = "Progressive sensorineural hearing loss",
  "surdez ns subita" = "Sudden sensorineural hearing loss",
  "surdez subita bilateral" = "Bilateral sudden sensorineural hearing loss"
)
translate_etiology <- function(x) {
  original <- stringr::str_squish(as.character(x))
  new <- unname(dic_etiologia[norm(original)])
  ifelse(is.na(original) | original == "", NA_character_,
         ifelse(is.na(new), "Other / unmapped etiology", new))
}

classify_electrode <- function(x) {
  n <- norm(x)
  dplyr::case_when(
    is.na(n) | n == "" ~ NA_character_,
    stringr::str_detect(n, "perimod") ~ "Perimodiolar",
    stringr::str_detect(n, "\\bre(c)?to\\b|straight|lateral") ~ "Lateral wall",
    TRUE ~ "Other / unmapped")
}

classify_side <- function(x, keep = FALSE) {
  original <- stringr::str_squish(as.character(x))
  s <- norm(original)
  dplyr::case_when(
    is.na(s) | s == "" ~ NA_character_,
    stringr::str_detect(s, "^(d|dt|dto|dta|dir|dire|direito|direita|od|r|right)\\b") ~ "Right",
    stringr::str_detect(s, "^(e|esq|esqd|esquerdo|esquerda|oe|l|left)\\b") ~ "Left",
    TRUE ~ if (keep) original else NA_character_)
}

levels_with_extras <- function(x, base) c(base, setdiff(unique(na.omit(x)), base))

fmt_np <- function(n, N) sprintf("%d (%.1f%%)", as.integer(n), 100 * n / N)
fmt_p <- function(p) ifelse(is.na(p), "–", ifelse(p < 0.001, "<0.001", sprintf("%.3f", p)))
fmt_mean_sd <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("–")
  sd_text <- if (length(x) > 1) sprintf("%.1f", stats::sd(x)) else "–"
  sprintf("%.1f (%s)", mean(x), sd_text)
}
fmt_median_iqr <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("–")
  sprintf("%.1f (%.1f)", stats::median(x), stats::IQR(x))
}
fmt_median_q <- function(x) {
  x <- x[!is.na(x)]
  if (!length(x)) return("–")
  sprintf("%.1f [%.1f–%.1f]", stats::median(x), stats::quantile(x, 0.25), stats::quantile(x, 0.75))
}

# ---- Data ----
s1    <- readxl::read_excel(file_path, sheet = 1, .name_repair = "unique_quiet")
vng   <- readxl::read_excel(file_path, sheet = 2, .name_repair = "unique_quiet")
audio <- readxl::read_excel(file_path, sheet = 3, .name_repair = "unique_quiet")
names(s1)    <- stringr::str_squish(names(s1))
names(vng)   <- stringr::str_squish(names(vng))
names(audio) <- stringr::str_squish(names(audio))

s1_key       <- find_col(s1, "^codificacao")
s1_age       <- find_col(s1, "^idade da implantacao", FALSE)
s1_electrode <- find_col(s1, "^tipo de eletrodo")
s1_etiol     <- find_col(s1, "^etiologia", required = FALSE)
s1_dm        <- find_col(s1, "^diabetes$")
s1_has       <- find_col(s1, "^hipertensao$")
s1_obes      <- find_col(s1, "^obesidade$")
s1_tobacco   <- find_col(s1, "^tabaco$")
s1_alcohol   <- find_col(s1, "^alcool$")
s1_dislip    <- find_col(s1, "^dislipidemia$")

vng_id_col    <- find_col(vng, "^codificacao")
vng_sex_col   <- find_col(vng, "^sexo$")
vng_ear_col   <- find_col(vng, "^ouvido$")
vng_birth_col <- find_col(vng, "^data de nascimento", FALSE)
vng_imp_col   <- find_col(vng, "^data de imp", FALSE)
vng_flag_col  <- find_col(vng, "^exames vng pos", FALSE)
audio_id_col  <- find_col(audio, "^codificacao")

# ---- Audiometry columns ----
audio_patterns <- c(
  "Hearing at 250 Hz (dB)" = "^ht250",
  "Hearing at 500 Hz (dB)" = "^ht500",
  "Hearing at 1000 Hz (dB)" = "^ht1k",
  "Hearing at 2000 Hz (dB)" = "^ht2k",
  "Hearing at 4000 Hz (dB)" = "^ht4k",
  "LPTA (dB)" = "^lpta",
  "PTA (dB)" = "^pta",
  "Maximum discrimination (%)" = "^discriminacaomaxima",
  "STR (%)" = "^str"
)

audio_pre_cols <- vapply(audio_patterns, function(pattern) {
  cols <- find_repeated_cols(audio, pattern, required_n = 1)
  if (length(cols) > 1) {
    message("Several columns match '", pattern, "': ",
            paste(cols, collapse = " | "), " -> using the first.")
  }
  cols[1]
}, character(1))

cat("\nAudiology columns selected as preoperative:\n")
print(data.frame(Measure = names(audio_pre_cols),
                 ExcelColumn = unname(audio_pre_cols), row.names = NULL))

# ---- Vestibular classification (service protocol) ----
# Per VNG: ear response = |SPV cold| + |SPV warm|; total = sum of the four tests.
# PL = |OD - OE| / (OD + OE) x 100; DP = |(OD warm + OE cold) - (OD cold + OE warm)| / total x 100.
# Type: Bilateral (bilateral hyporeflexia) | Ipsilateral / Contralateral / Unclassified
# (PL > cutoff or unilateral hyporeflexia, relative to the implanted ear) | None.
implanted_ear <- classify_side(vng[[vng_ear_col]])

classify_vng <- function(suffix, report = FALSE) {
  n <- nrow(vng); nas <- rep(NA_real_, n)
  cols <- vapply(c("od_fria", "oe_fria", "od_quente", "oe_quente"),
                 function(x) find_col(vng, paste0("^spv_", x, "_", suffix), required = FALSE),
                 character(1))
  have_cols <- !any(is.na(cols))
  tot_col  <- find_col(vng, paste0("^spv_total_", suffix), required = FALSE)
  pl_col   <- find_col(vng, paste0("^fraqueza_unilateral_\\(%\\)_", suffix), required = FALSE)
  dp_col   <- find_col(vng, paste0("^preponderancia_direcional_\\(%\\)_", suffix), required = FALSE)
  side_col <- find_col(vng, paste0("^fraqueza_unilateral_lado_", suffix), required = FALSE)

  if (have_cols) {
    o_f <- abs(to_num(vng[[cols[["od_fria"]]]]));   e_f <- abs(to_num(vng[[cols[["oe_fria"]]]]))
    o_q <- abs(to_num(vng[[cols[["od_quente"]]]])); e_q <- abs(to_num(vng[[cols[["oe_quente"]]]]))
    r_od <- o_f + o_q; r_oe <- e_f + e_q
  } else {
    o_f <- e_f <- o_q <- e_q <- r_od <- r_oe <- nas
    if (report) {
      warning("SPV per ear (SPV_OD/OE fria/quente) not found: absolute criteria per ear ",
              "cannot be applied; PL and DP come from the sheet columns.")
    }
  }
  total <- dplyr::coalesce(r_od + r_oe,
                           if (is.na(tot_col)) nas else abs(to_num(vng[[tot_col]])))
  pl_calc <- ifelse(!is.na(r_od) & !is.na(r_oe) & (r_od + r_oe) > 0,
                    abs(r_od - r_oe) / (r_od + r_oe) * 100, NA_real_)
  dp_calc <- ifelse(have_cols & !is.na(total) & total > 0,
                    abs((o_q + e_f) - (o_f + e_q)) / total * 100, NA_real_)
  pl_sheet <- if (is.na(pl_col)) nas else abs(to_num(vng[[pl_col]]))
  dp_sheet <- if (is.na(dp_col)) nas else abs(to_num(vng[[dp_col]]))
  pl <- dplyr::coalesce(pl_calc, pl_sheet)
  dp <- dplyr::coalesce(dp_calc, dp_sheet)

  if (report) {
    cat("\nPL from SPV vs. sheet column: rows compared =", sum(!is.na(pl_calc - pl_sheet)),
        "| differing by > 1 point =", sum(abs(pl_calc - pl_sheet) > 1, na.rm = TRUE), "\n")
    cat("DP from SPV vs. sheet column: rows compared =", sum(!is.na(dp_calc - dp_sheet)),
        "| differing by > 1 point =", sum(abs(dp_calc - dp_sheet) > 1, na.rm = TRUE), "\n")
  }

  hypo_od  <- r_od < hypo_unilateral_cutoff
  hypo_oe  <- r_oe < hypo_unilateral_cutoff
  hypo_bil <- total < hypo_bilateral_cutoff
  hyper    <- is_true(r_od > hyper_unilateral_cutoff) |
    is_true(r_oe > hyper_unilateral_cutoff) | is_true(total > hyper_bilateral_cutoff)

  w_ear_spv <- dplyr::case_when(
    is_true(hypo_od) & !is_true(hypo_oe) ~ "Right",
    is_true(hypo_oe) & !is_true(hypo_od) ~ "Left",
    !is.na(r_od) & !is.na(r_oe) & r_od < r_oe ~ "Right",
    !is.na(r_od) & !is.na(r_oe) & r_oe < r_od ~ "Left",
    TRUE ~ NA_character_)
  w_ear_side <- if (is.na(side_col)) rep(NA_character_, n) else classify_side(vng[[side_col]])
  w_ear <- dplyr::coalesce(w_ear_spv, w_ear_side)

  unilateral <- is_true(pl > pl_cutoff) | is_true(hypo_od) | is_true(hypo_oe)
  valid <- !is.na(pl) | !is.na(total)
  type <- dplyr::case_when(
    !valid ~ NA_character_,
    is_true(hypo_bil) ~ "Bilateral",
    unilateral & !is.na(w_ear) & !is.na(implanted_ear) & w_ear == implanted_ear ~ "Ipsilateral",
    unilateral & !is.na(w_ear) & !is.na(implanted_ear) & w_ear != implanted_ear ~ "Contralateral",
    unilateral ~ "Unclassified",
    TRUE ~ "None")

  tibble::tibble(pl = pl, dp = dp, total = total, type = type, hyper = hyper,
                 dp_abn = is_true(dp > dp_cutoff), r_od = r_od, r_oe = r_oe)
}

is_weak <- function(type, pl) {
  if (weakness_criterion == "threshold") ifelse(is.na(pl), NA, pl > pl_cutoff)
  else ifelse(is.na(type), NA, type != "None")
}

cls_pre  <- classify_vng("pre", report = TRUE)
cls_post <- classify_vng("pos")
if (all(is.na(implanted_ear))) {
  warning("Implanted ear not available: ipsilateral/contralateral cannot be determined.")
}
cat("\nPre-implant VNG type (protocol), all VNG rows:\n")
print(table(cls_pre$type, useNA = "ifany"))
cat("Hyperreflexia:", sum(cls_pre$hyper & !is.na(cls_pre$type)),
    "| Directional preponderance >", dp_cutoff, "%:",
    sum(cls_pre$dp_abn & !is.na(cls_pre$type)), "\n")

weak_pre <- is_weak(cls_pre$type, cls_pre$pl)
vng_group <- tibble::tibble(
  key = stringr::str_squish(as.character(vng[[vng_id_col]])),
  group = factor(ifelse(weak_pre, "Weakness", "Normal"), levels = c("Weakness", "Normal"))
) %>%
  dplyr::filter(!is.na(key), key != "")
duplicate_vng <- vng_group %>% dplyr::count(key) %>% dplyr::filter(n > 1)
if (nrow(duplicate_vng) > 0) {
  cat("\nDuplicate patient codes in sheet 2 (first row kept):\n")
  print(duplicate_vng)
  vng_group <- vng_group %>% dplyr::distinct(key, .keep_all = TRUE)
}

vng_class_tbl <- tibble::tibble(
  key = stringr::str_squish(as.character(vng[[vng_id_col]])),
  implanted_ear = implanted_ear,
  weakness_type = cls_pre$type,
  hyper_any = cls_pre$hyper,
  dp_abn = cls_pre$dp_abn,
  weakness = ifelse(is.na(cls_pre$type), NA, cls_pre$type != "None")
)

# ---- Tables 1-3: VNG examinations (one row per VNG in sheet 2) ----
s1_clin <- tibble(
  key       = str_squish(as.character(s1[[s1_key]])),
  in_s1     = TRUE,
  age_s1    = to_num(col_or_na(s1, s1_age)),
  electrode = classify_electrode(s1[[s1_electrode]]),
  etiology_original = str_squish(as.character(col_or_na(s1, s1_etiol))),
  dm      = to_logical(s1[[s1_dm]]),
  has     = to_logical(s1[[s1_has]]),
  dislip  = to_logical(s1[[s1_dislip]]),
  obes    = to_logical(s1[[s1_obes]]),
  tobacco = to_logical(s1[[s1_tobacco]]),
  alcohol = to_logical(s1[[s1_alcohol]])
)
dup_s1 <- s1_clin %>% count(key) %>% filter(n > 1)
if (nrow(dup_s1) > 0) {
  cat("\nDuplicate codes in sheet 1 (first row kept):\n")
  print(dup_s1)
}
s1_clin <- distinct(s1_clin, key, .keep_all = TRUE)

exams <- tibble(
  key    = str_squish(as.character(vng[[vng_id_col]])),
  sex    = translate(vng[[vng_sex_col]], dic_sex),
  ear    = classify_side(vng[[vng_ear_col]], keep = TRUE),
  age_calc = as.numeric(difftime(to_date(col_or_na(vng, vng_imp_col)),
                                 to_date(col_or_na(vng, vng_birth_col)),
                                 units = "days")) / 365.25,
  post_ok = if (is.na(vng_flag_col)) rep(TRUE, nrow(vng)) else
    to_num(vng[[vng_flag_col]]) %in% 1,
  pl_pre = cls_pre$pl,   dp_pre = cls_pre$dp,   spv_pre = cls_pre$total,
  pl_post = cls_post$pl, dp_post = cls_post$dp, spv_post = cls_post$total,
  weak_pre  = weak_pre,
  weak_post = is_weak(cls_post$type, cls_post$pl)
) %>%
  left_join(s1_clin, by = "key") %>%
  mutate(
    age = coalesce(age_s1, age_calc),
    across(c(dm, has, dislip, obes, tobacco, alcohol), ~ ifelse(is.na(in_s1), NA, .x)),
    etiology_group = case_when(
      is.na(etiology_original) | etiology_original == "" ~ NA_character_,
      etiology_original %in% c("Lab Pneumococos", "Lab Borrelia", "Labirintite",
                               "Meningite", "OMC") ~ "Infecciosa",
      etiology_original %in% c("Otosclerose", "Ménière", "Deiscência CSC Sup") ~ "Degenerativa",
      etiology_original %in% c("Trauma") ~ "Traumática",
      etiology_original %in% c("Ototoxicidade", "Iatrogenica (pós petrosectomia)") ~ "Tóxica/Iatrogénica",
      etiology_original %in% c("Défice da Biotinidase", "Sindrome Evans") ~ "Genética/Metabólica",
      etiology_original %in% c("Surdez NS progressiva", "Surdez NS súbita",
                               "Surdez Súbita bilateral") ~ "Idiopática",
      TRUE ~ "Outros"),
    group_en     = unname(dic_group[etiology_group]),
    etiology_en  = translate_etiology(etiology_original),
    group = factor(case_when(weak_pre ~ "Weakness", !weak_pre ~ "Normal"),
                   levels = c("Weakness", "Normal"))
  )

exm <- filter(exams, !is.na(group))
n_exam <- nrow(exm)
if (n_exam == 0) stop("No VNG with a preoperative weakness classification.")
n_exam_w <- sum(exm$group == "Weakness")
n_exam_n <- sum(exm$group == "Normal")

cat("\nVNG rows in sheet 2:", nrow(vng), "| without a match in sheet 1:", sum(is.na(exams$in_s1)), "\n")
cat("Weakness criterion:", weakness_criterion, "\nGroups (VNG pre):\n"); print(table(exm$group))
cat("\nImplanted ear by group:\n"); print(table(exm$ear, exm$group, useNA = "ifany"))
unknown_ear <- setdiff(unique(na.omit(exm$ear)), c("Left", "Right"))
if (length(unknown_ear) > 0) {
  cat("Unrecognised ear values (excluded from Table 1):", paste(unknown_ear, collapse = " | "), "\n")
}
cat("\nElectrode type by group:\n"); print(table(exm$electrode, exm$group, useNA = "ifany"))
unmapped_et <- exm %>% filter(etiology_group == "Outros") %>% count(etiology_original)
if (nrow(unmapped_et) > 0) {
  cat("\nEtiologies classified as 'Other' (check spelling):\n"); print(unmapped_et)
}
cat("VNG without etiology:", sum(is.na(exm$group_en)), "\n")
cat("VNG with a postoperative examination:", sum(exams$post_ok & !is.na(exams$pl_post)), "\n")

# Table 1
cat_block <- function(x, title, lv, test = TRUE) {
  tab <- table(factor(x, levels = lv), exm$group)
  p <- if (test) tryCatch(fisher.test(tab)$p.value, error = function(e) NA_real_) else NA_real_
  k <- length(lv)
  bind_rows(
    tibble(Characteristic = title, Weakness = "", Normal = "", Total = "", p = "", header = TRUE),
    tibble(Characteristic = lv,
           Weakness = fmt_np(tab[, "Weakness"], n_exam_w),
           Normal   = fmt_np(tab[, "Normal"], n_exam_n),
           Total    = fmt_np(rowSums(tab), n_exam),
           p = c(rep("", k - 1), if (test) fmt_p(p) else ""),
           header = FALSE))
}
num_block <- function(x, title) {
  xw <- x[exm$group == "Weakness"]; xn <- x[exm$group == "Normal"]
  p_t <- tryCatch(t.test(xw, xn)$p.value, error = function(e) NA_real_)
  p_w <- tryCatch(suppressWarnings(wilcox.test(xw, xn, exact = FALSE)$p.value),
                  error = function(e) NA_real_)
  bind_rows(
    tibble(Characteristic = title, Weakness = "", Normal = "", Total = "", p = "", header = TRUE),
    tibble(Characteristic = c("Mean (SD)", "Median (IQR)"),
           Weakness = c(fmt_mean_sd(xw), fmt_median_iqr(xw)),
           Normal   = c(fmt_mean_sd(xn), fmt_median_iqr(xn)),
           Total    = c(fmt_mean_sd(x), fmt_median_iqr(x)),
           p = c(fmt_p(p_t), fmt_p(p_w)),
           header = FALSE))
}
yes_no <- function(v) ifelse(v, "Yes", "No")

tab1 <- bind_rows(
  tibble(Characteristic = "Preoperative VNG findings, N (% of total VNG)",
         Weakness = fmt_np(n_exam_w, n_exam), Normal = fmt_np(n_exam_n, n_exam),
         Total = fmt_np(n_exam, n_exam), p = "", header = TRUE),
  cat_block(exm$sex, "Patient gender, N (%)", c("Female", "Male")),
  num_block(exm$age, "Age at implantation (years)"),
  cat_block(exm$ear, "Implanted ear, N (%)", c("Left", "Right"), test = FALSE),
  cat_block(exm$electrode, "Electrode type, N (%)",
            levels_with_extras(exm$electrode, c("Lateral wall", "Perimodiolar"))),
  cat_block(yes_no(exm$has),     "History of hypertension, N (%)", c("No", "Yes")),
  cat_block(yes_no(exm$dislip),  "History of dyslipidemia, N (%)", c("No", "Yes")),
  cat_block(yes_no(exm$dm),      "Diabetes, N (%)",                c("No", "Yes")),
  cat_block(yes_no(exm$obes),    "History of obesity, N (%)",      c("No", "Yes")),
  cat_block(yes_no(exm$tobacco), "Tobacco use, N (%)",             c("No", "Yes")),
  cat_block(yes_no(exm$alcohol), "Alcohol use, N (%)",             c("No", "Yes"))
)

ft1 <- flextable::flextable(tab1 %>% select(-header)) %>%
  flextable::set_header_labels(Characteristic = "", Weakness = "Weakness", Normal = "Normal",
                               Total = "Total", p = "p-value") %>%
  flextable::add_header_row(values = c("", "Caloric testing", ""), colwidths = c(1, 3, 1)) %>%
  flextable::border_remove() %>%
  flextable::hline_top(border = strong_border, part = "header") %>%
  flextable::hline(i = 1, j = 2:4, border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = strong_border, part = "body") %>%
  flextable::font(fontname = table_font, part = "all") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:5, align = "center", part = "all") %>%
  flextable::padding(i = which(!tab1$header), j = 1, padding.left = 14, part = "body") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::set_table_properties(layout = "fixed") %>%
  flextable::width(j = 1:5, width = c(2.5, 0.95, 0.95, 0.95, 0.75))

report_parts$table1 <- list(
  type = "table",
  title = officer::fpar(
    officer::ftext("Table 1. ", title_fp),
    officer::ftext(paste0("Demographic and clinical characteristics of cochlear implant ",
                          "candidates with and without vestibular weakness. ", definition_text),
                   description_fp)),
  ft = ft1,
  note = officer::fpar(officer::ftext(paste0(
    "Percentages are column percentages. N refers to VNG examinations. p-values: ",
    "Fisher's exact test (categorical variables), Welch's t-test (mean) and Mann-Whitney U ",
    "test (median). Implanted ear is presented descriptively, without statistical testing. ",
    "IQR - interquartile range (Q3-Q1); SD - standard deviation; SPV - slow phase velocity; ",
    "VNG - videonystagmography."), note_fp)))
write.csv2(tab1 %>% select(-header), file.path(output_folder, "Table1_weakness_vs_normal.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")

# Table 2: pre vs post VNG
pairs_df <- filter(exams, post_ok)
paired_row <- function(pre, post, label) {
  ok <- !is.na(pre) & !is.na(post)
  p <- tryCatch(suppressWarnings(
    wilcox.test(pre[ok], post[ok], paired = TRUE, exact = FALSE)$p.value),
    error = function(e) NA_real_)
  tibble(Variable = label, N = as.character(sum(ok)),
         Pre = fmt_median_q(pre[ok]), Post = fmt_median_q(post[ok]), p = fmt_p(p))
}
tab2 <- bind_rows(
  paired_row(pairs_df$pl_pre,  pairs_df$pl_post,  "Unilateral weakness (PL, %), median [Q1–Q3]"),
  paired_row(pairs_df$dp_pre,  pairs_df$dp_post,  "Directional preponderance (%), median [Q1–Q3]"),
  paired_row(pairs_df$spv_pre, pairs_df$spv_post, "Total SPV (°/s), median [Q1–Q3]")
) %>% filter(N != "0")

ok_f <- !is.na(pairs_df$weak_pre) & !is.na(pairs_df$weak_post)
if (sum(ok_f) > 0) {
  pre_f <- pairs_df$weak_pre[ok_f]; post_f <- pairs_df$weak_post[ok_f]
  n_new <- sum(!pre_f & post_f); n_lost <- sum(pre_f & !post_f)
  p_mc <- if (n_new + n_lost > 0) binom.test(n_new, n_new + n_lost, 0.5)$p.value else 1
  tab2 <- bind_rows(tab2, tibble(
    Variable = "Caloric weakness, N (%)", N = as.character(sum(ok_f)),
    Pre = fmt_np(sum(pre_f), sum(ok_f)), Post = fmt_np(sum(post_f), sum(ok_f)),
    p = fmt_p(p_mc)))
}
if (nrow(tab2) > 0) {
  ft2 <- flextable::flextable(tab2) %>%
    flextable::set_header_labels(Variable = "", N = "N pairs", Pre = "Pre-implant",
                                 Post = "Post-implant", p = "p-value") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = strong_border, part = "header") %>%
    flextable::hline_bottom(border = thin_border, part = "header") %>%
    flextable::hline_bottom(border = strong_border, part = "body") %>%
    flextable::font(fontname = table_font, part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::align(j = 2:5, align = "center", part = "all") %>%
    flextable::valign(valign = "center", part = "all") %>%
    flextable::set_table_properties(layout = "fixed") %>%
    flextable::width(j = 1:5, width = c(2.5, 0.7, 1.2, 1.2, 0.75))
  report_parts$table2 <- list(
    type = "table",
    title = officer::fpar(
      officer::ftext("Table 2. ", title_fp),
      officer::ftext("Preoperative versus postoperative vestibular testing in the same patients.",
                     description_fp)),
    ft = ft2,
    note = officer::fpar(officer::ftext(paste0(
      "Paired analysis of patients with both examinations. PL, directional preponderance ",
      "and total SPV were calculated from the SPV of the four caloric tests. p-values: ",
      "Wilcoxon signed-rank test (continuous variables) and exact McNemar test (caloric ",
      "weakness, as defined in Table 1)."), note_fp)))
  write.csv2(tab2 %>% rename(`Pre-implant` = Pre, `Post-implant` = Post),
             file.path(output_folder, "Table2_pre_vs_post.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
}

# Table 3: etiology
n_etiology <- sum(!is.na(exm$group_en))
groups_present <- group_order[group_order %in% exm$group_en]
tab3 <- bind_rows(lapply(groups_present, function(g) {
  sub <- exm %>% filter(group_en == g) %>% count(etiology_en, name = "n") %>%
    arrange(desc(n), etiology_en)
  tibble(Group = g, Etiology = c("Total", sub$etiology_en),
         Value = c(fmt_np(sum(sub$n), n_etiology), fmt_np(sub$n, n_etiology)))
}))
if (nrow(tab3) > 0) {
  ft3 <- flextable::flextable(tab3) %>%
    flextable::set_header_labels(Group = "Etiology", Etiology = "Etiology",
                                 Value = paste0("n = ", n_etiology)) %>%
    flextable::merge_h(part = "header") %>%
    flextable::merge_v(j = 1, part = "body") %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = strong_border, part = "header") %>%
    flextable::hline_bottom(border = thin_border, part = "header") %>%
    flextable::hline_bottom(border = strong_border, part = "body") %>%
    flextable::font(fontname = table_font, part = "all") %>%
    flextable::fontsize(size = 9, part = "all") %>%
    flextable::bold(j = 1, part = "body") %>%
    flextable::bold(i = which(tab3$Etiology == "Total"), j = 2:3, part = "body") %>%
    flextable::align(j = 1:2, align = "left", part = "all") %>%
    flextable::align(j = 3, align = "center", part = "all") %>%
    flextable::valign(valign = "center", part = "all") %>%
    flextable::set_table_properties(layout = "fixed") %>%
    flextable::width(j = 1:3, width = c(1.7, 2.8, 1.1))
  group_end <- which(tab3$Group != dplyr::lead(tab3$Group))
  if (length(group_end) > 0) {
    ft3 <- flextable::hline(ft3, i = group_end, border = thin_border, part = "body")
  }
  report_parts$table3 <- list(
    type = "table",
    title = officer::fpar(officer::ftext("Table 3. ", title_fp),
                          officer::ftext("Etiology of hearing pathology.", description_fp)),
    ft = ft3,
    note = officer::fpar(officer::ftext(paste0(
      "Values are N (%) of the VNG examinations with available etiology (n = ", n_etiology,
      "). Percentages refer to this total; each group total precedes its individual ",
      "etiologies."), note_fp)))
  write.csv2(tab3, file.path(output_folder, "Table3_etiology.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
}

# ---- Audiometry sample and missing data ----
audio_data <- tibble::tibble(
  key = stringr::str_squish(as.character(audio[[audio_id_col]])),
  !!!stats::setNames(lapply(audio_pre_cols, function(col) to_num(audio[[col]])),
                     names(audio_pre_cols))
) %>%
  dplyr::filter(!is.na(key), key != "")

duplicate_audio <- audio_data %>% dplyr::count(key) %>% dplyr::filter(n > 1)
if (nrow(duplicate_audio) > 0) {
  cat("\nWarning: duplicate patient codes in sheet 3; keeping the first row:\n")
  print(duplicate_audio)
  audio_data <- audio_data %>% dplyr::distinct(key, .keep_all = TRUE)
}

analysis_data <- audio_data %>%
  dplyr::left_join(vng_group %>% dplyr::select(key, group), by = "key") %>%
  dplyr::filter(!is.na(group))

if (nrow(analysis_data) == 0) {
  stop("No sheet 3 patients matched a valid preoperative VNG group in sheet 2.")
}

measure_cols <- names(audio_pre_cols)
n_sheet3 <- nrow(audio_data)
n_with_group <- nrow(analysis_data)

missing_summary <- data.frame(
  Measure = measure_cols,
  Missing = vapply(measure_cols,
                   function(m) sum(is.na(analysis_data[[m]])), numeric(1)),
  row.names = NULL
)
cat("\nMissing values per measure (patients with a valid VNG group):\n")
print(missing_summary)
cat("\nGroup counts BEFORE excluding missing data:\n")
print(table(analysis_data$group))

excluded_data <- NULL
if (complete_case_only) {
  if (is.null(required_measures)) required_measures <- measure_cols
  required_measures <- intersect(required_measures, measure_cols)

  keep <- stats::complete.cases(as.data.frame(analysis_data[, required_measures]))
  if (any(!keep)) {
    missing_matrix <- is.na(as.data.frame(analysis_data[!keep, required_measures]))
    excluded_data <- tibble::tibble(
      key = analysis_data$key[!keep],
      group = as.character(analysis_data$group[!keep]),
      missing_measures = apply(missing_matrix, 1, function(r) {
        paste(required_measures[r], collapse = "; ")
      })
    )
  }
  analysis_data <- analysis_data[keep, ]
}

if (nrow(analysis_data) == 0) {
  stop("No patients left after excluding rows with missing data. ",
       "Consider setting required_measures to fewer measures.")
}

n_final <- nrow(analysis_data)
n_weakness <- sum(analysis_data$group == "Weakness")
n_normal <- sum(analysis_data$group == "Normal")

cat("\n================ SAMPLE FLOW ================\n")
cat("Rows in sheet 3 (valid code):                      ", n_sheet3, "\n")
cat("With a valid preoperative VNG group (sheet 2):     ", n_with_group, "\n")
cat("Excluded for missing audiologic data:              ",
    n_with_group - n_final, "\n")
cat("FINAL N ANALYSED:                                  ", n_final,
    "  (Weakness n =", n_weakness, "| Normal n =", n_normal, ")\n")
cat("=============================================\n")

if (!is.null(excluded_data)) {
  cat("\nExcluded patients (code, group, missing measures):\n")
  print(excluded_data, n = Inf)
}
if (n_weakness < 2 || n_normal < 2) {
  warning("One of the groups has fewer than 2 patients; tests will not be estimable.")
}

# ---- Table 4: vestibular function ----
cat("\n\n############ TABLE 4: VESTIBULAR FUNCTION ############\n")

if (!table4_sample %in% c("table5", "all_vng")) {
  stop("table4_sample must be 'table5' or 'all_vng'.")
}

vest <- vng_class_tbl %>%
  dplyr::filter(!is.na(key), key != "")

n_vng_rows <- nrow(vest)
if (table4_sample == "table5") {

  vest <- vest %>%
    dplyr::distinct(key, .keep_all = TRUE) %>%
    dplyr::filter(key %in% analysis_data$key)
  n_vng_rows <- nrow(vest)
}
vest <- vest %>% dplyr::filter(!is.na(weakness_type))
n_vng_excluded <- n_vng_rows - nrow(vest)
unit_label <- if (table4_sample == "table5") "patients" else "VNG"
if (nrow(vest) == 0) stop("No VNG with enough data to classify vestibular function.")

vest <- vest %>% dplyr::mutate(weakness = weakness_type != "None")

n_total  <- nrow(vest)
n_weak   <- sum(vest$weakness)
n_normal_v <- n_total - n_weak
n_bil <- sum(vest$weakness_type == "Bilateral")
n_con <- sum(vest$weakness_type == "Contralateral")
n_ips <- sum(vest$weakness_type == "Ipsilateral")
n_unc <- sum(vest$weakness_type == "Unclassified")

frac <- function(n, d) {
  if (d == 0) return("0/0")
  sprintf("%d/%d=%.1f%%", n, d, 100 * n / d)
}

cat("\n=========== TABLE 4: SAMPLE ===========\n")
cat("Sample mode:", table4_sample,
    if (table4_sample == "table5") "(same patients as Table 5)" else "(all VNGs)", "\n")
cat("Rows considered:                                  ", n_vng_rows, "\n")
cat("Excluded (insufficient VNG data to classify):     ", n_vng_excluded, "\n")
cat("ANALYSED (", unit_label, "):                      ", n_total, "\n", sep = "")
cat("  Normal:", n_normal_v, "\n")
cat("  Weakness:", n_weak, "(bilateral", n_bil, "| contralateral", n_con,
    "| ipsilateral", n_ips, "| unclassified", n_unc, ")\n")
cat("========================================\n")

if (table4_sample == "table5") {
  same_groups <- (n_weak == n_weakness) && (n_normal_v == n_normal)
  cat(sprintf(paste0("\nConsistency with Table 5: Table 4 = Weakness %d / Normal %d | ",
                     "Table 5 = Weakness %d / Normal %d -> %s\n"),
              n_weak, n_normal_v, n_weakness, n_normal,
              if (same_groups) "OK (coincide)" else "DIFFERENT"))
  if (!same_groups) {
    warning("Table 4 and Table 5 groups differ. Use weakness_criterion <- 'classification' ",
            "(the Table 4 classification) so both tables share the same definition.")
  }
} else {
  cat("\nNote: table4_sample = 'all_vng' -> the Table 4 counts include every VNG ",
      "and will not match the Table 5 groups.\n", sep = "")
}
if (is.na(vng_ear_col) || all(is.na(vest$implanted_ear))) {
  warning("Implanted ear ('Ouvido') not available in sheet 2: ",
          "ipsilateral/contralateral cannot be determined.")
}

check <- vest %>%
  dplyr::distinct(key, .keep_all = TRUE) %>%
  dplyr::left_join(vng_group %>% dplyr::select(key, group), by = "key") %>%
  dplyr::filter(!is.na(group))
if (nrow(check) > 0) {
  cat("\nCheck: weakness by computed classification (rows) vs. group used in ",
      "Tables 5-6 (weakness_criterion = '", weakness_criterion, "'):\n", sep = "")
  print(table(Classification_weakness = check$weakness, Group = check$group))
}

sub_lines <- c(
  paste0("\u00B0 Bilateral weakness ", frac(n_bil, n_weak)),
  paste0("\u00B0 Contralateral weakness ", frac(n_con, n_weak)),
  paste0("\u00B0 Ipsilateral weakness ", frac(n_ips, n_weak))
)
if (n_unc > 0) {
  sub_lines <- c(sub_lines,
                 paste0("\u00B0 Unilateral, side not classifiable ", frac(n_unc, n_weak)))
}

vest_tab <- tibble::tibble(
  normal = c(sprintf("%d/%d %s=%.1f%%", n_normal_v, n_total, unit_label,
                     100 * n_normal_v / n_total),
             rep("", length(sub_lines))),
  weak = c(sprintf("%d/%d %s=%.1f%%", n_weak, n_total, unit_label,
                   100 * n_weak / n_total),
           sub_lines)
)

ft_vest <- flextable::flextable(vest_tab) %>%
  flextable::set_header_labels(normal = "Normal vestibular function",
                               weak = "Vestibular weakness present") %>%
  flextable::border_remove() %>%
  flextable::hline_top(border = strong_border, part = "header") %>%
  flextable::hline_bottom(border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = strong_border, part = "body") %>%
  flextable::font(fontname = "NewsGotT", part = "all") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::align(align = "left", part = "all") %>%
  flextable::padding(i = seq_len(nrow(vest_tab))[-1], j = 2,
                     padding.left = 14, part = "body") %>%
  flextable::valign(valign = "top", part = "all") %>%
  flextable::set_table_properties(layout = "fixed") %>%
  flextable::width(j = 1:2, width = c(2.6, 3.7))

n_hyper_v <- sum(vest$hyper_any)
n_dp_v    <- sum(vest$dp_abn)

vest_note <- paste0(
  "VNG - videonystagmography; SPV - slow phase velocity (sum of the cold and warm ",
  "irrigations of each ear). Vestibular weakness (hypofunction) followed the ",
  "protocol of the service: labyrinthine predominance (PL) above ", pl_cutoff,
  "%, unilateral hyporeflexia (SPV sum of one ear below ", hypo_unilateral_cutoff,
  " \u00B0/s) or bilateral hyporeflexia (SPV sum of the four tests below ",
  hypo_bilateral_cutoff, " \u00B0/s); bilateral hyporeflexia was classified as ",
  "bilateral weakness and the remaining cases as unilateral weakness. ",
  "Ipsilateral and contralateral refer to the weaker ear relative to the implanted ear. ",
  "Percentages of weakness subtypes refer to the ",
  if (table4_sample == "table5") "patients" else "VNGs", " with weakness. ",
  "Hyperreflexia (", n_hyper_v, ") and directional preponderance above ", dp_cutoff,
  "% (", n_dp_v, ") were recorded separately and not counted as weakness. ",
  if (table4_sample == "table5") paste0(
    "The sample and the weakness definition are the same as in Table 5 (patients ",
    "with a valid preoperative VNG classification and complete audiometry). ") else "",
  if (n_vng_excluded > 0) paste0(
    "Cases without enough data to be classified (n = ", n_vng_excluded,
    ") were excluded.") else ""
)

vest_title_text <- if (table4_sample == "table5") {
  paste0("Vestibular function of cochlear implant candidates. Summary of ",
         "vestibular findings in ", n_total, " patients with preoperative VNG ",
         "and audiometry (the sample analysed in Table 5).")
} else {
  paste0("Vestibular function of cochlear implant candidates. Summary of ",
         "vestibular findings in ", n_total, " routine preoperative VNGs obtained ",
         "in cochlear implant candidates.")
}

report_parts$table4 <- list(
  type = "table",
  title = officer::fpar(officer::ftext("Table 4. ", title_fp),
                        officer::ftext(vest_title_text, description_fp)),
  ft = ft_vest,
  note = officer::fpar(officer::ftext(vest_note, note_fp)))
write.csv2(vest_tab, file.path(output_folder, "Table4_vestibular_function.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
cat("\nTable 4 added to the combined Word report.\n")

# ---- Table 5: preoperative audiology by vestibular group ----
summarize_measure <- function(x, label, group) {
  available <- !is.na(x) & !is.na(group)
  x <- x[available]
  group <- droplevels(group[available])
  x_weakness <- x[group == "Weakness"]
  x_normal <- x[group == "Normal"]

  equal_var <- tryCatch({
    stats::var.test(x_weakness, x_normal)$p.value >= 0.05
  }, error = function(e) FALSE)

  p_mean <- tryCatch(
    stats::t.test(x_weakness, x_normal, var.equal = equal_var)$p.value,
    error = function(e) NA_real_
  )

  p_median <- tryCatch(
    suppressWarnings(stats::wilcox.test(x_weakness, x_normal, exact = FALSE)$p.value),
    error = function(e) NA_real_
  )

  tibble::tibble(
    Characteristic = c(label, "Mean (SD)", "Median (IQR)"),
    Weakness = c("", fmt_mean_sd(x_weakness), fmt_median_iqr(x_weakness)),
    Normal = c("", fmt_mean_sd(x_normal), fmt_median_iqr(x_normal)),
    p_value = c("", fmt_p(p_mean), fmt_p(p_median))
  )
}

table4 <- dplyr::bind_rows(lapply(measure_cols, function(label) {
  summarize_measure(analysis_data[[label]], label, analysis_data$group)
}))

sub_rows <- which(table4$Characteristic %in% c("Mean (SD)", "Median (IQR)"))

ft <- flextable::flextable(table4) %>%
  flextable::set_header_labels(
    Characteristic = "Audiologic findings in cochlear implant candidates",
    Weakness = paste0("Weakness (n=", n_weakness, ")"),
    Normal = paste0("Normal (n=", n_normal, ")"),
    p_value = "p-value"
  ) %>%
  flextable::add_header_row(
    values = c("", "Caloric testing", ""),
    colwidths = c(1, 2, 1)
  ) %>%
  flextable::border_remove() %>%
  flextable::hline_top(border = strong_border, part = "header") %>%
  flextable::hline(i = 1, j = 2:3, border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = strong_border, part = "body") %>%
  flextable::font(fontname = "NewsGotT", part = "all") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:4, align = "center", part = "all") %>%
  flextable::padding(i = sub_rows, j = 1, padding.left = 14, part = "body") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::set_table_properties(layout = "fixed") %>%
  flextable::width(j = 1:4, width = c(2.6, 1.3, 1.3, 0.9))

missing_text <- if (!complete_case_only) {
  "Missing observations were excluded on a measure-by-measure basis."
} else if (setequal(required_measures, measure_cols)) {
  paste0("Patients with missing data in any audiologic measure were excluded ",
         "(complete-case analysis; ", n_with_group - n_final,
         " excluded).")
} else {
  paste0("Patients with missing data in any of the following measures were ",
         "excluded: ", paste(required_measures, collapse = ", "),
         "; remaining missing observations were excluded measure by measure.")
}

note_text <- paste0(
  "Preoperative values are presented as mean (standard deviation) and ",
  "median (interquartile range, Q3-Q1). p-values for means are from Student's ",
  "t-test (or Welch's t-test if variances were unequal); p-values for medians ",
  "are from the Mann-Whitney U test. LPTA - low pure tone average; ",
  "PTA - pure tone average. ", missing_text,
  " Final sample: N = ", n_final, " (Weakness n = ", n_weakness,
  "; Normal n = ", n_normal, ")."
)

report_parts$table5 <- list(
  type = "table",
  title = officer::fpar(
    officer::ftext("Table 5. ", title_fp),
    officer::ftext(paste0(
      "Preoperative audiologic findings in cochlear implant candidates with ",
      "and without vestibular weakness on VNG testing. ", definition_text
    ), description_fp)),
  ft = ft,
  note = officer::fpar(officer::ftext(note_text, note_fp)))
write.csv2(table4, file.path(output_folder, "Table5_preoperative_audiology.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
if (!is.null(excluded_data)) {
  write.csv2(excluded_data,
             file.path(output_folder, "Table5_excluded_missing_data.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
}

cat("\nTable 5 added to the combined Word report.\n")

# ---- Figure 1: hearing loss distribution at figure_hz ----
cat("\n\n############ PART 2: FIGURE 1 ############\n")

figure_col_patterns <- c("250"  = "^ht(250|025)",
                         "500"  = "^ht(500|05k)",
                         "1000" = "^ht(1k|1000)",
                         "2000" = "^ht(2k|2000)",
                         "4000" = "^ht(4k|4000)",
                         "8000" = "^ht(8k|8000)")
fig_pattern <- unname(figure_col_patterns[as.character(figure_hz)])
if (is.na(fig_pattern)) {
  stop("figure_hz must be one of: ", paste(names(figure_col_patterns), collapse = ", "))
}

if (is.null(figure_col_manual) &&
    !any(stringr::str_detect(norm_col(names(audio)), fig_pattern))) {
  ht_cols <- names(audio)[stringr::str_detect(norm_col(names(audio)), "^ht")]
  cat("\nNo column for ", figure_hz, " Hz was found in sheet 3.\n", sep = "")
  cat("Columns starting with 'HT' in sheet 3:\n")
  print(ht_cols)
  stop("Set figure_hz to a frequency that exists in sheet 3, ",
       "or set figure_col_manual to the exact column name.")
}

fig_col <- if (!is.null(figure_col_manual)) figure_col_manual else
  find_repeated_cols(audio, fig_pattern, required_n = 1)[1]
cat("\nColumn used for the figure (preoperative ", figure_hz, " Hz): ",
    fig_col, "\n", sep = "")

fig_data <- tibble::tibble(
  key = stringr::str_squish(as.character(audio[[audio_id_col]])),
  hl = to_num(audio[[fig_col]])
) %>%
  dplyr::filter(!is.na(key), key != "") %>%
  dplyr::distinct(key, .keep_all = TRUE) %>%
  dplyr::left_join(vng_group %>% dplyr::select(key, group), by = "key")

n_fig_sheet3 <- nrow(fig_data)
fig_data <- fig_data %>% dplyr::filter(!is.na(group))
n_fig_group <- nrow(fig_data)
if (figure_use_table_sample) {
  fig_data <- fig_data %>% dplyr::filter(key %in% analysis_data$key)
}
n_fig_sample <- nrow(fig_data)
fig_data <- fig_data %>% dplyr::filter(!is.na(hl))

if (nrow(fig_data) == 0) stop("No patients with a VNG group and a value for the figure.")

if (any(abs(fig_data$hl - round(fig_data$hl / 5) * 5) > 1e-8)) {
  cat("Note: some thresholds are not multiples of 5 dB; ",
      "they were rounded to the nearest 5 dB.\n", sep = "")
}
fig_data$hl_bin <- round(fig_data$hl / 5) * 5

hl_w <- fig_data$hl_bin[fig_data$group == "Weakness"]
hl_n <- fig_data$hl_bin[fig_data$group == "Normal"]
n_fig_w <- length(hl_w); n_fig_n <- length(hl_n)

cat("\n=========== FIGURE 1 SAMPLE ===========\n")
cat("Rows in sheet 3 (valid code):                  ", n_fig_sheet3, "\n")
cat("With a valid preoperative VNG group:           ", n_fig_group, "\n")
if (figure_use_table_sample) {
  cat("In the final Table 4 sample:                   ", n_fig_sample, "\n")
}
cat("Excluded for missing ", figure_hz, " Hz value:            ",
    n_fig_sample - nrow(fig_data), "\n", sep = "")
cat("FINAL N IN FIGURE 1:                           ", nrow(fig_data),
    " (Weakness n =", n_fig_w, "| Normal n =", n_fig_n, ")\n")
cat("=======================================\n")
if (n_fig_w == 0 || n_fig_n == 0) stop("One of the groups is empty in Figure 1.")

hl_grid <- seq(min(5, floor(min(fig_data$hl_bin) / 5) * 5),
               max(115, ceiling(max(fig_data$hl_bin) / 5) * 5), by = 5)

cutoff_table <- tibble::tibble(
  cutoff_db   = hl_grid,
  sensitivity = vapply(hl_grid, function(v) mean(hl_w > v), numeric(1)),
  specificity = vapply(hl_grid, function(v) mean(hl_n <= v), numeric(1))
) %>%
  dplyr::mutate(youden = sensitivity + specificity - 1)

sens_cut <- mean(hl_w > cutoff_db)
spec_cut <- mean(hl_n <= cutoff_db)
best_cut <- cutoff_table[which.max(cutoff_table$youden), ]

cat(sprintf("\nCutoff >%g dB: sensitivity = %.1f%% (%d/%d) | specificity = %.1f%% (%d/%d)\n",
            cutoff_db, 100 * sens_cut, sum(hl_w > cutoff_db), n_fig_w,
            100 * spec_cut, sum(hl_n <= cutoff_db), n_fig_n))
cat(sprintf("Best cutoff by Youden index: >%g dB (sensitivity %.1f%%, specificity %.1f%%)\n",
            best_cut$cutoff_db, 100 * best_cut$sensitivity,
            100 * best_cut$specificity))

cutoff_label <- paste0("Cutoff >", cutoff_db, " dB")
prop_label   <- "Proportion at each cutoff"

plot_panel <- function(x, curve_type, title_text, n_group) {
  curve_label <- if (curve_type == "sensitivity") {
    "1-Cumulative Proportion (Sensitivity)"
  } else {
    "Cumulative Proportion (Specificity)"
  }

  prop_at <- vapply(hl_grid, function(v) mean(x == v), numeric(1))
  cum     <- vapply(hl_grid, function(v) mean(x <= v), numeric(1))
  curve   <- if (curve_type == "sensitivity") 1 - cum else cum

  long <- dplyr::bind_rows(
    tibble::tibble(threshold = hl_grid, value = curve, series = curve_label),
    tibble::tibble(threshold = hl_grid, value = prop_at, series = prop_label)
  )
  long$series <- factor(long$series, levels = c(curve_label, prop_label))

  fills <- stats::setNames(c(cor_azul, cor_verde), c(curve_label, prop_label))
  cut_df <- tibble::tibble(x = cutoff_db + 2.5, label = cutoff_label)

  ggplot2::ggplot(long, ggplot2::aes(x = threshold, y = value, fill = series)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 4), width = 4) +
    ggplot2::geom_vline(data = cut_df,
                        ggplot2::aes(xintercept = x, colour = label),
                        linewidth = 1.2) +
    ggplot2::scale_fill_manual(values = fills, breaks = c(prop_label, curve_label),
                               name = NULL) +
    ggplot2::scale_colour_manual(values = stats::setNames(cor_amarelo, cutoff_label),
                                 name = NULL) +
    ggplot2::scale_x_continuous(breaks = hl_grid,
                                limits = c(min(hl_grid) - 3, max(hl_grid) + 3),
                                expand = c(0, 0)) +
    ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1),
                                expand = c(0, 0),
                                labels = function(v) sub("\\.0$", "", sprintf("%.1f", v))) +
    ggplot2::labs(title = stringr::str_wrap(title_text, 55),
                  subtitle = paste0("n = ", n_group),
                  x = paste0("Hearing Loss at ", figure_hz, "Hz (dB)"),
                  y = "Proportion") +
    ggplot2::theme_classic(base_size = 9, base_family = fig_font) +
    ggplot2::theme(
      plot.title    = ggplot2::element_text(hjust = 0.5, size = 11, colour = "grey25"),
      plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8, colour = "grey40"),
      axis.title    = ggplot2::element_text(size = 9, colour = "grey25"),
      axis.text     = ggplot2::element_text(size = 8, colour = "grey30"),
      axis.line     = ggplot2::element_line(colour = "grey60"),
      axis.ticks    = ggplot2::element_line(colour = "grey60"),
      legend.position = "bottom",
      legend.box    = "horizontal",
      legend.text   = ggplot2::element_text(size = 8)
    )
}

p_weak <- plot_panel(
  hl_w, "sensitivity",
  paste0("Distribution of Hearing Loss at ", figure_hz,
         "Hz for Patients with Vestibular Weakness"),
  n_fig_w)
p_norm <- plot_panel(
  hl_n, "specificity",
  paste0("Distribution of Hearing Loss at ", figure_hz,
         "Hz for Patients without Vestibular Weakness"),
  n_fig_n)

fig1 <- patchwork::wrap_plots(p_weak, p_norm, ncol = 1) +
  patchwork::plot_annotation(tag_levels = "A") &
  ggplot2::theme(plot.tag = ggplot2::element_text(face = "bold", family = fig_font,
                                                  size = 11))

fig_width  <- 6.0
fig_height <- 7.4

fig_png <- file.path(output_folder, "Figure1_hearing_loss_distribution.png")
ggplot2::ggsave(fig_png, fig1, device = ragg::agg_png,
                width = fig_width, height = fig_height, units = "in",
                dpi = 300, bg = "white")

fig_caption <- paste0(
  "Detection of vestibular weakness by preoperative hearing loss at ", figure_hz,
  " Hz. (A) Patients with vestibular weakness: ", color_name(cor_azul),
  " bars show the sensitivity at each pure-tone threshold (1 - cumulative proportion) and ",
  color_name(cor_verde), " bars the proportion of patients at each threshold ",
  "(patients missed at each cutoff). (B) Patients without vestibular weakness: ",
  color_name(cor_azul), " bars show the specificity at each threshold (cumulative proportion) and ",
  color_name(cor_verde), " bars the proportion at each threshold (unnecessary tests). ",
  "The ", color_name(cor_amarelo), " line indicates the cutoff of >",
  cutoff_db, " dB. ", definition_text
)

fig_note <- sprintf(
  paste0("Patients with a valid preoperative VNG group and a %g Hz threshold: N = %d ",
         "(Weakness n = %d; Normal n = %d). At the cutoff of >%g dB, sensitivity was ",
         "%.1f%% and specificity was %.1f%%. Best cutoff by Youden index: >%g dB ",
         "(sensitivity %.1f%%, specificity %.1f%%). Missing observations were excluded."),
  figure_hz, nrow(fig_data), n_fig_w, n_fig_n, cutoff_db,
  100 * sens_cut, 100 * spec_cut, best_cut$cutoff_db,
  100 * best_cut$sensitivity, 100 * best_cut$specificity)

report_parts$figure1 <- list(
  type = "figure", png = fig_png, w = fig_width, h = fig_height,
  caption = officer::fpar(officer::ftext("Figure 1. ", fig_title_fp),
                          officer::ftext(fig_caption, fig_text_fp)),
  note = officer::fpar(officer::ftext(fig_note, fig_note_fp)))

write.csv2(cutoff_table,
           file.path(output_folder, "Figure1_cutoff_table.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")

cat("\nFigure 1 saved to:\n", fig_png, "\n(also added to the combined Word report)\n")

# ---- Table 6: screening accuracy of the cutoffs ----
cat("\n\n############ PART 3: TABLE 6 (SCREENING ACCURACY) ############\n")

lpta_tbl <- audio_data %>%
  dplyr::select(key, lpta = dplyr::all_of("LPTA (dB)"))

screen_data <- fig_data %>%
  dplyr::select(key, group, hl_bin) %>%
  dplyr::left_join(lpta_tbl, by = "key")
if (screening_common_sample) {
  screen_data <- screen_data %>% dplyr::filter(!is.na(lpta))
}
if (nrow(screen_data) == 0) stop("No patients with data for the Table 6 tests.")

tests <- list(
  list(label = paste0("Hearing at ", figure_hz, " Hz >", cutoff_db, " dB"),
       values = screen_data$hl_bin, cutoff = cutoff_db),
  list(label = paste0("Hearing at ", figure_hz, " Hz >", cutoff_db_2, " dB"),
       values = screen_data$hl_bin, cutoff = cutoff_db_2),
  list(label = paste0("LPTA >", lpta_cutoff_db, " dB"),
       values = screen_data$lpta, cutoff = lpta_cutoff_db)
)

acc_metric <- function(name, x, n) {
  ci <- if (n > 0) stats::binom.test(x, n)$conf.int else c(NA_real_, NA_real_)
  tibble::tibble(metric = name,
                 estimate = if (n > 0) x / n else NA_real_,
                 ci_low = ci[1], ci_high = ci[2],
                 numerator = x, denominator = n)
}

run_screen <- function(t) {
  ok <- !is.na(t$values) & !is.na(screen_data$group)
  v <- t$values[ok]; g <- screen_data$group[ok]
  tp <- sum(v > t$cutoff & g == "Weakness")
  fn <- sum(v <= t$cutoff & g == "Weakness")
  fp <- sum(v > t$cutoff & g == "Normal")
  tn <- sum(v <= t$cutoff & g == "Normal")
  list(label = t$label, tp = tp, fn = fn, fp = fp, tn = tn,
       n_w = tp + fn, n_n = fp + tn,
       metrics = dplyr::bind_rows(
         acc_metric("Sensitivity", tp, tp + fn),
         acc_metric("Specificity", tn, tn + fp),
         acc_metric("PPV", tp, tp + fp),
         acc_metric("NPV", tn, tn + fn)))
}
res <- lapply(tests, run_screen)

get_m <- function(r, m) r$metrics[r$metrics$metric == m, ]
ci_text <- function(r, m) {
  x <- get_m(r, m)
  if (is.na(x$estimate)) return("–")
  sprintf("%.1f%% (%.1f–%.1f)", 100 * x$estimate, 100 * x$ci_low, 100 * x$ci_high)
}
pct_only <- function(r, m) {
  x <- get_m(r, m)
  if (is.na(x$estimate)) "–" else sprintf("%.1f%%", 100 * x$estimate)
}

for (r in res) {
  cat(sprintf("\n%s: TP = %d | FN = %d | FP = %d | TN = %d (Weakness n = %d, Normal n = %d)\n",
              r$label, r$tp, r$fn, r$fp, r$tn, r$n_w, r$n_n))
  cat(sprintf("  Sensitivity %s | Specificity %s | PPV %s | NPV %s\n",
              ci_text(r, "Sensitivity"), ci_text(r, "Specificity"),
              ci_text(r, "PPV"), ci_text(r, "NPV")))
}

accuracy_long <- dplyr::bind_rows(lapply(res, function(r) {
  dplyr::mutate(r$metrics, test = r$label, .before = 1)
}))

row_labels <- c("N (Weakness / Normal)",
                "Weakness, test positive (n)", "Weakness, test negative (n)",
                "Normal, test positive (n)", "Normal, test negative (n)",
                "Sensitivity (95% CI)", "Specificity (95% CI)",
                "PPV (95% CI)", "NPV (95% CI)")
screen_tab <- tibble::tibble(Measure = row_labels)
for (i in seq_along(res)) {
  r <- res[[i]]
  screen_tab[[paste0("test", i)]] <- c(
    sprintf("%d (%d / %d)", r$n_w + r$n_n, r$n_w, r$n_n),
    as.character(r$tp), as.character(r$fn), as.character(r$fp), as.character(r$tn),
    ci_text(r, "Sensitivity"), ci_text(r, "Specificity"),
    ci_text(r, "PPV"), ci_text(r, "NPV"))
}

n_tests <- length(res)
test_labels <- stats::setNames(vapply(res, function(r) r$label, character(1)),
                               paste0("test", seq_len(n_tests)))

ft_screen <- flextable::flextable(screen_tab) %>%
  flextable::set_header_labels(values = c(list(Measure = ""), as.list(test_labels))) %>%
  flextable::add_header_row(
    values = c("", "Screening test (positive if above the cutoff)"),
    colwidths = c(1, n_tests)) %>%
  flextable::border_remove() %>%
  flextable::hline_top(border = strong_border, part = "header") %>%
  flextable::hline(i = 1, j = 2:(n_tests + 1), border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = thin_border, part = "header") %>%
  flextable::hline(i = 5, border = thin_border, part = "body") %>%
  flextable::hline_bottom(border = strong_border, part = "body") %>%
  flextable::font(fontname = "NewsGotT", part = "all") %>%
  flextable::fontsize(size = 9, part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2:(n_tests + 1), align = "center", part = "all") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::set_table_properties(layout = "fixed") %>%
  flextable::width(j = 1:(n_tests + 1),
                   width = c(2.0, rep(4.3 / n_tests, n_tests)))

screen_title <- "Screening performance of preoperative hearing loss cutoffs for vestibular weakness. "
screen_summary <- paste0(
  "When examined as screening tools for vestibular weakness in cochlear implant ",
  "candidates, ",
  paste(vapply(res, function(r) sprintf(
    paste0("%s showed a sensitivity of %s, a specificity of %s, a negative ",
           "predictive value (NPV) of %s and a positive predictive value (PPV) of %s"),
    r$label, pct_only(r, "Sensitivity"), pct_only(r, "Specificity"),
    pct_only(r, "NPV"), pct_only(r, "PPV")), character(1)), collapse = "; "),
  ".")

prev_txt <- vapply(res, function(r) sprintf("%.1f%%", 100 * r$n_w / (r$n_w + r$n_n)),
                   character(1))
prev_note <- if (length(unique(prev_txt)) == 1) {
  paste0("prevalence of vestibular weakness in the sample: ", prev_txt[1])
} else {
  paste0("prevalence of vestibular weakness: ",
         paste(paste0(test_labels, " - ", prev_txt), collapse = "; "))
}

screen_note <- paste0(
  "A positive test was defined as preoperative hearing loss above each cutoff (",
  figure_hz, " Hz thresholds rounded to 5 dB steps; LPTA as measured). ",
  if (screening_common_sample) paste0(
    "All tests were evaluated in the same patients (with both a ", figure_hz,
    " Hz threshold and an LPTA value). ") else "",
  prev_note, ". 95% CI: exact (Clopper-Pearson) confidence intervals. ",
  "PPV - positive predictive value; NPV - negative predictive value; ",
  "LPTA - low pure tone average. ",
  "Predictive values depend on the prevalence of weakness in this sample. ",
  definition_text, " Patients with missing data were excluded."
)

report_parts$table6 <- list(
  type = "table",
  title = officer::fpar(
    officer::ftext("Table 6. ", title_fp),
    officer::ftext(paste0(screen_title, screen_summary), description_fp)),
  ft = ft_screen,
  note = officer::fpar(officer::ftext(screen_note, note_fp)))

write.csv2(accuracy_long,
           file.path(output_folder, "Table6_screening_accuracy.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")

cat("\nTable 6 added to the combined Word report.\n")

# ---- Figure 2: sensitivity vs. specificity by threshold ----
cat("\n\n############ PART 4: FIGURE 2 ############\n")

curve_long <- dplyr::bind_rows(
  tibble::tibble(threshold = cutoff_table$cutoff_db,
                 value = cutoff_table$specificity, series = "Specificity"),
  tibble::tibble(threshold = cutoff_table$cutoff_db,
                 value = cutoff_table$sensitivity, series = "Sensitivity")
)
curve_long$series <- factor(curve_long$series,
                            levels = c("Specificity", "Sensitivity"))

curve_cols <- c("Specificity" = cor_roxo, "Sensitivity" = cor_azul)
x_max <- max(120, ceiling(max(hl_grid) / 10) * 10)

fig2 <- ggplot2::ggplot(curve_long,
                        ggplot2::aes(x = threshold, y = value,
                                     colour = series, group = series)) +

  ggplot2::geom_vline(xintercept = cutoff_db, colour = cor_amarelo,
                      linewidth = 1.2) +
  ggplot2::geom_line(linewidth = 0.9) +
  ggplot2::geom_point(size = 2.2) +
  ggplot2::annotate("text", x = cutoff_db + 1.5, y = 0.04, label = cutoff_label,
                    hjust = 0, size = 2.8, family = fig_font, colour = "grey30") +
  ggplot2::scale_colour_manual(values = curve_cols, name = NULL) +
  ggplot2::scale_x_continuous(breaks = seq(0, x_max, 10), limits = c(0, x_max),
                              expand = c(0.01, 0)) +
  ggplot2::scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2),
                              expand = c(0.01, 0),
                              labels = function(v) sprintf("%.2f", v)) +
  ggplot2::labs(x = paste0("Hearing loss at ", figure_hz, "Hz (dB)"),
                y = "Sensitivity / Specificity") +
  ggplot2::theme_classic(base_size = 9, base_family = fig_font) +
  ggplot2::theme(
    axis.title      = ggplot2::element_text(size = 9, colour = "grey25"),
    axis.text       = ggplot2::element_text(size = 8, colour = "grey30"),
    axis.line       = ggplot2::element_line(colour = "grey60"),
    axis.ticks      = ggplot2::element_line(colour = "grey60"),
    legend.position = "bottom",
    legend.text     = ggplot2::element_text(size = 8)
  )

fig2_width  <- 6.0
fig2_height <- 3.9
fig2_png <- file.path(output_folder, "Figure2_sensitivity_specificity.png")
ggplot2::ggsave(fig2_png, fig2, device = ragg::agg_png,
                width = fig2_width, height = fig2_height, units = "in",
                dpi = 300, bg = "white")

fig2_caption <- paste0(
  "Sensitivity vs. specificity across pure-tone thresholds at ", figure_hz,
  " Hz. The y-axis represents sensitivity and specificity (proportion) and the ",
  "x-axis represents each pure-tone threshold (dB) at ", figure_hz, " Hz. ",
  "As specificity increases, sensitivity declines. The ", color_name(cor_amarelo),
  " line indicates the ",
  "cutoff of >", cutoff_db, " dB. ", definition_text
)

fig2_note <- sprintf(
  paste0("N = %d (Weakness n = %d; Normal n = %d). At the cutoff of >%g dB, ",
         "sensitivity was %.1f%% and specificity was %.1f%%. Best cutoff by ",
         "Youden index: >%g dB (sensitivity %.1f%%, specificity %.1f%%). ",
         "Patients with missing data were excluded."),
  n_fig_w + n_fig_n, n_fig_w, n_fig_n, cutoff_db,
  100 * sens_cut, 100 * spec_cut, best_cut$cutoff_db,
  100 * best_cut$sensitivity, 100 * best_cut$specificity)

report_parts$figure2 <- list(
  type = "figure", png = fig2_png, w = fig2_width, h = fig2_height,
  caption = officer::fpar(officer::ftext("Figure 2. ", fig_title_fp),
                          officer::ftext(fig2_caption, fig_text_fp)),
  note = officer::fpar(officer::ftext(fig2_note, fig_note_fp)))

cat("\nFigure 2 saved to:\n", fig2_png, "\n(also added to the combined Word report)\n")

# ---- Table 8: raw data set (one patient per row) ----
cat("\n\n############ PART 5: TABLE 8 (RAW DATA SET) ############\n")

etiol_tbl <- tibble::tibble(
  key = stringr::str_squish(as.character(s1[[s1_key]])),
  etiology = if (is.na(s1_etiol)) NA_character_ else translate_etiology(s1[[s1_etiol]])
) %>%
  dplyr::filter(!is.na(key), key != "") %>%
  dplyr::distinct(key, .keep_all = TRUE)

key_vng <- stringr::str_squish(as.character(vng[[vng_id_col]]))
ids <- unique(c(key_vng, audio_data$key))
ids <- ids[!is.na(ids) & ids != ""]
i_vng <- match(ids, key_vng)
i_aud <- match(ids, audio_data$key)
i_et  <- match(ids, etiol_tbl$key)

fmt_auto <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) "NA" else if (abs(v - round(v)) < 1e-9) sprintf("%.0f", v) else sprintf("%.1f", v)
  }, character(1))
}
fmt_1 <- function(x) ifelse(is.na(x), "NA", sprintf("%.1f", x))
class_code <- function(type, hyper) {
  base <- c(Bilateral = "Bil", Ipsilateral = "Ips", Contralateral = "Con",
            Unclassified = "Uni", None = "N")[type]
  out <- ifelse(is.na(base), "NA", unname(base))
  paste0(out, ifelse(is_true(hyper) & out != "NA", "\u2020", ""))
}
aud_col <- function(label) {
  if (label %in% names(audio_data)) audio_data[[label]][i_aud] else rep(NA_real_, length(ids))
}

raw_tab <- tibble::tibble(
  id        = if (raw_table_id == "code") ids else as.character(seq_along(ids)),
  etiology  = ifelse(is.na(etiol_tbl$etiology[i_et]), "NA", etiol_tbl$etiology[i_et]),
  ear       = ifelse(is.na(implanted_ear[i_vng]), "NA",
                     c(Right = "R", Left = "L")[implanted_ear[i_vng]]),
  a250      = fmt_auto(aud_col("Hearing at 250 Hz (dB)")),
  a500      = fmt_auto(aud_col("Hearing at 500 Hz (dB)")),
  a1000     = fmt_auto(aud_col("Hearing at 1000 Hz (dB)")),
  a2000     = fmt_auto(aud_col("Hearing at 2000 Hz (dB)")),
  a4000     = fmt_auto(aud_col("Hearing at 4000 Hz (dB)")),
  lpta      = fmt_auto(aud_col("LPTA (dB)")),
  pta       = fmt_auto(aud_col("PTA (dB)")),
  disc      = fmt_auto(aud_col("Maximum discrimination (%)")),
  str_pct   = fmt_auto(aud_col("STR (%)")),
  spv_pre   = fmt_1(cls_pre$total[i_vng]),
  spv_post  = fmt_1(cls_post$total[i_vng]),
  pl_pre    = fmt_1(cls_pre$pl[i_vng]),
  pl_post   = fmt_1(cls_post$pl[i_vng]),
  dp_pre    = fmt_1(cls_pre$dp[i_vng]),
  dp_post   = fmt_1(cls_post$dp[i_vng]),
  cls_pre   = class_code(cls_pre$type[i_vng], cls_pre$hyper[i_vng]),
  cls_post  = class_code(cls_post$type[i_vng], cls_post$hyper[i_vng])
)
cat("Patients in the raw data table:", nrow(raw_tab),
    "(with VNG:", sum(!is.na(i_vng)), "| with audiometry:", sum(!is.na(i_aud)), ")\n")

raw_labels <- list(
  id = "ID", etiology = "Hearing loss etiology", ear = "CI ear",
  a250 = "250", a500 = "500", a1000 = "1k", a2000 = "2k", a4000 = "4k",
  lpta = "LPTA", pta = "PTA", disc = "Max. disc. (%)", str_pct = "STR (%)",
  spv_pre = "Pre", spv_post = "Post", pl_pre = "Pre", pl_post = "Post",
  dp_pre = "Pre", dp_post = "Post", cls_pre = "Pre", cls_post = "Post")

ft_raw <- flextable::flextable(raw_tab) %>%
  flextable::set_header_labels(values = raw_labels) %>%
  flextable::add_header_row(
    values = c("ID", "Hearing loss etiology", "CI ear",
               "Pure-tone audiometry, pre-op (dB HL)", "Speech audiometry, pre-op",
               "SPV total (\u00B0/s)", "UW / PL (%)", "DP (%)", "Caloric classification"),
    colwidths = c(1, 1, 1, 7, 2, 2, 2, 2, 2)) %>%
  flextable::merge_v(j = 1:3, part = "header") %>%
  flextable::border_remove() %>%
  flextable::hline_top(border = strong_border, part = "header") %>%
  flextable::hline(i = 1, j = 4:10, border = thin_border, part = "header") %>%
  flextable::hline(i = 1, j = 11:12, border = thin_border, part = "header") %>%
  flextable::hline(i = 1, j = 13:14, border = thin_border, part = "header") %>%
  flextable::hline(i = 1, j = 15:16, border = thin_border, part = "header") %>%
  flextable::hline(i = 1, j = 17:18, border = thin_border, part = "header") %>%
  flextable::hline(i = 1, j = 19:20, border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = thin_border, part = "header") %>%
  flextable::hline_bottom(border = strong_border, part = "body") %>%
  flextable::font(fontname = "NewsGotT", part = "all") %>%
  flextable::fontsize(size = 7, part = "all") %>%
  flextable::align(j = 1, align = "left", part = "all") %>%
  flextable::align(j = 2, align = "left", part = "all") %>%
  flextable::align(j = 3:20, align = "center", part = "all") %>%
  flextable::valign(valign = "center", part = "all") %>%
  flextable::padding(padding.top = 1, padding.bottom = 1, part = "all") %>%
  flextable::set_table_properties(layout = "fixed") %>%
  flextable::width(j = 1:20, width = c(0.35, 1.45, 0.35, rep(0.42, 9), rep(0.5, 8)))

raw_note <- paste0(
  "Pre - preoperative; Post - postoperative VNG. CI ear - implanted ear (R, right; L, left). ",
  "Hearing thresholds in dB HL; LPTA - low pure tone average; PTA - pure tone average; ",
  "Max. disc. - maximum speech discrimination; STR - as recorded in the database. ",
  "SPV total - sum of the slow phase velocity of the four caloric tests; UW / PL - ",
  "unilateral weakness (labyrinthine predominance); DP - directional preponderance. ",
  "Caloric classification (service protocol): Bil, bilateral weakness; Ips, ipsilateral ",
  "weakness; Con, contralateral weakness (relative to the implanted ear); Uni, unilateral ",
  "weakness with undetermined side; N, no weakness; \u2020, hyperreflexia present. ",
  "NA - not available.")

report_parts$table8 <- list(
  type = "table", landscape = TRUE,
  title = officer::fpar(
    officer::ftext("Table 8. ", title_fp),
    officer::ftext(paste0("Complete raw data set on all ", nrow(raw_tab), " patients."),
                   description_fp)),
  ft = ft_raw,
  note = officer::fpar(officer::ftext(raw_note, note_fp)))

raw_csv <- dplyr::bind_cols(
  tibble::tibble(code = ids), raw_tab) %>%
  dplyr::rename(ID = id, `Hearing loss etiology` = etiology, `CI ear` = ear,
                `250 Hz` = a250, `500 Hz` = a500, `1000 Hz` = a1000,
                `2000 Hz` = a2000, `4000 Hz` = a4000, LPTA = lpta, PTA = pta,
                `Max discrimination (%)` = disc, `STR (%)` = str_pct,
                `SPV total pre` = spv_pre, `SPV total post` = spv_post,
                `PL pre (%)` = pl_pre, `PL post (%)` = pl_post,
                `DP pre (%)` = dp_pre, `DP post (%)` = dp_post,
                `Classification pre` = cls_pre, `Classification post` = cls_post)
write.csv2(raw_csv, file.path(output_folder, "Table8_raw_data.csv"),
           row.names = FALSE, fileEncoding = "UTF-8")
cat("\nTable 8 added to the combined Word report (landscape page).\n")

# ---- Figure 3: pre vs. post caloric response by ear ----
cat("\n\n############ PART 6: FIGURE 3 (PRE vs POST VNG) ############\n")

p_label <- function(p) {
  if (is.na(p)) "p = n/a" else if (p < 0.001) "p < 0.001" else sprintf("p = %.3f", p)
}

ear_imp <- implanted_ear[i_vng]
side_pick <- function(od, oe, ear, want_implanted) {
  ifelse(is.na(ear), NA_real_, ifelse((ear == "Right") == want_implanted, od, oe))
}
ear_data <- tibble::tibble(
  id = ids, implanted_ear = ear_imp,
  imp_pre  = side_pick(cls_pre$r_od[i_vng],  cls_pre$r_oe[i_vng],  ear_imp, TRUE),
  imp_post = side_pick(cls_post$r_od[i_vng], cls_post$r_oe[i_vng], ear_imp, TRUE),
  non_pre  = side_pick(cls_pre$r_od[i_vng],  cls_pre$r_oe[i_vng],  ear_imp, FALSE),
  non_post = side_pick(cls_post$r_od[i_vng], cls_post$r_oe[i_vng], ear_imp, FALSE))

paired_stats <- function(pre, post) {
  ok <- !is.na(pre) & !is.na(post)
  n <- sum(ok)
  out <- list(n = n, ok = ok, med_pre = NA_real_, med_post = NA_real_,
              hl = NA_real_, lo = NA_real_, hi = NA_real_, p = NA_real_, red50 = NA_integer_)
  if (n < 3) return(out)
  w <- tryCatch(suppressWarnings(
    stats::wilcox.test(post[ok], pre[ok], paired = TRUE, conf.int = TRUE, exact = FALSE)),
    error = function(e) NULL)
  out$med_pre <- stats::median(pre[ok]); out$med_post <- stats::median(post[ok])
  if (!is.null(w)) {
    out$hl <- unname(w$estimate); out$lo <- w$conf.int[1]; out$hi <- w$conf.int[2]
    out$p <- w$p.value
  }
  out$red50 <- sum(pre[ok] > 0 & (pre[ok] - post[ok]) / pre[ok] > 0.5)
  out
}

have_per_ear <- any(!is.na(ear_data$imp_pre)) && any(!is.na(ear_data$imp_post))
if (!have_per_ear) {
  message("Figure 3 skipped: it needs the SPV per ear (pre and post) and the implanted ear.")
} else {
  st_imp <- paired_stats(ear_data$imp_pre, ear_data$imp_post)
  st_non <- paired_stats(ear_data$non_pre, ear_data$non_post)
  y_max <- max(c(ear_data$imp_pre, ear_data$imp_post, ear_data$non_pre,
                 ear_data$non_post), na.rm = TRUE) * 1.05

  pair_panel <- function(pre, post, st, title_text) {
    ok <- st$ok
    d <- tibble::tibble(
      id = rep(seq_len(sum(ok)), 2),
      time = factor(rep(c("Pre-implant", "Post-implant"), each = sum(ok)),
                    levels = c("Pre-implant", "Post-implant")),
      value = c(pre[ok], post[ok]),
      direction = factor(rep(ifelse(post[ok] < pre[ok], "Decrease",
                                    "Increase / no change"), 2),
                         levels = c("Decrease", "Increase / no change")))
    sub <- if (st$n >= 3) {
      sprintf("n = %d; median %.1f to %.1f \u00B0/s\nWilcoxon %s",
              st$n, st$med_pre, st$med_post, p_label(st$p))
    } else paste0("n = ", st$n)
    ggplot2::ggplot(d, ggplot2::aes(x = time, y = value)) +
      ggplot2::geom_boxplot(width = 0.3, outlier.shape = NA, fill = NA,
                            colour = "grey45", linewidth = 0.4) +
      ggplot2::geom_line(ggplot2::aes(group = id, colour = direction),
                         alpha = 0.75, linewidth = 0.5) +
      ggplot2::geom_point(ggplot2::aes(colour = direction), size = 1.6) +
      ggplot2::stat_summary(fun = stats::median, geom = "point", shape = 23,
                            size = 3.2, fill = cor_amarelo, colour = "grey20",
                            stroke = 0.6) +
      ggplot2::scale_colour_manual(
        values = c("Decrease" = cor_roxo, "Increase / no change" = cor_azul),
        name = NULL, drop = FALSE) +
      ggplot2::scale_y_continuous(limits = c(0, y_max), expand = c(0, 0)) +
      ggplot2::labs(title = title_text, subtitle = sub, x = NULL,
                    y = "Caloric response, SPV cold + warm (\u00B0/s)") +
      ggplot2::theme_classic(base_size = 9, base_family = fig_font) +
      ggplot2::theme(
        plot.title    = ggplot2::element_text(hjust = 0.5, size = 10, colour = "grey25"),
        plot.subtitle = ggplot2::element_text(hjust = 0.5, size = 8, colour = "grey40"),
        axis.title    = ggplot2::element_text(size = 9, colour = "grey25"),
        axis.text     = ggplot2::element_text(size = 8, colour = "grey30"),
        axis.line     = ggplot2::element_line(colour = "grey60"),
        axis.ticks    = ggplot2::element_line(colour = "grey60"),
        legend.text   = ggplot2::element_text(size = 8))
  }

  p3a <- pair_panel(ear_data$imp_pre, ear_data$imp_post, st_imp, "Implanted ear")
  p3b <- pair_panel(ear_data$non_pre, ear_data$non_post, st_non, "Non-implanted ear")
  fig3 <- patchwork::wrap_plots(p3a, p3b, ncol = 2, guides = "collect") +
    patchwork::plot_annotation(tag_levels = "A") &
    ggplot2::theme(legend.position = "bottom",
                   plot.tag = ggplot2::element_text(face = "bold", family = fig_font, size = 11))

  fig3_png <- file.path(output_folder, "Figure3_pre_post_caloric_response_by_ear.png")
  ggplot2::ggsave(fig3_png, fig3, device = ragg::agg_png, width = 6.5, height = 4.2,
                  units = "in", dpi = 300, bg = "white")

  stat_line <- function(label, st) {
    if (st$n < 3) return(paste0(label, ": n = ", st$n, " (too few pairs)."))
    sprintf(paste0("%s: n = %d; median difference (Hodges-Lehmann) %.1f \u00B0/s ",
                   "(95%% CI %.1f to %.1f); %s; reduction of more than 50%% in %d/%d (%.0f%%)."),
            label, st$n, st$hl, st$lo, st$hi, p_label(st$p), st$red50, st$n,
            100 * st$red50 / st$n)
  }
  fig3_caption <- paste0(
    "Caloric response before and after cochlear implantation, by ear. Each line is ",
    "one patient (", color_name(cor_roxo), ": decrease; ", color_name(cor_azul),
    ": increase or no change); boxes show the interquartile range and ",
    color_name(cor_amarelo), " diamonds the medians. The caloric response of ",
    "each ear is the sum of the absolute SPV of the cold and warm irrigations. ",
    "(A) Implanted ear. (B) Non-implanted ear.")
  fig3_note <- paste0(
    stat_line("Implanted ear", st_imp), " ", stat_line("Non-implanted ear", st_non),
    " p-values: Wilcoxon signed-rank test (paired). SPV - slow phase velocity. ",
    "Patients without both examinations were excluded.")
  report_parts$figure3 <- list(
    type = "figure", png = fig3_png, w = 6.2, h = 4.2 * 6.2 / 6.5,
    caption = officer::fpar(officer::ftext("Figure 3. ", fig_title_fp),
                            officer::ftext(fig3_caption, fig_text_fp)),
    note = officer::fpar(officer::ftext(fig3_note, fig_note_fp)))

  write.csv2(ear_data %>%
               dplyr::rename(`Implanted ear` = implanted_ear,
                             `Implanted ear, pre (deg/s)` = imp_pre,
                             `Implanted ear, post (deg/s)` = imp_post,
                             `Non-implanted ear, pre (deg/s)` = non_pre,
                             `Non-implanted ear, post (deg/s)` = non_post),
             file.path(output_folder, "Figure3_paired_data.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
  cat("\n", stat_line("Implanted ear", st_imp), "\n", stat_line("Non-implanted ear", st_non), "\n", sep = "")
  cat("Figure 3 saved to:\n", fig3_png, "\n(also added to the combined Word report)\n")
}

# ---- Table 7 and Figure 4: surgical factors ----
cat("\n\n############ PART 7: SURGICAL FACTORS (TABLE 7 AND FIGURE 4) ############\n")

surg_catalog <- list(
  electrode = list(label = "Electrode type",    pattern = "^tipo de eletrodo", type = "electrode"),
  approach  = list(label = "Surgical approach", pattern = "^abordagem",        type = "text"),
  brand     = list(label = "Implant brand",     pattern = "^marca",             type = "text")
)
clean_text <- function(x) {
  v <- stringr::str_squish(as.character(x))
  v <- ifelse(is.na(v) | v == "", NA_character_, v)
  k <- tolower(v)
  v[match(k, k)]
}

surg_values <- list()
for (f in surgical_factors) {
  spec <- surg_catalog[[f]]
  if (is.null(spec)) { message("Unknown surgical factor '", f, "'; skipped."); next }
  col <- find_col(s1, spec$pattern, required = FALSE)
  if (is.na(col)) {
    message("Surgical factor '", f, "' (", spec$label, ") not found in sheet 1; skipped.")
    next
  }
  v <- if (spec$type == "electrode") classify_electrode(s1[[col]]) else clean_text(s1[[col]])
  if (f == "approach") v <- translate(v, dic_approach)
  tbl <- tibble::tibble(key = stringr::str_squish(as.character(s1[[s1_key]])), v = v) %>%
    dplyr::filter(!is.na(key), key != "") %>% dplyr::distinct(key, .keep_all = TRUE)
  surg_values[[f]] <- tbl$v[match(ids, tbl$key)]
}

sev3 <- c(Normal = 0, Unilateral = 1, Bilateral = 2)
to_type3 <- function(type) {
  dplyr::case_when(is.na(type) ~ NA_character_, type == "None" ~ "Normal",
                   type == "Bilateral" ~ "Bilateral", TRUE ~ "Unilateral")
}
sev_pre  <- unname(sev3[to_type3(cls_pre$type[i_vng])])
sev_post <- unname(sev3[to_type3(cls_post$type[i_vng])])
out_worse <- ifelse(is.na(sev_pre) | is.na(sev_post), NA, sev_post > sev_pre)

if (have_per_ear) {
  imp_pre <- ear_data$imp_pre; imp_post <- ear_data$imp_post
} else {
  imp_pre <- imp_post <- rep(NA_real_, length(ids))
}
out_red50 <- ifelse(is.na(imp_pre) | is.na(imp_post) | imp_pre <= 0, NA,
                    (imp_pre - imp_post) / imp_pre > 0.5)
out_delta <- imp_post - imp_pre

cat("\nPatients with both VNGs classified:", sum(!is.na(out_worse)),
    "| worsening of the VNG type:", sum(out_worse, na.rm = TRUE), "\n")
cat("Patients with both implanted-ear responses:", sum(!is.na(out_delta)),
    "| reduction > 50%:", sum(out_red50, na.rm = TRUE), "\n")

fmt_bin_cell <- function(v, out, lev) {
  ok <- !is.na(v) & v == lev & !is.na(out)
  n <- sum(ok)
  if (n == 0) return("–")
  k <- sum(out[ok])
  sprintf("%d/%d (%.0f%%)", k, n, 100 * k / n)
}
fmt_delta_cell <- function(v, x, lev) {
  xx <- x[!is.na(v) & v == lev & !is.na(x)]
  if (length(xx) == 0) return("–")
  sprintf("%.1f [%.1f to %.1f]", stats::median(xx), stats::quantile(xx, 0.25), stats::quantile(xx, 0.75))
}
p_fisher_surg <- function(v, out, levels_f) {
  ok <- !is.na(v) & !is.na(out)
  tab <- table(factor(v[ok], levels = levels_f), factor(out[ok], levels = c(FALSE, TRUE)))
  tab <- tab[rowSums(tab) > 0, colSums(tab) > 0, drop = FALSE]
  if (nrow(tab) < 2 || ncol(tab) < 2) return(NA_real_)
  tryCatch(stats::fisher.test(tab)$p.value, error = function(e) NA_real_)
}
p_delta_surg <- function(v, x, levels_f) {
  ok <- !is.na(v) & !is.na(x)
  g <- factor(v[ok], levels = levels_f); g <- droplevels(g)
  if (nlevels(g) < 2) return(NA_real_)
  tryCatch(suppressWarnings(
    if (nlevels(g) == 2) stats::wilcox.test(x[ok] ~ g, exact = FALSE)$p.value
    else stats::kruskal.test(x[ok] ~ g)$p.value),
    error = function(e) NA_real_)
}

surg_rows <- list(); surg_csv <- list(); surg_levels <- list()
for (f in names(surg_values)) {
  v <- surg_values[[f]]
  lv <- names(sort(table(v), decreasing = TRUE))
  if (f == "electrode") {
    lv <- c(intersect(c("Lateral wall", "Perimodiolar"), lv), setdiff(lv, c("Lateral wall", "Perimodiolar")))
  }
  if (length(lv) < 2) {
    message("Surgical factor '", f, "' has fewer than 2 categories; skipped.")
    next
  }
  surg_levels[[f]] <- lv
  p_w <- p_fisher_surg(v, out_worse, lv)
  p_r <- p_fisher_surg(v, out_red50, lv)
  p_d <- p_delta_surg(v, out_delta, lv)

  head_row <- tibble::tibble(Factor = surg_catalog[[f]]$label, w = "", pw = "",
                             r = "", pr = "", d = "", pd = "", cab = TRUE)
  lev_rows <- tibble::tibble(
    Factor = lv,
    w  = vapply(lv, function(l) fmt_bin_cell(v, out_worse, l), character(1)),
    pw = c(rep("", length(lv) - 1), fmt_p(p_w)),
    r  = vapply(lv, function(l) fmt_bin_cell(v, out_red50, l), character(1)),
    pr = c(rep("", length(lv) - 1), fmt_p(p_r)),
    d  = vapply(lv, function(l) fmt_delta_cell(v, out_delta, l), character(1)),
    pd = c(rep("", length(lv) - 1), fmt_p(p_d)),
    cab = FALSE)
  surg_rows[[f]] <- dplyr::bind_rows(head_row, lev_rows)

  or_txt <- NA_character_
  if (length(lv) == 2) {
    ok <- !is.na(v) & !is.na(out_worse)
    tb <- table(factor(v[ok], levels = lv), factor(out_worse[ok], levels = c(FALSE, TRUE)))
    ft_or <- tryCatch(stats::fisher.test(tb), error = function(e) NULL)
    if (!is.null(ft_or) && !is.null(ft_or$conf.int)) {
      or_txt <- sprintf("OR %s vs %s = %.2f (95%% CI %.2f-%.2f)", lv[2], lv[1],
                        ft_or$estimate, ft_or$conf.int[1], ft_or$conf.int[2])
    }
  }
  surg_csv[[f]] <- tibble::tibble(
    factor = surg_catalog[[f]]$label, level = lv,
    worsening = lev_rows$w, p_worsening = p_w,
    reduction_gt50 = lev_rows$r, p_reduction = p_r,
    delta_median_iqr = lev_rows$d, p_delta = p_d,
    or_worsening = or_txt)
}

if (length(surg_rows) == 0) {
  message("Table 7 and Figure 4 skipped: no surgical factor with at least 2 categories.")
} else {
  tab7 <- dplyr::bind_rows(surg_rows)

  ft7 <- flextable::flextable(tab7 %>% dplyr::select(-cab)) %>%
    flextable::set_header_labels(Factor = "Surgical factor",
                                 w = "n/N (%)", pw = "p",
                                 r = "n/N (%)", pr = "p",
                                 d = "Median [Q1 to Q3]", pd = "p") %>%
    flextable::add_header_row(
      values = c("", "VNG type worsening", "Implanted-ear response reduction > 50%",
                 "Change in implanted-ear response (post - pre, \u00B0/s)"),
      colwidths = c(1, 2, 2, 2)) %>%
    flextable::border_remove() %>%
    flextable::hline_top(border = strong_border, part = "header") %>%
    flextable::hline(i = 1, j = 2:3, border = thin_border, part = "header") %>%
    flextable::hline(i = 1, j = 4:5, border = thin_border, part = "header") %>%
    flextable::hline(i = 1, j = 6:7, border = thin_border, part = "header") %>%
    flextable::hline_bottom(border = thin_border, part = "header") %>%
    flextable::hline_bottom(border = strong_border, part = "body") %>%
    flextable::font(fontname = "NewsGotT", part = "all") %>%
    flextable::fontsize(size = 8, part = "all") %>%
    flextable::align(j = 1, align = "left", part = "all") %>%
    flextable::align(j = 2:7, align = "center", part = "all") %>%
    flextable::padding(i = which(!tab7$cab), j = 1, padding.left = 14, part = "body") %>%
    flextable::valign(valign = "center", part = "all") %>%
    flextable::set_table_properties(layout = "fixed") %>%
    flextable::width(j = 1:7, width = c(1.3, 0.95, 0.5, 0.95, 0.5, 1.5, 0.5))

  n_pairs_vng <- sum(!is.na(out_worse)); n_pairs_ear <- sum(!is.na(out_delta))
  tab7_note <- paste0(
    "VNG type worsening: change of the VNG type towards a more severe one between the ",
    "preoperative and postoperative VNG (normal < unilateral < bilateral weakness, ",
    "service protocol; ", n_pairs_vng, " patients with both VNGs). Implanted-ear ",
    "response: sum of the absolute SPV of the cold and warm irrigations of the ",
    "implanted ear (", n_pairs_ear, " patients with both examinations). Cells show ",
    "n/N (%) of the patients of each category with data for that outcome. p-values: ",
    "Fisher's exact test (proportions) and Mann-Whitney U test (two categories) or ",
    "Kruskal-Wallis test (more than two categories) for the change in response (p). ",
    "Small numbers per category: results are exploratory. SPV - slow phase velocity; ",
    "VNG - videonystagmography; IQR - interquartile range. ",
    "Patients with missing data in an outcome were excluded from that outcome.")

  report_parts$table7 <- list(
    type = "table",
    title = officer::fpar(
      officer::ftext("Table 7. ", title_fp),
      officer::ftext(paste0("Surgical factors and vestibular deterioration after ",
                            "cochlear implantation."), description_fp)),
    ft = ft7,
    note = officer::fpar(officer::ftext(tab7_note, note_fp)))

  write.csv2(dplyr::bind_rows(surg_csv),
             file.path(output_folder, "Table7_surgical_factors.csv"),
             row.names = FALSE, fileEncoding = "UTF-8")
  cat("\nTable 7 added to the combined Word report.\n")
  print(as.data.frame(tab7 %>% dplyr::select(-cab)), right = FALSE)

  ff <- surgical_figure_factor
  if (!have_per_ear) {
    message("Figure 4 skipped: it needs the SPV per ear (pre and post) and the implanted ear.")
  } else if (!ff %in% names(surg_levels)) {
    message("Figure 4 skipped: factor '", ff, "' is not available or has fewer than 2 categories.")
  } else {
    v <- surg_values[[ff]]
    lv_ok <- surg_levels[[ff]][vapply(surg_levels[[ff]], function(l) {
      sum(!is.na(v) & v == l & !is.na(imp_pre) & !is.na(imp_post)) >= 1
    }, logical(1))]
    if (length(lv_ok) > 4) {
      message("Figure 4: only the 4 most frequent categories are shown.")
      lv_ok <- lv_ok[1:4]
    }
    if (length(lv_ok) < 2) {
      message("Figure 4 skipped: fewer than 2 categories with pre and post data.")
    } else {
      st_list <- lapply(lv_ok, function(l) {
        sel <- !is.na(v) & v == l
        paired_stats(imp_pre[sel], imp_post[sel])
      })
      panels <- lapply(seq_along(lv_ok), function(i) {
        sel <- !is.na(v) & v == lv_ok[i]
        pair_panel(imp_pre[sel], imp_post[sel], st_list[[i]], lv_ok[i])
      })
      fig4 <- patchwork::wrap_plots(panels, nrow = 1, guides = "collect") +
        patchwork::plot_annotation(tag_levels = "A") &
        ggplot2::theme(legend.position = "bottom",
                       plot.tag = ggplot2::element_text(face = "bold", family = fig_font, size = 11))

      fig4_w <- if (length(lv_ok) <= 2) 6.5 else 7.5
      fig4_h <- 4.2
      fig4_png <- file.path(output_folder, "Figure4_implanted_ear_by_surgical_factor.png")
      ggplot2::ggsave(fig4_png, fig4, device = ragg::agg_png, width = fig4_w,
                      height = fig4_h, units = "in", dpi = 300, bg = "white")

      stat_line4 <- function(label, st) {
        if (st$n < 3) return(paste0(label, ": n = ", st$n, " (too few pairs)."))
        sprintf(paste0("%s: n = %d; median difference (Hodges-Lehmann) %.1f \u00B0/s ",
                       "(95%% CI %.1f to %.1f); %s."),
                label, st$n, st$hl, st$lo, st$hi, p_label(st$p))
      }
      fig4_caption <- paste0(
        "Caloric response of the implanted ear before and after cochlear implantation, ",
        "by ", tolower(surg_catalog[[ff]]$label), ". Each line is one patient (", color_name(cor_roxo),
        ": decrease; ", color_name(cor_azul), ": increase or no change); boxes show the ",
        "interquartile range and ", color_name(cor_amarelo), " diamonds the medians. The caloric ",
        "response is the sum of the absolute SPV ",
        "of the cold and warm irrigations of the implanted ear.")
      fig4_note <- paste0(
        paste(vapply(seq_along(lv_ok), function(i) stat_line4(lv_ok[i], st_list[[i]]),
                     character(1)), collapse = " "),
        " p-values: Wilcoxon signed-rank test (paired). SPV - slow phase velocity. ",
        "Patients without both examinations were excluded.")
      report_parts$figure4 <- list(
        type = "figure", png = fig4_png, w = 6.2, h = fig4_h * 6.2 / fig4_w,
        caption = officer::fpar(officer::ftext("Figure 4. ", fig_title_fp),
                                officer::ftext(fig4_caption, fig_text_fp)),
        note = officer::fpar(officer::ftext(fig4_note, fig_note_fp)))
      cat("Figure 4 saved to:\n", fig4_png, "\n(also added to the combined Word report)\n")
    }
  }
}

# ---- Combined Word report ----
cat("\n\n############ COMBINED WORD REPORT ############\n")

section_props <- function(orient) {
  if (orient == "landscape") {
    officer::prop_section(
      page_size = officer::page_size(orient = "landscape", width = 21 / 2.54,
                                     height = 29.7 / 2.54),
      page_margins = officer::page_mar(left = 0.6, right = 0.6, top = 0.8, bottom = 0.8),
      type = "nextPage")
  } else {
    officer::prop_section(
      page_size = officer::page_size(orient = "portrait", width = 21 / 2.54,
                                     height = 29.7 / 2.54),
      page_margins = officer::page_mar(left = 1, right = 1, top = 1, bottom = 1,
                                       header = 0.5, footer = 0.5, gutter = 0),
      type = "nextPage")
  }
}
close_section <- function(doc, orient) {
  officer::body_end_block_section(doc, officer::block_section(section_props(orient)))
}

if (write_combined_report) {
  final_doc <- officer::read_docx()
  parts_added <- character()
  current_orient <- "portrait"
  for (nm in combined_order) {
    part <- report_parts[[nm]]
    if (is.null(part)) {
      message("Part '", nm, "' is not available; skipped.")
      next
    }
    orient <- if (isTRUE(part$landscape)) "landscape" else "portrait"
    if (length(parts_added) > 0) {
      if (orient != current_orient) {
        final_doc <- close_section(final_doc, current_orient)
      } else {
        final_doc <- if (page_break_between_parts) {
          officer::body_add_break(final_doc)
        } else {
          officer::body_add_par(final_doc, "", style = "Normal")
        }
      }
    }
    current_orient <- orient
    final_doc <- add_part(final_doc, part)
    parts_added <- c(parts_added, nm)
  }

  if (current_orient == "landscape") {
    final_doc <- officer::body_set_default_section(final_doc, section_props("landscape"))
  }
  combined_path <- file.path(output_folder, combined_docx_name)
  print(final_doc, target = combined_path)
  cat("Parts included (in order):", paste(parts_added, collapse = ", "), "\n")
  cat("Combined Word report saved to:\n", combined_path, "\n")
}
