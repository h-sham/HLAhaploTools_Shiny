library(shiny)
library(bslib)
library(bsicons)
library(shinycssloaders)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinyWidgets)
library(shinyalert)
library(vroom)
library(readxl)
library(HLAhaploTools)

reformat_ngs_engine_csv <- function(csv) {}
loci <- c("A", "B", "C", "DRB1", "DPA1", "DPB1", "DQA1", "DQB1")

## Source all R files from previous app....
library(purrr)
purrr::walk(
   list.files("../HLAhaploTools/R", pattern = "\\.R$", full.names = TRUE),
   source
)

clean_typing_data <- function(df_raw,
                              trim_selection = "trim2",
                              mac = TRUE) {
   detect_result <- HLAhaploTools::detect_data_type(df_raw, quiet = TRUE)
   family_data_val <- detect_result$is_family

   if (!family_data_val) {
      stop("Input is not family-based; segregation cannot be performed.")
   }

   df_formatted <- reformat_typing_data(
      df_raw,
      isfamilydata = TRUE,
      quiet = TRUE
   )

   df_decoded <- if (mac) {
      decode_classical_mac(df_formatted, quiet = TRUE) %>%
         HLAhaploTools::remove_mac_strings(decoded, quiet = TRUE)
   } else {
      df_formatted
   }

   if (trim_selection == "trim2") {
      df_decoded <- HLAhaploTools::trim_hla_results(df_decoded,
         resolution = 2,
         quiet = TRUE
      )
   } else if (trim_selection == "trim3") {
      df_decoded <- HLAhaploTools::trim_hla_results(df_decoded,
         resolution = 3,
         quiet = TRUE
      )
   }
   df_decoded
}

run_segregate <- function(df_segregate) {
   df_segregation <- HLAhaploTools::compute_hla_segregation(df_segregate,
      collapse = "~",
      verbose = FALSE
   )
   df_segregation
}

run_allele_string <- function(df_allele) {
   df_allele_string <- HLAhaploTools::compute_hla_segregation(df_allele,
      collapse = "~",
      verbose = FALSE
   )
   df_allele_string
}

run_em <- function(df_em_algorithm) {
   df_em <- HLAhaploTools::em_algorithm(
      df_raw = df_em_algorithm,
      collapse = "~",
      quiet = TRUE
   )
   df_em
}

compare_em_and_seg <- function(em, segregation) {
   df_compare <- HLAhaploTools::compare_EM_to_segregation(
      em_df = em,
      segregation_df = segregation,
      collapse = "~"
   )
   df_compare
}
