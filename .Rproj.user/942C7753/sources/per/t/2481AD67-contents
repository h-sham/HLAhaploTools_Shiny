#!/usr/bin/env Rscript
# HLAhaplotools server logic

server <- function(input, output, session) {
   # 1. VALIDATION ON SUBMIT (shinyalert pattern)
   observeEvent(input$Submit, {
      validate_inputs <- function(input) {
         # Required typing file
         if (is.null(input$typing_data)) {
            shinyalert::shinyalert(
               title = "Error!",
               text = "Please upload HLA typing data.",
               type = "error"
            )
            return(FALSE)
         }

         # Optional metadata
         if (isTRUE(input$family_metadata_checkbox)) {
            if (is.null(input$family_metadata)) {
               shinyalert::shinyalert(
                  title = "Error!",
                  text = "Metadata checkbox selected, but no metadata file uploaded.",
                  type = "error"
               )
               return(FALSE)
            }
         }

         shinyalert::shinyalert(
            title = "Success!",
            text = "Data submitted successfully!",
            type = "success"
         )
         return(TRUE)
      }

      # Stop if validation fails
      if (!validate_inputs(input)) {
         return()
      }

      # Force evaluation of raw_data()
      raw_data()
   })

   # 2. REACTIVE: PROCESS DATASETS
   ## Typing data - compulsory
   typing_data <- eventReactive(input$Submit, {
      req(input$typing_data)

      ext <- tools::file_ext(tolower(input$typing_data$name))

      switch(ext,
         xls = readxl::read_excel(input$typing_data$datapath, col_types = "text"),
         xlsx = readxl::read_excel(input$typing_data$datapath, col_types = "text"),
         csv = vroom::vroom(input$typing_data$datapath,
            delim = ",",
            show_col_types = FALSE, col_types = vroom::cols(.default = "c")
         ),
         tsv = vroom::vroom(input$typing_data$datapath,
            delim = "\t",
            show_col_types = FALSE, col_types = vroom::cols(.default = "c")
         ),
         txt = vroom::vroom(input$typing_data$datapath,
            delim = "\t",
            show_col_types = FALSE, col_types = vroom::cols(.default = "c")
         ),
         xml = parse_xml_typing(input$typing_data$datapath),
         hml = parse_hml_typing(input$typing_data$datapath),
         stop("Invalid file; Please upload .xls, .xlsx, .csv, .txt, .tsv, .xml, or .hml")
      )
   })

   ## Family data optional
   family_metadata <- eventReactive(input$Submit, {
      if (!isTRUE(input$family_metadata_checkbox)) {
         return(NULL)
      }
      req(input$family_metadata)

      ext <- tools::file_ext(tolower(input$family_metadata$name))

      switch(ext,
         xls = readxl::read_excel(input$family_metadata$datapath, col_types = "text"),
         xlsx = readxl::read_excel(input$family_metadata$datapath, col_types = "text"),
         csv = vroom::vroom(input$family_metadata$datapath,
            delim = ",",
            show_col_types = FALSE, col_types = vroom::cols(.default = "c")
         ),
         tsv = vroom::vroom(input$family_metadata$datapath,
            delim = "\t",
            show_col_types = FALSE, col_types = vroom::cols(.default = "c")
         ),
         txt = vroom::vroom(input$family_metadata$datapath,
            delim = "\t",
            show_col_types = FALSE, col_types = vroom::cols(.default = "c")
         ),
         stop("Invalid metadata file; Please upload .xls, .xlsx, .csv, .txt, or .tsv")
      )
   })


   # 4. REACTIVE: MERGED RAW DATA (typing + optional metadata)
   raw_data <- eventReactive(input$Submit, {
      df <- typing_data()

      if (isTRUE(input$family_metadata_checkbox) &&
         !is.null(input$family_metadata)) {
         meta <- family_metadata()
         dplyr::left_join(meta, df) %>%
            dplyr::select(-SampleID)
      } else {
         df
      }
   })

   cleaned_data <- eventReactive(input$Submit, {
      clean_typing_data(
         df_raw = raw_data(),
         trim_selection = input$trim_selection,
         mac = TRUE
      )
   })

   ## Preprocess typing
   segregation <- eventReactive(input$Submit, {
      req(input$typing_data)
      run_segregate(cleaned_data())
   })

   # em_df <- eventReactive(input$Submit, {
   #    req(input$typing_data)
   #
   #    run_segregate(
   #       df_raw = raw_data(),
   #       trim = (input$trim_selection != "no_trim"),
   #       resolution = if (input$trim_selection == "trim2") 2 else 3,
   #       mac = TRUE
   #    )
   # })

   compare_df <- eventReactive(input$Submit, {
      req(input$typing_data)
      compare_EM_to_segregation(
         em = em_df(),
         segregation = segregation()
      )
   })

   # 5. DT DATA TABLE RENDRING HANDLERS
   output$raw_typing <- DT::renderDT({
      req(raw_data())

      DT::datatable(
         raw_data(),
         caption = "Raw typing data",
         fillContainer = FALSE,
         options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = TRUE,
            scrollY = "400px",
            scrollCollapse = TRUE
         )
      )
   })

   output$clean_typing <- DT::renderDT({
      req(raw_data())

      DT::datatable(
         cleaned_data(),
         caption = "Cleaned typing data",
         fillContainer = FALSE,
         options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = TRUE,
            scrollY = "400px",
            scrollCollapse = TRUE
         )
      )
   })

   output$segregation_out <- DT::renderDT({
      req(cleaned_data())

      DT::datatable(
         segregation(),
         caption = "Segregation analysis",
         fillContainer = FALSE,
         options = list(
            paging = FALSE,
            searching = FALSE,
            info = FALSE,
            autoWidth = TRUE,
            scrollY = "400px",
            scrollCollapse = TRUE
         )
      )
   })

   # 6. DOWNLOAD HANDLERS
   # raw hla
   output$download_raw <- downloadHandler(
      filename = function() "raw_typing.xlsx",
      content = function(file) {
         openxlsx::write.xlsx(raw_data(), file)
      }
   )
   # cleaned hla
   output$download_clean <- downloadHandler(
      filename = function() "cleaned_typing.xlsx",
      content = function(file) {
         openxlsx::write.xlsx(cleaned_data(), file)
      }
   )
   # segregation
   output$download_seg <- downloadHandler(
      filename = function() "Segregation.xlsx",
      content = function(file) {
         openxlsx::write.xlsx(segregation(), file)
      }
   )


   # RESET HANDLERS
   observeEvent(input$resetButton, {
      # Clear output tables
      output$raw_typing <- NULL
      output$clean_typing <- NULL
      output$segregation_out <- NULL

      # Clear download handlers
      output$download_raw <- NULL

      output$download_clean <- NULL
      output$download_seg <- NULL

      # Reload the session and clear everything
      session$reload()
   })

   # Hide progress div once data is processed
   shinyjs::hide("progress_div")

   observeEvent(input$help, {
      shinyalert::shinyalert(
         title = "Usage Information",
         text = '
      <div style="color: black; text-align: justify;">
         This application provides an accessible interface for processing HLA typing data,
         performing family-based segregation analysis, and reconstructing HLA haplotypes
         using expectation–maximisation (EM) inference.<br><br>

         <strong>Required input:</strong>
         <ul style="list-style-type: square; color: blue; text-align: justify;">
            <li><strong>HLA typing file</strong> in CSV, TSV/TXT, XLS/XLSX, HML, or XML format
            for NGSEngine-style uploads.</li>
         </ul>

         <strong>Optional input:</strong>
         <ul style="list-style-type: square; color: blue; text-align: justify;">
            <li><strong>Family metadata</strong> (CSV, TSV/TXT, XLS/XLSX) in tabular format,
            enabling automatic joining with the typing file.</li>
         </ul>

         <br><strong>What the app does:</strong>
         <ul style="list-style-type: square; color: blue; text-align: justify;">
            <li>auto-detects whether the dataset is family-based or population-based,</li>
            <li>standardises and reformats HLA allele fields,</li>
            <li>decodes multi-allele codes (MAC) where present,</li>
            <li>optionally trims alleles to 2-field or 3-field resolution,</li>
            <li>checks for deleted alleles in the IMGT/HLA reference database,</li>
            <li>performs <strong>family segregation analysis</strong> for trio-based datasets,</li>
            <li>runs <strong>EM haplotype inference</strong> for population-based datasets, and</li>
            <li>provides downloadable results for all major analysis steps.</li>
         </ul>

         <br>You may download sample input files here:
         <ul style="list-style-type: square; color: blue; text-align: justify;">
            <li><a href="sample_family_typing.csv" download>Sample Family Typing (CSV)</a></li>
            <li><a href="sample_population_typing.csv" download>Sample Population Typing (CSV)</a></li>
            <li><a href="sample_metadata.csv" download>Sample Family Metadata (CSV)</a></li>
         </ul>
      </div>',
         type = "info",
         closeOnEsc = TRUE,
         closeOnClickOutside = TRUE,
         showConfirmButton = TRUE,
         html = TRUE,
         size = "m"
      )
   })

   observeEvent(input$terms_of_use_link, {
      shinyalert::shinyalert(
         title = "Terms of Use",
         text = '
      <div style="color: black; text-align: justify;">
         The authors and contributors assume no responsibility for any injury, loss,
         or damage arising from the use of this software or any results generated by it.<br><br>

         <strong>No warranty is provided that:</strong>
         <ul style="list-style-type: square; color: blue; text-align: justify;">
            <li>the application is free of errors or omissions,</li>
            <li>results will be accurate, complete, or suitable for clinical decision-making,</li>
            <li>access will be uninterrupted, timely, or secure,</li>
            <li>the application is compatible with all systems or environments,</li>
            <li>the hosting platform is free of viruses or harmful code,</li>
            <li>the application is free from infringement of third‑party rights, or</li>
            <li>any identified issues will be corrected.</li>
         </ul>

         This software is intended for research and educational use only and must not be used
         as a substitute for accredited clinical HLA typing or immunogenetics workflows.<br><br>

         This application was created with dedication and scientific curiosity by
         <a href="https://github.com/h-sham/HLAhaploTools" target="_blank">Hoiley Sham</a>.
          It is provided in good faith, without any guarantee of accuracy, reliability, or fitness for purpose.

      </div>',
         type = "warning",
         closeOnEsc = TRUE,
         closeOnClickOutside = TRUE,
         showConfirmButton = TRUE,
         html = TRUE,
         size = "m"
      )
   })
}
