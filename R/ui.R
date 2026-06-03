ui <- page_fluid(
   tags$head(
      tags$link(rel = "icon", sizes = "32x32", href = "favicon.ico"),
      includeCSS("www/styles.css")
   ),

   # --- TITLE PANEL ---
   titlePanel(
      title = div(
         id = "title-container",

         # Logo row (kept unchanged)
         div(
            id = "logo-row",
            style = "display: flex; justify-content: space-between; align-items: center;",
            tags$a(
               href = "https://pathwest.health.wa.gov.au/Our-Services/Clinical-Services/Immunology",
               target = "_blank",
               tags$img(src = "PathWestlogo.png", id = "logo", height = "75px")
            ),
            tags$a(
               href = "https://www.uwa.edu.au",
               target = "_blank",
               tags$img(src = "UWA_logo.png", id = "uwa_logo", height = "75px")
            )
         ),

         # Title text bar
         div(
            id = "title-text",
            h1("EXTENDED HLA HAPLOTYPE RECONSTRUCTION")
         )
      ),
      windowTitle = "Extended HLA Reconstruction"
   ),
   theme = bslib::bs_theme(bootswatch = "yeti"),

   # --- SIDEBAR + MAIN LAYOUT ---
   sidebarLayout(
      sidebarPanel(
         width = 2,
         position = c("left", "justify"),
         fluid = TRUE,
         helpText(a(HTML(
            "Program to reconstruct HLA haplotypes in family-based and population
            typing data.<br><br>"
         ))),
         helpText(h5(HTML("Upload files (Max 50Mb)"))),
         fileInput(
            "typing_data",
            NULL,
            buttonLabel = "HLA typing",
            multiple = FALSE,
            accept = c(".xls", ".xlsx", ".csv", ".txt", ".tsv", ".hml", ".xml")
         ),
         div(
            style = "margin-top: -25px; font-size: 15px; color: #bf3b34;",
            helpText(HTML(
               "Acceptable formats: .xls, xlsx, .csv, .txt/.tsv, .hml, .xml"
            ))
         ),
         br(),
         helpText(h6(HTML("Analysis type"), style = "margin-bottom: 5px;")),
         div(
            style = "margin-top: 10px; margin-bottom: -10px;",
            selectInput(
               inputId = "data_type",
               label = NULL,
               choices = c("Auto-detect", "Family-based", "Population-based"),
               selected = "Auto-detect"
            )
         ),
         helpText(h6(HTML("Allele Trimming"), style = "margin-bottom: 5px;")),
         div(
            style = "margin-top: 10px; margin-bottom: -10px;",
            selectInput(
               inputId = "trim_selection",
               label = NULL,
               choices = c(
                  "Yes (2-field)" = "trim2",
                  "Yes (3-field)" = "trim3",
                  "No Trimming" = "no_trim"
               ),
               selected = "trim2" # default = YES trimming
            )
         ),
         br(),

         # Optional input (Metadata for direct NGSEngine uploads of family data)
         div(
            style = "margin: 0;",
            helpText(h4(HTML("Optional (metadata)"))),
            tags$div(
               class = "checkbox",
               tags$input(
                  type = "checkbox", id = "family_metadata_checkbox", style = "margin: 0; color:red"
               ),
               tags$label(
                  "Family Metadata",
                  `for` = "family_metadata_checkbox",
                  tags$i(
                     class = "glyphicon glyphicon-info-sign slim-info-icon", # Added class for styling
                     title = "If selected, provide family metadata in table format (csv, tsv/txt, xls, xlsx)
                     to be joined with typing data input"
                  )
               )
            )
         ),
         conditionalPanel(
            condition = "input.family_metadata_checkbox == true",
            fileInput(
               "family_metadata",
               NULL,
               buttonLabel = "Metadata",
               multiple = FALSE,
               accept = c(".xls", ".xlsx", ".csv", ".txt", ".tsv")
            )
         ),
         br(),
         fluidRow(
            column(6, actionButton("Submit", "Submit", class = "btn-success")),
            column(6, actionButton("resetButton", "Reset")),
            br(),
            fluidRow(
               column(12, actionLink(
                  "help", HTML('<span style ="color: blue;">&quest;Help</span')
               ))
            ),
            fluidRow(
               column(12, actionLink(
                  "terms_of_use_link", HTML('<span style="color: blue;">&#128712;Terms of Use</span>')
               ))
            )
         )
      ),

      # --- MAIN PANEL ---
      mainPanel(
         width = 10,
         tabsetPanel(
            id = "Dataset",

            # --- RAW TYPING DATA TAB ---
            tabPanel(
               "Raw Typing Data",
               withSpinner(
                  DT::DTOutput("raw_typing"),
                  type = 7,
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.raw_typing",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Raw Typing Data"),
                     downloadButton("download_raw", "download xlsx")
                  )
               )
            ),

            # --- CLEANED TYPING DATA TAB ---
            tabPanel(
               "Cleaned Typing Data",
               withSpinner(
                  DT::DTOutput("clean_typing"),
                  type = 7,
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.clean_typing",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Reformatted Typing Data"),
                     downloadButton("download_clean", "download xlsx")
                  )
               )
            ),

            # --- SEGREGATION TAB ---
            tabPanel(
               "Segregation Analysis",
               withSpinner(
                  DT::DTOutput("segregation_out"),
                  type = 7,
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.segregation_out",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Segregation Table"),
                     downloadButton("download_segregation", "download xlsx")
                  )
               )
            ),
            tabPanel(
               "Haplotype Strings",
               withSpinner(
                  DT::DTOutput("haplotype_string_out"),
                  type = 7,
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.haplotype_string_out",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Haplotype Strings Data"),
                     downloadButton("download_haplotype_string", "download xlsx")
                  )
               )
            ),
            tabPanel(
               "Haplotype Inference",
               withSpinner(
                  DT::DTOutput("em_out"),
                  type = 7,
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.em_out",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Haplotype Inference"),
                     downloadButton("download8xls", "download xlsx")
                  )
               )
            ),
            tabPanel(
               "Comparison",
               withSpinner(
                  DT::DTOutput("comparison_out"),
                  type = 7,
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.comparison_out",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Comparison"),
                     downloadButton("download9xls", "download xlsx")
                  )
               )
            )
            # tabPanel(
            #    "Downloads",
            #    div(
            #       style = "text-align: right;",
            #       helpText("Download Segregation Analysis Results"),
            #       downloadButton("downloadData1", "Download tsv"),
            #       br(), br(),
            #       helpText("Download Haplotype Inference Results"),
            #       downloadButton("downloadData3xls", "Download xlsx"),
            #       br(), br(),
            #       helpText("Download Haplotype Comparison"),
            #       downloadButton("downloadData5xls", "Download xlsx"),
            #       br(), br(),
            #       helpText("Download Complete Report"),
            #       downloadButton("downloadData6xls", "Download xlsx"),
            #       br(), br()
            #    )
            # )
         )
      )
   ),
   br(),

   # --- FOOTER ---
   tags$div(
      class = "footer",
      HTML('
         <div class="footer">
            Copyright &copy; Hoiley Sham 2026 |
            Department of Clinical Immunology; PathWest&#x00AE Laboratory Medicine;
            Government of Western Australia Department of Health.<br/>
            This application is for research and reference purposes only and it may contain
            links to embargoed or legally privileged data.
            Except as permitted by the copyright law applicable to you,
            you may not reproduce or communicate any of the content produced on this page,
            including files downloadable from this page, without written permission
            of the copyright owner(s) or authorised PathWest personnel.
            The user acknowledges that they are using <em>HLAhaploTools</em> at their own risk and
            they agree with the terms of use.
            <br/>
            This application is maintained by
            <a href="https://pathwest.health.wa.gov.au/Our-Services/Clinical-Services/Immunology"
            target="_blank">PathWest&#x00AE Immunology</a>.
            <br/>If you\'ve used <em>HLAhaploTools</em> to analyse your data, please cite:
            H. Sham, F. Mobegi, D. De Santis and D. Edwards <em>HLAhaploTools: A Bioinformatics Suite for Comprehensive Analysis of
            Classical and Non-Classical HLA Haplotypes in Extended Families</em>.
            <a href="https://github.com/h-sham/HLAhaploTools" target="_blank">Link</a>.
         </div>')
   )
)
