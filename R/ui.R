ui <- page_fluid(
   tags$head(
      tags$link(rel = "icon", sizes = "32x32", href = "favicon.ico"),
      includeCSS("www/styles.css"),
      tags$style(
         HTML("
            #title-container {
                display: flex;
                flex-direction: column;
                align-items: center;
                width: 100%;
                margin-bottom: 20px;
            }
            #logo-row {
                display: flex;
                justify-content: space-between;
                align-items: center;
                width: 100%;
                padding: 10px 0;
            }
            #logo, #uwa_logo {
                height: 100px;
                width: auto;
            }
            #title-text {
                text-align: center;
                width: 100%;
                margin-top: 10px;
                font-family: 'Arial', sans-serif;
                font-weight: bold;
            }
         ")
      )
   ),
   titlePanel(
      title = div(
         id = "title-container",
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
         div(
            id = "title-text",
            style = "background-color: #130605; padding: 3px; text-align: center;",
            h1(
               "EXTENDED HLA HAPLOTYPE RECONSTRUCTION",
               style = "color: #414a66;"
            )
         )
      ),
      windowTitle = "Extended HLA Reconstruction"
   ),
   theme = bslib::bs_theme(bootswatch = "yeti"),
   sidebarLayout(
      sidebarPanel(
         width = 2,
         position = c("left", "justify"),
         fluid = TRUE,
         helpText(a(HTML(
            "Program to reconstruct HLA haplotypes in family-based and population
            typing data.<br><br>
            For detailed usage information, please use the help link below.<br><br>"
         ))),
         helpText(h5(HTML("Upload files (Max 50Mb)"))),
         fileInput(
            "file",
            NULL,
            buttonLabel = "Browse",
            multiple = FALSE
         ),
         div(style = "margin-top: -25px; font-size: 10px;"),
         helpText(HTML("Acceptable formats: .xls, .xlsx, .csv, .txt and .tsv")),
         br(), br(),
         card(
            style = "margin-bottom: 5px; padding: 5px;",
            helpText(h6(HTML("Type of Data"), style = "margin-bottom: 5px;")),
            div(
               style = "margin-top: -10px; margin-bottom: -10px;",
               selectInput(
                  inputId = "data_type",
                  label = NULL,
                  choices = c("Auto-detect", "Family-based", "Population-based"),
                  selected = "Auto-detect"
               )
            ),
            helpText(h6(HTML("Allele Trimming"), style = "margin-bottom: 5px;")),
            div(
               style = "margin-top: -10px; margin-bottom: -10px;",
               radioButtons(
                  inputId = "trim_selection",
                  label = NULL,
                  width = "180px",
                  choices = list("No Trimming" = "no_trim", "Trimming" = "trim"),
                  selected = "no_trim"
               )
            )
         ),
         fluidRow(
            column(6, actionButton("Submit", "Submit", class = "btn-success")),
            column(6, actionButton("restbutton", "Reset")),
            br(),
            fluidRow(
               column(12, actionLink("help", HTML('<span style ="color: blue;">&quest;Help</span')))
            ),
            fluidRow(
               column(12, actionLink("terms_of_use_link", HTML('<span style="color: blue;">&#128712;Terms of Use</span>')))
            )
         )
      ),
      mainPanel(
         width = 10,
         tabsetPanel(
            id = "Dataset",
            tabPanel(
               "Segregation Analysis",
               withSpinner(
                  DT::DTOutput("seg_table"),
                  type = 7,
                  color = "#414a66",
                  hide.ui = TRUE
               )
            ),
            tabPanel(
               "Haplotype Inference",
               withSpinner(
                  DT::DTOutput("EM"),
                  type = 7,
                  color = "#414a66",
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.EM !== null && output.EM !== undefined && $('#EM').text().trim() !== ''",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download EM Inference"),
                     downloadButton("download8xls", "download xlsx")
                  )
               )
            ),
            tabPanel(
               "Comparison",
               withSpinner(
                  DT::DTOutput("comparison"),
                  type = 7,
                  color = "#414a66",
                  hide.ui = TRUE
               ),
               conditionalPanel(
                  condition = "output.comparison !== null && output.comparison !== undefined && $('#comparison').text().trim() !== ''",
                  div(
                     style = "text-align: right;",
                     helpText("Click to download Comparison"),
                     downloadButton("download9xls", "download xlsx")
                  )
               )
            ),
            tabPanel(
               "Downloads",
               div(
                  style = "text-align: right;",
                  helpText("Download Segregation Analysis Results"),
                  downloadButton("downloadData1", "Download tsv"),
                  br(), br(),
                  helpText("Download Haplotype Inference Results"),
                  downloadButton("downloadData3xls", "Download xlsx"),
                  br(), br(),
                  helpText("Download Haplotype Comparison"),
                  downloadButton("downloadData5xls", "Download xlsx"),
                  br(), br(),
                  helpText("Download Complete Report"),
                  downloadButton("downloadData6xls", "Download xlsx"),
                  br(), br()
               )
            )
         )
      )
   ),
   br(),
   tags$div(
      class = "footer",
      style = "background-color: #130605; padding: 3px; text-align: center;",
      HTML('
         <div class="footer" style="color: #414a66;">
            Copyright &copy; Hoiley Sham |
            Department of Clinical Immunology; PathWest&#x00AE Laboratory Medicine;
            Government of Western Australia Department of Health. <br/>
            This application (xx) is for research and reference purposes only and it may contain
            links to embargoed or legally privileged data.
            Except as permitted by the copyright law applicable to you,
            you may not reproduce or communicate any of the content produced on this page,
            including files downloadable from this page, without written permission
            of the copyright owner(s) or authorised PathWest personnel.
            The user acknowledges that they are using xx at their own risk and they agree with the terms of use.
            <br/>
            This application is maintained by
            <a href="https://pathwest.health.wa.gov.au/Our-Services/Clinical-Services/Immunology"
            target="_blank">PathWest&#x00AE Immunology</a>.
            <br/><br/>
            If you\'ve used xx to analyse your data, please cite:
            H. Sham, F. Mobegi and D. De Santis <em>Transplantation 2025</em>.
            <a href="www.xxxx.com.au" target="_blank">PMID:xxxxxxxx</a>.
         </div>')
   )
)
