library(shiny)

row <- function(...) {
  tags$div(class="row", ...)
}

col <- function(width, ...) {
  tags$div(class=paste0("span", width), ...)
}
shinyUI(navbarPage("MAX UnMix",           
    tabPanel("Data Upload",
  fluidPage(
    titlePanel(
      "MAX UnMix: A simple program to unmix magnetic data"),
  sidebarLayout(position="right",
    sidebarPanel(
                 checkboxInput('example.data','Use Example Data?', FALSE),
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                # tags$hr(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                radioButtons('B.units',"Field Units",
                             c("millitesla" = "mT",
                               Tesla = "T",
                               Oersted = "oe",
                               Gauss = "G",
                               "A/m" = "A/m")
                               ),
                
                radioButtons('M.units',"Magnetization Units",
                             c("Am2/kg" = "Am2/kg",
                               Am2 = "Am2",
                               "A/m" = "A/m",
                               emu = "emu",
                               "emu/cm3" ="emu/cm3",
                               "emu/gm" ="emu/gm"
                               )),
                 selectInput("plot.type","View Plot:",
                             choices=c("magnetization data","coercivity spectra")),
                 hr(),
                p("Below you can select a smoothing factor. In the case that this parameter is set to zero, 
                  the spline will match your data exactly. Adjust this parameter to smooth out noise in measured data, 
                  but be aware of spurious features that can be produced as a result."),
                hr(),
                 sliderInput("smooth.factor", "Smoothing Factor:", 
                             min = 0, max = 1, value = 0.0, step= 0.01),
                 selectInput("scale","Select Scale:",
                             choices=c("linear"="linear","log"="log")),
                 hr(),
                p(tags$b("NOTE:"),"If data does not read in properly, ensure that the correct separator is selected. 
                  Prior to proceeding to the 'Fitting' page, ensure that your selected scale is logarithmic")
             
                 ), #sidebar panel
    mainPanel(
      tags$div(
        tags$p("Welcome to MAX UnMix. This web application is designed to make unmixing rock magnetic data more intuitive and accessible. If it is your first time
               using the program it might be helpful to visit the 'Resources' page where there are links to instructional videos and (eventually) the research paper describing this program. If you're familiar with 
               unmixing magnetic data already, feel free to begin by following the instructions on the individual pages."),
        hr(),
        tags$p(tags$b("Operating Instructions")),
      tags$p("Use the control panel on on the right side of the page to upload your data.
        Please upload a file of either demagnetization or acquisition data that has field values all > 0. If you would like to explore the program without
             uploading data please select the 'Use Example Data?' option. Be sure to deselect this option when you are ready to upload data."),
      tags$p("To upload your data, select the 'choose file' option at right. You will need to then
             select if your data file has a header or not and what type of separator is being used. It is recommended
             to upload either .csv or .txt files."),
      tags$p(tags$b("NOTE:"),"Your data file should have field values in column 1 and magnetization values in column 2"),
      tags$p("Once you've uploaded your data you can view both magnetization data and the coercivity spectra on 
             either log or linear scales by selecting options from the drop down menus")
      ),
      hr(),
      fluidRow(
        column(width=2,
          tableOutput("contents")),
        column(width=9,offset=1,
          plotOutput("plot1"))
      )#fluid row
  ) #main panel
  ) #sidebarLayout
  ) #fluid page 
  ), #tabPanel
    tabPanel("Fitting",
              fluidPage(
               # titlePanel("Component Fitting"
                #  ), #titlePanel
                fluidRow(
                  column(3,
                         numericInput("max.scale","Maximum Field:",value = 300),
                         numericInput("max.scale.log","Maximum Log Field:",value = 2.0),
                         checkboxInput("comp1", label = "component 1", value = TRUE),
                         checkboxInput("comp2", label = "component 2", value = FALSE),
                         checkboxInput("comp3", label = "component 3", value = FALSE),
                         checkboxInput("comp4", label = "component 4", value = FALSE),
                         checkboxInput("comp5", label = "component 5", value = FALSE),
                         checkboxInput("comp6", label = "component 6", value = FALSE),
                         hr(),
                         h4("Residual Sum Square (RSS):"),
                         verbatimTextOutput('RSS.plot2'),
                         h4("Common Components:"),
                         tableOutput("common_components")
                         ),
                  column(6,
                         plotOutput("plot2",click="plot_click"),
                         hr(),
                         tags$div(
                           tags$p(tags$b("Step 1:"),"enter your maximum log field value in the upper left corner"),
                           tags$p(tags$b("Step 2:"),"You may select any combination of the 6 components listed at left. When you select a component, a panel will appear at the 
                                  right side of the screen that will enable you to set values for that components parameters."),
                           tags$p(tags$b("Step 3:"),"Use the sliders on the right hand panels to adjust the mean coercivity, dispersion, and relative contribution 
                                  for each of the selected components. As you adjust these parameters you will be able to watch you model (represented in orange) begin to replicate 
                                  your data (shown as black data points; Note that all fitting is perfomed relative to your spline fit. If you selected a high smoothing factor 
                                  on the previous page your measured data will be visible in the background as light grey data points). As you adjust your model, pay attention to the residual sum 
                                  square value listed at left. The goal of component fitting is to minimize this value."),
                           tags$p(tags$b("Step 4:"),"When you are comfortable with you initial fit, proceed to the 'Optimization' page."),
                           tags$p(tags$b("NOTE:"),"The panel at right can be dragged around to ensure that your plot is visible will adjusting component parameters. Simply
                                  click anywhere on the panel (except on the slider handles) and drag."),
                           tags$p("The table at left lists a series of common magnetic components that have been identified by previous work (see Elgi, 2003 and 2004 parts 1-3 linked in the Resources tab). These 
                                  are included here for reference, but have no bearing on the actual analysis and do not necessarily correspond to the components available to select above.")
                           )
                         ),
                  absolutePanel(top = 100, right = 10,
                                draggable = TRUE,
                                uiOutput("ui.comp1"),
                                uiOutput("ui.comp2"),
                                uiOutput("ui.comp3"),
                                uiOutput("ui.comp4"),
                                uiOutput("ui.comp5"),
                                uiOutput("ui.comp6")
                                )
                  )                    
                )), #fluidPage
      tabPanel("Optimization",
               fluidPage(
                 fluidRow(
                   column(8,
                          plotOutput("plot3"),
                          fluidRow(
                            column(12,
                                   verbatimTextOutput('results'),
                                   hr(),
                                  # actionButton("Pi.reset","Click to temporarily save results"),
                                  # actionButton("reset","Click to revert"),
                                  # actionButton("newmodel","Click to calculate new model"),
                                  # hr(),
                                   tags$div(
                                     tags$p("The plot above represents the optimal fit achieved by MAX UnMIX given the inital parametes you 
                                            set on the 'Fitting' page. The RSS is displayed in the upper right corner. Comparison of this RSS 
                                            value to the RSS value on the 'Fitting' page should confirm that your inital fit has now imporved."),
                                     tags$p("It is advised, unless you have already analyzed a number of similar samples, to spend time comparing various models 
                                            (e.g., 2 component vs. 3 component) using this page and the 'Fitting' 
                                            page. In order to compare fits follow the steps below."),
                                     tags$p(tags$b("Step 1:"),"Copy the RSS value and paste it in the box labeled 'paste RSS here for comparison'."),
                                     tags$p(tags$b("Step 2:"),"Return to the 'Fitting' page and either adjust the initial parameter values for your components or add/remove components. It is suggested to use new components and 'uncheck' the components
                                            used in previous models. Doing this will save the initial parameters from previous models in the case that you'd like to revert to that model. Once you've constructed a new model that 
                                            you would like to test against your previous model return to this page."),
                                     tags$p(tags$b("Step 3:"),"The results that are displayed now represent your most current model optimized from your updated inital parameters. Compare the new RSS with the one you copied from your previous fit.
                                             If the RSS has decreased, your new model has a better fit with your data. If the new model produces a higher RSS value, you can revert to your previous model by returning to the 'Fitting' page and selecting the proper components."),
                                     tags$p(tags$b("Step 4:"),"When you have arrived at a model that provides a robust fit statistically and can be reasonably interpreted in light of prior knowledge about your samples magnetic mineralogy proceed to the 'Error Analysis' page."),
                                     hr()
                                     )
                                   #textInput("Reset.Pi","Paste results here to save for comparison:",value="")
                            )     
                            )),
                   column(4,
                          h5("Residual Sum Square (RSS):"),
                          verbatimTextOutput('RSS.plot3'),
                          hr(),
                          textInput("RSS.op1","Paste RSS here for comparison:",value=""),
                          sliderInput("n.comp2","Select number of components:",value = 2,min = 1,max = 6, step = 1),
                          hr(),
                          checkboxInput("ftest","Perform F-test?",value = FALSE),
                          h5("F-statistic"),
                          verbatimTextOutput('f.val'),
                          h5("p-value"),
                          verbatimTextOutput('p.val'),
                          p("This F-test is testing the Null hypothesis that your less complicated model is best. If the
                            reported F-statistic is much greater than 1, and the p-value is less than 0.05 you can confidently reject the Null
                            hypothesis and support a more complicated model.")
                          ))
               ) #fluidPage
      ), #tabPanel
      tabPanel("Error Analysis",
               fluidPage(
                 fluidRow(
                   column(4,
                          checkboxInput("calc.error","Perform error analysis?", value = FALSE),
                          sliderInput("n.resample","Number of Resamples:",value = 100,min = 100,max = 10000, step = 100),
                          sliderInput("p.r","Select Proportion:",value = 0.95, min = 0.80, max = 1.0, step = 0.05),
                          hr(),
                          textInput("file.name", "File Name:", value = "Enter file name"),
                          downloadButton('export.data', 'export results in log units'),
                          downloadButton('export.eps','export eps'),
                          downloadButton('export.extra.data','export results in normal units'),
                          downloadButton('export.png','export png'),
                          hr(),
                          tags$div(
                            tags$p("To perform error analysis check box labeled 'Perform error analysis?'. It is recommended to keep the number of resamples at 100 unless 
                                   actively assessing how stable your model is. Typical calculation times vary from ~8 seconds for 100 resamples, ~ 90 seconds for 1,000 resamples,
                                   and ~20 minutes for 5,000 resamples."),
                            tags$p("You are also given the option to vary the proportion of data that is being using during each resample. This routine may become more 
                                   important if unmixing is being performed on datasets with large field increments."),
                            tags$p("If the error analysis is finished and you are satified with your results you may export the data file that is printed in the results window.
                                   Simply type in a file name (without extension) and click 'export data file'. A .csv file of your results will download to your 
                                   computers download directory. In future versions of MAX UnMix it will be possible to export the final figure as a .eps file, stay tuned!")
                            )
                          ),
                   column(6,
                          plotOutput("plot4"),
                          column(12,
                                 h5("Results:"),
                                 verbatimTextOutput('final.results'),
                                 verbatimTextOutput('extra.results'))
                          )
                  
                   
                   )
                 )),
  tabPanel("Resources",
           fluidPage(
             mainPanel(
               h3("User Videos"),
               p("Below are some brief instructional videos to help get you started with unmixing your magnetic data. Links to previous magnetic unmixing methods and application papers 
                  are provided in the sidebar. This program is still in its infancy so please report any issues you come across to 
                 Dan Maxbauer (maxba001@umn.edu). Enjoy the unmixing!"),
               hr(),
               h5("Data Upload tutorial:"),
               br(),
               tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/vUcBN6mXmNs"),
               hr(),
               h5("Fitting and Optimization tutorial:"),
               br(),
               tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/1qPbpg1sgqM"),
               h5("Error Analysis and Data Export tutorial:"),
               br(),
               tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/4o9uivgk994")
               ),
             sidebarPanel(
               h3("Resources:"),
               p("Listed below are references to previous studies and methods for unmixing magnetic data."),
               a("Kruiver et al. (2001) IRM-CLG Model Description", href="http://www.sciencedirect.com/science/article/pii/S0012821X01003673",targer="_blank"),
               br(),
               a("Heslop et al. (2002) IRM UnMiX Model Description", href="http://gji.oxfordjournals.org/content/148/1/58.short",targer="_blank"),
               br(),
               a("Egli (2003) MAG-MIX Model Description", href="http://onlinelibrary.wiley.com/doi/10.1029/2002JB002023/full",targer="_blank"),
               br(),
               a("Egli (2004) part 1", href="http://link.springer.com/article/10.1023/B:SGEG.0000020839.45304.6d",target="_blank"),
               br(),
               a("Egli (2004) part 2", href="http://www.sciencedirect.com/science/article/pii/S1474706504001299",target="_blank"),
               br(),
               a("Egli (2004) part 3", href="http://www.sciencedirect.com/science/article/pii/S1474706504001184",target="_blank")
             )
             ) #fluidPage    
             ) 

))

 