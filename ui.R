library(shiny)
library(shinyBS)
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
      "MAX UnMix"),
  sidebarLayout(position="right",
    sidebarPanel(
                 checkboxInput('example.data','Use Example Data?', FALSE),
                 fileInput('file1', 'Choose File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Tab='\t'),
                              ','),
                selectInput("plot.type","Y-Axis Data:",
                            choices=c("magnetization data","coercivity spectra")),
                selectInput("scale","X-Axis Scale:",
                            choices=c("linear"="linear","log"="log")),
                hr(),
                p(tags$small("Below you can control the smoothing of your data. As a default, smoothing is performed on magnetization data 
                  before coercivity data is derived (see option 'Magnetization Smoother' below). There is an additional option to smooth the coercivity spectra 
                  derived from your raw magnetization data. Noisy datasets produce spurious features from smoothing that should be avoided if possible. It is suggested to 
                  test each method with variable smoothing factors to determine an adequate coercivity distribution for fitting.")),
                hr(),
                sliderInput("smooth.factor", "Smoothing Factor:", 
                            min = 0, max = 1, value = 0.0, step= 0.01),
                checkboxInput("smooth_magnetization","Magnetization Smoother (blue)",value = TRUE),
                checkboxInput("smooth_gradient","Coercivity Smoother (red)",value = FALSE),
                actionButton("exclude_toggle", "Toggle points"),
                actionButton("exclude_reset", "Reset"),
                hr(),
                p(tags$small("The options above allow you to remove outlier data points that may have a large influence on your smoothing. Each data point can be removed individually
                  by clicking on the plot. To remove a portion of the data, click and drag over the desired data and select 'Toggle'. To clear all edits to the data, select 'Reset'")),
                hr(),
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
                               ))
                 ), #sidebar panel
    mainPanel(
      tags$div(
        tags$p("Welcome to MAX UnMix. If it is your first time using the program it will be helpful to visit the 'Resources' page where there are
              instructional videos and the research paper describing this program. If you're familiar with 
               unmixing coercivity data already, feel free to begin by following the instructions on the individual pages."),
        hr(),
        tags$p(tags$b("Operating Instructions")),
      tags$p(tags$small("Use the control panel on on the right side of the page to upload your data.
             Data for upload should represent either demagnetization or acquisition data. If you would like to explore the program without
             uploading data please select the 'Use Example Data?' option. Be sure to deselect this option when you are ready to upload data.")),
      tags$p(tags$small("To upload your data, select the 'choose file' option at right. You will need to then
             select if your data file has a header or not and what type of separator is being used. It is recommended
             to upload either .csv or .txt files.")),
      tags$p(tags$small(tags$b("NOTE:"),"Your data file should have field values in column 1 and magnetization values in column 2")),
      tags$p(tags$small("Once you've uploaded your data you can view both magnetization data and the coercivity spectra on 
             either log or linear scales by selecting options from the drop down menus"))
      ),
      hr(),
      fluidRow(
        column(width=2,
          tableOutput("contents")),
        column(width=9,offset=1,
          plotOutput("plot1",click="plot1_click",brush = brushOpts(
            id = "plot1_brush"
          ))
          )
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
                         numericInput("max.scale.log","Maximum Log Field:",value = 3.0),
                         checkboxInput("comp1", label = actionButton("save_1","Component 1 - click to save"), value = TRUE),
                         tags$head(tags$script(src = "message-handler.js")),
                         checkboxInput("comp2", label = actionButton("save_2","Component 2 - click to save"), value = FALSE),
                         checkboxInput("comp3", label = actionButton("save_3","Component 3 - click to save"), value = FALSE),
                         checkboxInput("comp4", label = actionButton("save_4","Component 4 - click to save"), value = FALSE),
                         checkboxInput("comp5", label = actionButton("save_5","Component 5 - click to save"), value = FALSE),
                         checkboxInput("comp6", label = actionButton("save_6","Component 6 - click to save"), value = FALSE),
                         bsModal("savename1","Enter Component Name","save_1",size="small",textInput("name1","Component Name?",value = "Component 1")),
                         bsModal("savename2","Enter Component Name","save_2",size="small",textInput("name2","Component Name?",value = "Component 2")),
                         bsModal("savename3","Enter Component Name","save_3",size="small",textInput("name3","Component Name?",value = "Component 3")),
                         bsModal("savename4","Enter Component Name","save_4",size="small",textInput("name4","Component Name?",value = "Component 4")),
                         bsModal("savename5","Enter Component Name","save_5",size="small",textInput("name5","Component Name?",value = "Component 5")),
                         bsModal("savename6","Enter Component Name","save_6",size="small",textInput("name6","Component Name?",value = "Component 6")),
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
                           tags$p(tags$small(tags$b("Step 1:"),"enter your maximum field value in the upper left corner, using either log or linear option")),
                           tags$p(tags$small(tags$b("Step 2:"),"Select any combination of the 6 components listed at left. When you select a component, a panel will appear at the 
                                  right side of the screen that will enable you to set values for that components parameters. Note that this panel is movable by clicking and dragging.")),
                           tags$p(tags$small(tags$b("Step 3:"),"Use the sliders to adjust the mean coercivity, dispersion, relative proportion, and skewness
                                  for each of the selected components. As you adjust these parameters you will be able to watch you model (represented in orange) begin to replicate 
                                  your data (shown as black data points; Note that all fitting is perfomed relative to your spline fit. If you selected a high smoothing factor 
                                  on the previous page your measured data will be visible in the background as light grey data points). As you adjust your model, pay attention to the residual sum 
                                  square value listed at left. The goal of component fitting is to minimize this value.")),
                           tags$p(tags$small("Within a session it is possible", tags$b("to save components"),". Simply click the button for each component and enter a name in the pop up window. When you upload a new 
                                             dataset or change any inputs you can select the 'revert to last save' option to recover your saved component.")),
                           tags$p(tags$small(tags$b("Step 4:"),"When you are comfortable with you initial fit, proceed to the 'Optimization' page.")),
                           tags$p(tags$small("The table at left lists a series of common magnetic components that have been identified by previous work (see Elgi, 2003 and 2004 parts 1-3 linked in the Resources tab). These 
                                  are included here for reference, but have no bearing on the actual analysis and do not necessarily correspond to the components available to select above."))
                           )
                         ),
                  absolutePanel(top = 100, right = 10,
                                tags$p(tags$b("Note:"),"click and drag to move this panel."),
                                checkboxInput("skew.option",label="Click to remove skewness from analysis"),
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
                                   tags$div(
                                     tags$p(tags$small("The plot above represents the optimal fit achieved by MAX UnMIX given the inital parametes you 
                                            set on the 'Fitting' page. The RSS is displayed in the upper right corner. Comparison of this RSS 
                                            value to the RSS value on the 'Fitting' page should confirm that your inital fit has now imporved.")),
                                     tags$p(tags$small("It is advised, unless you have already analyzed a number of similar samples, to spend time comparing various models 
                                            (e.g., 2 component vs. 3 component) using this page and the 'Fitting' 
                                            page. In order to compare fits follow the steps below.")),
                                     tags$p(tags$small(tags$b("Step 1:"),"Copy the RSS value and paste it in the box labeled 'paste RSS here for comparison'.")),
                                     tags$p(tags$small(tags$b("Step 2:"),"Return to the 'Fitting' page and either adjust the initial parameter values for your components or add/remove components. It is suggested to use new components and 'uncheck' the components
                                            used in previous models. Doing this will save the initial parameters from previous models in the case that you'd like to revert to that model. Once you've constructed a new model that 
                                            you would like to test against your previous model return to this page.")),
                                     tags$p(tags$small(tags$b("Step 3:"),"The results that are displayed now represent your most current model optimized from your updated inital parameters. Compare the new RSS with the one you copied from your previous fit.
                                             If the RSS has decreased, your new model has a better fit with your data. If the new model produces a higher RSS value, you can revert to your previous model by returning to the 'Fitting' page and selecting the proper components.")),
                                     tags$p(tags$small(tags$b("Step 4:"),"When you have arrived at a model that provides a robust fit statistically and can be reasonably interpreted in light of prior knowledge about your samples magnetic mineralogy proceed to the 'Error Analysis' page.")),
                                     hr()
                                     )
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
                          p(tags$small("This F-test is testing the Null hypothesis that your less complicated model is best. If the
                            reported F-statistic is much greater than 1, and the p-value is less than 0.05 you can confidently reject the Null
                            hypothesis and support a more complicated model."))
                          ))
               ) #fluidPage
      ), #tabPanel
      tabPanel("Error Analysis",
               fluidPage(
                 fluidRow(
                   column(4,
                          actionButton("calc.error","START"),
                          hr(),
                          sliderInput("n.resample","Number of Resamples:",value = 100,min = 100,max = 1000, step = 100),
                          sliderInput("p.r","Select Proportion:",value = 0.95, min = 0.80, max = 1.0, step = 0.05),
                          hr(),
                          textInput("file.name", "File Name:", value = "Enter file name"),
                          downloadButton('export.data', 'export results in log units'),
                          downloadButton('export.eps','export eps'),
                          downloadButton('export.extra.data','export results in normal units'),
                          downloadButton('export.png','export png'),
                          hr(),
                          tags$div(
                            tags$p(tags$small("To perform error analysis, click the 'START' button above. You may vary your number of resample event from 100 (default) to 1000.")),
                            tags$p(tags$small("You are also given the option to vary the proportion of data that is being using during each resample. This routine may become more 
                                   important if unmixing is being performed on datasets with large field increments.")),
                            tags$p(tags$small("If the error analysis is finished and you are satified with your results you may export the data file that is printed in the results window.
                                   Simply type in a file name (without extension) and click 'export data file'. A .csv file of your results will download to your 
                                   computers download directory. In future versions of MAX UnMix it will be possible to export the final figure as a .eps file, stay tuned!"))
                            )
                          ),
                   column(6,
                          plotOutput("plot4"),
                          column(12,
                                 h5("Results:"),
                                 verbatimTextOutput('final.results'),
                                 verbatimTextOutput('extra.results')
                                )
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
               # uiOutput('pdfviewer'),
               h5("Data Upload tutorial:"),
               br(),
               tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/kBNVckatLRI"),
               hr(),
               h5("Fitting and Optimization tutorial:"),
               br(),
               tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/xQ8Vb-dn1c4"),
               h5("Error Analysis and Data Export tutorial:"),
               br(),
               tags$iframe(width="560", height="315", src="https://www.youtube.com/embed/dZFxlIL3gho")
               ),
             sidebarPanel(
               h3("Resources:"),
               a("User Manual - MAX UnMix",href="http://danielmaxbauer.weebly.com/uploads/3/9/8/4/39840459/manual.pdf",targer="_blank"),
               hr(),
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

 