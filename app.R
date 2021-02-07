#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("noninferiority_bin.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
            # Application title
            titlePanel(" Dichotomous outcomes: Sample size, power and detectable difference for 2 group comparisons"),
        
            # Sidebar with a slider input for number of bins 
            sidebarLayout(
                sidebarPanel( 
                    radioButtons("hyp", "Hypothesis:",
                             choices = list (
                               "Superiority" =1,
                               "Non-inferiority" = 2), selected = 1
                    ),
                    conditionalPanel("input.hyp == 1",
                        radioButtons("type",  "What do you want to estimate",
                              choices = list(
                                  "Power" = 1,
                                 "Sample size" = 2,
                                 "Detectable difference" = 3), selected = 1
                         ),
                        conditionalPanel("input.type == 1",  # power
                            numericInput(
                            "n1", "Sample size (per group)", value = 100, min = 5, max = 100000),
                            numericInput(
                            "p11", "Prop group 1",value = 0.5,min = 0.001,max = 0.99),
                            numericInput(
                            "p21", "Prop group 2", value = 0.6, min = 0.001,max = 0.99),
                            numericInput(
                            "alpha1", "Type I error rate", min = 0.000000001, max = 0.2, value = .05)
                        ),
                        conditionalPanel("input.type ==2", #sample size
                            numericInput(
                                "pow2", "Power", value = 0.8, min = 0.1, max = 1),
                            numericInput(
                                "p12", "Prop group 1", value = 0.5, min = 0.001, max = 0.99),
                            numericInput(
                                "p22", "Prop group 2", value = 0.6, min = 0.001, max = 0.99),
                            numericInput(
                                "alpha2", "Type 1 error rate", value = 0.05, min=0,max=1)
                        ),
                        conditionalPanel("input.type ==3",
                            numericInput(
                                "n3", "Sample size (per group)", value = 100),
                            numericInput(
                                "pow3", "Power", value = 0.8),
                            numericInput(
                                "p", "Control group proportion", value = .05, min = 0.001, max = 0.99),
                            numericInput(
                                "alpha3", "Type 1 error rate", value = .05)
                        )
                    ),
                    conditionalPanel("input.hyp == 2",
                            numericInput("n4", "Sample size (per group)", value =100, min = 10, max = 100000),
                            numericInput("delta", "Non-inferiority margin", value = 0.2),
                            numericInput("ctrl", "Ctrl proporiton", value = 0.5),
                            numericInput("trtm", "Trtm proporiton", value = 0.5),
                            numericInput("nsim", "Number of simulations", value = 0.5),
                            numericInput("alpha4", "Type I error rate", min = 0.000000001, max = 0.5, value = .05)
                    ),
                    actionButton("do","Submit")
                ),
                
        
                # Show a plot of the generated distribution
                mainPanel(
                   plotOutput("distPlot"),
                  textOutput("txtOutput")
                )
            )
)

    



# Define server logic
server <- function(input, output) {

    observeEvent(input$do,{
        res <- if(input$hyp == 1 & input$type == 1){
                  power.prop.test(power= , n=input$n1, p1 = input$p11, p2 = input$p21, sig.level=input$alpha1)
               }
               else{
                   if(input$hyp == 1 & input$type ==2){
                       power.prop.test(n= NULL, p1 = input$p12,p2 = input$p22, power=input$pow2, sig.level = input$alpha2)
                   }
                   else{
                       if(input$hyp == 1 & input$type ==3){
                           power.prop.test(n=input$n3, power=input$pow3, p1 = input$p, p2=NULL, sig.level = input$alpha3)
                       }
                       else{
                        p <- pow_noninf_bin(numsims = input$nsim, nperarm = input$n4, ctrlrate = input$ctrl, trmtrate = input$trtm, delta = input$delta)
                        list(power = p, n = input$n4, sig.level = input$alpha4)
                          }
                   }
               }
        output$txtOutput = renderText({
          if(input$hyp ==1){
            paste0("A sample size of ", ceiling(res$n), " in each group will give the study ", round(res$power,2)*100," % power to detect a between-group absolute difference of ", round(res$p2 -res$p1, 2 )," with a type 1 error rate of ", res$sig.level)
          }
          else{
            paste0("A sample size of ", res$n, " in each group will give ", round(res$p*100,1), "% power to declare the intervention is non-inferior to the control with a type 1 error rate of ", round(res$sig.level*100,2), "%")
          }
        })
        
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
