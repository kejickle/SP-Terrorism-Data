rm(list=ls())

require(shiny)
require(ggvis)

#load the terrorism data 
data = readRDS(file = "terrorismData.Rda")

#read in the codebook for all the variables in the terrorism data 
codebook = read.csv("codebook.csv")

# Population
population = read.csv("population.csv", stringsAsFactors=F)
colnames(population)[1] = "country"
population$country[which(population$country=="Ngorno-Karabakh")] = "Nagorno-Karabakh"
population = population[order(population$country),]
population = population[,c(1,which(colnames(population)=="X1970"):which(colnames(population)=="X2012"))]
population[,2:44]=lapply(population[,2:44], function(x) {as.numeric(gsub(",", "", x))})
for (i in 2:44){
  colnames(population)[i] <- paste("pop.", 1968+i, sep="")
}    
# Apply appropriate scaling to the population data. 
population[2:44] = population[2:44]/10e4


#-------------------------------------------------

#get the numeric values that should be used in the application as choices for the user to select 

numeric.variables = c("number.participating.perpetrators", "number.perpetrators.captured", "number.killed", "number.US.killed", "number.perpetrators.killed",        
                      "number.injured", "number.US.injured", "number.perpetrators.injured",  "damage.property.value", "number.hostages.kidnappings", 
                      "number.hostages.kidnappings.US", "hours.hostage.kidnap", "days.hostage.kidnap", "ransom.paid")

#Start shiny UI function and set up tabs for time series plot, mapping, regression, and displaying the codebook 
shinyUI(navbarPage("Visualizing Terrorism Data", 
                   tabPanel("Time Series Plot",
                            fluidRow(
                              column(4,
                                     sliderInput(inputId="timeseries.year", label="Select Year or press play:",
                                                 min=1970, max=2011, value=1994, step = 1,sep="",
                                                 animate=animationOptions(interval=700,loop=F)
                                     ),
                                     selectInput("Inputx", "Choose the x-variable:", choices = numeric.variables),
                                     selectInput("Inputy", "Choose the y-variable:", choices = numeric.variables),
                                     sliderInput(inputId="ylim", label="Y-Variable Range:",
                                                 min=0, max=3500, value=c(0,500), step = 25),
                                     sliderInput(inputId="xlim", label="X-Variable Range:",
                                                 min=0, max=3500, value=c(0,500), step = 25)
                                                
                                    ),
                                     column(8,
                                     ggvisOutput("ggvisPlot"),
                                     br(),
                                     wellPanel(
                                       span("This graph shows a scatter plot of the selected variables over 1970-2011"),
                                       br(),
                                       textOutput('textxvariable'),
                                       textOutput('textyvariable'),
                                       span("Population is in terms of millions of people")
                                     )
                                     )
                              )
                            ),
                   tabPanel("Mapping Numeric Variables", 
                            fluidRow(
                              column(4,
                                     sliderInput(inputId="map.year", label="Select Year:",
                                                 min=1970, max=2013, value=1994, step = 1,sep="",
                                                 animate=animationOptions(interval=2000,loop=F)),
                                     selectInput("mapInput", "Choose the variable to map:", choices = numeric.variables)                                     
                              ),
                              column(8,
                                     plotOutput("map"),
                                     wellPanel(
                                       span("This tab provides the user numerical variables to chose from to map over 1970-2013"),
                                       br(),
                                       textOutput('textmapvariable'))
                              ))),
                   tabPanel("Regression",
                            column(4, 
                                   selectInput(inputId="reg.year", label="Select Year",
                                               choices = seq(from = 2000, to = 2011)),
                                   
                                   selectInput("regPredictors", "Choose the predictor variable(s):", selected=c("womenInGov", "militarySpending", "unemployment"), choices = c("womenInGov", "militarySpending", "unemployment"), multiple = T),
                                   selectInput("regResponse", "Choose the response variable:", choices = numeric.variables),
                                   selectInput("pred", "Choose a predictor variable coefficient to interpret:", choices = c("womenInGov", "militarySpending", "unemployment"))
                                   ),       
                            column(7, 
                                   tableOutput("regSummary"),
                                    br(),
                                   plotOutput("regPlot"),
                                   br(),
                                    textOutput("interpretCoeff"))),
                   tabPanel("Code Book", 
                            tableOutput("codebook"))
))
