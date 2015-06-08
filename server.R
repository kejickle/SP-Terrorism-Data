rm(list=ls())

require(shiny)
require(ggvis)
require(rworldmap)

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


# ----------------------------------------------------------

#Start the shiny server function
shinyServer(function(input,output){
  
  #Create a tooltip function to describe each data point for time series plot
  description = function(x){
    if(is.null(x)) return(NULL)
    paste0(format(x$country),"<br />",
           input$Inputx, ": ", format(x[,2]),"<br />",
           input$Inputy, ": ", format(x[,3]),"<br />",
           "Population: ", format(x$size),"<br />")
  }
  #---------------------------------------------------------------
  
# Start function for time series plot
  vis = reactive({
    
    #Obtain the year from the slider input in ui
    year = input$timeseries.year 
    size = population[,c("country",(paste("pop.",year,sep="")))]   
    
    #Get region, x, y, nation, and size variables for each year and create a data frame 
    timeSeries.current <- data[data$year==year,]
    df = subset(timeSeries.current, select = c(input$Inputx, input$Inputy, "country", "year", "region"))
    df = df[-(which(is.na(df[,c(1,2)]))),]
    region = subset(df, select=c("region", "country"))
    region = unique(region)
    df[which(df[,1]<0),1] = 0
    df[which(df[,2]<0),2] = 0
    x.current =  aggregate(df[,1]~country,df,sum)
    colnames(x.current)[2] = input$Inputx
    y.current = aggregate(df[,2]~country, df, sum)
    colnames(y.current)[2] = input$Inputy
    timeSeries.dat = merge(x.current, y.current, by = "country")
    timeSeries.dat = merge(timeSeries.dat, region, by = "country")
    timeSeries.dat = merge(timeSeries.dat, size, by = "country")
    colnames(timeSeries.dat)[5] = "size"
    timeSeries.dat$region = factor(timeSeries.dat$region)
    levels(timeSeries.dat$region) = c("North America", "Central America", "South America", "Asia", "Asia", "Asia", "Asia", "Europe", "Europe", "Africa",
                              "Africa", 'Russia', 'Australasia')
    
    # Subset data based on ylimit selections
    timeSeries.dat = timeSeries.dat[which(timeSeries.dat[,3]>input$ylim[1]),]
    timeSeries.dat = timeSeries.dat[which(timeSeries.dat[,3]<input$ylim[2]),]
    timeSeries.dat = timeSeries.dat[which(timeSeries.dat[,2]>input$xlim[1]),]
    timeSeries.dat = timeSeries.dat[which(timeSeries.dat[,2]<input$xlim[2]),]  
    
    #Create the time series plot with ggvis and the timeSeriesData dataframe 
    timeSeries.dat %>%
      ggvis(~timeSeries.dat[,2], ~timeSeries.dat[,3], key := ~country) %>%
      layer_points(size := ~size, fill = ~region, opacity := 0.8) %>% 
      scale_nominal("fill", label = "Region",
                    range = c('red', 'orange', 'yellow', 'lightgreen', 'lightblue', 'darkblue', 'purple', 'pink', 'brown')) %>%
      add_axis("x", title = input$Inputx, title_offset = 50) %>%
      add_axis("y", title = input$Inputy, title_offset = 60) %>%
      scale_numeric("y", domain=input$ylim) %>%
      scale_numeric("x", domain = input$xlim) %>%
      set_options(width = 500, height = 400) %>%
      add_tooltip(description, "hover")
  })
  
  # bind the time series plot to a name the ui will recognize 
  vis %>% bind_shiny("ggvisPlot")

  #have the codebook display what x variable is being plotted 
  output$textxvariable = renderText({ 
    paste("The x-variable tells us: ", codebook[which(codebook$Variable.Name==input$Inputx),2])
    })

  #have the codebook display what y variable is being plotted 
  output$textyvariable = renderText({ 
    paste("The y-variable tells us: ", codebook[which(codebook$Variable.Name==input$Inputy),2])
  })
  #---------------------------------------------------------------------
  
#Start function for mapping different variables   

#join data to a map
  output$map = renderPlot({
    #Obtain the year and variable to map depending on the user's choice 
    year = input$map.year
    
    gtd.recent <- data[data$year==year,]
    mapvariable = input$mapInput
    df = subset(gtd.recent, select = c(mapvariable, "country"))
    df = df[-(which(is.na(df[,1]))),]
    df[which(df[,1]==-99),1] = 0
    
    
    gtd.recent =  aggregate(df[,1]~country,df,sum)
    
    colnames(gtd.recent)[2] = mapvariable
    #join data to a map
    gtdMapData = joinCountryData2Map(gtd.recent, nameJoinColumn = "country", 
                                 joinCode = "NAME")
    
    #plot the map
    mapCountryData(gtdMapData, 
                    nameColumnToPlot=mapvariable,  
                    catMethod="fixedWidth", 
                     missingCountryCol = "white", mapTitle = mapvariable, 
                    numCats = 100, oceanCol = "lightblue"
                    )
    #get the attack points plotted on the map
    deaths = data[data$year==year,]
    
    deaths = deaths[which(deaths[,which(colnames(deaths) == mapvariable)] > 0),]
    mapBubbles(deaths,
              nameX='longitude',
              nameY='latitude', 
              nameZSize=input$mapInput, 
              nameZColour='black',
              fill=FALSE, 
              addLegend=FALSE, 
              add=TRUE)
  })

#have the codebook display the chosen variable for the mapping
output$textmapvariable = renderText({ 
  paste("The chosen variable tells us: ", codebook[which(codebook$Variable.Name==input$mapInput),2])
})

  #-----------------------------------------------------------------
  
  #Start function for regression 
  
  runRegression = reactive({
    #obtain the correct information for the chosen year 
    year = input$reg.year
    response <- data[data$year==year,]
    df = subset(response, select = c(input$regResponse, "country"))
    df = df[-(which(is.na(df[,1]))),]
    df[which(df[,1]==-99),1] = 0
    response =  aggregate(df[,1]~country,df,sum)    
    colnames(response)[2] = input$regResponse
    
    #read in the explanatory variables as options for the model 
    womenInGov = readRDS(file = "womenInGov.Rda")
    militarySpending = readRDS(file = "militarySpending.Rda")
    unemployment = readRDS(file="unemployment.Rda")
    w = womenInGov[, c(1,grep(year, colnames(womenInGov)))]
    m = militarySpending[, c(1,grep(year, colnames(militarySpending)))]
    u = unemployment[, c(1,grep(year, colnames(unemployment)))]
    
    #merge all the data together and get a subset for all the selected variables 
    merged = merge(w, m, by="country")
    merged = merge(merged, u, by = "country")
    merged = merge(merged, response, by="country")
    pred = input$regPredictors
    for (i in 1:length(pred)){
      pred[i] <- paste(pred[i], ".", year, sep="")
    }
    reg.data = subset(merged, select = c(pred, input$regResponse))
    
    #run the regression for the chosen variables 
    lm(as.formula(paste(colnames(reg.data)[length(colnames(reg.data))], "~", paste(colnames(reg.data)[-length(colnames(reg.data))], collapse ="+"))), data = reg.data)
  })

  #display the output for the regression model 
  output$regSummary = renderTable({
    if(!is.null(input$regPredictors)){
      summary(runRegression())$coef
    }
    else{
      print(data.frame(Warning ="Please select predictor variables"))
    }
  })

#interpret a coefficient of the users choice 
  output$interpretCoeff = renderText({ 
    if(runRegression()$coeff[grep(input$pred, names(runRegression()$coeff))] > 0){
    paste("For every 1 unit increase in ", input$pred, ", ", input$regResponse, " is expected to increase by ", signif(runRegression()$coeff[grep(input$pred, names(runRegression()$coeff))], 4), " units", sep = "")
    }
    else {
      paste("For every 1 unit increase in ", input$pred, ", ", input$regResponse, " is expected to decrease by ", signif(abs(runRegression()$coeff[grep(input$pred, names(runRegression()$coeff))]), 4),  " units", sep = "")
      
    } 
  })

#have plots displayed for the selected model 
output$regPlot = renderPlot ({
  if(!is.null(input$regPredictors)){
    par(mfrow=c(2,2))
    mod = runRegression()
    plot(mod, which=c(1,2,5,6))
  }
  else{
    print(data.frame(Warning ="Please select predictor variables"))
  }
})

#display the codebook
output$codebook = renderTable({
  print(codebook)
})


  
})