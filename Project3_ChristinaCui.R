#loads in packages needed
library(shiny)
library(rsconnect)
library(dplyr)
library(ggplot2)
library(lubridate)
library(knitr)

#reads in the music dataset
music <- read.csv("mxmh_survey_results.csv")

#renaming the variables in the music dataset to follow standard naming conventions
music <- music %>%
  rename(age = Age, hours = Hours.per.day, depression = Depression, musicEffects = Music.effects, favGenre = Fav.genre, freqClassical = Frequency..Classical., freqRB = Frequency..R.B., freqKpop = Frequency..K.pop., freqLofi = Frequency..Lofi.)

#removing observations with NA values under the BPM variable
music <- music[!is.na(music$BPM), ]

#removing observations with blank values under the musicEffects variable
music <- filter(music, music$musicEffects != "")

#creating empty vectors to put the variables in later
months <- character(0)
times <- c()
days <- character(0)
timeStamps <- character(0)

#loops through entire Timestamp variable to get all the months and hours of the observations
for (i in 1:nrow(music)) {
  dt <- mdy_hms(music$Timestamp[i])
  
  #adding a common column Timestamps to merge with other dataset later
  timeStamps <- c(timeStamps, music$Timestamp[i])
  
  #gets the name of the month and adds to the empty character vectors
  monthName <- month.name[month(dt)]
  months <- c(months, monthName)
  
  #adds the hour to the empty character vectors
  times <- c(times, hour(dt))
  
  #gets the name of the day and adds to the empty character vectors
  dayName <- weekdays(dt)
  days <- c(days, dayName)
}

#adds the months and hours variables into a new datesAndTimes dataframe
datesAndTimes <- data.frame(month = months, hourOfTime = times, dayOfWeek = days, Timestamp = timeStamps)

#creates a subset of the cleaned dataset that only includes the variables that will be analyzed
subsettedMusic <- music %>%
  select(age, hours, depression, musicEffects, favGenre, freqClassical, freqRB, freqLofi, freqKpop, BPM, Timestamp)

#merges the datasets together by column "Timestamp"
cleanMusic <- merge(subsettedMusic, datesAndTimes, by = "Timestamp")

#removes the Timestamp variable because I will not be analyzing it in the project
cleanMusic <- subset(cleanMusic, select = -Timestamp)

#defines the UI for the application
ui <- fluidPage(
  
  #title of shiny app
  titlePanel("Music Versus Mental Health"),
  
  #sidebar layout including instructions, sliders for bin widths and data ranges,
  #drop menus to select variables to plot and colors for plots, buttons to generate and 
  #clear plots, checkboxes to show/hide descriptive statistics and variable descriptions
  sidebarLayout(
    sidebarPanel(
      
      #instructions for users
      p("Instructions: Select one or two variables from the drop menus below. 
      Hit the button 'Generate Plot' to generate a plot for the user's selected variable(s). 
      Hit the 'Clear Plot' button to reset the plot and to be able to choose new variables to plot.
      Choose a color for the plot under the drop menu. The user can adjust the bin width of histograms
      using the slider tool below. The second slider widget allows for the user to determine the
      data range for the x axis of the plots; the user can adjust the range of the data displayed. 
      The user can also choose to display descriptive statistics of the
      selected variable(s). The user can also choose to display a table containing a list of all the
      variables, their types, and descriptions of the variables. Depending on the device the user
      is viewing this on, the user may need to scroll down to see output descriptions, descriptive statistics,
      and variable descriptions."),
      
      #input fields for choosing variables
      selectInput("variable1", "Choose A First Variable", choices = colnames(cleanMusic)),
      selectInput("variable2", "Choose A Second Variable (optional)", choices = c("", colnames(cleanMusic))),
      
      #input field allowing user to choose a color for plots
      selectInput("color", "Choose a Color Input", choices = c("rosybrown3", "lightskyblue3", "aquamarine3", "plum2")),
      
      #button to generate plot
      actionButton("plot_btn", "Generate Plot"),
      
      #button to clear plot
      actionButton("clear_btn", "Clear Plot"),
      
      #condition to make sure user chose variable(s)
      conditionalPanel(
        condition = "input.variable1 !== '' && typeof input.variable1 !== 'undefined' 
        && typeof cleanMusic[input.variable1] !== 'undefined' && typeof cleanMusic[input.variable1][0] === 'number'",
        
        #slider input allowing user to adjust width of bins in histograms
        sliderInput("bins",
                    "Bin Width for Histograms",
                    min = 1,
                    max = 30,
                    value = 1),
        
        #condition to make sure user chose variable(s)
        conditionalPanel(
          condition = "input.variable1 !== '' && typeof input.variable1 !== 'undefined' && 
          typeof cleanMusic[input.variable1] !== 'undefined'",
          
          #slider input allowing user to adjust the range of the data displayed in the plots
          sliderInput("range",
                      "Data Range",
                      min = 1,
                      max = 250,
                      value = 1),
        
        #option to show descriptive statistics
        checkboxInput("checkbox1", label = "Display Descriptive Statistics", value = FALSE),
        
        #option to show variable descriptions
        checkboxInput("checkbox2", label = "Display Variable Descriptions", value = FALSE),
        
      )  
      )
      
    ),  
    
    
    #main panel showing image, plots, descriptive statistics,
    #variable descriptions, and output descriptions
    mainPanel(
      
      #image
      div(
        img(src = "https://cdn.punchng.com/wp-content/uploads/2022/03/28122921/Brain-Train-Blog-Image-2.jpg", width = 400, height = 300),
        style = "text-align: center;"
        ),
      
      hr(),
      
      #cites the image
      div("'Music Improves Mental Health.' https://www.google.com/url?sa=i&url=https%3A%2F%2Fpunchng.
          com%2Fmusic-improves-mental-health-report%2F&psig=AOvVaw2TJ4TaYrz2tfmiLkqaqV9n&ust=16993918
          90751000&source=images&cd=vfe&opi=89978449&ved=0CBMQjhxqFwoTCLjyr6KmsIIDFQAAAAAdAAAAABAE.",
          style = "text-align: center; font-size: 8px;"
      ),
      hr(),
      
      #displays the plots
      plotOutput("varPlot"),
      hr(),
      
      #displays descriptive statistics
      p('Descriptive Statistics:'),
      fluidRow(column(12, verbatimTextOutput("descStats"))),
      
      #displays plot descriptions
      p("Plot Description:"),
      fluidRow(column(12, verbatimTextOutput("plotDesc"))),
      
      #displays variable descriptions
      p('Variable Descriptions:'),
      fluidRow(column(12, verbatimTextOutput("varDesc"))),
      
    )
  )
)


#defines the server function to produce the plots/graphs and other outputs
server <- function(input, output) {
  
  #clears the plot when clear plot button is clicked
  observeEvent(input$clear_btn, {
    output$varPlot <- renderPlot(NULL) 
  })

  #renders the plot when the generate plot button is clicked
  observeEvent(input$plot_btn, {
    output$varPlot <- renderPlot({

      #renames input$variable1 and input$variable2 to make it easier to code
      var1 <- input$variable1
      var2 <- input$variable2
      co <- input$color
      
      #checks for when the user does not choose a second variable
      if(var2 == "")
        {
            
        #if the user selected a character variable/categorical variable
          if (is.character(cleanMusic[[var1]])) 
            {
            
            #creates a barplot of the user's selected categorical variable
            ggplot(cleanMusic, aes(x = !!sym(var1))) +
              geom_bar(fill = co, color = "black") +
              labs(title = paste("Barplot of", var1), x = var1, y = "Count") + 
              coord_cartesian(xlim = c(0, input$range))
            } 
          
        #if the user selected a numerical variable 
          else if (is.numeric(cleanMusic[[var1]])) 
            {
            
            #creates a histogram of the user's selected numerical variable
            ggplot(cleanMusic, aes(x = !!sym(var1))) +
              geom_histogram(fill = co, color = "black", binwidth = input$bins) +
              labs(title = paste("Histogram of", var1), x = var1, y = "Frequency") + 
              coord_cartesian(xlim = c(0, input$range))

            }
        }
      
      #checks if the user selects a second variable
      else if (var2 != "") {
        
        #checks for when the user selects the same variable for the 2 choices
        if (var1 == var2){
          
          #checks for when the user selects the same variable for the 2 choices
          #and if the variable is a character type
          if (is.character(cleanMusic[[var1]]))
          {
            
            #creates a barplot of the categorical variable that the user selected
            ggplot(cleanMusic, aes(x = !!sym(var1))) +
              geom_bar(fill = co, color = "black") +
              labs(title = paste("Barplot of", var1), x = var1, y = "Count") + 
              coord_cartesian(xlim = c(0, input$range))

          }
          
          #checks for when the user selects the same variable for the 2 choices
          #and if the variable is a numeric type
          else if (is.numeric(cleanMusic[[var1]]))
            {
            
            #creates a histogram of the numerical variable that the user selected
            ggplot(cleanMusic, aes(x = !!sym(var1))) +
              geom_histogram(fill = co, color = "black", binwidth = input$bins) +
              labs(title = paste("Histogram of", var1), x = var1, y = "Frequency") + 
              coord_cartesian(xlim = c(0, input$range))

          }
        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if both of the variables selected are of the character type
        else if (is.character(cleanMusic[[var1]]) && is.character(cleanMusic[[var2]])) 
          {
          
          #creates a grouped bar chart of the 2 categorical variables chosen by the user
          ggplot(cleanMusic, aes_string(x = var1, fill = var2)) +
            geom_bar(stat = "count", position = "dodge") +
            labs(title = paste("Grouped Bar Chart of", var1, "and", var2),
                 x = var1, y = "Count", fill = var2) +
            theme_minimal() + 
            coord_cartesian(xlim = c(0, input$range))

          }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if the first variable selected is categorical and the second is numeric
        else if ((is.character(cleanMusic[[var1]]) && is.numeric(cleanMusic[[var2]])))
        {
          
          #creates a boxplot of the numeric variable versus the categorical variable
          ggplot(cleanMusic, aes_string(x = var1, y = var2, fill = var1)) +
            geom_boxplot() +
            labs(title = paste("Box Plot of", var1, "by", var2),
                 x = var1, y = var2, fill = var1) +
            theme_minimal() + 
            coord_cartesian(xlim = c(0, input$range))

        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if the first variable selected is numeric and the second is categorical
        else if ((is.numeric(cleanMusic[[var1]]) && is.character(cleanMusic[[var2]])))
        {
          
          #creates a boxplot of the numeric variable versus the categorical variable
          ggplot(cleanMusic, aes_string(x = var2, y = var1, fill = var2)) +
            geom_boxplot() +
            labs(title = paste("Box Plot of", var2, "by", var1),
                 x = var2, y = var1, fill = var2) +
            theme_minimal() + 
            coord_cartesian(xlim = c(0, input$range))

        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if both of the variables selected are of the numeric type
        else if (is.numeric(cleanMusic[[var1]]) && is.numeric(cleanMusic[[var2]]))
        {
            #creates a scatterplot of the two numeric variables
            plot <- ggplot(cleanMusic, aes(x = cleanMusic[, var1], y = cleanMusic[, var2], color = co)) 
            plot + geom_point() + labs(title = paste("Scatterplot of", var1, "and", var2),
                                       x = var1, y = var2) + scale_color_identity() + 
              coord_cartesian(xlim = c(0, input$range))
        }
      }
    })
  })
 
    #calculates and prints out descriptive statistics based off of user's selected variable(s)
    output$descStats <- renderPrint({ 
      
      #renames input$variable1 and input$variable2 to make it easier to code
      var1 <- input$variable1
      var2 <- input$variable2
      
      #if the user selects the checkbox to display descriptive statistics
      #and if the user does not choose a second variable
      if (input$checkbox1 == TRUE && var2 == "") 
        {
        
        #if the user selected a numeric variable
        if (is.numeric(cleanMusic[[var1]])) 
          {
          
          #calculates mean, median, and five num summary for the user's selected numeric variable
          meanVal <- mean(cleanMusic[[var1]], na.rm = TRUE)
          medianVal <- median(cleanMusic[[var1]], na.rm = TRUE)
          fivenumVal <- fivenum(cleanMusic[[var1]], na.rm = TRUE)
          
          #prints the descriptive statistics for the numeric variable rounded to 2 decimal places
          cat("Mean: ", round(meanVal, 2), "\n")
          cat("Median: ", round(medianVal, 2), "\n")
          cat("Five-number summary: ", paste(fivenumVal, collapse = ", "), "\n")
        }
        
        #if the user selected a categorical variable
        else if (is.character(cleanMusic[[var1]])) 
          {
          
          #frequency table of categorical variable
          frequency_table <- table(cleanMusic[[var1]])
          print("Frequency table:")
          print(frequency_table)
          
          cat("\n\n")
          
          #proportion table of categorical variable
          prop_table <- prop.table(frequency_table)
          print("Table of proportions:")
          prop_table <- round(prop_table, 2)
          print(prop_table)
        }
      }
      
      #checks if the user selects the checkbox to display descriptive statistics
      #and if user selected a second variable
      else if(input$checkbox1 == TRUE && var2 != "") {
        
        #if the two selected variables are the same
        if (var1 == var2)
        {
          
          #if the variable is categorical
          if ((is.character(cleanMusic[[var1]])))
          {
            
            #frequency table of the categorical variable
            frequency_table <- table(cleanMusic[[var1]])
            print("Frequency table:")
            print(frequency_table)
            
            cat("\n\n")
            
            #table of proportions of the categorical variable
            prop_table <- prop.table(frequency_table)
            print("Table of proportions:")
            prop_table <- round(prop_table, 2)
            print(prop_table)
            
          }
          
          #if the variable is numeric
          else if ((is.numeric(cleanMusic[[var1]])))
          {
            
            #calculates mean, median, and five num summary for the user's selected numeric variable
            meanVal <- mean(cleanMusic[[var1]], na.rm = TRUE)
            medianVal <- median(cleanMusic[[var1]], na.rm = TRUE)
            fivenumVal <- fivenum(cleanMusic[[var1]], na.rm = TRUE)
            
            #prints the descriptive statistics for the numeric variable rounded to 2 decimal places
            cat("Mean: ", round(meanVal, 2), "\n")
            cat("Median: ", round(medianVal, 2), "\n")
            cat("Five-number summary: ", paste(fivenumVal, collapse = ", "), "\n")
            
          }
        }
        
        #checks if the first variable selected is numeric and the second is also numeric
        else if (is.numeric(cleanMusic[[var1]]) && is.numeric(cleanMusic[[var2]]))
        {
          #calculates the correlation coefficient between the numeric variables
          corVar <- cor(cleanMusic[[var1]], cleanMusic[[var2]])
          cat("Correlation: ", round(corVar, 2))
        }
        
        #checks if the first variable selected is numeric and the second is also categorical
        else if ((is.numeric(cleanMusic[[var1]]) && is.character(cleanMusic[[var2]]))) 
          {
          
          #aggregate mean
          mTab <- aggregate(cleanMusic[[var1]] ~ cleanMusic[[var2]], data = cleanMusic, mean)
          #aggregate standard deviation
          sdTab <- aggregate(cleanMusic[[var1]] ~ cleanMusic[[var2]], data = cleanMusic, sd)
          
          #rounds the values in the tables to 2 decimal places
          mTab$Mean <- round(mTab$`cleanMusic[[var1]]`, 2)
          sdTab$SD <- round(sdTab$`cleanMusic[[var1]]`, 2)
          
          #prints the tables
          cat("Mean Table (", var2, " vs. ", var1, "):\n")
          print(mTab)
          
          cat("\n\n")
          
          cat("Standard Deviation Table (", var2, " vs. ", var1, "):\n")
          print(sdTab)
          
          
        } 
        
        #checks if the first variable selected is categorical and the second is numeric
        else if ((is.character(cleanMusic[[var1]]) && is.numeric(cleanMusic[[var2]]))) 
          {
          
          #aggregate mean
          mTab <- aggregate(cleanMusic[[var2]] ~ cleanMusic[[var1]], data = cleanMusic, mean)
          #aggregate standard deviation
          sdTab <- aggregate(cleanMusic[[var2]] ~ cleanMusic[[var1]], data = cleanMusic, sd)
          
          #rounds the values in the tables to 2 decimal places
          mTab$Mean <- round(mTab$`cleanMusic[[var2]]`, 2)
          sdTab$SD <- round(sdTab$`cleanMusic[[var2]]`, 2)
          
          #prints the tables
          cat("Mean Table (", var1, " vs. ", var2, "):\n")
          print(mTab)
          
          cat("\n\n")
          
          cat("Standard Deviation Table (", var1, " vs. ", var2, "):\n")
          print(sdTab)
          
        }
        
        #checks if the first variable selected is categorical and the second is also categorical
        else if ((is.character(cleanMusic[[var1]]) && is.character(cleanMusic[[var2]]))) 
        {
          
          #finds the unique levels in each variable
          levels_var1 <- unique(cleanMusic[[var1]])
          levels_var2 <- unique(cleanMusic[[var2]])
          
          #create labels for each level
          labels_var1 <- paste(var1, ":", levels_var1)
          labels_var2 <- paste(var2, ":", levels_var2)
          
          #creates a new dataframe with factor variables
          newDF <- data.frame(
            cat1 = factor(cleanMusic[[var1]], levels = levels_var1, labels = labels_var1),
            cat2 = factor(cleanMusic[[var2]], levels = levels_var2, labels = labels_var2)
          )
          
          #displays the table and proportions
          table(newDF$cat1, newDF$cat2)
          round(prop.table(table(newDF$cat1, newDF$cat2)), 2)
          round(prop.table(table(newDF$cat1, newDF$cat2), margin = 2), 2)
          round(prop.table(table(newDF$cat1, newDF$cat2), margin = 1), 2)
          
        }
      }
    })
    
    #renders description of variables into a table format
    output$varDesc <- renderPrint({
      
      #if the user selects the checkbox to display the variable descriptions
      if (input$checkbox2 == TRUE)
      {
        
        #information of variable names, variable types, and descriptions to make into a table later
        tableInfo <- data.frame(
          Variable = c("age", "hours", "depression", "musicEffects", "favGenre", "freqClassical", "freqRB", "freqLofi", "freqKpop", "BPM"),
          Type = c("Numeric", "Numeric", "Numeric", "Character", "Character", "Character", "Character", "Character", "Character", "Numeric"),
          Description = c("Age of the individual recording their music and mental health information", 
                          "Number of hours an individual listens to music per day", 
                          "Self-reported depression levels on a scale of 1-10", 
                          "Individual's own thoughts on whether or not music has improved their mental health", 
                          "Individual's favorite genre", 
                          "How frequently does an individual listen to classical music", 
                          "How frequently does an individual listen to RnB music", 
                          "How frequently does an individual listen to lofi music", 
                          "How frequently does an individual listen to Kpop music", 
                          "Beats per minute of an individual's favorite genre")
        )
        
        #print the table with customized column names
        print(kable(tableInfo, format = "markdown", 
                    col.names = c("Variable Name", "Data Type", "Description")))
      }
    })
    
    #renders and prints descriptions of the plot
    output$plotDesc <- renderPrint({ 
      
      var1 <- input$variable1
      var2 <- input$variable2
      co <- input$color
      
      #checks for when the user leaves the second selection blank/does not choose a second variable
      if(var2 == "")
      {
        
        #if the user selected a character variable/categorical variable
        if (is.character(cleanMusic[[var1]])) 
        {
          st <- paste("This is a barplot of the single categorical variable", var1)
          print(st)
        } 
        
        #if the user selected a numerical variable 
        else if (is.numeric(cleanMusic[[var1]])) 
        {
          #output description
          st <- paste("This is a histogram of the single numerical variable", var1)
          print(st)
        }
      }
      
      #checks if the user selects a second variable
      else if (var2 != "") {
        
        #checks for when the user selects the same variable for the 2 choices
        if (var1 == var2){
          
          #checks for when the user selects the same variable for the 2 choices
          #and if the variable is a character type
          if (is.character(cleanMusic[[var1]]))
          {
            
            #output description
            st <- paste("This is a barplot of the single categorical variable", var1)
            print(st)
            
          }
          
          #checks for when the user selects the same variable for the 2 choices
          #and if the variable is a numeric type
          else if (is.numeric(cleanMusic[[var1]]))
          {
            
            #output description
            st <- paste("This is a histogram of the single numerical variable", var1)
            print(st)
            
          }
        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if both of the variables selected are of the character type
        else if (is.character(cleanMusic[[var1]]) && is.character(cleanMusic[[var2]])) 
        {
         
          #output description
          st <- paste("This is a grouped bar chart of the two categorical variables", var1, "and", var2)
          print(st)
          
        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if the first variable selected is categorical and the second is numeric
        else if ((is.character(cleanMusic[[var1]]) && is.numeric(cleanMusic[[var2]])))
        {
        
          #output description
          st <- paste("This is a boxplot of the categorical variable", var1, "and the numeric variable", var2)
          print(st)
          
        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if the first variable selected is numeric and the second is categorical
        else if ((is.numeric(cleanMusic[[var1]]) && is.character(cleanMusic[[var2]])))
        {
          
          #output description
          st <- paste("This is a boxplot of the numeric variable", var1, "and the categorical variable", var2)
          print(st)
          
        }
        
        #checks for when the user selects 2 variables that are not the same
        #checks if both of the variables selected are of the numeric type
        else if (is.numeric(cleanMusic[[var1]]) && is.numeric(cleanMusic[[var2]]))
        {
          
            #output description
            st <- paste("This is a scatterplot of the two numeric variables", var1, "and", var2)
            print(st)
            
        }
      }
    })
}

#runs the application 
shinyApp(ui = ui, server = server)


