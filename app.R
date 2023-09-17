# Load required libraries
library(shiny)
library(ggplot2)
library(tidyr)
library(R.matlab)

# MATLAB access functions
matacc = function(obj, attr) {
  return( obj[attr,,][[attr]] )
}
matacc2 = function(obj, attr1, attr2) {
  return( obj[attr1,,][[attr1]][attr2,,][[attr2]] )
}

# Define the UI
ui = navbarPage("QSPanalyse",
  tabPanel("Plots",
    sidebarLayout(
      sidebarPanel(
        textInput("file_location", "Enter File Location:", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/tQSSA/res.mat"),
        radioButtons("state", "Choose state:", 
                    choices = c("all"), selected = "all"),
        selectInput("index", "Choose Index:", 
                   choices = c("ir"), selected = "ir")
      ),
      mainPanel(
        plotOutput("plot_ref"),
        plotOutput("plot_indices"),
      )
    )
  ),
  tabPanel("Graph",
    sidebarLayout(
      sidebarPanel(
        textInput("file_location", "Enter File Location:", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/tQSSA/res.mat"),
      ),
      mainPanel(
       
      )
    )
  )
)

# Define the server
server = function(input, output, session) {
  
  # Reactive values to store data
  model_reactive = reactive({
    req(input$file_location)
    matdat = readMat(input$file_location)
    model = matdat$model
    return(model)
  })
  
  # Reactive state options
  states_reactive = reactive({
    # Ensure a file location is provided
    req(input$file_location)
    # Read model
    model = model_reactive()
    # Extract state options
    options = c("all", unique(unlist(matacc2(model, "I", "nmstate"))))
    return(options)
  })
  
  # Update state options
  observe({
    # Update the choices when states_reactive changes
    choices = states_reactive()
    updateRadioButtons(session, "state", choices = choices, selected = "all")
  })
  
  # Reactive indices options
  indices_reactive = reactive({
    # Ensure a file location is provided
    req(input$file_location)
    # Read model
    model = model_reactive()
    # Extract state options
    options = c("all", unique(unlist(matacc2(model, "I", "nm.indices"))))
    return(options)
  })
  
  # Update indices options
  observe({
    # Update the choices when indices_reactive changes
    choices = indices_reactive()
    updateSelectInput(session, "index", choices = choices, selected = "ir")
  })
  
  # Render state plot
  output$plot_ref = renderPlot({
    
    # Read model
    model = model_reactive()
    
    # Read ref solution
    refsol = data.frame(matacc(model, "t.ref"), matacc(model, "X.ref"))
    
    # Assign colnames of ref solution
    colnames(refsol) = c("t", unlist(matacc2(model, "I", "nmstate")))
    
    # Pivot ref solution
    refsol = pivot_longer(refsol, cols = -t, names_to = "state", values_to = "value")
    
    # Create a plot of chosen var
    if (input$state == "all") {
      ggplot(refsol, aes(x = t, y = value, color = state)) +
        geom_line() +
        coord_cartesian(ylim = c(1e-7, NA)) +
        scale_y_log10() +
        labs(title = "Concentration of all states", x = "Time", y = "Concentration (log scale)")
    } else {
      ggplot(subset(refsol, state == input$state), aes(x = t, y = value, color = state)) +
        geom_line() +
        coord_cartesian(ylim = c(1e-7, NA)) +
        scale_y_log10() +
        labs(title = paste0("Concentration of ", input$state), x = "Time", y = "Concentration (log scale)")
    }
  })
  
  # Render index plot
  output$plot_indices = renderPlot({
    
    # Read model
    model = model_reactive()
    
    # Read index frame
    if (input$index == "all" && input$state == "all") {
      # Do nothing
    } else if (input$index == "all") {
      # Read reference times
      indexframe = data.frame(matacc(model, "t.ref"))
      
      # Assign colname of reference times
      colnames(indexframe) = c("t")
      
      # Read all nindices
      for (indexname in unique(unlist(matacc2(model, "I", "nm.indices")))) {
        tryCatch(
          expr = {
            nindex = data.frame(matacc2(model, indexname, "nindex"))
            
            # Assign colnames of nindex
            colnames(nindex) = paste0(indexname, "-", unlist(matacc2(model, "I", "nmstate")))
            
            # Bind nindex to indexframe
            indexframe = cbind(indexframe, nindex)
          },
          error = function(e) {
            # Do nothing in case of error while reading nindex
          }
        )
      }
    } else {
      tryCatch(
        expr = {
          # Read index frame
          indexframe = data.frame(matacc(model, "t.ref"), matacc2(model, input$index, "nindex"))
        },
        error = function(e) {
          ggplot() +
            geom_blank() +
            geom_text(aes(x = 0, y = 0, label = paste0("Index ", input$index, " does not exist in the current dataset!"), size = 20)) +
            labs(title = "Normalized indices", x = "Time", y = "Normalized index")
        }
      )
      
      # Assign colnames of index frame
      colnames(indexframe) = c("t", unlist(matacc2(model, "I", "nmstate")))
    }
    
    if (!(input$index == "all" && input$state == "all")) {
      # Pivot index frame
      indexframe = pivot_longer(indexframe, cols = -t, names_to = "state", values_to = "value")
    }
    
    # Create a plot of chosen state index pair
    if (input$index == "all" && input$state == "all") {
      ggplot() +
        geom_blank() +
        geom_text(aes(x = 0, y = 0, label = "All indices not possible when showing all variables!", size = 20)) +
        labs(title = "Normalized indices", x = "Time", y = "Normalized index")
    } else if (input$index == "all") {
      ggplot(subset(indexframe, grepl(input$state, state) & !grepl(paste0("ir-", input$state), state)), aes(x = t, y = value, color = state)) +
        geom_line() +
        labs(title = paste0("All normalized indices of ", input$state), x = "Time", y = "Normalized index")
    } else if (input$state == "all") {
      ggplot(indexframe, aes(x = t, y = value, color = state)) +
        geom_line() +
        labs(title = paste0("Normalized ", input$index, "-index of all states"), x = "Time", y = "Normalized index")
    } else {
      ggplot(subset(indexframe, state == input$state), aes(x = t, y = value, color = state)) +
        geom_line() +
        labs(title = paste0("Normalized ", input$index, "-index of ", input$state), x = "Time", y = paste0("Normalized index"))
    }
  })
}

# Run the Shiny app
shinyApp(ui, server)
