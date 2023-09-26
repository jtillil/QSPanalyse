# Load required libraries
library(shiny)
library(ggplot2)
library(tidyr)
library(R.matlab)
library(visNetwork)

#### MATLAB access functions ####
matacc = function(obj, attr) {
  return( obj[attr,,][[attr]] )
}
matacc2 = function(obj, attr1, attr2) {
  return( obj[attr1,,][[attr1]][attr2,,][[attr2]] )
}

#### UI ####

# Define the UI
ui = navbarPage("QSPanalyse",
  tabPanel("Info",
    sidebarLayout(
       sidebarPanel(
         textInput("working_directory", "Working directory:", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/tQSSA"),
         textInput("file_location", "Model file:", "tQSSA/res.mat"),
       ),
       mainPanel(
         textOutput("model_summary"),
       )
     )
  ),
  tabPanel("Plot",
    sidebarLayout(
      sidebarPanel(
        textInput("file_location_plots", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", "tQSSA/res.mat"),
        # textOutput("model_summary"),
        checkboxGroupInput("state", "Choose states:", 
                    choices = NULL, selected = NULL),
        actionButton("select_all_states", "Select all states"),
        actionButton("unselect_all_states", "Unselect all states"),
        selectInput("index", "Choose index:", 
                   choices = c("ir"), selected = "ir"),
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
        textInput("file_location_graph", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", "tQSSA/res.mat"),
        textInput("save_location_graph", "Save file:", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/tQSSA/graph.html"),
        actionButton("save_graph_button", "Save Graph"),
      ),
      mainPanel(
        visNetworkOutput("graph"),
      )
    )
  ),
  tabPanel("Reduced Model",
    sidebarLayout(
      sidebarPanel(
        textInput("file_location_reduced_graph", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", "tQSSA/res.mat"),
        textInput("save_location_reduced_graph", "Save file:", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/tQSSA/graph.html"),
        actionButton("save_reduced_graph_button", "Save Graph"),
      ),
      mainPanel(
        visNetworkOutput("reduced_graph"),
      )
    )
  )
)

#### Server ####

# Define the server
server = function(input, output, session) {
  
  #### Sync Shared Inputs ####
  
  # Sync file location
  observeEvent(
    input$file_location_plots,
    updateTextInput(session, "file_location_graph", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", input$file_location_plots)
  )
  observeEvent(
    input$file_location_plots,
    updateTextInput(session, "file_location_reduced_graph", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", input$file_location_plots)
  )
  observeEvent(
    input$file_location_graph,
    updateTextInput(session, "file_location_plots", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", input$file_location_graph)
  )
  observeEvent(
    input$file_location_graph,
    updateTextInput(session, "file_location_plots", "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", input$file_location_graph)
  )
  
  #### Reactive Values ####
  
  # Reactive values to store data
  model_reactive = reactive({
    req(input$file_location_plots)
    matdat = readMat(paste0("/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/", input$file_location_plots))
    model = matdat$model
    return(model)
  })
  
  # Reactive state options
  states_reactive = reactive({
    # Ensure a file location is provided
    req(input$file_location_plots)
    # Read model
    model = model_reactive()
    # Extract state options
    options = c("all", unique(unlist(matacc2(model, "I", "nmstate"))))
    return(options)
  })
  
  # Reactive indices options
  indices_reactive = reactive({
    # Ensure a file location is provided
    req(input$file_location_plots)
    # Read model
    model = model_reactive()
    # Extract state options
    options = c("all", unique(unlist(matacc2(model, "I", "nm.indices"))))
    return(options)
  })
  
  #### Update UI ####
  
  # Update state options
  observe({
    # Update the choices when states_reactive changes
    choices = states_reactive()
    updateCheckboxGroupInput(session, "state", choices = choices, selected = "all")
  })
  
  # Update indices options
  observe({
    # Update the choices when indices_reactive changes
    choices = indices_reactive()
    updateSelectInput(session, "index", choices = choices, selected = "ir")
  })
  
  # Update state selection ticks
  observeEvent(
    input$select_all_states,
    {
      choices = states_reactive()
      updateCheckboxGroupInput(session, "state", choices = choices, selected = choices[-1])
    }
  )
  observeEvent(
    input$unselect_all_states,
    {
      choices = states_reactive()
      updateCheckboxGroupInput(session, "state", choices = choices, selected = NULL)
    }
  )
  
  #### Plot Reference Solution ####
  
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
    if ("all" %in% input$state) {
      ggplot(refsol, aes(x = t, y = value, color = state)) +
        geom_line() +
        coord_cartesian(ylim = c(1e-7, NA)) +
        scale_y_log10() +
        labs(title = "Concentration of all states", x = "Time", y = "Concentration (log scale)")
    } else {
      ggplot(subset(refsol, state %in% input$state), aes(x = t, y = value, color = state)) +
        geom_line() +
        coord_cartesian(ylim = c(1e-7, NA)) +
        scale_y_log10() +
        labs(title = paste0("Concentration of ", paste(input$state, collapse = ", ")), x = "Time", y = "Concentration (log scale)")
    }
  })
  
  #### Plot Indices ####
  
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
    if (input$index == "all" && "all" %in% input$state) {
      ggplot() +
        geom_blank() +
        geom_text(aes(x = 0, y = 0, label = "All indices not possible when showing all variables!", size = 20)) +
        labs(title = "Normalized indices", x = "Time", y = "Normalized index")
    } else if (input$index == "all") {
      ggplot(subset(indexframe, grepl(input$state, state) & !grepl(paste0("ir-", input$state), state)), aes(x = t, y = value, color = state)) +
        geom_line() +
        labs(title = paste0("All normalized indices of ", input$state), x = "Time", y = "Normalized index")
    } else if ("all" %in% input$state) {
      ggplot(indexframe, aes(x = t, y = value, color = state)) +
        geom_line() +
        labs(title = paste0("Normalized ", input$index, "-index of all states"), x = "Time", y = "Normalized index")
    } else {
      ggplot(subset(indexframe, state %in% input$state), aes(x = t, y = value, color = state)) +
        geom_line() +
        labs(title = paste0("Normalized ", input$index, "-index of ", paste(input$state, collapse = ", ")), x = "Time", y = paste0("Normalized index"))
    }
  })
  
  #### Handle full Model visNetwork Graphing ####
  
  visnetwork_reactive = reactive({
    # Change upon loading new file
    req(input$file_location_plots)
    
    # Read model
    model = model_reactive()
    
    # Read stoichiometric matrix
    # ...
    
    # Create nodes
    nodenames = unlist(matacc2(model, "I", "nmstate"))
    nodes <- data.frame(
      id = 1:length(nodenames),
      label = c("A", NA, "E", "C", "P"),
      group = c("S", "C", "E", "E", "S")
    )
    
    # Create edges
    edges <- data.frame(
      from = c(1, 2, 4, 5), 
      to = c(2, 3, 2, 3),
      length = c(NA, 0, NA, NA),
      width = c(NA, 0, NA, NA),
      arrows = c("to", NA, "to", "to"),
      # opacity = c(NA, 0, NA, NA),
      color = c(NA, "white", NA, NA)
      # physics = c(T, F, T, T),
      # scaling = c(NA, list(min=0, max=0), NA, NA))
      # label = c("Edge1", "Edge2", "Edge3", "Edge4", "Edge5")
    )
    
    # Create and customize visNetwork object
    visNetwork(nodes, edges) %>%
      
      # Edges
      visEdges(
        smooth = TRUE,
        color = "black") %>%
      
      # Species - "S"
      visGroups(
        groupname = "S",
        # color = "grey",
        shape = "square", 
        shadow = list(enabled = FALSE)) %>% 
      # Enzyme - "E"
      visGroups(
        groupname = "E",
        # color = "grey",
        shape = "dot", 
        shadow = list(enabled = FALSE)) %>% 
      # Connector - "C"
      visGroups(
        groupname = "C",
        label = "",
        shape = "dot",
        size = 0,
        hidden = FALSE,
        physics = TRUE) %>%
      
      # Physics Options
      visPhysics(
        # repulsion = list(
        #   nodeDistance = 0,
        #   springLength = 0
        # ),
        enabled = TRUE
      )
    # visEdges(smooth = list(enabled = TRUE, type = "diagonalCross"))
    # visEdges(edges = list(arrows = 'to', smooth = list(type = 'dynamic')))
  })
  
  observeEvent(
    input$save_graph_button,
    visSave(visnetwork_reactive(), input$save_location_graph, background = "white")
  )
  
  output$graph = renderVisNetwork({
    # Output reactive visnetwork object
    visnetwork_reactive()
  })
  
  #### Handle reduced Model visNetwork Graphing ####
  
  visnetwork_reduced_reactive = reactive({
    # Change upon loading new file
    req(input$file_location_plots)
    
    # Read model
    model = model_reactive()
    
    # Read stoichiometric matrix
    # ...
    
    # Create nodes
    nodenames = unlist(matacc2(model, "I", "nmstate"))
    nodes <- data.frame(
      id = 1:length(nodenames),
      label = c("A", NA, "E", "C", "P"),
      group = c("S", "C", "E", "E", "S")
    )
    
    # Create edges
    edges <- data.frame(
      from = c(1, 2, 4, 5), 
      to = c(2, 3, 2, 3),
      length = c(NA, 0, NA, NA),
      width = c(NA, 0, NA, NA),
      arrows = c("to", NA, "to", "to"),
      # opacity = c(NA, 0, NA, NA),
      color = c(NA, "white", NA, NA)
      # physics = c(T, F, T, T),
      # scaling = c(NA, list(min=0, max=0), NA, NA))
      # label = c("Edge1", "Edge2", "Edge3", "Edge4", "Edge5")
    )
    
    # Create and customize visNetwork object
    visNetwork(nodes, edges) %>%
      
      # Edges
      visEdges(
        smooth = TRUE,
        color = "black") %>%
      
      # Species - "S"
      visGroups(
        groupname = "S",
        # color = "grey",
        shape = "square", 
        shadow = list(enabled = FALSE)) %>% 
      # Enzyme - "E"
      visGroups(
        groupname = "E",
        # color = "grey",
        shape = "dot", 
        shadow = list(enabled = FALSE)) %>% 
      # Connector - "C"
      visGroups(
        groupname = "C",
        label = "",
        shape = "dot",
        size = 0,
        hidden = FALSE,
        physics = TRUE) %>%
      
      # Physics Options
      visPhysics(
        # repulsion = list(
        #   nodeDistance = 0,
        #   springLength = 0
        # ),
        enabled = TRUE
      )
    # visEdges(smooth = list(enabled = TRUE, type = "diagonalCross"))
    # visEdges(edges = list(arrows = 'to', smooth = list(type = 'dynamic')))
  })
  
  observeEvent(
    input$save_reduced_graph_button,
    visSave(visnetwork_reduced_reactive(), input$save_location_reduced_graph, background = "white")
  )
  
  output$reduced_graph = renderVisNetwork({
    # Output reactive visnetwork object
    visnetwork_reduced_reactive()
  })
}

#### Run ####

# Run the Shiny app
shinyApp(ui, server)
