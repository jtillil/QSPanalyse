# Load required libraries
library(shiny)
library(ggplot2)
library(tidyr)
library(R.matlab)
library(visNetwork)
library(reactlog)
library(tictoc)

#### MATLAB access functions ####
matacc = function(obj, attr) {
  return( obj[attr,,][[attr]] )
}
matacc2 = function(obj, attr1, attr2) {
  return( obj[attr1,,][[attr1]][attr2,,][[attr2]] )
}
matacc3 = function(obj, attr1, attr2, attr3) {
  return( obj[attr1,,][[attr1]][attr2,,][[attr2]][attr3,,][[attr3]] )
}

#### UI ####

# Define the UI
ui = navbarPage("QSPanalyse",
  tabPanel("Info",
    sidebarLayout(
      sidebarPanel(
        textInput("working_directory", "Working directory:", "/Users/jtm2/Documents/GitHub/ModelReduction/"),
        selectInput(
          "model_directory",
          "Model directory:",
          NULL,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE
        ),
        selectInput(
          "model_file",
          "Model file:",
          NULL,
          selected = NULL,
          multiple = FALSE,
          selectize = TRUE
        ),
        # textInput("file", "Model file:", "Core/minimalsolution.mat"),
        # textInput("file_location", "Model file:", "MAIN/results/modelEGFR_exh_baset60_0.1_Inf_out_dyncnegpnegenvirenvpss_new.mat"),
      ),
      mainPanel(
        verbatimTextOutput("info"),
      )
    )
  ),
  tabPanel("Solution",
    sidebarLayout(
      sidebarPanel(
        actionButton("select_all_states", "Select all states"),
        actionButton("unselect_all_states", "Unselect all states"),
        checkboxGroupInput("state", "Choose states:", 
                    choices = NULL, selected = NULL),
        selectInput("index", "Choose index:", 
                   choices = c("ir"), selected = "ir"),
      ),
      mainPanel(
        plotOutput("plot_ref"),
        plotOutput("plot_indices"),
      )
    )
  ),
  tabPanel("Error",
    verticalLayout(
      plotOutput("plot_out_err"),
      plotOutput("plot_int_err"),
    )
  ),
  tabPanel("Graph",
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "iteration",
          "MOR Iteration",
          min = 1,
          max = 100,
          value = 1,
          step = 1
        ),
        numericInput(
          "seed",
          "Graph Seed",
          value = 115,
          step = 1
        ),
        textInput("save_directory_graph", "Save directory:", "/Users/jtm2/Desktop/graphs/"),
        textInput("save_file_graph", "Save file:", "graph.html"),
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
      ),
      mainPanel(
      )
    )
  ),
  tabPanel("Internal States",
    sidebarLayout(
      sidebarPanel(
      ),
      mainPanel(
      )
    )
  )
)

#### Server ####

# Define the server
server = function(input, output, session) {
  
  #### Sync Shared Inputs ####
  
  
  
  #### Reactive Values ####
  
  # Reactive working directory
  wd_reactive = reactive({
    req(input$working_directory)
    return(input$working_directory)
  })
  
  # Reactive model directory
  md_reactive = reactive({
    req(input$model_directory)
    return(input$model_directory)
  })
  
  # Reactive model path
  model_path_reactive = reactive({
    req(input$model_file)
    return(paste0(input$working_directory, input$model_directory, "/", input$model_file))
  })
  
  # Reactive model object
  model_reactive = reactive({
    path = model_path_reactive()
    print(paste0("Reading model from: ", path))
    tic()
    matdat = readMat(path)
    model = matdat$model
    print("Finished reading model.")
    toc()
    return(model)
  })
  
  # Reactive state options
  states_reactive = reactive({
    # Read model
    model = model_reactive()
    # Extract state options
    options = c("all", unique(unlist(matacc2(model, "I", "nmstate"))))
    return(options)
  })
  
  # Reactive indices options
  indices_reactive = reactive({
    # Read model
    model = model_reactive()
    # Extract state options
    options = c("all", unique(unlist(matacc2(model, "I", "nm.indices"))))
    return(options)
  })
  
  #### Update UI ####
  
  # Update model directory options
  observe({
    # Update the choices when states_reactive changes
    wd = wd_reactive()
    folders <- list.dirs(path = wd, recursive = TRUE, full.names = FALSE)
    for (i in length(folders):1) {
      if (substr(folders[i], 1, 1) == ".") {
        folders = folders[-i]
      }
    }
    if (folders[1] == "") {
      folders = folders[-1]
    }
    if (!is.null(folders)) {
      updateSelectInput(session, "model_directory", choices = folders, selected = folders[1])
    } else {
      updateSelectInput(session, "model_directory", choices = "Working directory empty!", selected = "Working directory empty!")
    }
  })
  
  # Update model file options
  observe({
    # Update the choices when states_reactive changes
    wd = wd_reactive()
    md = md_reactive()
    files = setdiff(list.files(path = paste0(wd, md)), list.dirs(path = paste0(wd, md), recursive = FALSE, full.names = FALSE))
    if (!is.null(files)) {
      updateSelectInput(session, "model_file", choices = files, selected = files[1])
    } else {
      updateSelectInput(session, "model_file", choices = "Working directory empty!", selected = "Working directory empty!")
    }
  })
  
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
  
  # Update iteration selector
  observe({
    # Update the choices when indices_reactive changes
    model = model_reactive()
    updateSliderInput(session, "iteration", max = matacc(model, "nsteps"))
  })
  observeEvent(
    input$iteration,
    {
      updateTextInput(session, "save_file_graph", value = paste0("graph_", input$iteration, ".html"))
    }
  )
  
  #### Render Info Text ####
  
  output$info = renderText({
    
    # Read model
    model = model_reactive()
    
    display_string = paste0(
      "Model:         ", matacc(model, "name"), "\n",
      "Scenario:      ", matacc(model, "scenario"), "\n",
      "\n",
      "Input:         ", as.character(unlist(matacc2(model, "I", "nmstate")[[matacc2(model, "I", "input")]])), "\n",
      "output:        ", as.character(unlist(matacc2(model, "I", "nmstate")[[matacc2(model, "I", "output")]])), "\n",
      "\n",
      "#States:       ", as.character(matacc2(model, "I", "nstates")), "\n",
      "#Parameters:   ", as.character(matacc2(model, "I", "npar"))
    )
    
    # Start text string
    return(display_string)
  })
  
  #### Plot Reference Solution ####
  
  # Render state plot
  output$plot_ref = renderPlot({
    
    # Read model
    model = model_reactive()
    
    # Read ref solution
    refsol = data.frame(matacc(model, "t.ref"), matacc(model, "X.ref"))
    
    # Assign colnames of ref solution
    # print(unlist(matacc2(model, "I", "nmstate")))
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
  
  #### Handle visNetwork Graphing ####
  
  visnetwork_reactive = reactive({
    # Read model
    model = model_reactive()
    
    # Read indices
    indices = which(matacc(model, "adjmat") == 1, arr.ind = TRUE)
    substrindices = which(matacc(model, "substratemask") == 1, arr.ind = TRUE)
    
    # Create nodes
    speciesnames = unlist(matacc2(model, "I", "nmstate"))
    paramnames = unlist(matacc2(model, "I", "nmpar"))
    config_numeric = matacc(model, "configs")[input$iteration,]
    # config_numeric = matacc(model, "configs")[422,]
    config = unlist(matacc(model, "classifs")[config_numeric])
    nodes <- data.frame(
      id = 1:(length(speciesnames) + length(paramnames)),
      label = c(speciesnames, paramnames),
      group = c(config, rep("k", matacc2(model, "I", "npar")))
    )
    nodes$group[matacc2(model, "I", "output")] = "output"
    nodes$group[matacc2(model, "I", "input")] = "input"
    
    # Create edges
    edgecolors = rep("black", nrow(indices))
    # edgecolors[nodes$group[indices[,1]] %in% c("pneg", "cneg")] = "lightgray"
    # edgecolors[indices[,1] %in% indices[,2][nodes$group[indices[,1]] %in% c("pneg", "cneg")]] = "lightgray"
    # edgecolors[indices[,2] %in% indices[,2][nodes$group[indices[,1]] %in% c("pneg", "cneg")]] = "lightgray"
    # edgecolors[nodes$group[indices[,2]] == "cneg"] = "lightgray"
    # edgecolors[indices[,2] %in% indices[,1][nodes$group[indices[,2]] == "cneg"]] = "lightgray"
    # edgecolors[indices[,1] %in% indices[,1][nodes$group[indices[,2]] == "cneg"]] = "lightgray"
    edgecolors[nodes$group[indices[,1]] %in% c("pneg", "cneg")] = "white"
    edgecolors[indices[,1] %in% indices[,2][nodes$group[indices[,1]] %in% c("pneg", "cneg")]] = "white"
    edgecolors[indices[,2] %in% indices[,2][nodes$group[indices[,1]] %in% c("pneg", "cneg")]] = "white"
    edgecolors[nodes$group[indices[,2]] == "cneg"] = "white"
    edgecolors[indices[,2] %in% indices[,1][nodes$group[indices[,2]] == "cneg"]] = "white"
    edgecolors[indices[,1] %in% indices[,1][nodes$group[indices[,2]] == "cneg"]] = "white"
    
    # count params in model
    print(length(unique(c(indices[indices[, 1] > 112, 1], indices[indices[, 1] > 112, 1]))))
    print(length(paramnames) - length(unique(indices[nodes$group[indices[,1]] %in% c("pneg", "cneg"), 2])))
    # params_in_model = numeric(length(paramnames)) + 1
    # print(sum(params_in_model))
    # for (i in 1:length(edgecolors)) {
    #   if (edgecolors[i] == "white") {
    #     if (indices[i, 1] < 113) {
    #       params_in_model[indices[i, 1] - 112] = 0
    #     }
    #   }
    # }
    # print(sum(params_in_model))
    
    edges <- data.frame(
      from = indices[,1],
      to = indices[,2],
      # length = ifelse(nodes$group[indices[,1]] == "k", 5, 20),
      # width = c(NA, 0, NA, NA),
      arrows = ifelse(apply(indices, 1, function(index) {any(apply(substrindices, 1, function(substrindex) identical(index, substrindex)))}), NA, "to"),
      # opacity = c(NA, 0, NA, NA),
      color = edgecolors
      # physics = T
      # scaling = c(NA, list(min=0, max=0), NA, NA))
      # label = c("Edge1", "Edge2", "Edge3", "Edge4", "Edge5")
    )
    
    # Set a seed
    # 13 101 103 104 115 117
    # 19
    set.seed(input$seed)
    
    # Create and customize visNetwork object
    visNetwork(nodes, edges, height = "100em", width = "100em") %>%
      visLayout(
        randomSeed = input$seed
      ) %>%
      visIgraphLayout() %>%
      
      # Edges
      visEdges(
        smooth = T,
        color = "black"
      ) %>%
      
      # Species - "input"
      visGroups(
        groupname = "input",
        color = "blue",
        shape = "dot", 
        size = 60,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "output"
      visGroups(
        groupname = "output",
        color = "blue",
        shape = "star", 
        size = 100,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "dyn"
      visGroups(
        groupname = "dyn",
        color = "blue",
        shape = "dot", 
        size = 20,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "env"
      visGroups(
        groupname = "irenv",
        color = "lightgreen",
        shape = "dot", 
        size = 20,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "env"
      visGroups(
        groupname = "env",
        color = "lightgreen",
        shape = "dot", 
        size = 20,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "env"
      visGroups(
        groupname = "pss",
        color = "red",
        shape = "dot", 
        size = 20,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "cneg"
      visGroups(
        groupname = "cneg",
        # color = "lightgrey",
        color = "white",
        shape = "dot", 
        size = 20,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Species - "pneg"
      visGroups(
        groupname = "pneg",
        # color = "lightgrey",
        color = "white",
        shape = "dot", 
        size = 20,
        shadow = list(enabled = FALSE)
      ) %>%
      
      # Reaction - "reaction"
      visGroups(
        groupname = "k",
        # label = "",
        shape = "dot",
        size = 0
        # hidden = FALSE
        # physics = F
      ) %>% 
      
      visInteraction(
        dragNodes = TRUE,
        dragView = TRUE,
        zoomView = TRUE,
        navigationButtons = TRUE,
        tooltipDelay = 100
      ) %>%
      
      visOptions(
        highlightNearest = list(enabled = T, degree = 2, hover = T)
        # nodesIdSelection = T,
        # selectedBy = "group"
      ) %>%
      
      visExport(
        type = "pdf",
        name = "EGFR_graph",
        label = "Save network",
        background = "white",
      ) 
    
      # visExport()
    
    
    #%>%
      
      # visLegend(
      #   addNodes = list(
      #     list(
      #       label = "dynamic",
      #       color = "blue",
      #       shape = "dot", 
      #       size = 10
      #     ),
      #     list(
      #       label = "env",
      #       color = "green",
      #       shape = "dot", 
      #       size = 10
      #     ),
      #     list(
      #       label = "pss",
      #       color = "red",
      #       shape = "dot", 
      #       size = 10
      #     ),
      #     list(
      #       label = "neg",
      #       color = "lightgray",
      #       shape = "dot", 
      #       size = 10
      #     )),
      #   useGroups = F
      # )
  })
  
  observeEvent(
    input$save_graph_button,
    visSave(visnetwork_reactive(), paste0(input$save_directory_graph, input$save_file_graph), selfcontained = TRUE, background = "white")
  )
  
  output$graph = renderVisNetwork({
    # Output reactive visnetwork object
    visnetwork_reactive()
  })
  
  #### Handle internal States ####
  
  
  
}

#### Run ####

# Run the Shiny app
shinyApp(ui, server)
