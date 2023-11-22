library(R.matlab)

matacc = function(obj, attr) {
  return( obj[attr,,][[attr]] )
}
matacc2 = function(obj, attr1, attr2) {
  return( obj[attr1,,][[attr1]][attr2,,][[attr2]] )
}

# file = "/Users/jtm2/Desktop/Matlab/Johannes Model Reduction/tQSSA/res2.mat"
# matdat = readMat(file)
# model = matdat$model
# 
# input = as.character(matacc2(model, "I", "nmstate")[[matacc2(model, "I", "input")]])

a = "/Users/jtm2/Documents/GitHub/ModelReduction/"
b = "Core/adjmat.mat"
p = paste0(a,b)
c = "Core/modelfiles/modelEGFR_full.mat"
c = "Core/minimalsolution.mat"
p2 = paste0(a,c)

model = readMat(p2)$model
adjmatdat = readMat(p)

indices = which(adjmatdat$adjmat == 1, arr.ind = TRUE)
substrindices = which(adjmatdat$substratemask == 1, arr.ind = TRUE)

# Create nodes
speciesnames = unlist(matacc2(model, "I", "nmstate"))
paramnames = unlist(matacc2(model, "I", "nmpar"))
nodes <- data.frame(
  id = 1:(length(speciesnames) + length(paramnames)),
  label = c(speciesnames, paramnames),
  group = t(ifelse(adjmatdat$mask == 1, "dyn", "k"))
)

nodes$group[matacc2(model, "I", "output")] = "output"
nodes$group[matacc2(model, "I", "input")] = "input"

# Create edges
edges <- data.frame(
  from = indices[,1],
  to = indices[,2],
  length = ifelse(nodes$group[indices[,1]] == "k", 5, 20),
  # width = c(NA, 0, NA, NA),
  arrows = ifelse(apply(indices, 1, function(index) {any(apply(substrindices, 1, function(substrindex) identical(index, substrindex)))}), NA, "to"),
  # opacity = c(NA, 0, NA, NA),
  # color = c(NA, "white", NA, NA)
  physics = T
  # scaling = c(NA, list(min=0, max=0), NA, NA))
  # label = c("Edge1", "Edge2", "Edge3", "Edge4", "Edge5")
)

# Create and customize visNetwork object
net = visNetwork(nodes, edges, height = "500px", width = "1000px") %>%
  visLayout(
    randomSeed = 2
  ) %>%
  visIgraphLayout() %>%
  
  # Edges
  visEdges(
    smooth = TRUE,
    color = "black"
  ) %>%
  
  # Species - "important"
  visGroups(
    groupname = "input",
    color = "blue",
    shape = "dot", 
    size = 60,
    shadow = list(enabled = FALSE)
  ) %>%
  
  # Species - "important"
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
    groupname = "env",
    color = "green",
    shape = "dot", 
    size = 20,
    shadow = list(enabled = FALSE)
  ) %>%
  
  # Species - "neg"
  visGroups(
    groupname = "neg",
    color = "grey",
    shape = "dot", 
    size = 10,
    shadow = list(enabled = FALSE)
  ) %>%
  
  # Reaction - "reaction"
  visGroups(
    groupname = "k",
    label = "",
    shape = "dot",
    size = 0,
    hidden = FALSE,
    physics = T
  ) %>% 
  
  visInteraction(
    dragNodes = TRUE,
    dragView = TRUE,
    zoomView = TRUE,
    navigationButtons = TRUE,
    tooltipDelay = 0
  ) %>%
  
  visOptions(
    highlightNearest = list(enabled = T, degree = 2, hover = T),
    nodesIdSelection = T,
    selectedBy = "group"
  ) %>%

  # Connector - "C"
  # visGroups(
  #   groupname = "C",
  #   label = "",
  #   shape = "dot",
  #   size = 0,
  #   hidden = FALSE,
  #   physics = TRUE) %>%
  
  # Physics Options
  visPhysics(
    # repulsion = list(
    #   nodeDistance = 0,
    #   springLength = 0
    # ),
    enabled = T
  )

net

# configs = matacc(model, "configs")
# classifs = matacc(model, "classifs")
# 
# row = configs[45,]
# row = unlist(classifs[row])
