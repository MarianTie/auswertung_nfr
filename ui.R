


navbarPage(title = 
                     div(add_busy_spinner(spin = "fading-circle", color = "#005A83"),"Graph Analyse Tool", div(img(src="logo.png", style= "position: relative; top: -34px; right: -1750px;", height =50))), 
                 
                 tabPanel("Deskriptive Analyse",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("group_node", "Wähle Kategorie", choices = unique(nodes$group)),
                                  uiOutput("ui1"),
                                  #selectInput("node", "Wähle alternatives Szenario", choices = unique(nodes_initial_scenarios$label)),
                                  h3("Analyse"),
                                  actionButton("get_scenario", "Erhalte ausgewähltes Szenario", style='margin-bottom:2px'),
                                  actionButton("risikotreiberSzenario", "Erhalte alle Risikotreiber für ausgewähltes Szenario", style='margin-bottom:2px'),
                                  actionButton("outgoing_paths", "Alle ausgehenden Pfade ab Knoten"),
                                  h3("Reset"),
                                  actionButton("back", "Zurück"),
                                  actionButton("global_graph", "Globaler Graph"),
                                  h3("Anordnung Szenarien"),
                                  actionButton("coord", "Erhalte Positionen", style='margin-bottom:2px'),
                                  actionButton("save", "Speicher Koordinaten in Datei", style='margin-bottom:2px'),
                                  h3("Download"),
                                  downloadButton("downloadNetwork", "Download Graph", style='margin-bottom:2px'),
                                  checkboxGroupInput("checkEdges", label = h3("Checkbox Kanten"),
                                                     choices = list("Teil" = "Teil", "wirkt auf" = "wirkt auf", "ist Teil" = "ist Teil",
                                                                    "ist erforderlich" = "ist erforderlich", "löst aus" ="löst aus"),
                                                     selected = c("Teil","wirkt auf","ist Teil","ist erforderlich","löst aus")), width = 3),
                              mainPanel(
                                  visNetworkOutput("network",width = "1300px", height = "900px")
                              )
                          )
                 ) ,
                 tabPanel("Quantitative Analyse",
                          sidebarLayout(
                              sidebarPanel(
                                  selectInput("scenario_node", "Wähle Szenario",""),
                                  uiOutput("ui2"),
                                  uiOutput("ui3"), 
                                  actionButton("analyse", "Erhalte Auswertung", style='margin-bottom:2px'),
                                  actionButton("verteilung", "Starte Simulation"),width = 3),
                              mainPanel(
                                  # verbatimTextOutput("test"),
                                  # verbatimTextOutput("test1"),
                                  tableOutput("values"),
                                  visNetworkOutput("exp_val_2",width = "1300px", height = "250px"),
                                  visNetworkOutput("exp_val_1",width = "1300px", height = "500px"),
                                  plotOutput("hist", width = "1300px", height = "500px"))
                              
                          )
                 )
)
