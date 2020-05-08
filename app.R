library(shiny)
library(neo4r)
library(tidyverse)
library(visNetwork)
library(igraph)
library(scales)
library(shinybusy)
library(future.apply)

## Working Directory anpassen

#setwd("Z:/AG_NFR/neo4r")

#####


con <- neo4j_api$new(
    url = "http://localhost:7474",
    user = "neo4j", 
    password = rstudioapi::askForPassword()
)

nodes <- readRDS("nodes.rds")
edges <- readRDS("edges.rds")
edges_prob <- edges
nodes_prob <- nodes
nodes_initial_scenarios <- nodes %>% filter(group == "scenario")

dummy <- 0


ui <- navbarPage(title = 
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


######################


server <- function(input, output, session) {
    
    
    #### update select knoten je nach auswahl der kategorie
    output$ui1 <- renderUI({
        
        df <- nodes %>% filter(group %in% input$group_node)
        
        selectInput("node", "Wähle Knoten", choices = unique(df$label))
        
    })
    
    
    
    ### mache variablen jeweils reactive
    nodes <- nodes
    
    makeReactiveBinding("nodes")
    
    #### 
    edges <- edges
    
    makeReactiveBinding("edges")
    
    ###  
    nodes_back <- NULL

    makeReactiveBinding("nodes_back")

    ####
    edges_back <- NULL

    makeReactiveBinding("edges_back")
    
    #### legend layout fix
    lnodes <- tibble(label = c("Legende","Ereignis", "Risikoereignis","Voraussetzung","Verwundbarkeit","Negative Auswirkung","Risikoart"),
                     shape = c("text",rep("dot",6)), color =  c("black","#44706F", "#C00000","#665728","#BDA660","#005A83","#96B9D2"),
                     font.color =  "black", font.size = c(20, rep(10,6)))  
    
    
    #### speicher nodes und edge daten vor verdichtung -> für zurück button
    
    observeEvent(input$outgoing_paths ,  {nodes_back <<- nodes 
                                          edges_back <<- edges})
    
    observeEvent(input$node_change , {nodes_back <<- nodes
                                      edges_back <<- edges})

    observeEvent(input$get_scenario ,  {nodes_back <<- nodes
                                        edges_back <<- edges})

    observeEvent(input$risikotreiberSzenario ,  {nodes_back <<- nodes
                                                 edges_back <<- edges})


    #c(), input$checkEdges)
    
    #### render network: wenn keine x,y koordinaten vorhanden -> initiale anordnung mit sugiyama algorithmus

    
    output$network <- renderVisNetwork({
        
        
        if (any(names(nodes) %in% "x") & any(names(nodes) %in% "y")) {
            
            visNetwork(nodes, edges)  %>%
                visGroups(groupname = "negativeImpact", size = 25, color = list(
                    background = "#005A83",
                    border = "#005A83")) %>%
                visGroups(groupname = "riskCategory", size = 30,   color = list(
                    background = "#96B9D2",
                    border = "#005A83")) %>%
                visGroups(groupname = "riskEvent",size = 20,  color = list(
                    background = "#C00000",
                    border = "#C00000")) %>%
                visGroups(groupname = "event", size = 20, color = list(
                    background = "#44706F",
                    border = "#44706F")) %>%
                visGroups(groupname = "vulnerability",size = 20, color = list(
                    background = "#BDA660",
                    border = "#BDA660")) %>%
                visGroups(groupname = "precondition",size = 20, color = list(
                    background = "#665728",
                    border = "#665728")) %>%
                visGroups(groupname = "scenario",size = 15, color = list(
                    background = "#ffffff",
                    border = "#f7c633")) %>%
                visEdges(smooth = F, font = list("size"=10), color = "black", arrows = "to") %>% 
                #visIgraphLayout("layout_with_sugiyama") %>%
                visLegend(width = 0.16, useGroups = F, addNodes = lnodes, stepY = 65) %>%
                visInteraction(navigationButtons = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:200px;word-break: break-word') %>%
                visOptions(highlightNearest = list(enabled = T,algorithm = "hierarchical", degree = list(from = nrow(nodes), to = nrow(nodes)))) %>%
                visPhysics(enabled = FALSE) %>%
                visEvents(doubleClick = "function(nodes) {
            Shiny.onInputChange('node_change', nodes.nodes);
            ;}")
            
            
        } else {
        
            g <- visNetwork(nodes, edges)  %>%
                visGroups(groupname = "negativeImpact", size = 25, color = list(
                    background = "#005A83",
                    border = "#005A83")) %>%
                visGroups(groupname = "riskCategory", size = 30,   color = list(
                    background = "#96B9D2",
                    border = "#005A83")) %>%
                visGroups(groupname = "riskEvent",size = 20,  color = list(
                    background = "#C00000",
                    border = "#C00000")) %>%
                visGroups(groupname = "event", size = 20, color = list(
                    background = "#44706F",
                    border = "#44706F")) %>%
                visGroups(groupname = "vulnerability",size = 20, color = list(
                    background = "#BDA660",
                    border = "#BDA660")) %>%
                visGroups(groupname = "precondition",size = 20, color = list(
                    background = "#665728",
                    border = "#665728")) %>%
                visGroups(groupname = "scenario",size = 15, color = list(
                    background = "#ffffff",
                    border = "#f7c633")) %>%
                visEdges(smooth = F, font = list("size"=10), color = "black", arrows = "to") %>%
                visIgraphLayout("layout_with_sugiyama") %>%
                #visHierarchicalLayout(direction = "LR", sortMethod = "directed", levelSeparation = 200) %>%
                visLegend(width = 0.16, useGroups = F, addNodes = lnodes, stepY = 65) %>%
                visInteraction(navigationButtons = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                    font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                    -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                     border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                     max-width:200px;word-break: break-word') %>%
                visOptions(highlightNearest = list(enabled = T,algorithm = "hierarchical", degree = list(from = nrow(nodes), to = nrow(nodes)))) %>%
                visPhysics(enabled = FALSE) %>%
                visEvents(doubleClick = "function(nodes) {
                Shiny.onInputChange('node_change', nodes.nodes);
                ;}")

            # change the x,y coordinates for LR arrangement and negImp right and riskCat top+random
            
       
            
            changed_coords <- g$x$nodes %>% mutate(y = ifelse(group == "negativeImpact", max(y)+0.1, y), 
                                                  x = ifelse(group == "riskCategory", min(x)-0.1,x)) %>%
              rowwise %>% mutate(y = ifelse(group == "riskCategory", y-runif(1,min=0.1,max=0.8), y),
                                 x = ifelse(group == "negativeImpact", x+runif(1,min=0.1,max=0.8), x))
                                                  
            coord_y <- changed_coords$y
            g$x$nodes$y <- changed_coords$x
            g$x$nodes$x <- coord_y
            g
            
           
            
        }
        
    })
    
   

   
    ### filter edges von kompletten graphen
    
    
    observeEvent(input$checkEdges, {
        
        graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
            call_neo4j(con, type = "graph")

        nodes <- graph$nodes %>%
            unnest_nodes() %>%
            rename(group = value, label = name_de)

        edges <- graph$relationships %>%
            unnest_relationships() %>%
            rename(from = startNode, to = endNode, label = name_de) %>%
            mutate(color = ifelse(type =="isSubRisk", "#A5ABB6",
                                  ifelse(type == "isPart","#A5ABB6",
                                         ifelse(type == "impacts", "#C00000",
                                                ifelse(type == "triggers","#C00000",
                                                       ifelse(type == "isRequired","#99833E","#f7c633"))))))
        #letztes oder für isMember -> "#f7c633"
        
        
        
        edges <<- edges %>% filter(label %in% input$checkEdges)
        
        nodes <<- nodes
        
    })
    
    
    #### algo abgehende pfade
    
    observeEvent(input$outgoing_paths, {
        
        
        
        ## Prüfe ob Szenario Node ausgewählt: Falls ja Fehlermeldung
        
        req(input$node)
        
        group_node <- nodes %>% filter(label == input$node) %>%
            pull(group)
        
        
        
        if(group_node == "scenario") {
            
            showNotification(ui = "Szenario Knoten haben keine abgehenden Pfade", type = "error", duration = 5)
            
            
            ## Ansonsten: query für Datenimport  + Nodes_check für späteren Check ob End-Node
        } else {
            
            ##################
            
            
            graph <- paste0('MATCH  (a)-[rel*]->(b) WHERE a.name_de = ',"'", input$node ,"'", " RETURN a,rel,b;") %>%
                call_neo4j(con, type = "graph")
            
            
            nodes_check <- graph$nodes %>%
                unnest_nodes() %>%
                rename(group = value, label = name_de) %>%
                filter(group != "scenario") %>%
                mutate(title = "...")
            
            
        } 
        
        ## Wenn scenario Node nicht ausgewählt wurde, checke ob ein End-Node ausgewählt wurde: Falls ja Fehlermeldung
        
        if (exists("nodes_check")) {
            
            if(dim(nodes_check)[1] == 1) {
                
                showNotification(ui = "Wähle einen Knoten, der kein End-Knoten ist", type = "error", duration = 5)
                
                ## Ansonsten normale Transformation der Daten für Graph-Visualisierung   
                
            } else {
                
                nodes <- graph$nodes %>%
                    unnest_nodes() %>%
                    rename(group = value, label = name_de) %>%
                    mutate(title = "...")
                
                id_scenario <- nodes %>%
                    filter(group == "scenario") %>%
                    pull(id)
                
                ##
                nodes <<- nodes %>% filter(!id %in% id_scenario)
                
                ###
                edges <- graph$relationships %>%
                    unnest_relationships() %>%
                    rename(from = startNode, to = endNode, label = name_de)%>% 
                    mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                          ifelse(type == "isPart","#A5ABB6",
                                                 ifelse(type == "impacts", "#C00000",
                                                        ifelse(type == "triggers","#C00000",
                                                               ifelse(type == "isRequired","#99833E","#f7c633"))))))
                # letztes oder für isMember -> "#f7c633"
                ##
                edges <<- edges %>% filter(!to %in% id_scenario & !from %in% id_scenario)
                
            }
        }
        
    })
    ####################### 
    
    
    
    observeEvent(input$node_change, {
        
        
        ########## Algo um alle isPart nodes und edges zu erhalten
        
        graph <- "MATCH  (a)-[rel]-(b) RETURN a,rel,b" %>%
            call_neo4j(con, type = "graph")
        
        ##
        nod <- graph$nodes %>%
            unnest_nodes() %>%
            rename(group = value, label = name_de) %>%
            mutate(title = "...")
        
        
        ### 
        edg <- graph$relationships %>%
            unnest_relationships() %>%
            rename(from = startNode, to = endNode, label = name_de) %>% 
            mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                  ifelse(type == "isPart","#A5ABB6",
                                         ifelse(type == "impacts", "#C00000",
                                                ifelse(type == "triggers","#C00000",
                                                       ifelse(type == "isRequired","#99833E","#f7c633"))))))
        # letztes oder für isMember -> "#f7c633"
        # %>%
        #   filter(!to %in% scenario_id)
        
        ### erhalte alle isPart Edges
        ispart_edg <- edg %>% filter(type == "isPart") 
        
        ### erhalte nodes von isPart Edges
        ispart_nod <- nod %>% filter(id %in% ispart_edg$from | id %in% ispart_edg$to)
        
        
        
        
        
        
        ################ Algo für Verdichtung
        
        ### erhalte label von double-clicked node
        choosen_node_label <- nod %>% filter(id == input$node_change) %>% pull(label)
        
        #choosen_node_label <- nod %>% filter(id == "18") %>% pull(label)
        
        
        ### erhalte group von double-clicked node
        choosen_node_group <- nod %>% filter(id == input$node_change) %>% pull(group)
        
        ### speicher query ergebnis um im anschluss zu prüfen ob daten importiert wurden
        
        # query <- suppressMessages(paste0("MATCH p = (a)-[*]->(b)-[*]->(c) WHERE b.name = ","'", choosen_node_label ,"'" ," RETURN p") %>%
        #                             call_neo4j(con, type = "graph"))
        
        
        ### für risikotreiber verdichtung
        
        # alle quellen
        quellen <- nod %>% filter(nod$id %in% edg$from & !nod$id %in% edg$to)
        
        # label von doppel clicked node
        
        label_quelle_clicked_node <- quellen %>% filter(id == input$node_change) %>% pull(label)
        
        
        ### wenn scenario, negativeImpact oder riskCategory groups ausgewählt wurden -> fehlermeldung
        
        suppressWarnings(if(choosen_node_group == "scenario" | choosen_node_group == "negativeImpact" | choosen_node_group == "riskCategory") {
            
            showNotification(ui = "Endknoten eignen sich nicht zur Verdichtung", type = "error", duration = 5)
            
            # check ob geklickter node in df 'alle quellen'
            
        } else if (any(quellen$id %in% input$node_change))  {
            
            
            
            
            # falls ja, neue query mit alle abgehenden pfaden
            
            graph <- paste0('MATCH  (a)-[rel*]->(b) WHERE a.name_de = ',"'", label_quelle_clicked_node ,"'", " RETURN a,rel,b;") %>%
                call_neo4j(con, type = "graph")
            
            
            ## unnest zu dataframe und umbennenung + riskCategory gefiltert, da später durch isPart angefügt werden
            nod <- graph$nodes %>%
                unnest_nodes() %>%
                rename(group = value, label = name_de) %>%
                filter(group != "riskCategory") %>%
                mutate(title = "...")
            
            ### pull scenario node id
            scenario_id <- nod %>% filter(group == "scenario") %>%
                pull(id)
            
            
            ### unnest zu dataframe und umbenennung + lösche edges, die zum scenario node gehen
            edg <- graph$relationships %>%
                unnest_relationships() %>%
                rename(from = startNode, to = endNode, label = name_de) %>%
                filter(!to %in% scenario_id) %>% 
                mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                      ifelse(type == "isPart","#A5ABB6",
                                             ifelse(type == "impacts", "#C00000",
                                                    ifelse(type == "triggers","#C00000",
                                                           ifelse(type == "isRequired","#99833E","#f7c633"))))))
            # letztes oder für isMember -> "#f7c633"
            
            ###
            
            ## node ids der pfade des gewählten nodes
            rel_nod_id <- nod %>% pull(id)
            
            ## wähle isPart Edges von den relevanten nodes
            ispart_edg_rel <- ispart_edg %>% filter(from %in% rel_nod_id)
            
            ## wähle relevante nodes die isPart edges haben
            ispart_node_rel <- ispart_nod %>% filter(id %in% ispart_edg_rel$from | id %in% ispart_edg_rel$to)
            
            ## id des gewählten nodes
            #id_choosen_node <- nod %>% filter(label == label_quelle_clicked_node) %>% pull(id)
            id_choosen_node <- input$node_change
            
            ## richte isPart pfeile auf gewählten node und entferne duplicates, da multiple, vorherige nodes einer risikoart
            # zugeordnet sein können
            ispart_edg_rel_auf_node <- ispart_edg_rel %>% mutate(from = id_choosen_node) %>% distinct(to, .keep_all = T)
            
            ## filter riskCategory
            ispart_node_rel_riskCategory <- ispart_node_rel %>% filter(group == "riskCategory")
            
            
            ### ausgewählter node
            b <- nod %>% filter(label == label_quelle_clicked_node)
            
            
            ### alle senken
            c <- nod %>% filter( group != "scenario", nod$id %in% edg$to & !nod$id %in% edg$from)
            
            ## füge angeklickten risikotreiber-node, alle senken & alle relevanten Risikoarten zusammen
            nodes <<- bind_rows(b,c,ispart_node_rel_riskCategory) 
            
            ## erstelle edges von b zu c
            e <- tibble(from = b$id, to = c$id)
            
            ## füge edges (b)-->(c) sowie alle relevanten isPart Rel auf node hinzu
            edges <<- bind_rows(e, ispart_edg_rel_auf_node)
            
            
        } else  {
            
            #graph <- query
            graph <- paste0("MATCH p = (a)-[*]->(b)-[*]->(c) WHERE b.name_de = ","'", choosen_node_label ,"'" ," RETURN p") %>%
                call_neo4j(con, type = "graph")
            
            
            # graph <- paste0("MATCH p = (a)-[*]->(b)-[*]->(c) WHERE b.name_de = ","'", "Wertverlust Kreditportfolio des Institutes" ,"'" ," RETURN p") %>%
            #   call_neo4j(con, type = "graph")
            
            
            ## unnest zu dataframe und umbennenung + riskCategory gefiltert, da später durch isPart angefügt werden + scenario node raus
            nod <- graph$nodes %>%
                unnest_nodes() %>%
                rename(group = value, label = name_de) %>%
                filter(group != "riskCategory") %>%
                mutate(title = "...")
            
            ### pull scenario node id
            scenario_id <- nod %>% filter(group == "scenario") %>%
                pull(id)
            
            
            ### unnest zu dataframe und umbenennung + lösche edges, die zum scenario node gehen
            edg <- graph$relationships %>%
                unnest_relationships() %>%
                rename(from = startNode, to = endNode, label = name_de) %>%
                filter(!to %in% scenario_id) %>% 
                mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                      ifelse(type == "isPart","#A5ABB6",
                                             ifelse(type == "impacts", "#C00000",
                                                    ifelse(type == "triggers","#C00000",
                                                           ifelse(type == "isRequired","#99833E","#f7c633"))))))
            # letztes oder für isMember -> "#f7c633"
            
            ###
            
            ## node ids der pfade des gewählten nodes
            rel_nod_id <- nod %>% pull(id)
            
            ## wähle isPart Edges von den relevanten nodes
            ispart_edg_rel <- ispart_edg %>% filter(from %in% rel_nod_id)
            
            ## wähle relevante nodes die isPart edges haben
            ispart_node_rel <- ispart_nod %>% filter(id %in% ispart_edg_rel$from | id %in% ispart_edg_rel$to)
            
            ## id des gewählten nodes
            id_choosen_node <- nod %>% filter(label == choosen_node_label) %>% pull(id)
            
            
            ## richte isPart pfeile auf gewählten node und entferne duplicates, da multiple, vorherige nodes einer risikoart
            # zugeordnet sein können
            ispart_edg_rel_auf_node <- ispart_edg_rel %>% mutate(from = id_choosen_node) %>% distinct(to, .keep_all = T)
            
            ## filter riskCategory
            ispart_node_rel_riskCategory <- ispart_node_rel %>% filter(group == "riskCategory")
            
            
            ### alle quellen
            a <- nod %>% filter(nod$id %in% edg$from & !nod$id %in% edg$to) 
            
            ### ausgewählter node
            b <- nod %>% filter(label == choosen_node_label)
            
            ### alle senken
            c <- nod %>% filter(group != "scenario", nod$id %in% edg$to & !nod$id %in% edg$from)
            
            
            ###
            nodes <<- bind_rows(a,b,c,ispart_node_rel_riskCategory)
            
            d <- tibble(from = a$id, to= b$id)
            e <- tibble(from = b$id, to = c$id)
            
            edges <<- bind_rows(d,e, ispart_edg_rel_auf_node)
            
            
        })
        
    })
    
    
    
    
    ############
    
    # erhalte positionen von ausgewähltem network nodes
    observeEvent(input$coord, {
        visNetworkProxy("network") %>% visGetPositions()
    })
    
    # erstelle reactive value von network positions um sie auf NULL setzen zu können
    rv <- reactiveValues(coord = NULL)
    observe({  rv$coord <- input$network_positions})
    
    ####################################################################
    # speicher scenario node name für koord-tabelle
    scenario_node <- eventReactive(input$get_scenario, {input$node})
    
    
    # format node positionen
    
    nodes_positions <- reactive({
        
        positions <- rv$coord
        
        if(!is.null(positions)) {
            nodes_positions <- do.call("rbind", lapply(positions, function(x){ data.frame(x = x$x, y = x$y)}))
            nodes_positions$id <- names(positions)
            #nodes_positions <- left_join(nodes_positions, nodes, by = "id") %>% mutate(scenario = scenario_node())
            nodes_positions
        } else {
            NULL
        }
        
    })
    # 
    
    
    
    ### speicher die koordinaten in externe datei
    
    observeEvent(input$save, {
        
        
        
        positions <- nodes_positions()
        
        ## wenn koordinaten vor speicherung noch nicht abgefragt wurden -> warnmeldung
        if (is.null(positions)) {
            
            showNotification(ui = "Klicken sie zuerst auf den Button 'Erhalte Positionen' um
    im Anschluss die Koordinaten in eine externe Datei zu speichern", type = "warning", duration = 10)
            
            
            
            ## wenn externe datei in der working directory vorhanden -> datei wird um neue koordinaten erweitert
            ## falls koordinaten für das scenario schon vorhanden -> koordinaten werden aktualisiert
            
        } else if (file.exists("node_positions.csv")) {
            
            # importiere koord-tabelle
            df <- read_csv("node_positions.csv", col_types = cols()) 
            df <- mutate(df, id = as.character(id))
            
            # erhalte aktuelle koordinaten und füge scenario name an
            data <- nodes_positions() 
            data <- mutate(data, scenario = scenario_node())
            
            # füge neues scenario an. falls schon vorhanden wird altes gelöscht
            df_neu <- bind_rows(df, data) %>% map_df(rev) %>% distinct(id,scenario, .keep_all = T)
            
            # exportiere koord-tabelle
            write_excel_csv(df_neu,"node_positions.csv")
            
            # reset reactive koordinaten
            rv$coord <- NULL
            
            
            ## wenn externe datei mit koordinaten noch nicht vorhanden -> wird mit koordinaten von aktuellem scenario erstellt
        } else  {
            
            data <- nodes_positions() 
            data <- mutate(data, scenario = scenario_node())
            write_excel_csv(data,"node_positions.csv")
            
            # reset reactive koordinaten
            rv$coord <- NULL
        }
        
    })
    
    
    #### algo für scenario auswahl
    
    observeEvent(input$get_scenario, {
        
        req(input$node)
        
        node_group <- nodes %>% filter(label == input$node) %>% pull(group)
        
        table_positions <- NULL
        
        
        if (file.exists("node_positions.csv"))  {
            
            table_positions <- read_csv("node_positions.csv", col_types = cols())
            table_positions <- mutate(table_positions, id = as.character(id)) 
        }
        
        ## prüfe ob ein scenario ausgewählt wurde
        
        if(node_group != "scenario") {
            
            showNotification(ui = "Wähle ein Szenario aus", type = "error", duration = 5)
            
            
            
            ## wenn koordinaten für das scenario bereits vorhanden -> scenario wird damit erstellt  
            
        } else if (file.exists("node_positions.csv") & any(table_positions$scenario %in% scenario_node()))  {
            
            
            graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
                call_neo4j(con, type = "graph")
            
            nod <- graph$nodes %>%
                unnest_nodes() %>%
                rename(group = value, label = name_de) %>%
                mutate(title = "...")
            
            edg <- graph$relationships %>%
                unnest_relationships() %>%
                rename(from = startNode, to = endNode, label = name_de) %>% 
                mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                      ifelse(type == "isPart","#A5ABB6",
                                             ifelse(type == "impacts", "#C00000",
                                                    ifelse(type == "triggers","#C00000",
                                                           ifelse(type == "isRequired","#99833E","#f7c633"))))))
            # letztes oder für isMember -> "#f7c633"
            
            ### id von scenario node
            
            s_id <- nod %>% filter(label == input$node) %>%
                pull(id)
            
            
            ### id von nodes n: (n)->(scenario node)
            
            nod_id_s <- edg %>% filter(to == s_id) %>%
                pull(from)
            
            ## filter alle nodes von scenario und füge koordinaten hinzu
            
            
            nodes <<- nod %>% filter(id %in% nod_id_s) %>% mutate(scenario = scenario_node()) %>%
                left_join(table_positions, by = c("id","scenario"))
            
            ## filter alle relevanten edges
            
            edges <<- edg %>% filter(from %in% nodes$id & to %in% nodes$id)
            
            ## wenn für scenario noch keine koordinaten vorhanden -> anordnung mit sugiyama algorithmus
            
        } else  {
            
            graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
                call_neo4j(con, type = "graph")
            
            nod <- graph$nodes %>%
                unnest_nodes() %>%
                rename(group = value, label = name_de) %>%
                mutate(title = "...")
            
            edg <- graph$relationships %>%
                unnest_relationships() %>%
                rename(from = startNode, to = endNode, label = name_de) %>% 
                mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                      ifelse(type == "isPart","#A5ABB6",
                                             ifelse(type == "impacts", "#C00000",
                                                    ifelse(type == "triggers","#C00000",
                                                           ifelse(type == "isRequired","#99833E","#f7c633"))))))
            # letztes oder für isMember -> "#f7c633"
            
            ### id von scenario node
            
            s_id <- nod %>% filter(label == input$node) %>%
                pull(id)
            
            
            ### id von nodes n: (n)->(scenario node)
            
            nod_id_s <- edg %>% filter(to == s_id) %>%
                pull(from)
            
            ## filter alle nodes von scenario
            
            nodes <<- nod %>% filter(id %in% nod_id_s)
            
            ## filter alle relevanten edges
            
            edges <<- edg %>% filter(from %in% nodes$id & to %in% nodes$id)
            
            
        }
            
        #     req(input$alternative_scenario)
        #     
        #    
        #     
        #     node_group <- nodes %>% filter(label == input$alternative_scenario) %>% pull(group)
        #     
        #     table_positions <- NULL
        #     
        #     
        #     if (file.exists("node_positions.csv"))  {
        #         
        #         table_positions <- read_csv("node_positions.csv", col_types = cols())
        #         table_positions <- mutate(table_positions, id = as.character(id)) 
        #     }
        #     
        #     ## prüfe ob ein scenario ausgewählt wurde
        #     
        #     if(node_group != "scenario") {
        #         
        #         showNotification(ui = "Wähle ein Szenario aus", type = "error", duration = 5)
        #         
        #         
        #         
        #         ## wenn koordinaten für das scenario bereits vorhanden -> scenario wird damit erstellt  
        #         
        #     } else if (file.exists("node_positions.csv") & any(table_positions$scenario %in% scenario_node()))  {
        #         
        #         
        #         graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
        #             call_neo4j(con, type = "graph")
        #         
        #         nod <- graph$nodes %>%
        #             unnest_nodes() %>%
        #             rename(group = value, label = name_de) %>%
        #             mutate(title = "...")
        #         
        #         edg <- graph$relationships %>%
        #             unnest_relationships() %>%
        #             rename(from = startNode, to = endNode, label = name_de) %>% 
        #             mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
        #                                   ifelse(type == "isPart","#A5ABB6",
        #                                          ifelse(type == "impacts", "#C00000",
        #                                                 ifelse(type == "triggers","#C00000",
        #                                                        ifelse(type == "isRequired","#99833E","#f7c633"))))))
        #         # letztes oder für isMember -> "#f7c633"
        #         
        #         ### id von scenario node
        #         
        #         s_id <- nod %>% filter(label == input$alternative_scenario) %>%
        #             pull(id)
        #         
        #         
        #         ### id von nodes n: (n)->(scenario node)
        #         
        #         nod_id_s <- edg %>% filter(to == s_id) %>%
        #             pull(from)
        #         
        #         ## filter alle nodes von scenario und füge koordinaten hinzu
        #         
        #         
        #         nodes <<- nod %>% filter(id %in% nod_id_s) %>% mutate(scenario = scenario_node()) %>%
        #             left_join(table_positions, by = c("id","scenario"))
        #         
        #         ## filter alle relevanten edges
        #         
        #         edges <<- edg %>% filter(from %in% nodes$id & to %in% nodes$id)
        #         
        #         ## wenn für scenario noch keine koordinaten vorhanden -> anordnung mit sugiyama algorithmus
        #         
        #     } else  {
        #         
        #         graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
        #             call_neo4j(con, type = "graph")
        #         
        #         nod <- graph$nodes %>%
        #             unnest_nodes() %>%
        #             rename(group = value, label = name_de) %>%
        #             mutate(title = "...")
        #         
        #         edg <- graph$relationships %>%
        #             unnest_relationships() %>%
        #             rename(from = startNode, to = endNode, label = name_de) %>% 
        #             mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
        #                                   ifelse(type == "isPart","#A5ABB6",
        #                                          ifelse(type == "impacts", "#C00000",
        #                                                 ifelse(type == "triggers","#C00000",
        #                                                        ifelse(type == "isRequired","#99833E","#f7c633"))))))
        #         # letztes oder für isMember -> "#f7c633"
        #         
        #         ### id von scenario node
        #         
        #         s_id <- nod %>% filter(label == input$alternative_scenario) %>%
        #             pull(id)
        #         
        #         
        #         ### id von nodes n: (n)->(scenario node)
        #         
        #         nod_id_s <- edg %>% filter(to == s_id) %>%
        #             pull(from)
        #         
        #         ## filter alle nodes von scenario
        #         
        #         nodes <<- nod %>% filter(id %in% nod_id_s)
        #         
        #         ## filter alle relevanten edges
        #         
        #         edges <<- edg %>% filter(from %in% nodes$id & to %in% nodes$id)
        #     
        #     
        # }
        # 
             
    })
    
    
    observeEvent(input$risikotreiberSzenario, {
        
        # 
        # req(input$node)
        # 
        # ## prüfe ob ein szenario ausgewählt wurde
        # 
        # node_group <- nodes %>% filter(label == input$node) %>% pull(group)
        # 
        # if(node_group != "scenario") {
        #     
        #     showNotification(ui = "Wähle ein Szenario aus", type = "error", duration = 5)
        #     
        #     
        #     
        #     
        # } else   {
            
            
            # graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
            #     call_neo4j(con, type = "graph")
            # 
            # nod <- graph$nodes %>%
            #     unnest_nodes() %>%
            #     rename(group = value, label = name_de)
            # 
            # edg <- graph$relationships %>%
            #     unnest_relationships() %>%
            #     rename(from = startNode, to = endNode, label = name_de) %>% 
            #     mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
            #                           ifelse(type == "isPart","#A5ABB6",
            #                                  ifelse(type == "impacts", "#C00000",
            #                                         ifelse(type == "triggers","#C00000",
            #                                                ifelse(type == "isRequired","#99833E","#f7c633"))))))
            # # letztes oder für isMember -> "#f7c633"
            # 
            # ### id von szenario node
            # 
            # s_id <- nod %>% filter(label == input$node) %>%
            #     pull(id)
            # 
            # # scenario node
            # szenario_node <- nod %>% filter(id == s_id)      
            # ######
            # 
            # 
            # ### id von nodes n: (n)->(szenario node)
            # 
            # nod_id_s <- edg %>% filter(to == s_id) %>%
            #     pull(from)
            # 
            # 
            # ## filter alle nodes von szenario 
            # 
            # 
            # nod <- nod %>% filter(id %in% nod_id_s)
            # 
            # ## alle quellen von scenario
            # 
            # a <- nod %>% filter(nod$id %in% edg$from & !nod$id %in% edg$to)
            # 
            # 
            # ##
            # nodes <<- bind_rows(a,szenario_node)
            # 
            # ##
            # edges <<- tibble(from = a$id, to = szenario_node$id, label = "Risikotreiber von")
            
            ## filter quellen
            nod <- nodes %>% filter(nodes$id %in% edges$from & !nodes$id %in% edges$to)
            
            ## erstelle szenario node mit neuer id
            zufallszahlen <- sample(1:100000,100000)
            
            zufallszahlen_exclusive_existing_ids <- zufallszahlen[!zufallszahlen %in% nod$id]
            
            scenario <- tibble(id = as.character(sample(zufallszahlen_exclusive_existing_ids,1)), group = "scenario", label = scenario_node())
            
            nodes <<- bind_rows(nod, scenario)
            ##
            edges <<- tibble(id = 1:nrow(nod), from = nod$id, to = scenario$id, label = "Risikotreiber von")
            
            
        #}
        
        
    })
    
    
    # 
    
    observeEvent(input$back, {

   nodes <<- nodes_back
   edges <<- edges_back


    })


    # ######################################
    
    observeEvent(input$global_graph, {
        
        graph <- "MATCH  (a)-[rel]-(b) RETURN a,rel,b;" %>%
            call_neo4j(con, type = "graph")
        
        
        ## unnest graph to dataframe & change names, sodass visNetwork die Spalten nutzen kann
        nodes <<- graph$nodes %>% 
            unnest_nodes() %>%
            rename(group = value, label = name_de) %>%
            mutate(title = "...")
        
        edges <<- graph$relationships %>% 
            unnest_relationships() %>%
            rename(from = startNode, to = endNode, label = name_de) %>% 
            mutate(color = ifelse(type =="isSubRisk", "#A5ABB6", 
                                  ifelse(type == "isPart","#A5ABB6",
                                         ifelse(type == "impacts", "#C00000",
                                                ifelse(type == "triggers","#C00000",
                                                       ifelse(type == "isRequired","#99833E","#f7c633"))))))
        # letztes oder für isMember -> "#f7c633"
        
        
        
        
    })
    
    
    
    # 
    output$downloadNetwork <- downloadHandler(
        filename = function() {
            paste0('Graph-', Sys.Date(), '.html')
        },
        content = function(con) {
            
            if (any(names(nodes) %in% "x") & any(names(nodes) %in% "y")) {
                
                visNetwork(nodes, edges)  %>%
                    visGroups(groupname = "negativeImpact", size = 25, color = list(
                        background = "#005A83",
                        border = "#005A83")) %>%
                    visGroups(groupname = "riskCategory", size = 30,   color = list(
                        background = "#96B9D2",
                        border = "#005A83")) %>%
                    visGroups(groupname = "riskEvent",size = 20,  color = list(
                        background = "#C00000",
                        border = "#C00000")) %>%
                    visGroups(groupname = "event", size = 20, color = list(
                        background = "#44706F",
                        border = "#44706F")) %>%
                    visGroups(groupname = "vulnerability",size = 20, color = list(
                        background = "#BDA660",
                        border = "#BDA660")) %>%
                    visGroups(groupname = "precondition",size = 20, color = list(
                        background = "#665728",
                        border = "#665728")) %>%
                    visGroups(groupname = "scenario",size = 15, color = list(
                        background = "#ffffff",
                        border = "#f7c633")) %>%
                    visEdges(smooth = F, font = list("size"=10), color = "black", arrows = "to") %>% 
                    #visIgraphLayout("layout_with_sugiyama") %>%
                    visLegend(width = 0.12, useGroups = F, addNodes = lnodes, stepY = 65) %>%
                    visInteraction(navigationButtons = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:200px;word-break: break-word') %>%
                    visOptions(highlightNearest = list(enabled = T,algorithm = "hierarchical", degree = list(from = nrow(nodes), to = nrow(nodes)))) %>%
                    visPhysics(enabled = FALSE) %>%
                    visEvents(doubleClick = "function(nodes) {
            Shiny.onInputChange('node_change', nodes.nodes);
            ;}") %>% visSave(con)
                
                
            } else  {
                
                g <- visNetwork(nodes, edges)  %>%
                    visGroups(groupname = "negativeImpact", size = 25, color = list(
                        background = "#005A83",
                        border = "#005A83")) %>%
                    visGroups(groupname = "riskCategory", size = 30,   color = list(
                        background = "#96B9D2",
                        border = "#005A83")) %>%
                    visGroups(groupname = "riskEvent",size = 20,  color = list(
                        background = "#C00000",
                        border = "#C00000")) %>%
                    visGroups(groupname = "event", size = 20, color = list(
                        background = "#44706F",
                        border = "#44706F")) %>%
                    visGroups(groupname = "vulnerability",size = 20, color = list(
                        background = "#BDA660",
                        border = "#BDA660")) %>%
                    visGroups(groupname = "precondition",size = 20, color = list(
                        background = "#665728",
                        border = "#665728")) %>%
                    visGroups(groupname = "scenario",size = 15, color = list(
                        background = "#ffffff",
                        border = "#f7c633")) %>%
                    visEdges(smooth = F, font = list("size"=10), color = "black", arrows = "to") %>% 
                    visIgraphLayout("layout_with_sugiyama") %>%
                    #visHierarchicalLayout(direction = "LR", sortMethod = "directed", levelSeparation = 200) %>%
                    visLegend(width = 0.12, useGroups = F, addNodes = lnodes, stepY = 65) %>%
                    visInteraction(navigationButtons = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:200px;word-break: break-word') %>%
                    visOptions(highlightNearest = list(enabled = T,algorithm = "hierarchical", degree = list(from = nrow(nodes), to = nrow(nodes)))) %>%
                    visPhysics(enabled = FALSE) %>%
                    visEvents(doubleClick = "function(nodes) {
            Shiny.onInputChange('node_change', nodes.nodes);
            ;}") %>% visSave(con)
                
                # change the x,y coordinates for LR arrangement
                coord_y <- g$x$nodes$y
                g$x$nodes$y <- g$x$nodes$x
                g$x$nodes$x <- coord_y
                g
            }
            
        }
    )
    
    ######################### Quantitative Analyse
    
    ### Quellen und Senken je Szenario für Auswahl
    
    output$ui2 <- renderUI({
        
        scenario_id <- nodes_prob %>% filter(label == input$scenario_node) %>% pull(id)
        
        #scenario_id <- nodes %>% filter(label == "Szenario Beispiel EW/WHS") %>% pull(id)
        #############################################################################################################
        
        nodes_ids_scenario <- edges_prob %>% filter(to == scenario_id) %>% pull(from)
        
        nodes_scenario <- nodes_prob %>% filter(id %in% nodes_ids_scenario)
        
        ##
        edges_ohne_isMember <- edges_prob %>% filter(type != "isMember")
        
        ##start nodes
        
        quellen_szenario <- nodes_scenario %>% filter(nodes_scenario$id %in% edges_ohne_isMember$from & !nodes_scenario$id %in% edges_ohne_isMember$to,
                                                      !group %in% c("vulnerability","precondition"))
        
        selectInput("start_node", "Wähle eine Quelle", choices = unique(quellen_szenario$label))
        
        
    })
    
    
    
    output$ui3 <- renderUI({
        
        scenario_id <- nodes_prob %>% filter(label == input$scenario_node) %>% pull(id)
        #scenario_id <- nodes %>% filter(label == "Szenario Beispiel EW/WHS") %>% pull(id)
        
        nodes_ids_scenario <- edges_prob %>% filter(to == scenario_id) %>% pull(from)
        
        nodes_scenario <- nodes_prob %>% filter(id %in% nodes_ids_scenario)
        
        ##
        edges_ohne_isMember <- edges_prob %>% filter(type != "isMember")
        
        ##end nodes
        senken_szenario <- nodes_scenario %>% filter(group != "riskCategory" ,!nodes_scenario$id %in% edges_ohne_isMember$from & nodes_scenario$id %in% edges_ohne_isMember$to)
        
        
        selectInput("end_node", "Wähle eine Senke", choices = unique(senken_szenario$label))
        
        
        
    })
    
    
    # 
    # ### mache variablen jeweils reactive
    
    nodes_exp_value_1 <- nodes_prob
    
    makeReactiveBinding("nodes_exp_value_1")
    
    ####
    edges_exp_value_1 <- edges_prob
    
    makeReactiveBinding("edges_exp_value_1")
    
    
    ### mache variable 'nodes' und 'edges' jeweils reactive
    nodes_exp_value_2 <- nodes_prob
    
    makeReactiveBinding("nodes_exp_value_2")
    
    ####
    edges_exp_value_2 <- edges_prob
    
    makeReactiveBinding("edges_exp_value_2")
    # 
    
    ### für verlustverteilung
    
    
    ##
    
    loss_verteilung <- 0
    
    makeReactiveBinding("loss_verteilung")
    
    
    rv <- reactiveValues(exp_value_2 = dummy,
                         exp_value_1 = dummy,
                         prob_start_end = dummy,
                         anzahl_pfade = dummy,
                         nodes_cumprod_pro_path = dummy,
                         anzahl_simulationen = 0)
    
    
    #selectInput("scenario_node", "Wähle Szenario", choices = unique(nodes_initial_scenarios$label), selected = )
    
    observe({
        updateSelectInput(session, "scenario_node",
                          label = "Wähle Szenario",
                          choices = unique(nodes_initial_scenarios$label),
                          selected = scenario_node())
    })
    
    
    
    
    observeEvent(input$analyse, {
        
        ### filter gewähltes szenario
        
        graph <- "MATCH (a)-[rel]-(b)  RETURN a,rel,b" %>%
            call_neo4j(con, type = "graph")
        
        nod <- graph$nodes %>%
            unnest_nodes() %>%
            rename(group = value) %>%
            mutate(title = "...")
        
        edg <- graph$relationships %>%
            unnest_relationships()
        ### id von scenario node
        
        ###############################################################################################################
        s_id <- nod %>% filter(name_de == input$scenario_node) %>%
            pull(id)
        
        # s_id <- nod %>% filter(name_de == "Szenario Beispiel EW/WHS") %>%
        #   pull(id)
        
        
        ### id von nodes n: (n)->(scenario node)
        
        nod_id_s <- edg %>% filter(endNode == s_id) %>%
            pull(startNode)
        
        ## ändern name bei nodes und reihenfolge bei den edges für igraph
        
        
        nodes_igraph <- nod %>% filter(id %in% nod_id_s) %>% rename(name = name_de)
        ## filter alle relevanten edges
        
        edges_igraph <- edg %>% filter(type != "isMember", startNode %in% nodes_igraph$id & endNode %in% nodes_igraph$id) %>% 
            select(startNode, endNode, type, everything())
        
        
        
        
        ### berechnung
        
        
        
        
        ## 
        
        ###
        
        igraph_object <- graph_from_data_frame(d = edges_igraph, vertices = nodes_igraph, directed = T)
        
        all_paths <- all_simple_paths(igraph_object, from = input$start_node, to = input$end_node)
        
        ##############################################################################################################
        #all_paths <- all_simple_paths(igraph_object, from = "RE1", to = "Finanzlage")
        
        
        ## konvertierung von igraph.vs objekt zu tibble
        
        pfade_temp <- map(all_paths, as_ids) %>% map(as.tibble)
        
        
        pfade <- bind_rows(pfade_temp, .id = "path") %>% rename(name = value)
        
        
        ## füge id (neo4j), group, ggf. loss an die nodes
        nodes_pfade <- left_join(pfade, nodes_igraph, by =  "name")
        
        rv$anzahl_pfade <- max(nodes_pfade$path)
        
        ########algo um den einzelnen knoten der pfade die richtige whs zuzuordnen
        # beachtet das ein node mehrere eingehende pfade hat.
        # je nach pfad wird die richtige whs ausgewählt
        
        
        split <- nodes_pfade %>% group_split(path)
        
        
        ## edges ohne edge id
        edges_igraph_excl_id <- edges_igraph %>% select(-id)
        
        ### die probs der kanten werden an den Endknoten der jeweiligen Kante gejoint.
        
        
        ####### vectorisieren####################################
        
        # anzahl durchläufe entspricht anzahl pfade
        for(i in 1:length(split)) {
            
            # window länge 2 über ids der pfade -> je window wird die richtige kante ermittelt und nacheinander in edges_join geschrieben
            
            for(j in 1:(nrow(split[[i]])-1)) {
                
                a <- j
                b <- j+1
                
                temp <- edges_igraph_excl_id %>% filter(startNode == split[[i]]$id[a] & endNode == split[[i]]$id[b])
                
                if ( j == 1) {
                    edges_join <- temp
                    
                } else {
                    edges_join <- bind_rows(edges_join, temp)
                    
                }
                
            }
            # die pfadrelevanten edges werden pfad für pfad gejoint und in output geschrieben
            
            temp_join<- left_join(split[[i]], edges_join, by = c("id" = "endNode"))
            
            if (i == 1 ) {
                output <- temp_join
                
            } else {
                
                output <- bind_rows(output, temp_join)
                
            }
        }
        
        
        
        ####### Füge vulnerabilities/preconditions korrekt an
        
        
        ## vulnerabilities/preconditions mit deren endNodes
        vuln_prec_nodes <- nodes_igraph %>% filter(group %in% c("vulnerability","precondition")) %>%
            left_join(edges_igraph[c("startNode","endNode")], by = c("id" = "startNode"))
        
        
        #### füge korrekten path an vulnerabilities an
        
        ##split pfade in list elemente
        temp_a <- nodes_pfade %>%
            group_split(path)
        
        ## füge an jedes listelement vulnerabilities/preconditions an
        temp_b <-   map(temp_a, ~bind_rows(., vuln_prec_nodes))
        
        
        ## filter relevante vulnerabilities
        temp_c <-  map(temp_b, ~filter(.x,endNode %in% id))
        
        ## füge relevante vulnerabilities/preconditions and listelemente an
        list_temp <- list(temp_a, temp_c)
        
        
        nodes_incl_vuln_pre_pfade <-  pmap(list_temp, bind_rows) %>% map(~fill(.,path)) %>%
            map(~rename(., end_vuln_prec = endNode))
        
        
        nodes_vuln_pre_pfade <- nodes_incl_vuln_pre_pfade %>% map(~filter(.x, group %in% c("vulnerability","precondition")))
        
        
        #### füge probability an vulnerabilities an
        
        edges_vuln_prec <- edges_igraph %>% filter(type == "isRequired") %>% select(-id) #, probability_0, probability_1)
        
        
        split_2 <-  nodes_vuln_pre_pfade
        
        
        for(i in 1:length(split_2)) {
            
            
            # wenn listelement leer i+1
            if(nrow(split_2[[i]]) == 0) next
            
            for(j in 1:(nrow(split_2[[i]]))) {
                
                
                
                temp_v_c <- edges_vuln_prec %>% filter(startNode == split_2[[i]]$id[j] & endNode == split_2[[i]]$end_vuln_prec[j])
                
                if (j == 1) {
                    edges_join_v_c <- temp_v_c
                    
                } else {
                    edges_join_v_c <- bind_rows(edges_join_v_c, temp_v_c)
                    
                }
                
            }
            # die pfadrelevanten edges werden pfad für pfad gejoint und in output geschrieben
            
            temp_join_v_c <- left_join(split_2[[i]], edges_join_v_c, by = c("id" = "startNode", "end_vuln_prec" = "endNode"))
            
            if (i == 1 ) {
                output_v_c <- temp_join_v_c
                
            } else {
                
                output_v_c <- bind_rows(output_v_c, temp_join_v_c)
                
            }
        }
        
        
        
        ##### füge die vulnerbailities an richtiger stelle pro pfad ein
        
        nodes_pfade_reihenfolge <- output %>% group_by(path) %>% mutate(reihenfolge_in_pfad = row_number()) %>% ungroup()
        
        
        
        nodes_komplett_probs <- bind_rows(nodes_pfade_reihenfolge, output_v_c)
        
        ###
        
        # splitte pfade
        pfade_split <- nodes_komplett_probs %>% group_split(path)
        
        
        # vuln pro pfade gesplittet
        vuln_pfade <- pfade_split %>% map(~filter(.x, end_vuln_prec %in% id & path %in% path))
        
        # filter nodes mit vuln gesplittet
        
        nodes_vuln_eingehend_gesplittet <- pfade_split %>% map(~mutate(.x, helper = ifelse(id %in% end_vuln_prec, "vuln_eingehend",NA))) %>%
            map(~filter(.x, helper == "vuln_eingehend"))
        
        
        
        
        ## füge an vulnerabilities richtige reihenfolgenummer an
        
        
        
        for(i in 1:length(vuln_pfade)) {
            
            
            
            # wenn listelement leer i+1
            if(nrow(vuln_pfade[[i]]) == 0) next
            
            
            for(j in 1:nrow(vuln_pfade[[i]])) {
                
                temp <-  vuln_pfade[[i]][j,] %>%  mutate(reihenfolge_in_pfad = ifelse(end_vuln_prec == nodes_vuln_eingehend_gesplittet[[i]][j,]$id &&
                                                                                          path == nodes_vuln_eingehend_gesplittet[[i]][j,]$path,
                                                                                      nodes_vuln_eingehend_gesplittet[[i]][j,]$reihenfolge_in_pfad,NA))
                if(j ==1) {
                    
                    pro_pfad <- temp
                    
                } else {
                    
                    pro_pfad <- bind_rows(pro_pfad, temp)
                    
                }
                
            }
            
            if(i == 1) {
                
                reihenfolge_vuln <- pro_pfad
                
            } else {
                
                reihenfolge_vuln <- bind_rows(reihenfolge_vuln, pro_pfad)
                
            }
            
            
        }
        
        
        # vuln vor dem entsprechenden node -> mit map_df
        final_nodes_vuln_pfade <-   bind_rows(nodes_pfade_reihenfolge, reihenfolge_vuln) %>% group_split(path) %>%
            map(~map_df(.x,rev)) %>% map(~arrange(.x,reihenfolge_in_pfad)) %>% bind_rows()
        
        
        
        
        
        ### Ermittlung der kumulierten Produkte je Pfad
        
        ## wenn loss = NA -> 0 und wenn prob = NA -> 1 , da so keinen Einfluss
        
        nodes_cumprod_pro_path <- final_nodes_vuln_pfade %>% mutate_at(vars("probability","loss"), as.numeric) %>%
            mutate(loss = ifelse(is.na(loss),0,loss),
                   probability = ifelse(is.na(probability),1, probability)) %>%
            group_by(path) %>%
            mutate(cumprod_prob_je_path = cumprod(probability)) %>% dplyr::ungroup()
        
        rv$nodes_cumprod_pro_path <- nodes_cumprod_pro_path
        
        #### wahrscheinlichkeit von start knoten zu end knoten
        
        prob_pro_pfad <- nodes_cumprod_pro_path %>% group_by(path) %>%
            summarise(prob_prod_pfad = prod(probability))
        
        
        rv$prob_start_end <- prob_pro_pfad %>% summarise(Wahrscheinlichkeit_Start_End_Knoten = sum(prob_prod_pfad))
        
        
        #### Erwartungswert von start zu end knoten
        
        #### mehrfache berücksichtigung
        # berücksichtigung whs letzte kante vor negativeImpact
        
        sumproduct_pro_pfad <- nodes_cumprod_pro_path %>% group_split(path) %>%
            map(~summarise(.,sumproduct = sum(cumprod_prob_je_path*loss, na.rm = T))) %>% bind_rows()
        
        #
        prob_letzter_node_pro_Pfad <- nodes_cumprod_pro_path %>% group_split(path) %>%
            map(~tail(.x,1)) %>%
            map(~pull(.x, probability)) %>% unlist() %>%
            enframe(name = "path", value = "prob_letzter_node_pro_Pfad")
        
        #
        exp_values_pro_pfad_1 <- bind_cols(sumproduct_pro_pfad, prob_letzter_node_pro_Pfad) %>% select(path,sumproduct, prob_letzter_node_pro_Pfad) %>%
            mutate(exp_value_pro_pfad = sumproduct*prob_letzter_node_pro_Pfad)
        
        rv$exp_value_1 <- sum(exp_values_pro_pfad_1$exp_value_pro_pfad, na.rm = T)
        
        
        
        #### berücksichtigung wenn teilpfad erwartungswert bereits kalkuliert
        
        
        #
        
        
        sumproducts_pro_pfad <- nodes_cumprod_pro_path %>% distinct(name, cumprod_prob_je_path, .keep_all = T) %>% group_split(path) %>%
            map(~summarise(.,sumproduct = sum(cumprod_prob_je_path*loss, na.rm = T))) %>% bind_rows()
        
        # berücksichtigung whs letzte kante vor negativeImpact
        # prob_letzter_node_pro_Pfad <- nodes_cumprod_pro_path %>% group_split(path) %>%
        #   map(~filter(.x,group == "negativeImpact")) %>%
        #   map(~pull(.x, probability)) %>% unlist() %>% enframe(name = "path", value = "prob_letzter_node_pro_Pfad")
        
        prob_letzter_node_pro_Pfad <- nodes_cumprod_pro_path %>% group_split(path) %>%
            map(~tail(.x,1)) %>%
            map(~pull(.x, probability)) %>% unlist() %>% enframe(name = "path", value = "prob_letzter_node_pro_Pfad")
        
        
        
        #
        exp_values_pro_pfad_2 <- bind_cols(sumproducts_pro_pfad, prob_letzter_node_pro_Pfad) %>% select(path,sumproduct, prob_letzter_node_pro_Pfad) %>%
            mutate(exp_value_pro_pfad = sumproduct*prob_letzter_node_pro_Pfad)
        
        rv$exp_value_2 <- sum(exp_values_pro_pfad_2$exp_value_pro_pfad, na.rm = T)
        
        
        
        #### nodes und edges für visualisierung
        
        
        ## wenn ev von teilpfad bereits berücksichtigt: exp_value_2
        
        start_end_path <- paste0('MATCH  (a)-[rel*]->(b) WHERE a.name_de = ',"'", input$start_node ,"'", 'AND b.name_de = ',"'",input$end_node,"'", " RETURN a,rel,b;") %>%
            call_neo4j(con, type = "graph")
        
        ##############################################################################################################
        # start_end_path <- paste0('MATCH  (a)-[rel*]->(b) WHERE a.name_de = ',"'", "RE1" ,"'", 'AND b.name_de = ',"'", "Finanzlage" ,"'", " RETURN a,rel,b;") %>%
        #   call_neo4j(con, type = "graph")
        
        
        nodes_exp_value_2_temp <- start_end_path$nodes %>%
            unnest_nodes() %>%
            rename(group = value, label = name_de) 
        
        
        #
        edges_exp_value_2_temp <- start_end_path$relationships %>%
            unnest_relationships() %>%
            rename(from = startNode, to = endNode, label = probability) 
        # vuln/prec hinzufügen
        
        all <- "MATCH  (a)-[rel]->(b) RETURN  a,rel,b;" %>%
            call_neo4j(con, type = "graph")
        
        
        nodes_vulnerabilities_preconditions <- all$nodes %>%
            unnest_nodes() %>%
            rename(group = value, label = name_de) %>% filter(group %in% c("vulnerability","precondition"))
        
        
        ##
        edges_vulnerabilities_preconditions_rel <- all$relationships %>%
            unnest_relationships() %>%
            rename(from = startNode, to = endNode, label = probability) %>% filter(type == "isRequired", to %in% nodes_exp_value_2_temp$id)
        
        
        ##
        nodes_vulnerabilities_preconditions_rel <- nodes_vulnerabilities_preconditions %>% filter(id %in% edges_vulnerabilities_preconditions_rel$from)
        
        
        ####
        nodes_exp_value_2 <<-  bind_rows(nodes_exp_value_2_temp, nodes_vulnerabilities_preconditions_rel) %>%
            mutate(loss = as.numeric(loss), loss = ifelse(is.na(loss),0, loss) ,title = paste("Der Schaden des Knoten", label ,"beträgt", "<B>",dollar(loss, suffix = "€", prefix = "", big.mark = ".", decimal.mark = ",", accuracy = .01)),"</B>")
        
        
        
        edges_exp_value_2 <<- bind_rows(edges_exp_value_2_temp, edges_vulnerabilities_preconditions_rel) %>%
            mutate(color = ifelse(type =="isSubRisk", "#A5ABB6",
                                  ifelse(type == "isPart","#A5ABB6",
                                         ifelse(type == "impacts", "#C00000",
                                                ifelse(type == "triggers","#C00000",
                                                       ifelse(type == "isRequired","#99833E","#f7c633"))))))
        
        
        
        # letztes oder für isMember -> "#f7c633"
        
        ################## wenn ev von teilpfad nicht berücksichtigt: exp_value_1
        
        
        nodes_exp_value_1_split <- nodes_pfade %>% group_split(path)
        
        
        for(i in 1:(length(nodes_exp_value_1_split))) {
            
            
            for(j in 1:(nrow(nodes_exp_value_1_split[[i]])-1)) {
                
                a <- j
                b <- j+1
                
                temp_vis <- edges_exp_value_2 %>% filter(from == nodes_exp_value_1_split[[i]]$id[a] & to == nodes_exp_value_1_split[[i]]$id[b]) %>%
                    mutate(path = nodes_exp_value_1_split[[i]]$path[j])
                
                if ( j == 1) {
                    edges_vis_path <- temp_vis
                    
                } else {
                    
                    edges_vis_path <- bind_rows(edges_vis_path, temp_vis)
                    
                }
                
            }
            
            if (i == 1) {
                
                edges_ev_1 <- edges_vis_path
                
            } else {
                
                edges_ev_1 <- bind_rows(edges_ev_1, edges_vis_path)
                
                
            }
            
        }
        
        
        
        ###
        nodes_exp_value_1 <<- final_nodes_vuln_pfade %>% mutate(id_neu = paste0(path,id)) %>%
            select(-id) %>%
            rename(id = id_neu, label = name) %>%
            mutate(loss = ifelse(is.na(loss), 0, as.numeric(loss)),
                   title = paste("Der Schaden des Knoten",label,"beträgt", "<B>",dollar(loss, suffix = "€",prefix = "", big.mark = ".", decimal.mark = ",", accuracy = .01)),"</B>")
        
        
        ##
        
        
        edges_exp_value_1_temp <- edges_ev_1 %>% mutate(from_neu = paste0(path,from), to_neu = paste0(path,to), id_neu = paste0(path,id)) %>%
            select(-from,-to,-id) %>% rename(from = from_neu, to = to_neu, id = id_neu)
        
        ##
        random_numbers <- sample(1:100000,100000)
        
        random_numbers_exclusive_existing_ids <- random_numbers[!random_numbers %in% edges_exp_value_1_temp$id]
        
        edges_vuln_prec_pfade_exp_value_1 <- final_nodes_vuln_pfade %>% mutate(end_vuln_prec_neu = paste0(path,end_vuln_prec),id_neu = paste0(path,id)) %>% filter(group %in% c("vulnerability","precondition")) %>%
            select(type, id_neu, end_vuln_prec_neu,probability) %>% rename(label = probability, from = id_neu, to = end_vuln_prec_neu) %>% rowwise %>% mutate(id = as.character(sample(random_numbers_exclusive_existing_ids,1)))
        
        
        edges_exp_value_1 <<- bind_rows(edges_exp_value_1_temp, edges_vuln_prec_pfade_exp_value_1) %>%
            mutate(color = ifelse(type =="isSubRisk", "#A5ABB6",
                                  ifelse(type == "isPart","#A5ABB6",
                                         ifelse(type == "impacts", "#C00000",
                                                ifelse(type == "triggers","#C00000",
                                                       ifelse(type == "isRequired","#99833E","#f7c633"))))))
        
        
        
        
        
    })
    
    
    
    
    output$values <- renderTable( {tibble(Erwartungswert_mehrfach = dollar(rv$exp_value_1, prefix = "", suffix = "€", big.mark = ".", decimal.mark = ",", accuracy = .01), 
                                          Erwartungswert_einfach = dollar(rv$exp_value_2, prefix = "", suffix = "€", big.mark = ".", decimal.mark = ",", accuracy = .01), 
                                          Wahrscheinlichkeit = rv$prob_start_end,
                                          Anzahl_Pfade = rv$anzahl_pfade)})
    
    
    
    
    
    
    #### legend layout fix
    lnodes <- tibble(label = c("Legende","Ereignis", "Risikoereignis","Voraussetzung","Verwundbarkeit","Negative Auswirkung","Risikoart"),
                     shape = c("text",rep("dot",6)), color =  c("black","#44706F", "#C00000","#665728","#BDA660","#005A83","#96B9D2"),
                     font.color =  "black", font.size = c(20, rep(10,6)))  
    
    
    
    output$exp_val_1 <- renderVisNetwork({
        
        
        g <- visNetwork(nodes_exp_value_1, edges_exp_value_1, main = "Graph - alle Pfade")  %>%
            visGroups(groupname = "negativeImpact", size = 25, color = list(
                background = "#005A83",
                border = "#005A83")) %>%
            visGroups(groupname = "riskCategory", size = 30,   color = list(
                background = "#96B9D2",
                border = "#005A83")) %>%
            visGroups(groupname = "riskEvent",size = 20,  color = list(
                background = "#C00000",
                border = "#C00000")) %>%
            visGroups(groupname = "event", size = 20, color = list(
                background = "#44706F",
                border = "#44706F")) %>%
            visGroups(groupname = "vulnerability",size = 20, color = list(
                background = "#BDA660",
                border = "#BDA660")) %>%
            visGroups(groupname = "precondition",size = 20, color = list(
                background = "#665728",
                border = "#665728")) %>%
            visGroups(groupname = "scenario",size = 15, color = list(
                background = "#ffffff",
                border = "#f7c633")) %>%
            visEdges(smooth = F, font = list("size"=10), color = "black", arrows = "to") %>% 
            visIgraphLayout("layout_with_sugiyama") %>%
            #visHierarchicalLayout(direction = "LR", sortMethod = "directed", levelSeparation = 200) %>%
            visLegend(width = 0.16, useGroups = F, addNodes = lnodes, stepY = 65) %>%
            visInteraction(navigationButtons = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:200px;word-break: break-word') %>%
            visOptions(highlightNearest = list(enabled = T,algorithm = "hierarchical", degree = list(from = nrow(nodes_exp_value_1), to = nrow(edges_exp_value_1)))) %>%
            visPhysics(enabled = FALSE) %>%
            visEvents(doubleClick = "function(nodes) {
            Shiny.onInputChange('node_change', nodes.nodes);
            ;}")
        
        # change the x,y coordinates for LR arrangement
        coord_y <- g$x$nodes$y
        g$x$nodes$y <- (g$x$nodes$x)*0.5
        g$x$nodes$x <- coord_y
        g
        
    } )
    
    
    output$exp_val_2 <- renderVisNetwork({
        
        
        g <- visNetwork(nodes_exp_value_2, edges_exp_value_2, main = "Graph")  %>%
            visGroups(groupname = "negativeImpact", size = 25, color = list(
                background = "#005A83",
                border = "#005A83")) %>%
            visGroups(groupname = "riskCategory", size = 30,   color = list(
                background = "#96B9D2",
                border = "#005A83")) %>%
            visGroups(groupname = "riskEvent",size = 20,  color = list(
                background = "#C00000",
                border = "#C00000")) %>%
            visGroups(groupname = "event", size = 20, color = list(
                background = "#44706F",
                border = "#44706F")) %>%
            visGroups(groupname = "vulnerability",size = 20, color = list(
                background = "#BDA660",
                border = "#BDA660")) %>%
            visGroups(groupname = "precondition",size = 20, color = list(
                background = "#665728",
                border = "#665728")) %>%
            visGroups(groupname = "scenario",size = 15, color = list(
                background = "#ffffff",
                border = "#f7c633")) %>%
            visEdges(smooth = F, font = list("size"=10), color = "black", arrows = "to") %>% 
            visIgraphLayout("layout_with_sugiyama") %>%
            #visHierarchicalLayout(direction = "LR", sortMethod = "directed", levelSeparation = 200) %>%
            visLegend(width = 0.16, useGroups = F, addNodes = lnodes, stepY = 65) %>%
            visInteraction(navigationButtons = TRUE, tooltipStyle = 'position: fixed;visibility:hidden;padding: 5px;
                font-family: verdana;font-size:14px;font-color:#000000;background-color: #f5f4ed;
                -moz-border-radius: 3px;-webkit-border-radius: 3px;border-radius: 3px;
                 border: 1px solid #808074;box-shadow: 3px 3px 10px rgba(0, 0, 0, 0.2);
                 max-width:200px;word-break: break-word') %>%
            visOptions(highlightNearest = list(enabled = T,algorithm = "hierarchical", degree = list(from = nrow(nodes_exp_value_2), to = nrow(edges_exp_value_2)))) %>%
            visPhysics(enabled = FALSE) %>%
            visEvents(doubleClick = "function(nodes) {
            Shiny.onInputChange('node_change', nodes.nodes);
            ;}")
        
        # change the x,y coordinates for LR arrangement
        coord_y <- g$x$nodes$y
        g$x$nodes$y <- g$x$nodes$x
        g$x$nodes$x <- (coord_y)*1.8
        g
        
    } )
    
    ##################verteilung histogramm
    
    
    observeEvent(input$verteilung, {
        
        data <-  rv$nodes_cumprod_pro_path %>% as_tibble() #%>% select(- reihenfolge_in_pfad,-end_vuln_prec, -name_en.x, -name_en.y, -name_de, -startNode, -type, -title)
        
        
        # # parallele berechnung verteilt auf kerne
        # rv$anzahl_simulationen <- 1000
        # #
        # 
        # sim <- numeric(rv$anzahl_simulationen)
        # 
        # plan(multisession)
        # #
        # loss_verteilung <<- future_sapply(1:length(sim), function(x) {
        # 
        #   data_1 <-  data %>% rowwise() %>% mutate(loss_rnorm = rnorm(1,loss, 0.1*loss)) %>% ungroup() %>%
        #     group_by(name) %>% mutate(random_number = runif(1)) %>% ungroup() %>%
        #     group_by(path) %>% mutate(eingetreten = ifelse(!group %in% c("vulnerability", "precondition"),ifelse(random_number > cumprod_prob_je_path, 0,1), probability),
        #                               eingetreten_bis = cumprod(eingetreten),
        #                               loss_temp = eingetreten_bis*loss_rnorm)  %>% ungroup() %>%
        #     distinct(name,cumprod_prob_je_path, .keep_all = T) %>% group_by(path) %>% summarise(sum = sum(loss_temp))
        # 
        # 
        #   #print(data_1, n= 15)
        # 
        #   loss_verteilung <- sum(data_1$sum, na.rm = T)
        # 
        #   
        # 
        #    })
        # 
        # plan("default")
        
        ##############################
        withProgress(message = 'Kalkulation Simulation', value = 0, {
            
            
            rv$anzahl_simulationen <- 1000
            
            
            for (i in 1:rv$anzahl_simulationen) {
                
                
                data_1 <-  data %>% rowwise() %>% mutate(loss_rnorm = rnorm(1,loss, 0.1*loss)) %>% ungroup() %>%
                    group_by(name) %>% mutate(random_number = runif(1)) %>% ungroup() %>%
                    group_by(path) %>% mutate(eingetreten = ifelse(!group %in% c("vulnerability", "precondition"),ifelse(random_number > cumprod_prob_je_path, 0,1), probability),
                                              eingetreten_bis = cumprod(eingetreten),
                                              loss_temp = eingetreten_bis*loss_rnorm)  %>% ungroup() %>%
                    distinct(name,cumprod_prob_je_path, .keep_all = T) %>% group_by(path) %>% summarise(sum = sum(loss_temp))
                
                
                sum_loss_der_pfade_pro_ziehung <- sum(data_1$sum, na.rm = T)
                
                incProgress(1/rv$anzahl_simulationen, detail = paste("", i))
                
                
                if (i == 1) {
                    
                    loss_verteilung_2 <- sum_loss_der_pfade_pro_ziehung
                    
                    
                } else {
                    
                    # loss_verteilung[i] <- sum_loss_pro_ziehung
                    loss_verteilung_2[i] <- sum_loss_der_pfade_pro_ziehung
                }
                
                
                
            }
            
        })
        
        loss_verteilung <<- loss_verteilung_2
        
        
        
    })
    
    
    
    output$hist <- renderPlot({
        
        
        
        ggplot() + geom_histogram(aes(loss_verteilung), bins = 100) + scale_x_continuous(name = "Verlust in Mio €",
                                                                                         breaks = seq(min(loss_verteilung),max(loss_verteilung),1000000),
                                                                                         labels = scales::unit_format(unit = "", scale = 1e-6)) +
            scale_y_continuous(name = "Anzahl", breaks = scales::extended_breaks(n=20)) +
            ggtitle(paste0("Verteilung der Verluste von ",rv$anzahl_simulationen, " Ziehungen")) +
            theme(plot.title = element_text(size = 25, hjust = 0.5))
        
        
        
    })
    
    
}

shinyApp(ui = ui, server = server)
