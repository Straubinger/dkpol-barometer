## Define global parameters for DKpol Barometer app ----

# Load packages ----
library(RCurl)
library(tidyverse)
library(shiny)
library(DT)
library(shinythemes)

# Load dataset from GitHub ----
dta <- read.csv(text=getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv"))

# Format dataset ----
dta <- dta %>% 
  mutate(dato = as.Date(paste(year, month, day, sep = "-"))) %>% 
  select(-id, -year, -month, -day, -source) %>% 
  gather(party, pct, party_a:party_aa) %>% 
  mutate(ci_95 = 1.96 * sqrt(pct*(100-pct)/n)) %>% 
  mutate(party = replace(party, party == "party_a", "Socialdemokratiet")) %>%
  mutate(party = replace(party, party == "party_b", "Radikale Venstre")) %>%
  mutate(party = replace(party, party == "party_c", "Konservative")) %>%
  mutate(party = replace(party, party == "party_d", "Nye Borgerlige")) %>%
  mutate(party = replace(party, party == "party_f", "SF")) %>%
  mutate(party = replace(party, party == "party_i", "Liberal Alliance")) %>%
  mutate(party = replace(party, party == "party_k", "Kristendemokraterne")) %>%
  mutate(party = replace(party, party == "party_o", "Dansk Folkeparti")) %>%
  mutate(party = replace(party, party == "party_v", "Venstre")) %>%
  mutate(party = replace(party, party == "party_oe", "Enhedslisten")) %>%
  mutate(party = replace(party, party == "party_aa", "Alternativet"))

# Reorder parties ----
dta$party <- factor(dta$party, levels = c("Socialdemokratiet", "Radikale Venstre", "Konservative", "Nye Borgerlige",
                                      "SF", "Liberal Alliance", "Kristendemokraterne", "Dansk Folkeparti",
                                      "Venstre", "Enhedslisten", "Alternativet"))

# Create vectors for party and polling firm names ----
party.name <- unique(dta$party)
firm.name <- unique(dta$pollingfirm)

# Create vector for party colours ----
party_colours <- c("#ff0000", "#712f87", "#4aa127", "#103021", "#f50896", "#fd7322", "#e74310", "#f4b912", "#065bb2",
             "#980000", "#78c31e")
names(party_colours) <- c("Socialdemokratiet", "Radikale Venstre", "Konservative", "Nye Borgerlige", "SF", "Liberal Alliance",
                   "Kristendemokraterne", "Dansk Folkeparti", "Venstre", "Enhedslisten", "Alternativet")

# Create vector for polling firm colours, colour blind palette ----
house_colours <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
names(house_colours) <- c("Epinion", "Gallup", "Greens", "Megafon", "Norstat", "Voxmeter", "Wilke", "YouGov", "Rambøll")

# Define size of plot elements ----
size_dot <- 4         # size of dots in scatter plot
size_trend <- 1.5     # size of trendline
size_text <- 16       # size of plot text
level_alpha <- 0.5    # alpha level


# Define pollPlot theme ----
theme_polls <- function() {
  theme_bw() +
    theme(panel.border = element_blank(),
          panel.grid.minor = element_blank(),
          axis.line.x = element_line("black"),
          plot.title = element_text(size = 26, margin = margin(t = 20, b = 10)),
          plot.subtitle = element_text(size = 18, margin = margin(b = 20)),
          axis.title.y = element_text(size = size_text, margin = margin(r = 10)),
          axis.text.x = element_text(size = size_text, margin = margin(t = 10)),
          axis.text.y = element_text(size = size_text, margin = margin(r = 10)),
          legend.title = element_blank(),
          legend.text = element_text(size = size_text),
          legend.key.size = unit(0.8, "cm"),
          strip.text = element_text(size = size_text, colour = "black"),
          axis.ticks = element_blank())
}

## Define UI for DKpol Barometer app ----
ui <- fluidPage(
  
  # Shiny theme: Spacelab ----
  theme = shinytheme("spacelab"),
  
  # App title ----
  tags$div(
    tags$h1("DKpol Barometer"), 
    tags$h3("Danske politiske meningsmålinger og hus-effekter")
  ),
  
  br(), 
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      tags$div(
        HTML("Denne app giver mulighed for at følge danske partiers udvikling i meningsmålingerne 
             for perioden 2010 og frem. Desuden kan ses hus-effekter dvs. det enkelte instituts 
             over-/undervurdring af partierne ift. gennemsnittet af samtlige målinger i  den valgte periode.")
        ),
      
      br(),
      
      # Input: Select date range ----
      dateRangeInput("date",
                     label = "Periode (2010-)",
                     start = Sys.Date() - 365, end = Sys.Date(),
                     min = "2010-01-01", max = Sys.Date(),
                     separator = " til ", format = "dd/mm/yyyy",
                     language = 'da', weekstart = 1),
      
      # Input: Select parties ---
      selectInput("party", 
                  label = "Partier",
                  selected = c("Venstre", "Socialdemokratiet"), 
                  choices = party.name,
                  multiple = TRUE),

      # Input: Select polling firms
      selectInput("firm", 
                  label = "Institutter",
                  selected = c("Megafon", "Gallup", "Greens", "Rambøll", "YouGov",
                               "Voxmeter", "Epinion", "Norstat", "Wilke"),
                  choices = firm.name,
                  multiple = TRUE),
      
      tags$hr(),
      
      tags$em("Benyttes kun under fanen 'Målinger'"),
      
      br(),

      # Input: Select or deselect smoothed conditional means (geom_smooth) ----
      checkboxInput("trend", strong("Trendlinjer for partier"), value = TRUE),
      
      # Input: Select confidence interval, conditional on trendline ----
      conditionalPanel(condition = "input.trend == true",
                       selectInput("ci",
                                   label = "Konfidensinterval",
                                   selected = "0.95",
                                   choice = c("Intet", "0.90", "0.95", "0.99"))
                       ),

      #  Input: Select or deselect faceting ----
      checkboxInput("facet", strong("Opdel graf på partier"), value = FALSE),
      
      #  Input: Select or deselect free y-axis on facets, conditional on faceting ----
      conditionalPanel(condition = "input.facet == true",
                       checkboxInput("axis", strong("Frigør y-akse ved opdeling"), value = FALSE)
      ),
      
      tags$hr(),
      
      # Links to source code and dataset on GitHub ----
      
      tags$em("Kode og data"),
      
      tags$ul(
        tags$li(HTML(paste(tags$a(href="https://github.com/Straubinger/dkpol-barometer", "Kildekode"),"af", 
                     tags$a(href="https://twitter.com/straubingerdk", "@straubingerdk"), sep = " "))),
        tags$li(HTML(paste(tags$a(href="https://github.com/erikgahner/polls", "Datamateriale"),"indsamlet af", 
                           tags$a(href="https://twitter.com/erikgahner", "@erikgahner"), sep = " ")))
      ),
      
      
    width = 3),   # select width out of 12 units
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Tabset w/ plots and tables ----
      tabsetPanel(type = "tabs",
                  tabPanel("Målinger", plotOutput("pollPlot", height = "720px")),
                  tabPanel("Hus-effekter", plotOutput("housePlot", height = "720px")),
                  tabPanel("Tabel", DTOutput("table"))
      ),
    width = 9)   # select width out of 12 units
  )
)

## Define server logic for DKpol Barometer app ----
server <- function(input, output) {
  
  # Subset data on party, polling firm and date input ----
  dta_poll <- reactive({
    dta %>% filter(party %in% input$party & pollingfirm %in% input$firm & 
               dato >= input$date[1] & dato <= input$date[2])
  })
  
  # Subset and calculate data to plot house effects ----
  dta_house <- reactive({
    dta %>% filter(dato >= input$date[1] & dato <= input$date[2]) %>%
      group_by(pollingfirm, party) %>% 
      summarise(mean_firm = mean(pct, na.rm = TRUE)) %>% 
      group_by(party) %>% 
      mutate(mean_party = mean(mean_firm, na.rm = TRUE)) %>% 
      mutate(diff = round(mean_firm-mean_party, digits = 2)) %>% 
      filter(party %in% input$party & pollingfirm %in% input$firm)
  })

  # Subset data to create table output ----
  dta_tbl <- reactive({
    dta %>% filter(party %in% input$party & pollingfirm %in% input$firm & 
                     dato >= input$date[1] & dato <= input$date[2]) %>% 
    select(dato, pollingfirm, party, pct) %>% 
    rename(Dato = dato, Institut = pollingfirm, Parti = party, Procent = pct) %>% 
    spread(Parti, Procent)
  })
  
  # Generate pollPlot ----
  output$pollPlot <- renderPlot({
    if(input$facet & input$trend & input$axis) {
      ggplot(dta_poll(), aes(dato, pct)) +
        geom_point(size = size_dot, aes(colour = pollingfirm)) + 
        geom_smooth(size=size_trend, level = as.numeric(input$ci)) + 
        facet_wrap(~party, scales = "free_y") +
        scale_colour_manual(values = house_colours) +
        labs(x = "",
             y = "Stemmer (%)", 
             title = "Opbakning til udvalgte partier",
             subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"), 
                              "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
        theme_polls()
    }
    
    else if(input$facet & input$trend) {
      ggplot(dta_poll(), aes(dato, pct)) +
        geom_point(size = size_dot, aes(colour = pollingfirm)) + 
        geom_smooth(size=size_trend, level = as.numeric(input$ci)) + 
        facet_wrap(~party) +
        scale_colour_manual(values = house_colours) +
        labs(x = "",
             y = "Stemmer (%)", 
             title = "Opbakning til udvalgte partier",
             subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"), 
                              "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
        theme_polls()
    }
    
    else if(input$facet & input$axis) {
      ggplot(dta_poll(), aes(dato, pct)) +
        geom_point(size = size_dot, aes(colour = pollingfirm)) +
        facet_wrap(~party, scales = "free_y") +
        scale_colour_manual(values = house_colours) +
        labs(x = "",
             y = "Stemmer (%)",
             title = "Opbakning til udvalgte partier",
             subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"),
                              "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
        theme_polls()
    }
    
    else if(input$facet) {
      ggplot(dta_poll(), aes(dato, pct)) +
        geom_point(size = size_dot, aes(colour = pollingfirm)) + 
        facet_wrap(~party) +
        scale_colour_manual(values = house_colours) +
        labs(x = "",
             y = "Stemmer (%)", 
             title = "Opbakning til udvalgte partier",
             subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"), 
                              "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
        theme_polls()
    }
    
    else if(input$trend) {
      ggplot(dta_poll(), aes(dato, pct, colour = party, fill = party)) +
        geom_point(size = size_dot, alpha = level_alpha, stroke = NA) + 
        geom_smooth(se = T, size=size_trend, level = as.numeric(input$ci)) +
        scale_colour_manual(values = party_colours) +
        scale_fill_manual(values = party_colours) +
        labs(x = "",
             y = "Stemmer (%)", 
             title = "Opbakning til udvalgte partier",
             subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"), 
                              "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
        theme_polls()
    }
    
    else {
      ggplot(dta_poll(), aes(dato, pct, colour = party, fill = party)) +
        geom_point(size = size_dot, alpha = level_alpha, stroke = NA) +
        scale_colour_manual(values = party_colours) +
        labs(x = "",
             y = "Stemmer (%)", 
             title = "Opbakning til udvalgte partier",
             subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"), 
                              "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
        theme_polls()
    }
  })
  
  # Generate housePlot ----
  output$housePlot <- renderPlot({
    ggplot(dta_house(), aes(pollingfirm, diff, fill = party)) +
      geom_col() +
      facet_wrap(~party) +
      coord_flip() +
      scale_fill_manual(values = party_colours) +
      theme_bw() +
      theme(legend.position = "none",
            panel.grid.major.y = element_blank(),
            panel.grid.minor.y = element_blank(),
            panel.border = element_blank(),
            axis.line.x = element_line("black"),
            axis.ticks.y = element_blank(),
            plot.title = element_text(size = 26, margin = margin(t = 20, b = 10)),
            plot.subtitle = element_text(size = 18, margin = margin(b = 20)),
            axis.title.x = element_text(size = size_text, margin = margin(t = 15)),
            axis.text.x = element_text(size = size_text),
            axis.text.y = element_text(size = size_text),
            strip.text = element_text(size = size_text, colour = "black")) +
      labs(x = "",
           y = "Afvigelse ift. gennemsnit for perioden (%-point)",
           title = "Hus-effekter for udvalgte institutter og partier",
           subtitle = paste("Periode:", format(as.Date(input$date[1]), "%d/%m-%Y"), 
                            "til", format(as.Date(input$date[2]), "%d/%m-%Y"), sep = " ")) +
    geom_text(aes(x = pollingfirm, y = ifelse(diff>0, diff+0.25, diff-0.25), label = diff, size = 12))

  })
  
  # Generate table ----
  output$table <- renderDT(dta_tbl(), options = list(order = list(1, 'desc'), searching = FALSE, pageLength = 15))
  
}

# Create Shiny app ----
shinyApp(ui, server)
