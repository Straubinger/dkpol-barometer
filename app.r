#* Define global parameters ----

# Install packages if not already installed ----
if(!require(shiny))
{install.packages("shiny")}
if(!require(shinydashboard))
{install.packages("shinydashboard")}
if(!require(shinyWidgets))
{install.packages("shinyWidgets")}
if(!require(tidyverse))
{install.packages("tidyverse")}
if(!require(RCurl))
{install.packages("RCurl")}
if(!require(DT))
{install.packages("DT")}
if(!require(Cairo))
{install.packages("Cairo")}

# Load packages ----
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(tidyverse)
library(RCurl)
library(DT)
library(Cairo)

# Load and restructure dataset from GitHub ----
dta <- read.csv(text=getURL("https://raw.githubusercontent.com/erikgahner/polls/master/polls.csv")) %>% 
  mutate(dato = as.Date(paste(year, month, day, sep = "-"))) %>% 
  select(-id, -year, -month, -day, -source)

# Prepare party dataset ----
dta.party <- dta %>% 
  gather(party, pct, party_a:party_aa) %>% 
  mutate(party = case_when(
    party == "party_a" ~ "Socialdemokratiet",
    party == "party_b" ~ "Radikale Venstre",
    party == "party_c" ~ "Konservative",
    party == "party_d" ~ "Nye Borgerlige",
    party == "party_e" ~ "Klaus Riskær Pedersen",
    party == "party_f" ~ "SF",
    party == "party_i" ~ "Liberal Alliance",
    party == "party_k" ~ "Kristendemokraterne",
    party == "party_o" ~ "Dansk Folkeparti",
    party == "party_v" ~ "Venstre",
    party == "party_oe" ~ "Enhedslisten",
    party == "party_aa" ~ "Alternativet"))

# Prepare coalition dataset ----
dta.coalition <- dta %>%
  mutate(rød = rowSums(select(.,party_a, party_b, party_f, party_oe, party_aa), na.rm = TRUE)) %>%
  mutate(blå = rowSums(select(.,party_c, party_d, party_i, party_o, party_v), na.rm = TRUE)) %>% 
  select(dato, rød, blå, pollingfirm) %>%
  gather(coalition, pct, rød, blå) %>% 
  mutate(coalition = case_when(
    coalition == "rød" ~ "Rød blok",
    coalition == "blå" ~ "Blå blok"))

# Reorder parties ----
dta.party$party <- factor(dta.party$party, levels = c("Socialdemokratiet", "Radikale Venstre", "Konservative", "Nye Borgerlige",
                                                      "Klaus Riskær Pedersen", "SF", "Liberal Alliance", "Kristendemokraterne",
                                                      "Dansk Folkeparti", "Venstre", "Enhedslisten", "Alternativet"))

# Create vectors for party and polling firm names ----
party.name <- unique(dta.party$party)
firm.name <- unique(dta.party$pollingfirm)

# Create vector for party colours ----
party.colours <- c("#ff0000", "#EB4295", "#0f854b", "#103021", "#914A4F", "#fd7322", "#e74310", "#f4b912", "#065bb2",
                   "#9C1D2A", "#78c31e", "#537D7A")
names(party.colours) <- c("Socialdemokratiet", "Radikale Venstre", "Konservative", "Nye Borgerlige", "SF", "Liberal Alliance",
                          "Kristendemokraterne", "Dansk Folkeparti", "Venstre", "Enhedslisten", "Alternativet",
                          "Klaus Riskær Pedersen")

#* Define UI ----
ui <- dashboardPage(
  
  skin = "black",
  
  # Header with app title and links  ----
  dashboardHeader(title = HTML(paste(icon("poll"), "DKpol Barometer")),
                  
                  # Link to source code on GitHub ----
                  tags$li(class = "dropdown",
                          tags$a(href="https://github.com/Straubinger/dkpol-barometer", icon("github"), "|", "Kildekode")),
                  
                  # Link to data on GitHub ----
                  tags$li(class = "dropdown",
                          tags$a(href="https://github.com/erikgahner/polls", icon("github"), "|", "Datamateriale"))
                  ),
  
  # Sidebar with menu items and input definitions ----
  dashboardSidebar(
    sidebarMenu(id = "menu",
                menuItem("Målinger", tabName = "polls", icon = icon("line-chart"),
                         menuSubItem("Partier", tabName = "party", icon = icon("angle-right")),
                         menuSubItem("Blokke", tabName = "block", icon = icon("angle-right")),
                         startExpanded = TRUE
                         ),
                menuItem("Hus-effekter", tabName = "house", icon = icon("building")),
                menuItem("Tabel", tabName = "table", icon = icon("table"))
    ),
    hr(),
    dateRangeInput("date",
                   label = "Periode",
                   start = Sys.Date() - 365, end = Sys.Date(),
                   min = "2010-01-01", max = Sys.Date(),
                   separator = " til ", format = "dd/mm-yyyy",
                   language = 'da', weekstart = 1),
    div(style = "padding-left:1em; padding-right:1em", textOutput("text_polls")),
    br(),
    conditionalPanel(condition = 'input.menu == "party" || input.menu == "table"',
                     selectInput("party",
                                 label = "Partier",
                                 selected = c("Socialdemokratiet", "Radikale Venstre", "Konservative", 
                                              "Nye Borgerlige", "SF", "Liberal Alliance",
                                              "Dansk Folkeparti", "Venstre", "Enhedslisten", 
                                              "Alternativet"), 
                                 choices = party.name,
                                 multiple = TRUE)
                     ),
    conditionalPanel(condition = 'input.menu == "party" || input.menu == "block" || input.menu == "table"',
                     selectInput("firm",
                                 label = "Institutter",
                                 selected = c("Megafon", "Gallup", "Greens", "Rambøll", "YouGov",
                                              "Voxmeter", "Epinion", "Norstat", "Wilke"),
                                 choices = firm.name,
                                 multiple = TRUE)
                     ),
    conditionalPanel(condition = 'input.menu == "block"',
                     HTML(paste("<br> <li> Rød blok: Socialdemokratiet, Radikale Venstre, SF, Enhedslisten, Alternativet </li>
                                <br> <li> Blå blok: Venstre, Konservative, Dansk Folkeparti, Liberal Alliance, Nye Borgerlige
                                </li>")),
                     style = "padding-left:1em"),
    helpText("Udviklet af ", 
             a("@StraubingerDK", href = "https://twitter.com/straubingerdk"),
             style = "padding-left:1em; padding-right:1em; bottom:1em; position:absolute"),
    width = 300
  ),
  
  # Body with output definitions ----
  dashboardBody(
    tags$head(
      tags$style(HTML('body {font-family: Verdana;}')),
      tags$style(HTML('.rightAlign{margin-left: 1360px;}'))
    ),
  
    tabItems(
      
      tabItem(tabName = "party",
              fluidRow(
                column(width = 12,
                       box(
                         dropdownButton(
                           
                           awesomeRadio(inputId = "id01",
                                        label = "Graftype",
                                        choices = c("Samlet graf", "Graf opdelt på partier", "Markering af institutter"),
                                        selected = "Samlet graf"),
                           switchInput(inputId = "id03",
                                       label = "95%-konfidensinterval",
                                       value = FALSE,
                                       onLabel = "Ja",
                                       offLabel = "Nej",
                                       size = "mini",
                                       handleWidth = 60),
                           
                           circle = TRUE, icon = icon("bars"), size = "sm",
                           tooltip = tooltipOptions(title = "Valg af graftype")),
                         plotOutput("pollPlot", height = "750px"),
                         width = NULL, solidHeader = TRUE)
                       )
                )
              ),
      tabItem(tabName = "block",
              fluidRow(
                column(width = 12,
                       box(
                         dropdownButton(
                           
                           awesomeRadio(inputId = "id02",
                                        label = "Graftype",
                                        choices = c("Samlet graf", "Graf opdelt på blokke", "Markering af institutter"),
                                        selected = "Samlet graf"),
                           switchInput(inputId = "id04",
                                       label = "95%-konfidensinterval",
                                       value = FALSE,
                                       onLabel = "Ja",
                                       offLabel = "Nej",
                                       size = "mini",
                                       handleWidth = 60),
                           
                         circle = TRUE, icon = icon("bars"), size = "sm",
                         tooltip = tooltipOptions(title = "Valg af graftype")),
                         plotOutput("coalPlot", height = "750px"),
                         width = NULL, solidHeader = TRUE)
                       )
                )
              ),
      tabItem(tabName = "house",
              fluidRow(
                column(width = 12,
                       box(
                         plotOutput("housePlot", height = "780px"),
                         width = NULL, solidHeader = TRUE)
                       )
                )
              ),
      tabItem(tabName = "table",
              fluidRow(
                column(width = 12,
                       box(DTOutput("table"), width = NULL, solidHeader = TRUE)
                       )
                )
              )
      )
    )
  )

#* Define server logic ----
server <- function(input, output) {
  
  options(shiny.usecairo=TRUE)

  # Number of polls in chosen date interval ----
  n_polls <- reactive({
    dta %>% filter(dato >= input$date[1] & dato <= input$date[2]) %>%
      nrow()
  })
  
  # Subset party data on party, polling firm and date input ----
  dta.poll <- reactive({
    dta.party %>% filter(party %in% input$party & pollingfirm %in% input$firm & 
                     dato >= input$date[1] & dato <= input$date[2])
  })
  
  # Subset coalition data on polling firm and date input ----
  dta.coal <- reactive({
    dta.coalition %>% filter(pollingfirm %in% input$firm & 
                               dato >= input$date[1] & dato <= input$date[2])
  })
  
  # Subset and calculate data to plot house effects ----
  dta.house <- reactive({
    dta.party %>% filter(dato >= input$date[1] & dato <= input$date[2]) %>%
      group_by(party) %>% 
      mutate(mean.party = mean(pct, na.rm = TRUE)) %>%
      group_by(party, pollingfirm) %>% 
      mutate(mean.firm = mean(pct, na.rm = TRUE)) %>% 
      add_count(party, pollingfirm, name = "n_polls") %>% 
      select(-n, -dato, -pct) %>% 
      distinct() %>% 
      mutate(diff = mean.firm-mean.party) %>% 
      mutate(error = qnorm(0.975)*2/sqrt(n_polls)) %>% 
      mutate(left = diff-error) %>% 
      mutate(right = diff+error) %>% 
      mutate(sign = case_when(left >= 0 & right >= 0 ~ "positive",
                              left < 0 & right < 0 ~ "negative",
                              (left >= 0 & right < 0) | (left < 0 & right >= 0) ~ "none"))
  })

  # Subset data to create party table output ----
  dta.tbl <- reactive({
    dta.party %>% filter(party %in% input$party & pollingfirm %in% input$firm & 
                           dato >= input$date[1] & dato <= input$date[2]) %>% 
      select(dato, pollingfirm, party, pct, n) %>% 
      rename(Dato = dato, Institut = pollingfirm, Parti = party, Procent = pct, Respondenter = n) %>% 
      spread(Parti, Procent)
  })

  # Text output with number of polls ----
  output$text_polls <- renderText({
    paste("Antal målinger i perioden:", n_polls())
  })
  
  # Generate pollPlot ----
  p1 <- reactive({ 
    ggplot(dta.poll(), aes(dato, pct)) +
      labs(title = "Danske partiers udvikling i meningsmålingerne",
           subtitle = paste("Periode:", format(input$date[1], "%d/%m-%Y"), "til", format(input$date[2], "%d/%m-%Y")),
           x = "",
           y = "Stemmer (%)",
           caption = "DKpol Barometer / @StraubingerDK
                     Data: github.com/erikgahner/polls") +
      theme_minimal() +
      theme(plot.title = element_text(size = 25),
            plot.subtitle = element_text(size = 18, margin = margin(t = 8, r = 0, b = 16, l = 0)),
            plot.caption = element_text(size = 12, colour = "gray20"),
            axis.text = element_text(size = 14),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            legend.key.size = unit(2, "line"),
            legend.justification = "bottom",
            strip.text = element_text(size = 14),
            plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
            )
  })
  
  output$pollPlot <- renderPlot({
    
    if(input$id01 == "Samlet graf" & input$id03 == "FALSE") {
      plot(p1() +
             geom_point(aes(colour = party), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = party), se = FALSE, method = "loess") +
             scale_colour_manual(values = party.colours)
           )
    }

    else if(input$id01 == "Graf opdelt på partier" & input$id03 == "FALSE") {
      plot(p1() +
             geom_point(aes(colour = party), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = party), se = FALSE, method = "loess") +
             scale_colour_manual(values = party.colours) +
             facet_wrap(~party) +
             theme(legend.position = "none") +
             theme(axis.text = element_text(size = 12))
           )
    }

    else if(input$id01 == "Markering af institutter" & input$id03 == "FALSE") {
      plot(p1() +
             geom_point(aes(colour = pollingfirm, shape = pollingfirm), size = 2) +
             geom_smooth(se = FALSE, method = "loess", color = "grey55") +
             scale_colour_brewer(palette="Paired") +
             scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
             facet_wrap(~party, scales = "free_y")+
             theme(axis.text = element_text(size = 12))
           )
    }
    
    else if(input$id01 == "Samlet graf" & input$id03 == "TRUE") {
      plot(p1() +
             geom_point(aes(colour = party), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = party, fill = party), method = "loess") +
             scale_colour_manual(values = party.colours) +
             scale_fill_manual(values = party.colours)
           )
    }
    
    else if(input$id01 == "Graf opdelt på partier" & input$id03 == "TRUE") {
      plot(p1() +
             geom_point(aes(colour = party), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = party, fill = party), method = "loess") +
             scale_colour_manual(values = party.colours) +
             scale_fill_manual(values = party.colours) +
             facet_wrap(~party) +
             theme(legend.position = "none") +
             theme(axis.text = element_text(size = 12))
           )
    }
    
    else if(input$id01 == "Markering af institutter" & input$id03 == "TRUE") {
      plot(p1() +
             geom_point(aes(colour = pollingfirm, shape = pollingfirm), size = 2) +
             geom_smooth(method = "loess", color = "grey55") +
             scale_colour_brewer(palette="Paired") +
             scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
             facet_wrap(~party, scales = "free_y")+
             theme(axis.text = element_text(size = 12))
           )
    }

  })
  
  # Generate coalPlot ----
  p2 <- reactive({ 
    ggplot(dta.coal(), aes(dato, pct)) +
      labs(title = "Rød og blå blok i meningsmålingerne",
           subtitle = paste("Periode:", format(input$date[1], "%d/%m-%Y"), "til", format(input$date[2], "%d/%m-%Y")),
           x = "",
           y = "Stemmer (%)",
           caption = "Rød blok: Socialdemokratiet, Radikale Venstre, SF, Enhedslisten, Alternativet
            Blå blok: Venstre, Konservative, Dansk Folkeparti, Liberal Alliance, Nye Borgerlige

            DKpol Barometer / @StraubingerDK
           Data: github.com/erikgahner/polls") +
      theme_minimal() +
      theme(plot.title = element_text(size = 25),
            plot.subtitle = element_text(size = 18, margin = margin(t = 8, r = 0, b = 16, l = 0)),
            plot.caption = element_text(size = 12, colour = "gray20"),
            axis.text = element_text(size = 14),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            legend.title = element_blank(),
            legend.text = element_text(size = 14),
            legend.key.size = unit(2, "line"),
            legend.justification = "bottom",
            strip.text = element_text(size = 14),
            plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
            )
  })
  
  output$coalPlot <- renderPlot({
    
    if(input$id02 == "Samlet graf" & input$id04 == "FALSE") {
      plot(p2() +
             geom_point(aes(colour = coalition), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = coalition), se = FALSE, method = "loess") +
             geom_hline(yintercept = 50, colour="#990000", linetype="dashed") +
             scale_colour_manual(values = c("blue", "red"))
           )
    }
    
    else if(input$id02 == "Graf opdelt på blokke" & input$id04 == "FALSE") {
      plot(p2() +
             geom_point(aes(colour = coalition), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = coalition), se = FALSE, method = "loess") +
             geom_hline(yintercept = 50, colour="#990000", linetype="dashed") +
             scale_colour_manual(values = c("blue", "red")) +
             facet_wrap(~coalition) +
             theme(legend.position = "none")
           )
    }
    
    else if(input$id02 == "Markering af institutter" & input$id04 == "FALSE") {
      plot(p2() +
             geom_point(aes(colour = pollingfirm, shape = pollingfirm), size = 2) +
             geom_smooth(se = FALSE, method = "loess", color = "grey55") +
             scale_colour_brewer(palette="Paired") +
             scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
             facet_wrap(~coalition, scales = "free_y")
           )
    }
    
    else if(input$id02 == "Samlet graf" & input$id04 == "TRUE") {
      plot(p2() +
             geom_point(aes(colour = coalition), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = coalition, fill = coalition), method = "loess") +
             geom_hline(yintercept = 50, colour="#990000", linetype="dashed") +
             scale_colour_manual(values = c("blue", "red")) +
             scale_fill_manual(values = c("blue", "red"))
           )
    }
    
    else if(input$id02 == "Graf opdelt på blokke" & input$id04 == "TRUE") {
      plot(p2() +
             geom_point(aes(colour = coalition), size = 2, alpha = 0.3) +
             geom_smooth(aes(colour = coalition, fill = coalition), method = "loess") +
             geom_hline(yintercept = 50, colour="#990000", linetype="dashed") +
             scale_colour_manual(values = c("blue", "red")) +
             scale_fill_manual(values = c("blue", "red")) +
             facet_wrap(~coalition) +
             theme(legend.position = "none")
           )
    }
    
    else if(input$id02 == "Markering af institutter" & input$id04 == "TRUE") {
      plot(p2() +
             geom_point(aes(colour = pollingfirm, shape = pollingfirm), size = 2) +
             geom_smooth(method = "loess", color = "grey55") +
             scale_colour_brewer(palette="Paired") +
             scale_shape_manual(values = c(16,0,18,2,3,4,17,1)) +
             facet_wrap(~coalition, scales = "free_y")
           )
    }
  })
  
  # Generate housePlot ----
  output$housePlot <- renderPlot({
    ggplot(dta.house(), aes(pollingfirm, diff)) +
      geom_point(aes(colour = sign), size = 2) +
      geom_linerange(aes(ymin = left, ymax = right, colour = sign)) +
      geom_hline(yintercept = 0, colour="#990000", linetype="dashed") +
      facet_wrap(~party) +
      labs(title = "Hus-effekter for danske meningsmålingsinstitutter",
           subtitle = paste("Periode:", format(input$date[1], "%d/%m-%Y"), "til", format(input$date[2], "%d/%m-%Y")),
           x = "",
           y = "Difference (%-point)",
           caption = "Hus-effekten beregnes som diff. ml. det uvægtede gns. af det enkelte instituts målinger og det samlede uvægtede gns. for hvert parti i den angivne periode
            Der er angivet 95%-konfidensinterval, grøn og rød angiver hhv. positiv og negativ difference, grå angiver insignifikant difference

            DKpol Barometer / @StraubingerDK
           Data: github.com/erikgahner/polls") +
      scale_color_manual(values = c("red", "grey55", "green")) +
      theme_minimal() +
      theme(plot.title = element_text(size = 25),
            plot.subtitle = element_text(size = 18, margin = margin(t = 8, r = 0, b = 16, l = 0)),
            plot.caption = element_text(size = 12, colour = "gray20"),
            axis.text = element_text(size = 14),
            axis.text.x = element_text(angle = 60, hjust = 1),
            axis.title.y = element_text(size = 14, margin = margin(t = 0, r = 20, b = 0, l = 0)),
            legend.position = "none",
            strip.text = element_text(size = 14),
            plot.margin = margin(0.5,0.5,0.5,0.5, "cm")
            )
    
    })
  
  # Generate table ----
  output$table <- renderDT(dta.tbl(), 
                           rownames = FALSE,
                           extensions = c('Scroller', 'Buttons'),
                           options = list(order = list(0, 'desc'), 
                                          searching = FALSE,
                                          deferRemder = TRUE,
                                          scrollY = 680,
                                          scroller = TRUE,
                                          dom = 'frtBip',
                                          buttons =
                                            list(list(
                                              extend = 'collection',
                                              buttons = c('copy', 'excel', 'csv', 'pdf'),
                                              text = 'Download tabel'
                                            ))
                                          )
                           )
}

# Create Shiny app ----
shinyApp(ui, server)