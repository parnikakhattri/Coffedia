library(shiny)
library(plotly)
library(ggplot2)
library(DT)
library(readxl)
library(bslib)
library(shinycssloaders)
library(tidyverse)
library(shinyWidgets)
library(wordcloud2)

coffee_chains <- read_excel("data/coffee_chains.xlsx")

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  tags$head(
    tags$style(HTML(
      "
        /* Global styles */
        body {
          background: linear-gradient(120deg, #f8e7d4, #d3c4af);
          font-family: 'Arial', sans-serif;
        }
        .navbar {
          background-color: #c2b280 !important;
        }
        .navbar .navbar-brand, .navbar-nav .nav-link {
          color: white !important;
          font-weight: bold;
        }
        .navbar .navbar-nav .nav-link:hover {
          color: #f8e7d4 !important;
        }
        .panel {
          background-color: white;
          border-radius: 10px;
          padding: 20px;
          box-shadow: 0px 4px 10px rgba(0, 0, 0, 0.2);
          margin-bottom: 20px;
        }
        .app-header {
          background-color: #6f4e37;
          color: white;
          text-align: center;
          padding: 10px 0;
          font-size: 24px;
          font-weight: bold;
        }
        h1, h2, h3, p {
          color: #4d4d4d;
        }
        h1 {
          font-weight: bold;
          color: #6f4e37;
        }
        .section-title {
          text-align: left;
          color: #6f4e37;
          font-weight: bold;
        }
        .section-description {
          font-size: 16px;
          color: #4d4d4d;
          text-align: left;
          margin-bottom: 20px;
        }
        "
    ))
  ),
  div(class = "app-header", "Coffedia ☕ by Parnika"),

  # About Tab
  navbarPage(
    title = NULL,
    tabPanel(
      "About",
      class = "about-tab",
      fluidRow(
        column(
          10, offset = 1,
          div(
            class = "panel",
            h3("Welcome to Coffedia: Your Encyclopedia of Coffee", class = "section-title"),
            p("Discover a world where coffee is more than just a drink—it’s a story, a tradition and a culture that connects people across the globe. Coffedia is your trusted guide to exploring everything coffee, from urban cafés to hidden gems in small towns. With insights into coffee trends, brands and communities, we’re here to enhance your coffee experience, one sip at a time.",
              class = "section-description"
            ),
            hr(),
            h3("Our Mission", class = "section-title"),
            p("At Coffedia, our mission is to empower coffee enthusiasts with knowledge and tools to explore and appreciate the diversity of coffee traditions worldwide. We aim to connect a global community of coffee lovers, support local coffee culture and inspire new adventures in the pursuit of the perfect brew.",
              class = "section-description"
            ),
            hr(),
            h3("Why Choose Coffedia?", class = "section-title"),
            tags$ul(
              style = "font-size: 18px; color: #4d4d4d; line-height: 1.8;",
              tags$li("🌎 Discover coffee store locations worldwide with ease."),
              tags$li("📊 Visualise coffee trends and brand popularity across regions."),
              tags$li("🔎 Use personalised filters and maps to find your next favorite coffee spot."),
              tags$li("🌟 Share and explore experiences from the global coffee community."),
              tags$li("📅 Stay ahead of coffee trends and uncover hidden gems near you.")
            ),
            hr(),
            h3("Data You Can Trust", class = "section-title"),
            p("Coffedia relies on data from trusted open-source repositories to ensure accurate insights into global coffee trends and quality. Whether you're a coffee aficionado or a curious explorer, you can count on Coffedia for reliable information that helps you make the best coffee discoveries.",
              class = "section-description"
            ),
            hr(),
            h3("What’s Brewing at Coffedia?", class = "section-title"),
            p("We’re working on exciting new features, including personalised coffee recommendations, advanced visualisations and enhanced community-driven content. Have ideas or feedback? We’d love to hear from you!",
              class = "section-description")
          )
        )
      )
    ),


    # Popularity Tab
    tabPanel(
      "Popularity",
      sidebarLayout(
        sidebarPanel(
          class = "panel",
          h3("Filters", class = "section-title"),
          p("Refine your search to explore coffee brand popularity within the country.",
            class = "section-description"
          ),

          selectInput("Country", "Choose Country",
                      choices = unique(coffee_chains$country),
                      selected = "Australia",
                      width = "100%"),

          selectInput("Brands", "Select Brands",
                      choices = unique(coffee_chains$Brand),
                      selected = c("Starbucks", "Timhorton", "Dunkin Donuts"),
                      multiple = TRUE,
                      width = "100%"),

          actionButton(inputId = "resetFilters",label = "Reset Filters",icon = icon("refresh"))),

        mainPanel(
          class = "panel",
          h3("Brand Popularity by Country", class = "section-title"),
          p("Visualise the popularity of selected coffee brands in your chosen country.",
            class = "section-description"),
          withSpinner(plotlyOutput("brand_plot", height = "400px")),
          hr(),
          p("Tip: Use the filters on the left to update the chart dynamically.",
            class = "section-description")
        )
      )
    ),


    # Outlets Tab
    tabPanel(
      "Outlets",
      sidebarLayout(
        sidebarPanel(
          class = "panel",
          h3("Filters", class = "section-title"),
          p("Refine your search to explore coffee outlets in selected cities.",
            class = "section-description"),

          selectInput(inputId = "City",
                      label = "Choose City:",
                      choices = unique(coffee_chains$City),
                      selected = "Broadbeach",
                      selectize = TRUE),

          selectInput(
            inputId = "BrandTable",
            label = "Select Brands:",
            choices = unique(coffee_chains$Brand),
            selected = c("Starbucks", "Timhorton", "Dunkin Donuts"),
            multiple = TRUE,
            selectize = TRUE
          )
        ),


        mainPanel(
          class = "panel",
          h3("Coffee Outlets Table", class = "section-title"),
          p("View coffee outlets based on your selected city.",
            class = "section-description"),
          withSpinner(DTOutput("data_table"), color = "#007BFF", type = 6)
        )
      )
    ),


    #Feedback Tab
    tabPanel(
      "Feedback",
      sidebarLayout(
        sidebarPanel(
          class = "panel",
          h3("Submit Your Feedback", class = "section-title"),
          p("We value your feedback! Share your thoughts and rate your coffee experience.",
            class = "section-description"),
          textInput("UserName", "Your Name", placeholder = "Enter your name"),
          textInput("UserEmail", "Your Email", placeholder = "Enter your email"),
          selectizeInput(
            "CityFeedback",
            "Select City",
            choices = c("Broadbeach", unique(coffee_chains$City)),
            selected = NULL
          ),
          selectizeInput(
            "BrandFeedback",
            "Select the Brand",
            choices = c("Starbucks", unique(coffee_chains$Brand)),
            selected = NULL
          ),
          sliderInput("SliderFeedback", "Rating", min = 1, max = 5, value = 0),
          textAreaInput("ReviewFeedback", "Write Your Review", placeholder = "Share your thoughts...", height = "100px"),
          actionButton(
            inputId = "SubmitFeedback",
            label = "Submit Feedback",
            icon = icon("paper-plane")
          )
        ),
        mainPanel(
          class = "panel",
          h3("Word Cloud of Reviews", class = "section-title"),
          p("See what others are saying! The word cloud below highlights the most frequently mentioned terms in user reviews.",
            class = "section-description"),
          wordcloud2Output("review_wordcloud"),
          uiOutput("feedback_review")
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # Popularity Tab
  output$brand_plot <- renderPlotly({
    filtered_data <- coffee_chains %>%
      filter(country %in% input$Country, Brand %in% input$Brands) %>%
      group_by(Brand) %>%
      tally(name = "Count")
    validate(need(nrow(filtered_data) > 0, "No data available for the selected filters."))
    selected_countries <- paste(input$Country, collapse = ", ")
    plot_title <- if (length(input$Country) > 1) {
      "Brand Popularity in Selected Countries"
    } else {
      paste("Brand Popularity in", selected_countries)
    }
    p <- ggplot(filtered_data, aes(x = Brand, y = Count, size = Count, color = Brand)) +
      geom_point(alpha = 0.7) +
      labs(title = plot_title, x = "Brands", y = "Brand Popularity Count") +
      theme_minimal() +
      theme(legend.position = "none")

    ggplotly(p)
  })
  observeEvent(input$resetFilters, {
    updateSelectInput(
      session,
      "Country",
      selected = "Australia"
    )

    updateSelectInput(
      session,
      "Brands",
      selected = c("Starbucks", "Timhorton", "Dunkin Donuts")
    )
  })


  # Outlets Tab
  output$data_table <- renderDT({
    filtered_table <- coffee_chains %>%
      filter(City %in% input$City, Brand %in% input$BrandTable) %>%
      select(`Brand`, `Store Name`, `Street Address`,`country`, `Phone Number`)
    validate(need(nrow(filtered_table) > 0, "No data available for the selected filters."))
    datatable(filtered_table, options = list(pageLength = 10))
  })


  # Feedback Tab
  feedback_data <- reactiveVal(data.frame(
    Name = character(0),
    Email = character(0),
    City = character(0),
    Brand = character(0),
    Rating = numeric(0),
    Review = character(0)
  ))

  observe({
    updateSelectizeInput(
      session,
      "CityFeedback",
      choices = unique(coffee_chains$City),
      server = TRUE
    )
  })

  observeEvent(input$CityFeedback, {
    updateSelectizeInput(
      session,
      "BrandFeedback",
      choices = unique(coffee_chains$Brand[coffee_chains$City == input$CityFeedback]),
      server = TRUE
    )
  })

  observeEvent(input$SubmitFeedback, {
    validate(
      need(input$UserName != "", "Please enter your name."),
      need(input$UserEmail != "", "Please enter your email."),
      need(input$CityFeedback != "", "Please select a city."),
      need(input$BrandFeedback != "", "Please select a brand."),
      need(input$SliderFeedback > 0, "Please rate the store."),
      need(input$ReviewFeedback != "", "Please write your review.")
    )

    new_feedback <- data.frame(
      Name = input$UserName,
      Email = input$UserEmail,
      City = input$CityFeedback,
      Brand = input$BrandFeedback,
      Rating = input$SliderFeedback,
      Review = input$ReviewFeedback
    )

    updated_feedback <- rbind(feedback_data(), new_feedback)
    feedback_data(updated_feedback)
    write.csv(updated_feedback, "feedback_data.csv", row.names = FALSE)
    showNotification("Thank you for your feedback!", type = "message")
  })

  output$review_wordcloud <- renderWordcloud2({
    feedback <- feedback_data()
    validate(need(nrow(feedback) > 0, "No feedback submitted yet!"))

    review_words <- unlist(strsplit(tolower(paste(feedback$Review, collapse = " ")), "\\s+"))
    word_freq <- as.data.frame(table(review_words))
    colnames(word_freq) <- c("word", "freq")
    word_freq <- word_freq[order(-word_freq$freq), ]

    wordcloud2(word_freq, size = 1)
  })

  output$feedback_reviews <- renderUI({
    feedback <- feedback_data()
    validate(need(nrow(feedback) > 0, "No feedback submitted yet!"))

    tagList(
      lapply(1:nrow(feedback), function(i) {
        feedback_row <- feedback[i, ]
        wellPanel(
          h4(strong(feedback_row$Brand), " - ", feedback_row$City),
          p("Rating: ", paste(rep("⭐", feedback_row$Rating), collapse = "")),
          p(strong("Review: "), feedback_row$Review),
          p(em("By: ", feedback_row$Name))
        )
      })
    )
  })

}

shinyApp(ui, server)
