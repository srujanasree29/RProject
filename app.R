library(shiny)

# English Songs
english_songs <- data.frame(
  Song = c(
    "Ed Sheeran - Shape of You",
    "Billie Eilish - Bad Guy",
    "Maroon 5 - Sugar",
    "Taylor Swift - Love Story",
    "Justin Bieber - Sorry",
    "Dua Lipa - New Rules",
    "The Weeknd - Blinding Lights",
    "Adele - Rolling in the Deep",
    "Bruno Mars - Just the Way You Are",
    "Katy Perry - Firework",
    "Rihanna - Umbrella",
    "Coldplay - Viva La Vida",
    "Sam Smith - Stay With Me",
    "Ariana Grande - Thank U, Next",
    "Shawn Mendes - Señorita",
    "Taylor Swift - Blank Space",
    "Lizzo - Truth Hurts",
    "Justin Timberlake - Can't Stop the Feeling!",
    "Camila Cabello - Havana",
    "Maroon 5 - Girls Like You"
  ),
  Mood = rep(c("Happy", "Sad", "Excited", "Chill"), each = 5),
  Rating = c(5, 4, 4, 3, 3, 2, 5, 4, 3, 2, 4, 3, 4, 5, 4, 3, 5, 4, 3, 4),
  Language = rep("English", 20)
)

# Hindi Songs
hindi_songs <- data.frame(
  Song = c(
    "Arijit Singh - Tum Hi Ho",
    "Neha Kakkar - Dilbar",
    "Atif Aslam - Pehli Dafa",
    "Arijit Singh - Channa Mereya",
    "Badshah - DJ Waley Babu",
    "Neha Kakkar - Garmi",
    "Arijit Singh - Tera Ban Jaunga",
    "Badshah - Paagal",
    "Arijit Singh - Tera Yaar Hoon Main",
    "Guru Randhawa - High Rated Gabru",
    "Arijit Singh - Agar Tum Saath Ho",
    "Neha Kakkar - Kala Chashma",
    "Arijit Singh - Gerua",
    "Neha Kakkar - O Saki Saki",
    "Badshah - Mercy",
    "Arijit Singh - Kabira",
    "Guru Randhawa - Suit Suit",
    "Neha Kakkar - Morni Banke",
    "Arijit Singh - Hamari Adhuri Kahani",
    "Badshah - Kar Gayi Chull"
  ),
  Mood = rep(c("Happy", "Sad", "Excited", "Chill"), each = 5),
  Rating = c(5, 4, 4, 3, 3, 2, 5, 4, 3, 2, 4, 3, 4, 5, 4, 3, 5, 4, 3, 4),
  Language = rep("Hindi", 20)
)

# Telugu Songs
telugu_songs <- data.frame(
  Song = c(
    "Sid Sriram - Inkem Inkem Inkem Kaavaale",
    "Armaan Malik - Butta Bomma",
    "Shreya Ghoshal - Oka Praanam",
    "Sid Sriram - Samajavaragamana",
    "Sid Sriram - Vachinde",
    "Anurag Kulkarni - Choosi Chudangane",
    "Sid Sriram - Maate Vinadhuga",
    "Chinmayi - Ye Manishike Majiliyo",
    "Anirudh Ravichander - Dimaak Kharaab",
    "Swaroop Khan - Pilla Puli",
    "Kaala Bhairava - Ramuloo Ramulaa",
    "Geetha Madhuri - Maguva Maguva",
    "Armaan Malik - Ninnu Chuse Anandamlo",
    "Rahul Sipligunj - Mangli - Ramuloo Ramulaa",
    "Anurag Kulkarni - Hey Choosa",
    "Sid Sriram - Nee Kannu Neeli Samudram",
    "Armaan Malik - Ee Maya Peremito",
    "Anurag Kulkarni - Guche Gulabi",
    "Anurag Kulkarni - Uppena",
    "Kaala Bhairava - Nee Kallu Neeli Samudram"
  ),
  Mood = rep(c("Happy", "Sad", "Excited", "Chill"), each = 5),
  Rating = c(5, 4, 4, 3, 3, 2, 5, 4, 3, 2, 4, 3, 4, 5, 4, 3, 5, 4, 3, 4),
  Language = rep("Telugu", 20)
)

# Define UI
ui <- fluidPage(
  titlePanel("Song Recommendation"),
  sidebarLayout(
    sidebarPanel(
      selectInput("language", "Select Language:",
                  choices = c("English", "Hindi", "Telugu")),
      selectInput("mood", "Select Mood:",
                  choices = c("Happy", "Sad", "Excited", "Chill")),
      actionButton("recommend", "Recommend Song")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Plots", plotOutput("scatterplot"), plotOutput("barplot"), plotOutput("piechart")),
        tabPanel("Recommendation", verbatimTextOutput("recommendation"))
      )
    )
  )
)

# Define server
server <- function(input, output) {
  song_list <- reactive({
    if (input$language == "English") {
      english_songs
    } else if (input$language == "Hindi") {
      hindi_songs
    } else if (input$language == "Telugu") {
      telugu_songs
    }
  })
  
  output$scatterplot <- renderPlot({
    song_list_df <- song_list()
    filtered_songs <- song_list_df[song_list_df$Mood == input$mood, ]
    plot(filtered_songs$Rating, ylim = c(0, 5), xlim = c(0, nrow(filtered_songs)), xlab = "Song", ylab = "Rating",
         main = "Songs Based on Mood and Rating")
    text(1:nrow(filtered_songs), filtered_songs$Rating, labels = filtered_songs$Song, cex = 0.8, pos = 3, xpd = TRUE)
  })
  
  output$barplot <- renderPlot({
    song_list_df <- song_list()
    filtered_songs <- song_list_df[song_list_df$Mood == input$mood, ]
    barplot(filtered_songs$Rating, names.arg = filtered_songs$Song, 
            xlab = "Song", ylab = "Rating", main = "Songs Based on Mood and Rating", horiz = TRUE)
  })
  
  output$piechart <- renderPlot({
    song_list_df <- song_list()
    filtered_songs <- song_list_df[song_list_df$Mood == input$mood, ]
    pie(filtered_songs$Rating, labels = filtered_songs$Song, main = "Songs Based on Mood")
  })
  
  output$recommendation <- renderPrint({
    song_list_df <- song_list()
    filtered_songs <- song_list_df[song_list_df$Mood == input$mood, ]
    top_song <- filtered_songs[which.max(filtered_songs$Rating), "Song"]
    cat("Recommended Song:\n")
    print(top_song)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
