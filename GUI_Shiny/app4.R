library(shiny)
library(bsts)
library(ggplot2)
library(zoo)

# Informasi login
credentials <- list(user = "admin", password = "1234")

# CSS background
custom_css <- "
body {
  background-image: url('background.png');
  background-size: cover;
  background-position: center;
  background-attachment: fixed;
  margin: 0; padding: 0; height: 100vh;
  font-family: 'Arial', sans-serif;
  position: relative; z-index: -2;
}
body::before {
  content: ''; position: fixed; top: 0; left: 0;
  width: 100vw; height: 100vh;
  background-color: rgba(0, 0, 0, 0.5);
  z-index: -1;
}
#login-box {
  position: absolute; top: 50%; left: 50%;
  transform: translate(-50%, -50%);
  background-color: rgba(0,0,0,0.6);
  padding: 30px; border-radius: 10px; width: 350px;
  color: white; box-shadow: 0px 0px 15px rgba(0,0,0,0.5);
  animation: fadeIn 1s ease-in-out;
}
@keyframes fadeIn {
  from { opacity: 0; transform: translate(-50%, -60%); }
  to { opacity: 1; transform: translate(-50%, -50%); }
}
.panel, .well, .tab-content, .form-control, .shiny-input-container {
  background-color: rgba(0, 0, 0, 0.6) !important; color: white;
}
h1, h2, h4, label { color: white; text-align: center; }
"

ui <- fluidPage(
  tags$head(tags$style(HTML(custom_css))),
  uiOutput("login_ui"),
  uiOutput("app_ui")
)

server <- function(input, output, session) {
  user_authenticated <- reactiveVal(FALSE)
  processed_data <- reactiveVal()
  
  observeEvent(input$login_button, {
    if (input$username == credentials$user && input$password == credentials$password) {
      user_authenticated(TRUE)
    } else {
      showModal(modalDialog(title = "Login Gagal", "Username atau password salah.", easyClose = TRUE))
    }
  })
  
  output$login_ui <- renderUI({
    if (!user_authenticated()) {
      div(id = "login-box",
          h2("Selamat Datang di Aplikasi SiPREDIK"),
          h4("Sistem Prediksi Capaian Pendapatan Pasar Tradisional"),
          textInput("username", "Username"),
          passwordInput("password", "Password"),
          actionButton("login_button", "Login"))
    }
  })
  
  output$app_ui <- renderUI({
    if (user_authenticated()) {
      fluidPage(
        h1("Prediksi Capaian Pendapatan Pasar Tradisional Menggunakan Metode BSTS", align = "center"),
        sidebarLayout(
          sidebarPanel(
            fileInput("file1", "Upload CSV File", accept = c(".csv")),
            actionButton("processData", "Pemrosesan Data"),
            checkboxGroupInput("model_components", "Pilih Komponen Model:",
                               choices = list("Level Lokal" = "local_level", 
                                              "Tren Linear Lokal" = "local_linear_trend", 
                                              "Musiman (12 Bulan)" = "seasonal")),
            selectInput("niter_choice", "Jumlah Iterasi:", choices = c("1000", "5000", "10000"), selected = "1000"),
            selectInput("burnin_choice", "Burn-in Sample:", choices = c("400", "2000", "4000"), selected = "400"),
            actionButton("runModel", "Jalankan Model"),
            downloadButton("downloadData", "Unduh Hasil Prediksi")
          ),
          mainPanel(
            tabsetPanel(
              tabPanel("Visualisasi Pola Deret Waktu", plotOutput("plotOriginal")),
              tabPanel("Evaluasi Model", 
                       plotOutput("plotEvaluation"),
                       verbatimTextOutput("evalMetrics")),
              tabPanel("Visualisasi Hasil Prediksi", plotOutput("plotForecast")),
              tabPanel("Data Hasil Prediksi", tableOutput("predictedData"))
            )
          )
        )
      )
    }
  })
  
  observeEvent(input$processData, {
    req(input$file1)
    data <- read.csv(input$file1$datapath)
    colnames(data) <- c("Tahun", "Capaian_Pendapatan")
    
    data$Tahun <- as.Date(paste0(data$Tahun, "-01-01"))
    
    # Tangani outlier dan interpolasi
    Q1 <- quantile(data$Capaian_Pendapatan, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$Capaian_Pendapatan, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    lower <- Q1 - 1.5 * IQR
    upper <- Q3 + 1.5 * IQR
    data$Capaian_Pendapatan[data$Capaian_Pendapatan < lower | data$Capaian_Pendapatan > upper] <- NA
    data$Capaian_Pendapatan <- na.approx(data$Capaian_Pendapatan, na.rm = FALSE)
    
    # Membulatkan nilai prosentase di kolom Capaian_Pendapatan
    data$Capaian_Pendapatan <- round(data$Capaian_Pendapatan, 2)
    
    processed_data(data)
    
    output$plotOriginal <- renderPlot({
      ggplot(data, aes(x = Tahun, y = Capaian_Pendapatan)) +
        geom_line(color = "blue") +
        labs(title = "Visualisasi Pola Deret Waktu Capaian Pendapatan",
             x = "Tahun", y = "Capaian Pendapatan (%)") +
        theme_minimal()
    })
  })
  
  observeEvent(input$runModel, {
    req(processed_data())
    data <- processed_data()
    niter <- as.numeric(input$niter_choice)
    burn <- as.numeric(input$burnin_choice)
    
    ### Step 1: Split data untuk evaluasi ###
    train_size <- floor(0.9 * nrow(data))
    train_data <- data[1:train_size, ]
    test_data <- data[(train_size + 1):nrow(data), ]
    
    y_train <- train_data$Capaian_Pendapatan
    ss_train <- list()
    
    if ("local_level" %in% input$model_components) {
      ss_train <- AddLocalLevel(ss_train, y_train)
    }
    if ("local_linear_trend" %in% input$model_components) {
      ss_train <- AddLocalLinearTrend(ss_train, y_train)
    }
    if ("seasonal" %in% input$model_components) {
      ss_train <- AddSeasonal(ss_train, y_train, nseasons = 12)
    }
    
    model_eval <- bsts(y_train, state.specification = ss_train, niter = niter)
    pred_eval <- predict(model_eval, horizon = nrow(test_data), burn = burn)
    predicted_eval <- pred_eval$mean
    
    # Evaluasi
    mape <- function(actual, predicted) {
      mean(abs((actual - predicted) / actual)) * 100
    }
    rmse <- function(actual, predicted) {
      sqrt(mean((actual - predicted)^2))
    }
    mape_value <- mape(test_data$Capaian_Pendapatan, predicted_eval)
    rmse_value <- rmse(test_data$Capaian_Pendapatan, predicted_eval)
    
    output$evalMetrics <- renderPrint({
      cat("MAPE pada data uji:", round(mape_value, 2), "%\n")
      cat("RMSE pada data uji:", round(rmse_value, 2), "\n")
    })
    
    output$plotEvaluation <- renderPlot({
      plot(test_data$Capaian_Pendapatan, type = "o", col = "red", pch = 19,
           xlab = "Index Bulan", ylab = "Capaian Pendapatan (%)", main = "Actual vs Predicted",
           ylim = range(c(test_data$Capaian_Pendapatan, predicted_eval)))
      lines(predicted_eval, col = "blue", lwd = 2)
      legend("topleft", legend = c("Actual", "Predicted"), col = c("red", "blue"), lwd = 2, pch = 19)
    })
    
    ### Step 2: Model seluruh data untuk prediksi 6 bulan ###
    y_full <- data$Capaian_Pendapatan
    ss_full <- list()
    
    if ("local_level" %in% input$model_components) {
      ss_full <- AddLocalLevel(ss_full, y_full)
    }
    if ("local_linear_trend" %in% input$model_components) {
      ss_full <- AddLocalLinearTrend(ss_full, y_full)
    }
    if ("seasonal" %in% input$model_components) {
      ss_full <- AddSeasonal(ss_full, y_full, nseasons = 12)
    }
    
    model_full <- bsts(y_full, state.specification = ss_full, niter = niter)
    pred_full <- predict(model_full, horizon = 6, burn = burn)
    
    future_dates <- seq.Date(from = max(data$Tahun) + 28, by = "month", length.out = 6)
    hasil_prediksi <- data.frame(
      Tanggal = format(future_dates, "%Y-%m-%d"),
      Prediksi_Capaian_Pendapatan = round(pred_full$mean, 2)
    )
    
    output$predictedData <- renderTable({ hasil_prediksi })
    
    output$plotForecast <- renderPlot({
      ggplot(hasil_prediksi, aes(x = as.Date(Tanggal), y = Prediksi_Capaian_Pendapatan)) +
        geom_line(color = "blue") +
        geom_point(color = "red") +
        labs(title = "Prediksi 6 Bulan ke Depan",
             x = "Tanggal", y = "Capaian Pendapatan (%)") +
        theme_minimal()
    })
    
    output$downloadData <- downloadHandler(
      filename = function() {
        paste("hasil_prediksi_", Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        write.csv(hasil_prediksi, file, row.names = FALSE)
      }
    )
  })
}

shinyApp(ui = ui, server = server)