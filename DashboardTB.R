#DASHBOARD VISUALISASI

library(shiny)
library(shinydashboard)
library(ggplot2)
library(plotly)
library(DT)
library(forecast)
library(dplyr)

forecast <- forecast(model_final, h = 36)
df_historis <- data.frame(
  Tanggal = seq(as.Date("2020-01-01"), 
                by = "month", length.out = 72),
  Kasus   = as.numeric(full_ts),
  Tipe    = "Historis"
)

df_prediksi <- data.frame(
  Tanggal  = seq(as.Date("2026-01-01"), 
                 by = "month", length.out = 36),
  Kasus    = as.numeric(forecast$mean),
  Lo80     = as.numeric(forecast$lower[,1]),
  Hi80     = as.numeric(forecast$upper[,1]),
  Lo95     = as.numeric(forecast$lower[,2]),
  Hi95     = as.numeric(forecast$upper[,2]),
  Tipe     = "Prediksi"
)

df_tahunan <- df_prediksi %>%
  mutate(Tahun = format(Tanggal, "%Y")) %>%
  group_by(Tahun) %>%
  summarise(
    Total_Kasus     = round(sum(Kasus)),
    Rata_rata_Bulan = round(mean(Kasus), 1),
    Min_Bulan       = round(min(Kasus), 1),
    Max_Bulan       = round(max(Kasus), 1)
  )

mae_val  <- 30.90046
mape_val <- 14.93409
rmse_val <- 42.3918

ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(
    title = "Prediksi Tuberkulosis Paru Kab. Pemalang 2026-2028",
    titleWidth = 400
  ),
  dashboardSidebar(
    width = 220,
    sidebarMenu(
      menuItem("Beranda",        tabName = "beranda",   icon = icon("home")),
      menuItem("Hasil Prediksi", tabName = "prediksi",     icon = icon("table"))
    )
  ),
  dashboardBody(
    # CSS custom
    tags$head(tags$style(HTML("
      .small-box { border-radius: 8px; }
      .box { border-radius: 8px; overflow: hidden; }
      .box-body { overflow: hidden; }
      .skin-blue .main-header .logo { font-size: 14px; font-weight: bold; }
    "))),
    
    tabItems(
      tabItem(tabName = "beranda",
              
              h3("Dashboard Prediksi Kejadian Tuberkulosis Paru", style = "font-weight: bold;"),
              h5("Kabupaten Pemalang | Model SARIMA(0,1,1)(1,0,0)[12]"),
              hr(),
              fluidRow(
                valueBox(228.6,  "Rata-Rata Kasus/Bulan (2020-2025)",
                         icon = icon("calendar"),  color = "blue"),
                valueBox(76,     "Kasus Terendah (2020-2025)",
                         icon = icon("arrow-down"), color = "green"),
                valueBox(430,    "Kasus Tertinggi (2020-2025)",
                         icon = icon("arrow-up"),   color = "red")
              ),
              fluidRow(
                valueBox(
                  paste0(round(sum(df_prediksi$Kasus[1:12])), " kasus"),
                  "Total Prediksi 2026",
                  icon = icon("calendar-alt"), color = "yellow"),
                valueBox(
                  paste0(round(sum(df_prediksi$Kasus[13:24])), " kasus"),
                  "Total Prediksi 2027",
                  icon = icon("calendar-alt"), color = "orange"),
                valueBox(
                  paste0(round(sum(df_prediksi$Kasus[25:36])), " kasus"),
                  "Total Prediksi 2028",
                  icon = icon("calendar-alt"), color = "red")
              ),
              fluidRow(
                box(width = 12, title = "Plot Data Prediksi",
                    status = "primary", solidHeader = TRUE,
                    plotlyOutput("plot_forecast", height = "450px"))
              )
      ),
      #TAB TABEL
      tabItem(tabName = "prediksi",
              
              h3("Tabel & Grafik Hasil Prediksi Kejadian TB Paru 2026-2028", style = "font-weight: bold;"),
              hr(),
              
              fluidRow(
                box(width = 12, title = "Ringkasan Per Tahun",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("tabel_tahunan"))
              ),
              fluidRow(
                box(width = 6, title = "Detail Prediksi Per Bulan",
                    status = "info", solidHeader = TRUE,
                    DTOutput("tabel_bulanan", height = "300px")),
                
                box(width = 6, title = "Pola Musiman Prediksi",
                    status = "warning", solidHeader = TRUE,
                    plotlyOutput("plot_musiman", height = "300px"))
              )
      )
    )
  )
)

server <- function(input, output) {
  
  #Plot forecast 
  output$plot_forecast <- renderPlotly({
    
    p <- ggplot() +
      #CI 95%
      geom_ribbon(data = df_prediksi,
                  aes(x = Tanggal, ymin = Lo95, ymax = Hi95),
                  fill = "steelblue", alpha = 0.15) +
      #CI 80%
      geom_ribbon(data = df_prediksi,
                  aes(x = Tanggal, ymin = Lo80, ymax = Hi80),
                  fill = "steelblue", alpha = 0.25) +
      #Garis historis
      geom_line(data = df_historis,
                aes(x = Tanggal, y = Kasus, color = "Historis"),
                linewidth = 0.8) +
      geom_point(data = df_historis,
                 aes(x = Tanggal, y = Kasus, color = "Historis"),
                 size = 1.5) +
      #Garis prediksi
      geom_line(data = df_prediksi,
                aes(x = Tanggal, y = Kasus, color = "Prediksi"),
                linewidth = 0.8) +
      geom_point(data = df_prediksi,
                 aes(x = Tanggal, y = Kasus, color = "Prediksi"),
                 size = 1.5) +
      #Garis pemisah
      geom_vline(xintercept = as.numeric(as.Date("2026-01-01")),
                 linetype = "dashed", color = "red", linewidth = 0.8) +
      scale_color_manual(values = c("Historis" = "black", 
                                    "Prediksi" = "steelblue")) +
      scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
      labs(title = "Data Historis & Prediksi TB Paru Kab. Pemalang",
           x = "Tahun", y = "Jumlah Kasus", color = "") +
      theme_minimal() +
      theme(legend.position = "bottom",
            plot.title = element_text(face = "bold", hjust = 0.5))
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(hovermode = "x unified")
  })
  
  
  #Plot pola musiman
  output$plot_musiman <- renderPlotly({
    
    df_musiman <- df_prediksi %>%
      mutate(Bulan = format(Tanggal, "%b"),
             Tahun = format(Tanggal, "%Y"),
             BulanNum = as.numeric(format(Tanggal, "%m"))) %>%
      arrange(BulanNum)
    
    df_musiman$Bulan <- factor(df_musiman$Bulan,
                               levels = c("Jan","Feb","Mar","Apr","May","Jun",
                                          "Jul","Aug","Sep","Oct","Nov","Dec"))
    
    p3 <- ggplot(df_musiman,
                 aes(x = Bulan, y = Kasus, 
                     color = Tahun, group = Tahun)) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 2) +
      scale_color_manual(values = c("2026" = "#3498db",
                                    "2027" = "#e67e22",
                                    "2028" = "#e74c3c")) +
      labs(title = "Pola Musiman Prediksi per Tahun",
           x = "Bulan", y = "Jumlah Kasus", color = "Tahun") +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5))
    
    ggplotly(p3)
  })
  
  #Tabel ringkasan tahunan
  output$tabel_tahunan <- renderDT({
    datatable(df_tahunan,
              colnames  = c("Tahun", "Total Kasus", "Rata-rata/Bulan", "Min", "Max"),
              width = "100%",
              options   = list(dom = "t", pageLength = 3, autoWidth  = FALSE,
                               columnDefs = list(list(className = "dt-center", targets = "_all", width = "20%"))),
              rownames  = FALSE) %>%
      formatStyle("Total_Kasus",
                  background         = styleColorBar(df_tahunan$Total_Kasus, "steelblue"),
                  backgroundSize     = "100% 90%",
                  backgroundRepeat   = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle(columns = c("Tahun", "Total_Kasus", "Rata_rata_Bulan", 
                              "Min_Bulan", "Max_Bulan"),
                  textAlign = "center")
  })
  
  #Tabel detail per bulan
  output$tabel_bulanan <- renderDT({
    
    df_tampil <- df_prediksi %>%
      mutate(
        Bulan  = format(Tanggal, "%B %Y"),
        Kasus  = round(Kasus, 1),
        Lo95   = round(Lo95, 1),
        Hi95   = round(Hi95, 1)
      ) %>%
      select(Bulan, Kasus, Lo95, Hi95)
    
    datatable(df_tampil,
              colnames = c("Periode", "Prediksi Kasus",
                           "Batas Bawah 95%", "Batas Atas 95%"),
              options = list(dom = "t", pageLength = 36, scrollY = "300px"),
              rownames = FALSE)
  })
  
}

shinyApp(ui = ui, server = server)
