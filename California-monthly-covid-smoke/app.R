#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(sf)
library(shiny)
library(plotly)
library(ggplot2)
library(dplyr)

# Assuming covid_monthly_df and median_california are already loaded and transformed as per your code
covid_monthly_df <- st_read("final_dataset_covid_smoke_2020.shp")
# Ensure that the Month column is a factor with the levels in the correct order
covid_monthly_df$Month <- factor(covid_monthly_df$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"))


# Calculate median cases for California for each month
median_california <- covid_monthly_df %>%
  group_by(Month) %>%
  summarize(median_california = median(css_p10, na.rm = TRUE))


# Add a custom hover text column to your data frames
covid_monthly_df$hover_text <- paste("County:", covid_monthly_df$NAME, 
                                     "\nMonth:", covid_monthly_df$Month, 
                                     "\nCase Incidence/10,000 people:", sprintf("%.2f", covid_monthly_df$css_p10))

median_california$hover_text <- paste("Month:", median_california$Month, 
                                      "\nPM Statewide Median Case Incidence:", sprintf("%.2f", median_california$median_california))


# Calculate median deaths for California for each month
median_california2 <- covid_monthly_df %>%
  group_by(Month) %>%
  summarize(median_california_dths = median(dths_10, na.rm = TRUE))

# Add a custom hover text column to your data frames
covid_monthly_df$hover_text <- paste("County:", covid_monthly_df$NAME, 
                                     "\nMonth:", covid_monthly_df$Month, 
                                     "\nDeath Incidence/10,000 people:", sprintf("%.2f", covid_monthly_df$dths_10))

median_california2$hover_text <- paste("Month:", median_california2$Month, 
                                       "\nStatewide County Median:", sprintf("%.2f", median_california2$median_california_dths))



# Calculate median cases for California for each month
median_california3 <- covid_monthly_df %>%
  group_by(Month) %>%
  summarize(median_cali_smoke = median(smoke, na.rm = TRUE))

# Add a custom hover text column to your data frames
covid_monthly_df$hover_text <- paste("County:", covid_monthly_df$NAME, 
                                     "\nMonth:", covid_monthly_df$Month, 
                                     "\nMean PM2.5 (µg/m^3):", sprintf("%.2f", covid_monthly_df$smoke))

median_california3$hover_text <- paste("Month:", median_california3$Month, 
                                       "\nStatewide County Median PM2.5 (µg/m^3):", sprintf("%.2f", median_california3$median_cali_smoke))



# Create the plot
monthly_incidence <- ggplot() +
  geom_point(data = covid_monthly_df, aes(x = Month, y = css_p10, color = "County Incidence", text = hover_text), 
             size = 3, alpha = 0.7) +
  geom_point(data = median_california, aes(x = Month, y = median_california, color = "Statewide Median", text = hover_text), 
             size = 3, shape = 17) +
  scale_color_manual(values = c("County Incidence" = "red", "Statewide Median" = "blue")) +
  scale_shape_manual(values = c("Statewide Median" = 17)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  labs(title = "COVID-19 Case Incidence",
       x = "Month",
       y = "Cases per 10,000 Persons",
       color = "Incidence Type") +
  guides(color = guide_legend(title = NULL)) + # Remove legend title
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) # Rotate x-axis labels)
monthly_incidence_plotly <- ggplotly(monthly_incidence, tooltip = "text")

# Create the plot
monthly_incidence_dths <- ggplot() +
  geom_point(data = covid_monthly_df, aes(x = Month, y = dths_10, color = "Death Incidence", text = hover_text), 
             size = 3, alpha = 0.7) +
  geom_point(data = median_california2, aes(x = Month, y = median_california_dths, color = "Statewide Median", text = hover_text), 
             size = 3, shape = 17) +
  scale_color_manual(values = c("Death Incidence" = "red", "Statewide Median" = "blue")) +
  scale_shape_manual(values = c("Statewide Median" = 17)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  labs(title = "COVID-19 Death Incidence",
       x = "Month",
       y = "Deaths per 10,000 Persons",
       color = "Incidence Type") +
  guides(color = guide_legend(title = NULL)) + # Remove legend title
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) # Rotate x-axis labels
monthly_incidence_dhts_plotly <- ggplotly(monthly_incidence_dths, tooltip = "text")

# Create the plot
monthly_smoke <- ggplot() +
  geom_point(data = covid_monthly_df, aes(x = Month, y = smoke, color = "County",
                                          text = hover_text), 
             size = 3, alpha = 0.7) +
  geom_point(data = median_california3, aes(x = Month, y = median_cali_smoke, color = "Statewide Median",
                                            text = hover_text), 
             size = 3, shape = 17) +
  scale_color_manual(values = c("County" = "purple", "Statewide Median" = "green")) +
  scale_shape_manual(values = c("Statewide" = 17)) +
  scale_x_discrete(labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")) +
  theme_minimal() +
  labs(title = "Smoke Specific PM2.5",
       x = "Month",
       y = "Mean PM2.5 Exposure, µg/m^3",
       color = "Incidence Type") +
  guides(color = guide_legend(title = NULL)) + # Remove legend title
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) # Rotate x-axis labels
monthly_smoke_plotly <- ggplotly(monthly_smoke, tooltip = "text")


# Define UI
ui <- fluidPage(
  titlePanel("COVID-19 and PM2.5 Data Visualizations"),
  tabsetPanel(
    tabPanel("COVID-19 Case Incidence", plotlyOutput("case_incidence_plot")),
    tabPanel("COVID-19 Death Incidence", plotlyOutput("death_incidence_plot")),
    tabPanel("Smoke Specific PM2.5", plotlyOutput("smoke_plot"))
  )
)

# Define server logic
server <- function(input, output, session) {
  
  # Render the COVID-19 Case Incidence Plot
  output$case_incidence_plot <- renderPlotly({
    # Assuming monthly_incidence_plotly is prepared as per your code
    monthly_incidence_plotly
  })
  
  # Render the COVID-19 Death Incidence Plot
  output$death_incidence_plot <- renderPlotly({
    # Assuming monthly_incidence_dhts_plotly is prepared as per your code
    monthly_incidence_dhts_plotly
  })
  
  # Render the Smoke Specific PM2.5 Plot
  output$smoke_plot <- renderPlotly({
    # Assuming monthly_smoke_plotly is prepared as per your code
    monthly_smoke_plotly
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

