# Load necessary libraries
library(readr)
library(dplyr)  
library(shiny)
library(shinydashboard)
library(ggplot2)
library(ggmosaic)
library(plotly)
library(fmsb)
library(tidyr)

# Read the dataset
file_path <- "Results_21Mar2022.csv"
df <- read_csv(file_path)

# ---------------- Data Cleaning ---------------------
# Store original row count
initial_rows <- nrow(df)

# Remove missing values if any exist
missing_rows <- sum(is.na(df))  # Count missing values
df <- na.omit(df)
after_na_removal <- nrow(df)

# Remove duplicate rows if any exist
df <- df %>% distinct()
after_duplicate_removal <- nrow(df)

# Print number of rows removed
cat("Rows removed due to missing values:", initial_rows - after_na_removal, "\n")
cat("Rows removed due to duplicates:", after_na_removal - after_duplicate_removal, "\n")
cat("Final row count:", after_duplicate_removal, "\n")

# Check categorical variables for erroneous value
cat("Unique values for categorical variables:\n")
categorical_vars <- df %>% select(where(is.character))  # Select categorical columns

for (col_name in colnames(categorical_vars)) {
  unique_values <- unique(categorical_vars[[col_name]])
  cat(col_name, ":", paste(unique_values, collapse = ", "), "\n\n")
}

# Check range of numerical variables for erroneous value
cat("Range of values for numerical variables:\n")
numerical_vars <- df %>% select(where(is.numeric))  # Select numerical columns

for (col_name in colnames(numerical_vars)) {
  value_range <- range(numerical_vars[[col_name]], na.rm = TRUE)  # Get min and max
  cat(col_name, ": Min =", value_range[1], ", Max =", value_range[2], "\n")
}

# ---------------- Data Pre-Processing ---------------------
# Round all numerical columns to 3 decimal places
df <- df %>% mutate(across(where(is.numeric), ~ round(.x, 3)))

# Remove unnecessary columns
df <- df %>% select(-c(mc_run_id,sd_ghgs, sd_land, sd_watscar,sd_eut,sd_ghgs_ch4,sd_ghgs_n2o,sd_bio,sd_watuse,sd_acid))

# Calculate overall environmental impact
df <- df %>%
  mutate(overall_impact = rowSums(select(., where(is.numeric), -n_participants), na.rm = TRUE))
df$overall_impact <- as.numeric(df$overall_impact)

# Change categorical values for clarity
df <- df %>%
  mutate(diet_group = recode(diet_group, "meat" = "low-meat"))
df <- df %>%
  mutate(diet_group = recode(diet_group, "meat50" = "medium-meat"))
df <- df %>%
  mutate(diet_group = recode(diet_group, "meat100" = "high-meat"))

# Combine age group with sex
df <- df %>%
  mutate(age_grp_sex = paste(age_group, sex, sep = ":"))

# Calculate mean ghgs by co2
df <- df %>%
  mutate(mean_ghgs_co2 = mean_ghgs - mean_ghgs_ch4 - mean_ghgs_n2o)

# -----------------Dashboard-----------------------------
# Custom CSS to increase header height without changing width
# Custom CSS with color integration
custom_css <- tags$head(tags$style(HTML("
  /* Header logo customization */
  .main-header .logo {
    height: 80px;  /* Increase the height */
    line-height: 30px !important;  /* Adjust text positioning */
    font-size: 18px; /* Adjust font size */
    white-space: normal; /* Allow text wrapping */
    padding-top: 10px;  /* Add some spacing */
    background-color: #FD7600 !important; /* Set header background color */
  }
  
  /* Navbar customization */
  .main-header .navbar {
    min-height: 80px !important; /* Match header height */
    background-color: #FD7600 !important; /* Set navbar background color */
  }
  
  /* Sidebar customization */
  .main-sidebar {
    padding-top: 80px !important; /* Push sidebar down */
    background-color: #2C3E50; /* Set sidebar background color */
  }
  
  /* Page title customization */
  .page-title {
    margin-top: 0 !important;  /* Remove extra space at the top */
    font-size: 24px;  /* Adjust font size */
    font-weight: bold;
    color: #ffffff;  /* Set page title text color */
  }
  
  /* General body background color */
  .content-wrapper {
    background-color: #000000;  /* Set page background color */
  }

  /* Adjust text color in the sidebar */
  .sidebar-menu li a {
    color: #ffffff;  /* Set text color in sidebar */
  }
  
  /* Hover color for sidebar links */
  .sidebar-menu li a:hover {
    background-color: #34495E;  /* Set hover color for sidebar links */
    color: #ffffff;  /* Set text color on hover */
    border-color: #FD7600 !important;  /* Set custom border color when active */
  }
  
   /* Active/Selected color for sidebar links */
  .sidebar-menu li.active a {
    background-color: #34495E;  /* Set background color when active */
    color: #ffffff;  /* Set text color when active */
    border-color: #FD7600 !important;  /* Set custom border color when active */
  }
  
  .plotly .main-svg text {
      fill: white !important;  /* Ensure all text is white */
  }
  #ghg_select label {
    color: white;
  }
  #water_metric label {
  color: white;
  }
  .plot-container .slice text {
    fill: black !important;
  }
")))

# Define UI
ui <- dashboardPage(
  dashboardHeader(title = div("Environmental Impact of Different Dietary Practices", 
                              style = "white-space: normal; text-align: center;"), 
                  titleWidth = 230),  # Keep width fixed
  dashboardSidebar(
    sidebarMenu(
      menuItem("Overall", tabName = "overall", icon = icon("dashboard")),
      menuItem("Greenhouse Gas Emissions", tabName = "ghg", icon = icon("cloud")),
      menuItem("Land Use", tabName = "land", icon = icon("tree")),
      menuItem("Water", tabName = "water", icon = icon("tint")),
      menuItem("Eutrophication", tabName = "eutro", icon = icon("leaf")),
      menuItem("Biodiversity Loss", tabName = "biodiversity", icon = icon("paw")),
      menuItem("Acidification", tabName = "acidification", icon = icon("flask"))
    )
  ),
  dashboardBody(
    custom_css,  # Include custom CSS
    tabItems(
      tabItem(tabName = "overall",
              h2("Overall Environmental Impact", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  plotlyOutput("heatMap_overall", width = "100%", height = "600px"),
                  plotOutput("plotMosaic", height = "500px")
                ),
                
                # Right column: Other Charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_overall", height = "380px"),
                  plotlyOutput("barChart_overall", height = "400px"),   
                  plotlyOutput("dot_age_overall", height = "350px")   
                )
              )
      ),
      tabItem(tabName = "ghg",
              h2("Greenhouse Gas Emissions", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  checkboxGroupInput("ghg_select", 
                                     "Select Greenhouse Gases", 
                                     choices = c("CO2" = "co2", 
                                                 "N2O" = "n2o", 
                                                 "CH4" = "ch4"),
                                     selected = c("co2", "n2o", "ch4")),
                  plotlyOutput("heatMap_ghgs", width = "100%", height = "600px")
                ),
                
                # Right column: Other charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_ghgs", height = "380px"),
                  plotlyOutput("barChart_ghgs", height = "400px"),   
                  plotlyOutput("dot_age_ghgs", height = "350px")    
                )
              )
      ),
      tabItem(tabName = "land",
              h2("Land Use", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  plotlyOutput("heatMap_land", width = "100%", height = "600px")
                ),
                
                # Right column: Other Charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_land", height = "380px"),
                  plotlyOutput("barChart_land", height = "400px"),   
                  plotlyOutput("dot_age_land", height = "350px")   
                )
              )
      ),
      tabItem(tabName = "water",
              h2("Water Scarcity & Water Use", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  radioButtons(
                    inputId = "water_metric",
                    label = "Select Water Impact Type:",
                    choices = c("Water Scarcity" = "mean_watscar", "Water Use" = "mean_watuse"),
                    selected = "mean_watscar",
                    inline = TRUE
                  ),
                  plotlyOutput("heatMap_water", width = "100%", height = "600px")
                ),
                
                # Right column: Other Charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_water", height = "380px"),
                  plotlyOutput("barChart_water", height = "400px"),   
                  plotlyOutput("dot_age_water", height = "350px")   
                )
              )
      ),
      tabItem(tabName = "eutro",
              h2("Eutrophication Risk", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  plotlyOutput("heatMap_eutro", width = "100%", height = "600px")
                ),
                
                # Right column: Other Charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_eutro", height = "380px"),
                  plotlyOutput("barChart_eutro", height = "400px"),   
                  plotlyOutput("dot_age_eutro", height = "350px")   
                )
              )
      ),
      tabItem(tabName = "biodiversity",
              h2("Potential Biodiversity Loss", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  plotlyOutput("heatMap_bio", width = "100%", height = "600px")
                ),
                
                # Right column: Other Charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_bio", height = "380px"),
                  plotlyOutput("barChart_bio", height = "400px"),   
                  plotlyOutput("dot_age_bio", height = "350px")   
                )
              )
      ),
      tabItem(tabName = "acidification",
              h2("Potential Acidification", class = "page-title"),
              fluidRow(
                # Left column: Heatmap
                column(
                  width = 8,
                  plotlyOutput("heatMap_acid", width = "100%", height = "600px")
                ),
                
                # Right column: Other Charts
                column(
                  width = 4,
                  plotlyOutput("donutChart_acid", height = "380px"),
                  plotlyOutput("barChart_acid", height = "400px"),   
                  plotlyOutput("dot_age_acid", height = "350px")   
                )
              )
      )
    )
  )
)

# Define Server
server <- function(input, output) {
  #Heatmap for overall impact
  output$heatMap_overall <- renderPlotly({
    plot_ly(
      data = df,
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~overall_impact,
      type = "heatmap",
      colorscale = list(
        list(0.00, "#aaaaaa"),
        list(0.25, "#edf8e9"),
        list(0.50, "#74c476"),
        list(0.75, "#238b45"),
        list(1.00, "#0a6129")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Overall Impact: ", round(overall_impact, 2)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Interactive Heatmap: Overall Environmental Impact",
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  # Donut chart for overall impact by dietgroup
  # Calculate overall impact and participants number by diet group
  df_diet_impact <- df %>%
    group_by(diet_group) %>%
    summarize(
      total_impact = sum(overall_impact),      
      total_participants = sum(n_participants), 
      .groups = 'drop'                         
    )%>%
    mutate(
      percent_impact = round((total_impact / sum(total_impact)) * 100, 1),  
      hover_text = paste( diet_group,
                          "<br>Overall Impact: ", round(total_impact, 3), 
                          "<br>Impact Percentage: ", percent_impact, "%",
                          "<br>No. participants: ", total_participants),
    )
  output$donutChart_overall <- renderPlotly({
    total_impact_value <- sum(df_diet_impact$total_impact)  # Calculate the total overall impact
    plot_ly(
      data = df_diet_impact,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',  
      text = ~hover_text,  
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(
          text = "Overall Impact Percentage<br>by Diet Group",         
          x = 0.52,                        
          xanchor = "center",             
          y = 0.95,                       
          yanchor = "top",
          font = list(size = 19)
        ),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',   # Transparent plot background
        paper_bgcolor = 'rgba(0,0,0,0)',   # Transparent outer background
        margin = list(t = 50),
        annotations = list(
          text = paste("Total Overall Impact:<br>", round(total_impact_value, 2)),  # Display the total impact
          font = list(size = 12),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  # Bar chart: Overall impact by sex, stacked by diet group
  df_sex_diet_impact <- df %>%
    group_by(sex, diet_group) %>%
    summarise(
      total_impact = sum(overall_impact),
      .groups = 'drop'
    )
  df_sex_total <- df_sex_diet_impact %>%
    group_by(sex) %>%
    summarise(
      total_impact = sum(total_impact),
      .groups = 'drop'
    ) %>%
    mutate(
      percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
      label_text = paste0(percent_impact, "%")
    )
  output$barChart_overall <- renderPlotly({
    # Bar chart (stacked)
    p <- plot_ly(
      data = df_sex_diet_impact,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Total Impact: ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    # Add percentage text labels on top of bars
    p <- p %>% add_trace(
      data = df_sex_total,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    # Layout settings
    p %>% layout(
      barmode = 'stack',
      title = "Overall Environmental Impact <br>by Sex and Diet Group",
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = "Total Overall Impact",showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)
    )
  })
  # Dotplot for overall impact by age group
  output$dot_age_overall <- renderPlotly({
    ggplot_plot <- ggplot(df, aes(x = age_group, y = overall_impact, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = "Overall Impact") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y"))%>%
      layout(
        title = list(
          text = "Overall Environmental Impact<br> by Age Group",   # Title text
          x = 0.5,  
          xanchor = "center",  
          y = 0.90,  # Position the title
          yanchor = "top",  # Anchor the title at the top
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',   # Transparent plot background
        paper_bgcolor = 'rgba(0, 0, 0, 0)',   # Transparent outer background
        margin = list(t = 50)
      )
  })
  # Reactive expression for the selected greenhouse gases
  selected_ghg_data <- reactive({
    selected_gases <- input$ghg_select  # Get the selected gases
    
    # Create a new column based on the selected gases
    df_selected <- df %>%
      mutate(
        ghg_value = case_when(
          "co2" %in% selected_gases & "n2o" %in% selected_gases & "ch4" %in% selected_gases ~ mean_ghgs,
          "co2" %in% selected_gases & "n2o" %in% selected_gases ~ mean_ghgs_co2 + mean_ghgs_n2o,
          "co2" %in% selected_gases & "ch4" %in% selected_gases ~ mean_ghgs_co2 + mean_ghgs_ch4,
          "n2o" %in% selected_gases & "ch4" %in% selected_gases ~ mean_ghgs_n2o + mean_ghgs_ch4,
          "co2" %in% selected_gases ~ mean_ghgs_co2,
          "n2o" %in% selected_gases ~ mean_ghgs_n2o,
          "ch4" %in% selected_gases ~ mean_ghgs_ch4,
          TRUE ~ mean_ghgs  # Default to all gases if none of the above
        )
      )
    
    
    return(df_selected)
  })
  # Heatmap for greenhouse gases
  output$heatMap_ghgs <- renderPlotly({
    df_ghg <- selected_ghg_data()  # Get the filtered data with ghg_value column
    
    plot_ly(
      data = df_ghg,
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~ghg_value,  # Use dynamically calculated GHG value
      type = "heatmap",
      colorscale = list(
        list(0.00, "#fffee0"),
        list(0.25, "#FFFFB7"),
        list(0.50, "#FFF192"),
        list(0.75, "#FFEA61"),
        list(1.00, "#FFD400")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>GHG Value: ", round(ghg_value, 2)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Interactive Heatmap: Greenhouse Gas Emissions",
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Donut chart for greenhouse gases
  output$donutChart_ghgs <- renderPlotly({
    df_selected <- selected_ghg_data()  # Get selected data
    
    df_diet_ghgs <- df_selected %>%
      group_by(diet_group) %>%
      summarize(
        total_impact = sum(ghg_value),
        total_participants = sum(n_participants), 
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),  
        hover_text = paste(diet_group,
                           "<br>GHG Emissions: ", round(total_impact, 3), 
                           "<br>Emissions Percentage: ", percent_impact, "%",
                           "<br>No. participants: ", total_participants)
      )
    total_impact_value <- sum(df_diet_ghgs$total_impact)  # Calculate the total ghg impact
    
    plot_ly(
      data = df_diet_ghgs,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~hover_text, 
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(text = "Greenhouse Gas Emissions Percentage<br>by Diet Group", 
                     x = 0.52, xanchor = "center", 
                     y = 0.95, yanchor = "top",
                     font = list(size = 18)),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t=50),
        annotations = list(
          text = paste("Total GHG Emissions:<br>", round(total_impact_value, 2)),  # Display the total impact
          font = list(size = 12),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  
  # Bar chart for greenhouse gases by sex, stacked by diet group
  output$barChart_ghgs <- renderPlotly({
    df_selected <- selected_ghg_data()  # Get selected data
    
    df_sex_ghgs <- df_selected %>%
      group_by(sex, diet_group) %>%
      summarise(
        total_impact = sum(ghg_value),   
        .groups = 'drop'
      )
    
    # Total impact by sex
    df_sex_total_ghgs <- df_sex_ghgs %>%
      group_by(sex) %>%
      summarise(
        total_impact = sum(total_impact),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        label_text = paste0(percent_impact, "%")
      )
    
    # Stacked bar chart for greenhouse gases impact by sex and diet group
    p <- plot_ly(
      data = df_sex_ghgs,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Total Emissions: ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    
    # Add percentage text labels on top of bars
    p <- p %>% add_trace(
      data = df_sex_total_ghgs,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    
    # Layout settings
    p %>% layout(
      barmode = 'stack',
      title = "Greenhouse Gas Emissions <br>by Sex and Diet Group",
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = "Total GHG Emissions",showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)  # Adjust margin for title space
    )
  })
  
  # Dot plot for greenhouse gases by age group
  output$dot_age_ghgs <- renderPlotly({
    df_selected <- selected_ghg_data()  # Get selected data
    
    ggplot_plot <- ggplot(df_selected, aes(x = age_group, y = ghg_value, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = "Greenhouse Gas Emissions") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y")) %>%
      layout(
        title = list(
          text = "Greenhouse Gas Emissions<br> by Age Group",   # Title text
          x = 0.5,  
          xanchor = "center",  
          y = 0.90,  # Position the title
          yanchor = "top",  # Anchor the title at the top
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',   # Transparent plot background
        paper_bgcolor = 'rgba(0, 0, 0, 0)',   # Transparent outer background
        margin = list(t = 60)
      )
  })
  #Heatmap for landuse
  output$heatMap_land <- renderPlotly({
    plot_ly(
      data = df,
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~mean_land,
      type = "heatmap",
      colorscale = list(
        list(0.00, "#D3B683"),
        list(0.25, "#C97F2A"),
        list(0.50, "#B5651F"),
        list(0.75, "#865124"),
        list(1.00, "#664327")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Land Use: ", round(mean_land, 2)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Interactive Heatmap: Land Use",
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  # Donut chart for land use by dietgroup
  # Calculate land use and participants number by diet group
  df_diet_land <- df %>%
    group_by(diet_group) %>%
    summarize(
      total_impact = sum(mean_land),      
      total_participants = sum(n_participants), 
      .groups = 'drop'                         
    )%>%
    mutate(
      percent_impact = round((total_impact / sum(total_impact)) * 100, 1),  
      hover_text = paste( diet_group,
                          "<br>Land Use: ", round(total_impact, 3), 
                          "<br>Land Use Percentage: ", percent_impact, "%",
                          "<br>No. participants: ", total_participants),
    )
  output$donutChart_land <- renderPlotly({
    total_impact_value <- sum(df_diet_land$total_impact)  # Calculate the total overall impact
    plot_ly(
      data = df_diet_land,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',  # Use the custom hover_text field
      text = ~hover_text,  # Set the hover text as the custom formatted text
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(
          text = "Land Use Percentage<br>by Diet Group",         
          x = 0.52,                        
          xanchor = "center",             
          y = 0.95,                       
          yanchor = "top",
          font = list(size = 19)
        ),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',   # Transparent plot background
        paper_bgcolor = 'rgba(0,0,0,0)',   # Transparent outer background
        margin = list(t = 50),
        annotations = list(
          text = paste("Total Land Use:<br>", round(total_impact_value, 2)),  # Display the total impact
          font = list(size = 12),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  # Bar chart: Land use by sex, stacked by diet group
  output$barChart_land <- renderPlotly({
    
    df_sex_diet_land <- df %>%
      group_by(sex, diet_group) %>%
      summarise(
        total_impact = sum(mean_land),
        .groups = 'drop'
      )
    
    df_sex_total <- df_sex_diet_land %>%
      group_by(sex) %>%
      summarise(
        total_impact = sum(total_impact),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        label_text = paste0(percent_impact, "%")
      )
    
    # Bar chart (stacked)
    p <- plot_ly(
      data = df_sex_diet_land                                                                                                                       ,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Total Land Use: ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    # Add percentage text labels on top of bars
    p <- p %>% add_trace(
      data = df_sex_total,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    # Layout settings
    p %>% layout(
      barmode = 'stack',
      title = "Land Use <br>by Sex and Diet Group",
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = "Total Land Uset",showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)
    )
  })
  # Dotplot for land use by age group
  output$dot_age_land <- renderPlotly({
    ggplot_plot <- ggplot(df, aes(x = age_group, y = mean_land, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = "Land Use") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y"))%>%
      layout(
        title = list(
          text = "Land Use<br> by Age Group",   # Title text
          x = 0.5,  
          xanchor = "center",  
          y = 0.90,  # Position the title
          yanchor = "top",  # Anchor the title at the top
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',   # Transparent plot background
        paper_bgcolor = 'rgba(0, 0, 0, 0)',   # Transparent outer background
        margin = list(t = 60)
      )
  })
  # Reactive output for water
  df_water <- reactive({
    req(input$water_metric)
    df %>%
      mutate(selected_metric = .data[[input$water_metric]])
  })
  
  metric_label <- reactive({
    if (input$water_metric == "mean_watscar") "Water Scarcity" else "Water Use"
  })
  # Heatmap for water
  output$heatMap_water <- renderPlotly({
    metric_label <- ifelse(input$water_metric == "mean_watscar", "Water Scarcity", "Water Use")
    plot_ly(
      data = df_water(),
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~selected_metric,
      type = "heatmap",
      colorscale = list(
        list(0.00, "#DBE9F3"),
        list(0.25, "#B8CCE0"),
        list(0.50, "#7392BA"),
        list(0.75, "#5175A7"),
        list(1.00, "#2E5894")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>", metric_label, ": ", round(selected_metric, 2)),
      hoverinfo = "text",
      showscale = TRUE,
      colorbar = list(title = metric_label)  
    ) %>%
      layout(
        title = paste("Interactive Heatmap:", metric_label),
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  # Donut chart for water
  output$donutChart_water <- renderPlotly({
    df_diet <- df_water() %>%
      group_by(diet_group) %>%
      summarize(
        total_impact = sum(selected_metric),
        total_participants = sum(n_participants),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        hover_text = paste(
          diet_group,
          paste0("<br>", metric_label(), ": ", round(total_impact, 3)),
          paste0("<br>", metric_label(), " Percentage: ", percent_impact, "%"),
          "<br>No. participants: ", total_participants
        )
      )
    
    total_impact_value <- sum(df_diet$total_impact)
    
    plot_ly(
      data = df_diet,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~hover_text,
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(
          text = paste0(metric_label(), " Percentage<br>by Diet Group"),
          x = 0.52,
          xanchor = "center",
          y = 0.95,
          yanchor = "top",
          font = list(size = 19)
        ),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 50),
        annotations = list(
          text = paste("Total", metric_label(), ":<br>", round(total_impact_value, 2)),
          font = list(size = 12),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  #Bar chart for water
  output$barChart_water <- renderPlotly({
    df_sex_diet <- df_water() %>%
      group_by(sex, diet_group) %>%
      summarize(
        total_impact = sum(selected_metric),
        .groups = 'drop'
      )
    
    df_sex_total <- df_sex_diet %>%
      group_by(sex) %>%
      summarize(
        total_impact = sum(total_impact),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        label_text = paste0(percent_impact, "%")
      )
    
    p <- plot_ly(
      data = df_sex_diet,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>", metric_label(), ": ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    
    p <- p %>% add_trace(
      data = df_sex_total,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    
    p %>% layout(
      barmode = 'stack',
      title = paste(metric_label(), "<br>by Sex and Diet Group"),
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = paste("Total", metric_label()),showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)
    )
  })
  #Dotplot for water
  output$dot_age_water <- renderPlotly({
    ggplot_plot <- ggplot(df_water(), aes(x = age_group, y = selected_metric, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = metric_label()) +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y")) %>%
      layout(
        title = list(
          text = paste(metric_label(), "<br>by Age Group"),
          x = 0.5,
          xanchor = "center",
          y = 0.90,
          yanchor = "top",
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',
        paper_bgcolor = 'rgba(0, 0, 0, 0)',
        margin = list(t = 60)
      )
  })
  # Heatmap for Eutrophication Risk
  output$heatMap_eutro <- renderPlotly({
    plot_ly(
      data = df,
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~mean_eut,
      type = "heatmap",
      colorscale = list(
        list(0.00, "#DFDD94"),
        list(0.25, "#8D9529"),
        list(0.50, "#798026"),
        list(0.75, "#576216"),
        list(1.00, "#4C5711")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Eutrophication Risk: ", round(mean_eut, 2)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Interactive Heatmap: Eutrophication Risk",
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Donut chart for Eutrophication Risk by diet group
  output$donutChart_eutro <- renderPlotly({
    df_diet_impact <- df %>%
      group_by(diet_group) %>%
      summarize(
        total_impact = sum(mean_eut),      
        total_participants = sum(n_participants), 
        .groups = 'drop'                         
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),  
        hover_text = paste( diet_group,
                            "<br>Eutrophication Risk: ", round(total_impact, 3), 
                            "<br>Risk Percentage: ", percent_impact, "%",
                            "<br>No. participants: ", total_participants),
      )
    total_impact_value <- sum(df_diet_impact$total_impact)
    plot_ly(
      data = df_diet_impact,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~hover_text,
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(
          text = "Eutrophication Risk Percentage<br>by Diet Group",         
          x = 0.52, xanchor = "center", y = 0.95, yanchor = "top",
          font = list(size = 19)
        ),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 50),
        annotations = list(
          text = paste("Total Eutrophication Risk:<br>", round(total_impact_value, 2)),
          font = list(size = 11),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  # Bar chart: Eutrophication Risk by sex, stacked by diet group
  output$barChart_eutro <- renderPlotly({
    
    df_sex_diet_impact <- df %>%
      group_by(sex, diet_group) %>%
      summarise(
        total_impact = sum(mean_eut),
        .groups = 'drop'
      )
    
    df_sex_total <- df_sex_diet_impact %>%
      group_by(sex) %>%
      summarise(
        total_impact = sum(total_impact),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        label_text = paste0(percent_impact, "%")
      )
    
    p <- plot_ly(
      data = df_sex_diet_impact,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Eutrophication Risk: ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    p <- p %>% add_trace(
      data = df_sex_total,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    p %>% layout(
      barmode = 'stack',
      title = "Eutrophication Risk<br>by Sex and Diet Group",
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = "Total Eutrophication Risk",showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)
    )
  })
  
  # Dotplot for Eutrophication Risk by age group
  output$dot_age_eutro <- renderPlotly({
    ggplot_plot <- ggplot(df, aes(x = age_group, y = mean_eut, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = "Eutrophication Risk") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y")) %>%
      layout(
        title = list(
          text = "Eutrophication Risk<br>by Age Group",
          x = 0.5,  
          xanchor = "center",  
          y = 0.90,  
          yanchor = "top",  
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',
        paper_bgcolor = 'rgba(0, 0, 0, 0)',
        margin = list(t = 50)
      )
  })
  # Heatmap for Biodiversity Loss
  output$heatMap_bio <- renderPlotly({
    plot_ly(
      data = df,
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~mean_bio,
      type = "heatmap",
      colorscale = list(
        list(0.00, "#FFE8F1"),
        list(0.25, "#FFB3DE"),
        list(0.50, "#FF95CD"),
        list(0.75, "#FF69B4"),
        list(1.00, "#FF1493")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Biodiversity Loss: ", round(mean_bio, 2)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Interactive Heatmap: Biodiversity Loss",
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Donut chart for Biodiversity Loss by diet group
  output$donutChart_bio <- renderPlotly({
    df_diet_impact <- df %>%
      group_by(diet_group) %>%
      summarize(
        total_impact = sum(mean_bio),      
        total_participants = sum(n_participants), 
        .groups = 'drop'                         
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),  
        hover_text = paste( diet_group,
                            "<br>Biodiversity Loss: ", round(total_impact, 3), 
                            "<br>Loss Percentage: ", percent_impact, "%",
                            "<br>No. participants: ", total_participants),
      )
    total_impact_value <- sum(df_diet_impact$total_impact)
    plot_ly(
      data = df_diet_impact,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~hover_text,
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(
          text = "Biodiversity Loss Percentage<br>by Diet Group",         
          x = 0.52, xanchor = "center", y = 0.95, yanchor = "top",
          font = list(size = 19)
        ),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 50),
        annotations = list(
          text = paste("Total Biodiversity Loss:<br>", round(total_impact_value, 2)),
          font = list(size = 12),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  
  # Bar chart: Biodiversity Loss by sex, stacked by diet group
  output$barChart_bio <- renderPlotly({
    
    df_sex_diet_impact <- df %>%
      group_by(sex, diet_group) %>%
      summarise(
        total_impact = sum(mean_bio),
        .groups = 'drop'
      )
    
    df_sex_total <- df_sex_diet_impact %>%
      group_by(sex) %>%
      summarise(
        total_impact = sum(total_impact),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        label_text = paste0(percent_impact, "%")
      )
    
    p <- plot_ly(
      data = df_sex_diet_impact,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Biodiversity Loss: ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    p <- p %>% add_trace(
      data = df_sex_total,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    p %>% layout(
      barmode = 'stack',
      title = "Biodiversity Loss<br>by Sex and Diet Group",
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = "Total Biodiversity Loss",showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)
    )
  })
  
  # Dotplot for Biodiversity Loss by age group
  output$dot_age_bio <- renderPlotly({
    ggplot_plot <- ggplot(df, aes(x = age_group, y = mean_bio, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = "Biodiversity Loss") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y")) %>%
      layout(
        title = list(
          text = "Biodiversity Loss<br>by Age Group",
          x = 0.5,  
          xanchor = "center",  
          y = 0.90,  
          yanchor = "top",  
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',
        paper_bgcolor = 'rgba(0, 0, 0, 0)',
        margin = list(t = 50)
      )
  })
  # Heatmap for Acidification
  output$heatMap_acid <- renderPlotly({
    plot_ly(
      data = df,
      x = ~age_grp_sex,
      y = ~diet_group,
      z = ~mean_acid,
      type = "heatmap",
      colorscale = list(
        list(0.00, "#EAD5FB"),
        list(0.25, "#DBABFF"),
        list(0.50, "#C485F6"),
        list(0.75, "#B063E0"),
        list(1.00, "#9331CC")
      ),
      text = ~paste("Age & Gender: ", age_grp_sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Acidification: ", round(mean_acid, 2)),
      hoverinfo = "text",
      showscale = TRUE
    ) %>%
      layout(
        title = "Interactive Heatmap: Acidification",
        xaxis = list(title = "Age Group & Gender"),
        yaxis = list(title = "Diet Group"),
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)'
      )
  })
  
  # Donut chart for Acidification by diet group
  output$donutChart_acid <- renderPlotly({
    df_diet_impact <- df %>%
      group_by(diet_group) %>%
      summarize(
        total_impact = sum(mean_acid),      
        total_participants = sum(n_participants), 
        .groups = 'drop'                         
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),  
        hover_text = paste( diet_group,
                            "<br>Acidification: ", round(total_impact, 3), 
                            "<br>Acidification %: ", percent_impact, "%",
                            "<br>No. participants: ", total_participants),
      )
    total_impact_value <- sum(df_diet_impact$total_impact)
    plot_ly(
      data = df_diet_impact,
      labels = ~diet_group,
      values = ~total_impact,
      type = 'pie',
      textinfo = 'label+percent',
      hoverinfo = 'text',
      text = ~hover_text,
      hole = 0.5,
      marker = list(
        colors = c(
          "vegan" = "#FC6FB1",
          "veggie" = "#BDF5A9",
          "fish" = "#FBD537",
          "low-meat" = "#78E0F5",
          "medium-meat" = "#6996E6",
          "high-meat" = "#786BCF"
        )[df_diet_impact$diet_group]
      )
    ) %>%
      layout(
        title = list(
          text = "Acidification Percentage<br>by Diet Group",         
          x = 0.52, xanchor = "center", y = 0.95, yanchor = "top",
          font = list(size = 19)
        ),
        showlegend = FALSE,
        plot_bgcolor = 'rgba(0,0,0,0)',
        paper_bgcolor = 'rgba(0,0,0,0)',
        margin = list(t = 50),
        annotations = list(
          text = paste("Total Acidification:<br>", round(total_impact_value, 2)),
          font = list(size = 12),
          showarrow = FALSE,
          x = 0.5,
          y = 0.5,
          align = "center"
        )
      )
  })
  
  # Bar chart: Acidification by sex, stacked by diet group
  output$barChart_acid <- renderPlotly({
    
    df_sex_diet_impact <- df %>%
      group_by(sex, diet_group) %>%
      summarise(
        total_impact = sum(mean_acid),
        .groups = 'drop'
      )
    
    df_sex_total <- df_sex_diet_impact %>%
      group_by(sex) %>%
      summarise(
        total_impact = sum(total_impact),
        .groups = 'drop'
      ) %>%
      mutate(
        percent_impact = round((total_impact / sum(total_impact)) * 100, 1),
        label_text = paste0(percent_impact, "%")
      )
    
    p <- plot_ly(
      data = df_sex_diet_impact,
      x = ~sex,
      y = ~total_impact,
      type = 'bar',
      color = ~diet_group,
      text = ~paste("Sex: ", sex,
                    "<br>Diet Group: ", diet_group,
                    "<br>Acidification: ", round(total_impact, 2)),
      hoverinfo = 'text',
      textposition = "none"
    )
    p <- p %>% add_trace(
      data = df_sex_total,
      x = ~sex,
      y = ~total_impact,
      type = 'scatter',
      mode = 'text',
      text = ~label_text,
      textposition = 'top center',
      showlegend = FALSE,
      hoverinfo = 'none',
      inherit = FALSE
    )
    p %>% layout(
      barmode = 'stack',
      title = "Acidification<br>by Sex and Diet Group",
      xaxis = list(title = "Sex",showgrid = FALSE),
      yaxis = list(title = "Total Acidification",showgrid = FALSE),
      plot_bgcolor = 'rgba(0,0,0,0)',
      paper_bgcolor = 'rgba(0,0,0,0)',
      margin = list(t = 50)
    )
  })
  
  # Dotplot for Acidification by age group
  output$dot_age_acid <- renderPlotly({
    ggplot_plot <- ggplot(df, aes(x = age_group, y = mean_acid, color = age_group)) +
      geom_jitter(width = 0.2, height = 0, size = 2) +
      labs(x = "Age Group", y = "Acidification") +
      theme_minimal() +
      theme(legend.title = element_blank())
    
    ggplotly(ggplot_plot, tooltip = c("x", "y")) %>%
      layout(
        title = list(
          text = "Acidification<br>by Age Group",
          x = 0.5,  
          xanchor = "center",  
          y = 0.90,  
          yanchor = "top",  
          font = list(size = 19)
        ),
        xaxis = list(
          gridcolor = "darkgrey"
        ),
        yaxis = list(
          gridcolor = "darkgrey"
        ),
        plot_bgcolor = 'rgba(0, 0, 0, 0)',
        paper_bgcolor = 'rgba(0, 0, 0, 0)',
        margin = list(t = 50)
      )
  })
  output$plotMosaic <- renderPlot({
    ggplot(df) +
      geom_mosaic(aes(x = product(diet_group, age_group), weight = n_participants, fill = sex),alpha =1) +
      theme_minimal() +
      scale_fill_manual(
        values = c("male" = "#4A90E2", "female" = "#E87EBB")
      ) +
      labs(
        title = "Mosaic Plot of Number of Participants by Sex, Diet & Age Group",
        x = "Age Group & Gender",
        y = "Diet Group",
        fill = "Sex"
      ) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1, size = 12,color = "white"),  # Label size
        axis.text.y = element_text(size = 12,color = "white"),
        axis.title = element_text(size = 14,color = "white"),
        plot.title = element_text(size = 18,hjust = 0.5,color = "white"),
        legend.title = element_text(size = 13,color = "white"),
        legend.text = element_text(size = 11,color = "white"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()
      )
  },bg='transparent')
}

# Run the app
shinyApp(ui, server)