library("tidyverse")
library("sf")
library("shiny")
library("leaflet")
library("htmlwidgets")
library("rsconnect")
library("shinythemes")
options(scipen = 999)

# Load data 
# Census data 
census <- st_read("2020_CensusData.shp") %>% st_transform(crs = 4326)

# Process Census data 
census_clean <- census %>% 
    select(1:2, # geoid and total population
           4, # Population Density (Per Sq. Mile)
           7,8, # Total males, total females 
           19:21, # Population aged 65+ 
           23:24, # Population white, black
           39, # Median household income
           41, 43, # Occupied housing units, Renter occupied housing units  
           47, 48) %>% # Population doing poor or struggling, Population doing okay
    mutate("Population Density (Per Sq. Mile)" = A00002_2,
           "% Males" = round(100*A02001_2/(A02001_2 + A02001_3), digits = 1),
           "% Females" = round(100*A02001_3/(A02001_2 + A02001_3), digits = 1),
           "% Population 65+" = round((100*A01001_11 + A01001_12 + A01001_13)/A00001_1, digits = 1),
           "% White Population" = round(100*A03001_2/A00001_1, digits = 1),
           "% Black Population" = round(100*A03001_3/A00001_1, digits = 1),
           "Median Household Income" = A14006_1,
           "% Renter Occupied" = round(100*A10060_3/A10060_1, digits = 1),
           "% Population Doing Poor or Struggling" = round(100*B13004_4/(B13004_4 + B13004_5), digits = 1)) %>% 
    select(1,17:25)

census_clean_numeric <- census_clean %>% st_set_geometry(NULL) # No geometry 

# Turn census variables to quintiles 
for (i in names(census_clean)[2:10]) {
    census_clean[[i]] <- Hmisc::cut2(census_clean[[i]],
                                     g = 5,
                                     digits = 1)
}

census_geometry_NA <- census_clean %>% st_set_geometry(NULL)

# Public facilities 
pub_facilities <- data.table::fread("Public_Facilities.csv", colClasses = c("POPL_TYPE" = "factor")) %>% 
    mutate(POPL_TYPE = as.factor(str_to_title(POPL_TYPE))) %>% 
    st_as_sf(coords = c("Lon", "Lat")) %>% 
    st_set_crs(value = 4326) %>% 
    st_intersection(census_clean) # Keep only points in the South Bend census data 

pub_facilities_geometry_NA <- pub_facilities %>% st_set_geometry(NULL)

# Ensure geometries are the same 
st_crs(census) == st_crs(pub_facilities)

# Street Lights
light <- data.table::fread("Street_Lights.csv") %>%
    select(1:13,15:17,33,36) %>%
    mutate(Wattage = ifelse(Wattage=="", "Unknown",Wattage),
           wattage_type=as.factor(Wattage)) %>%
    st_as_sf(coords=c("Lon","Lat")) %>%
    st_set_crs(value=4326) %>%
    st_intersection(census_clean)

light_geometry_NA <- light %>% st_set_geometry(NULL)

# Abandoned Properties
prop <- st_read("Abandoned_Property_Parcels.shp") %>%
    filter(Outcome_St != "") %>%
    mutate(outcome_type = as.factor(str_to_title(Outcome_St)),
           prop_type = ifelse(grepl("House|Duplex",Structures), "Residential", 
                              ifelse(grepl("Commercial|Industrial|Building|Community|Mixed|Mu",Structures),"Commercial",NA))) %>%
    st_transform(crs = 4326) %>% 
    st_centroid() %>% # turn to centroid 
    st_intersection(census_clean) # Keep only points in the South Bend census data 

prop_geometry_NA <- prop %>% st_set_geometry(NULL)

# Schools & Census 
schoolCol <- c('Elementary' = 'blue', 'Middle' = 'green', 'High' = 'orange', 'Day Care' = 'pink', 'Other' = 'red')

cols <- colorFactor(schoolCol, names(schoolCol))

census <- read_sf('2020_CensusData.shp') %>% 
    select(GEOID, A01001_1:A01001_6) %>% 
    st_transform(crs = 4326) %>% 
    rename('Total' = 'A01001_1',
           'Under 5 years' = 'A01001_2',
           '5-9 years' = 'A01001_3',
           '10-14 years' = 'A01001_4',
           '15-17 years' = 'A01001_5',
           '18-24 years' = 'A01001_6')

census_geometry_NA_gulshan <- census %>% st_set_geometry(NULL)

schools <- read_sf('School_Boundaries.shp') %>% 
    st_transform(crs = 4326) %>% 
    st_centroid() %>% 
    mutate(school1 = toupper(School),
           school2 = ifelse(str_detect(school1, 'PRIMARY'), 'Elementary',
                            ifelse(str_detect(school1, 'HIGH'), 'High School',
                                   ifelse(str_detect(school1, 'DAY|CHILD|CARE|NURSERY|LITTLE|PETITE|ABC'), 'Day Care',
                                          ifelse(str_detect(school1, 'INTERMEDIATE'), 'Middle School',
                                                 ifelse(str_detect(school1, 'COLLEGE|UNI'), 'Higher Education', 'Other'))))))

cent_distances <- data.table::fread('census_centroid_dist.csv')

# Census data - First tab
# Process Census data 
census_clean_carolina <- st_read("2020_CensusData.shp", stringsAsFactors = FALSE) %>%
    st_transform(crs = 4326) %>% 
    select(1:2, # geoid and total population
           7,8, # Total males, total females 
           19:21, # Population aged 65+ 
           23:24, # Population white, black
           39, # Median household income
           47, 48) %>%# Population doing poor or struggling, Population doing okay
    mutate("Total Population" = A00001_1,
           "Total Males" = A02001_2,
           "Total Females" = A02001_3,
           "Total Population 65+" = A01001_11 + A01001_12 + A01001_13,
           "Total White Population" = A03001_2,
           "Total Black Population" = A03001_3,
           "Median Household Income" = A14006_1,
           "Total Population Doing Poor or Struggling" = B13004_4,
           "Total Population Doing OK" = B13004_5) %>% 
    select(1,14:22)

census_center_carolina <- st_centroid(census_clean_carolina)

census_center_geometry_NA_carolina <- census_center_carolina %>% st_set_geometry(NULL)

# City Council Districts Data
# Load City Council Districts Shapefile City
city_council <- st_read("City_Council_Districts.shp", stringsAsFactors = FALSE) %>% st_transform(crs = 4326)

city_council_geometry_NA <- city_council %>% st_set_geometry(NULL)

#Create merged data
ov <- st_join(x = census_center_carolina, y = city_council %>% select(Num))
summary <- ov %>% 
    filter(!is.na(Num)) %>%
    group_by(Num) %>%
    summarise('Total Population' = sum(`Total Population`),
              'Total Males' = sum(`Total Males`),
              'Total Females' = sum(`Total Females`),
              'Total Population 65+' = sum(`Total Population 65+` ),
              'Total White Population' = sum(`Total White Population`),
              'Total Black Population' = sum(`Total Black Population`),
              'Median Household Income' = mean(`Median Household Income`),
              'Total Population Doing Poor or Struggling' = sum(`Total Population Doing Poor or Struggling`),
              'Total Population Doing OK' = sum(`Total Population Doing OK`)) %>%
    mutate("% Males" = round(100*`Total Males`/(`Total Males` + `Total Females`), digits = 1),
           "% Females" = round(100*`Total Females`/(`Total Males` + `Total Females`), digits = 1),
           "% Population 65+" = round(100*`Total Population 65+`/`Total Population`, digits = 1),
           "% White Population" = round(100*`Total White Population`/`Total Population`, digits = 1),
           "% Black Population" = round(100*`Total Black Population`/`Total Population`, digits = 1),
           "% Population Doing Poor or Struggling" = round(100*`Total Population Doing Poor or Struggling`/(`Total Population Doing Poor or Struggling` + `Total Population Doing OK`), digits = 1))

summary_clean <- head(summary, -1) %>%
    select(1:2, 10, 12:17)

summary_clean_numeric <- summary_clean %>% st_set_geometry(NULL)

# Establish palette
pal_census1 <- colorFactor(palette = "PuRd", domain = NULL, na.color = NA) # for census variables - Aaron tab
pal_census2 <- colorFactor(palette = "YlGn", domain = NULL, na.color = NA) # for census variables - Ani tab
pal_gulshan <- colorNumeric(palette = "YlOrRd", domain = NULL, na.color = NA) # Gulshan tab
factpal <- colorFactor(topo.colors(5), city_council$Num) # for Carolina tab

# Define UI for application that draws a histogram
ui <- fluidPage(
    
    theme = shinytheme("darkly"),
    
    # Application title
    titlePanel("South Bend: Understanding the Needs of a Vibrant City"),
    
    tabsetPanel(
        tabPanel("Public Facilities",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "census_var1",
                                              label = "Tract Characteristic:",
                                              choices = names(census_clean %>% select(-1) %>% st_set_geometry(NULL)),
                                              selected = "Population Density (Per Sq. Mile)"),
                                  selectInput(inputId = "pub_facility",
                                              label = "Public Facility:",
                                              choices = c("All", levels(pub_facilities$POPL_TYPE)),
                                              selected = "All")),
                     mainPanel(leafletOutput(outputId = "aaron_tab")))
                 ),
        tabPanel("Abandoned Properties",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "census_var2",
                                              label = "Tract Characteristic:",
                                              choices = names(census_clean %>% select(-1) %>% st_set_geometry(NULL)),
                                              selected = "Population Density (Per Sq. Mile)"),
                                  selectInput(inputId = "lights",
                                              label = "Light Wattage:",
                                              choices = c("All", levels(light$wattage_type)),
                                              selected = "Unknown"),
                                  selectInput(inputId = "abandoned_prop",
                                              label = "Abandoned Properties:",
                                              choices = c("All", levels(prop$outcome_type)),
                                              selected = "Demolished")),
                     mainPanel(leafletOutput(outputId = "ani_tab")))
                 ),
        tabPanel("Schools",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId =  'selectSchoolLevel',
                                              label = 'Select Education Level',
                                              choices = c('All', unique(schools$school2)),
                                              selected   = 'All'),
                                  selectInput(inputId =  'selectSchoolType',
                                              label = 'Select School Type',
                                              choices = c('All', unique(schools$SchoolType)),
                                              selected = 'All'),
                                  selectInput(inputId =  'selectAgeGroup',
                                              label =  'Select population age group',
                                              choices = colnames(census_geometry_NA_gulshan)[2:7])
                                  ),
                     mainPanel(leafletOutput(outputId =  "gulshan_tab"),
                               plotOutput(outputId =  'gulshan_plot1'),
                               plotOutput(outputId = 'gulshan_plot2'))
                 )
            ),
        tabPanel("City Districts",
                 sidebarLayout(
                     sidebarPanel(selectInput(inputId = "census_variables",
                                              label = "Demographic Characteristic:",
                                              choices = names(summary_clean %>% select(-1) %>% st_set_geometry(NULL)),
                                              selected = "Total Population")),
                     mainPanel(leafletOutput(outputId = "carolina_tab"),
                               fluidRow(column(width = 12, plotOutput('carolina_plot'))))
                     
                 )
            )
        )
    )

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    # Conditional for public facility  
    pub_fac_to_display <- reactive({
        if(input$pub_facility == "All"){
            levels(pub_facilities$POPL_TYPE)
        } else {
            input$pub_facility 
        }
    })
    
    # Add reactive filter to pick census data to display - For Aaron tab
    census_subset1 <- reactive({
        census_geometry_NA[, input$census_var1]
    })
    
    # Add reactive filter to pick census data to display - For Ani tab
    census_subset2 <- reactive({
        census_geometry_NA[, input$census_var2]
    })
    
    # Add reactive filter to pick census data to display 
    census_subset_carolina <- reactive({
        summary_clean_numeric[, input$census_variables]
    })
    
    light_subset <- reactive({
        light %>% 
            filter(if('All' %in% input$lights) {TRUE} else {wattage_type %in% input$lights})
    })
    
    prop_subset <- reactive({
        prop %>% 
            filter(if('All' %in% input$abandoned_prop) {TRUE} else {outcome_type %in% input$abandoned_prop})
        
    })
    
    schoolMapData <- reactive(
        schools %>% 
            filter(if('All' %in% input$selectSchoolLevel) {TRUE} else {school2 %in% input$selectSchoolLevel}) %>% 
            filter(if('All' %in% input$selectSchoolType) {TRUE} else {SchoolType %in% input$selectSchoolType})
    )
    
    output$gulshan_plot1 <- renderPlot({
        schools %>%
            st_set_geometry(NULL) %>%
            count(school2) %>%
            mutate(school2 = as.factor(school2)) %>% 
            ggplot(aes(x = school2, y = n, fill = school2)) +
            geom_bar(stat = "identity") +
            scale_fill_brewer(palette = "Dark2") +
            theme_bw() +
            theme(legend.position = "none",
                  title = element_text(face = "bold", size = 12),
                  axis.text = element_text(face = "bold", size = 11)) + 
            xlab("School level") +
            ylab("Total") +
            ggtitle(label = "School level frequency count")
    })
    
    output$carolina_plot <- renderPlot({
        ggplot(summary_clean, aes(x = Num, y = get(input$census_variables), fill =  Num)) +
            geom_col() +
            scale_fill_brewer(palette = "Dark2") +
            xlab("City Council District") +
            ylab(input$census_variables) +
            theme_bw() +
            theme(legend.position = "none",
                  title = element_text(face = "bold", size = 12),
                  axis.text = element_text(face = "bold", size = 11)) + 
            ggtitle(label = paste0("Distribution of ", input$census_variables, " Across City Council Districts"))
    })
    
    output$gulshan_plot2 <- renderPlot({
        
        req(input$map1_shape_click) 
        
        cent_distances %>% 
            filter(census_area == input$map1_shape_click) %>% 
            arrange(distmiles) %>% 
            head(10) %>% 
            ggplot(aes(x = reorder(school, distmiles), y = distmiles)) +
            geom_bar(stat = 'identity', fill = '#00AFBB')+
            labs(title = paste('Top 10 closest schools to census area:', input$map1_shape_click)) +
            theme_bw() +
            theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
        
        
    })
    
    output$aaron_tab <- renderLeaflet({
        
        # Establish popup for census variables 
        census_clean$popup_census <- paste("GEOID: ", census_clean_numeric$GEOID, "</b><br>",
                                           input$census_var1, ": ", round(census_clean_numeric[, input$census_var1], digits = 1), "<br>")
        
        # Establish popup for pub facility 
        pub_facilities$popup_pub_fac <- paste("Public Facility: ", pub_facilities$POPL_TYPE, "</b><br>",
                                              "Name: ", pub_facilities$POPL_NAME, "<br>")
        
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addPolygons(data = census_clean, fillColor = ~pal_census1(census_subset1()), popup  = ~popup_census,
                        weight = 2,
                        opacity = 1,
                        color = "black",
                        fillOpacity = 0.7) %>% 
            addCircleMarkers(data = pub_facilities %>% filter(POPL_TYPE %in% pub_fac_to_display()), popup  = ~popup_pub_fac,
                             color = "black", stroke = 0.5, fillOpacity = 1, radius = 5) %>%
            addLegend(position = "bottomright",
                      values = census_subset1(),
                      pal = pal_census1,
                      title = input$census_var1)
    })
    
    output$ani_tab <- renderLeaflet({
        
        # Establish popup for census variables 
        census_clean$popup_census <- paste("GEOID: ", census_clean_numeric$GEOID, "</b><br>",
                                           input$census_var2, ": ", round(census_clean_numeric[, input$census_var2], digits = 1), "<br>")
        
        # Establish popup for lights
        light$popup_light_fac <- paste("Wattage: ", light$Wattage, "</b><br>",
                                       "Ownership: ", light$Ownership, "<br>")
        
        # Establish popup for abandoned properties
        prop$popup_prop_fac <- paste("Property Type: ", prop$prop_type, "</b><br>",
                                     "Code Enforcement: ", prop$Code_Enfor, "<br>")
        
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addPolygons(data = census_clean, fillColor = ~pal_census2(census_subset2()), popup  = ~popup_census,
                        weight = 2,
                        opacity = 1,
                        color = "black",
                        fillOpacity = 0.7) %>% 
            addCircleMarkers(data = light_subset(), popup  = light$popup_light_fac,
                             color = "black", stroke = 0.5, fillOpacity = 1, radius = 1.5) %>%
            addCircleMarkers(data = prop_subset(), popup  = prop$popup_prop_fac,
                             color = "blue", stroke = 0.5, fillOpacity = 1, radius = 1.5) %>%
            addLegend(position = "bottomright",
                      values = census_subset2(),
                      pal = pal_census2,
                      title = input$census_var2)
    })
    
    
    output$gulshan_tab <- renderLeaflet({
        
        # Establish popup for population
        census$popup_population <- paste("GEOID: ", census_geometry_NA_gulshan$GEOID, "</b><br>",
                                         input$selectAgeGroup, ": ", census_geometry_NA_gulshan[[input$selectAgeGroup]])
        
        # Establish popup for school
        schools$popup_school <- paste("School Name: ", schools$School, "</b><br>",
                                      "School Type: ", schools$SchoolType, "</b><br>",
                                      "School Level: ", schools$school2, "<br>")

        if(nrow(schoolMapData()) == 0) {
            census %>% 
                select(GEOID, pop = input$selectAgeGroup) %>%  
                leaflet() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(weight=2, fillOpacity = .7, color = 'black',
                            fillColor = ~pal_gulshan(pop), popup  = census$popup_population) %>% 
                addLegend(pal = pal_gulshan, values = ~pop, position =  "bottomright", title = "Population")
        } else {
            census %>% 
                select(GEOID, pop = input$selectAgeGroup) %>%  
                leaflet() %>%
                addProviderTiles(providers$CartoDB.Positron) %>%
                addPolygons(weight=2, fillOpacity = .7, color = 'black',
                            fillColor = ~pal_gulshan(pop), popup = census$popup_population) %>% 
                addCircleMarkers(data = schoolMapData(),
                                 color = "black", stroke = 0.5, fillOpacity = 1, radius = 5, popup = schools$popup_school) %>% 
                addLegend(pal = pal_gulshan, values = ~pop, position =  "bottomright", title = "Population")
            
        }
    })
    
    output$carolina_tab <- renderLeaflet({
        
        to.merge <- summary_clean_numeric %>%
            select(Num, input$census_variables)%>%
            rename(active = input$census_variables)
        
        # Establish popup for districts: 
        city_council <- city_council %>%
            left_join(y = to.merge, by = "Num")
        
        city_council$popup_districts <- paste("District: ", city_council$Num, "</b><br>",
                                              "Council: ", city_council$Council_Me, "<br>",
                                              "Email: ", city_council$Email, "<br>",
                                              input$census_variables, ":",city_council$active, "<br>")
        
        leaflet() %>% 
            addProviderTiles(providers$CartoDB.Positron) %>% 
            addPolygons(data = city_council, fillColor = topo.colors(8, alpha = NULL), popup  = city_council$popup_districts,
                        stroke = FALSE, smoothFactor = 0.2, opacity = 1,
                        color = ~factpal(Num),
                        fillOpacity = 1)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

