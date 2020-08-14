library(shiny)
library(tidyverse)
library(readxl)
library(cowplot)
library(plotly)
library(shinythemes)
library(shinycustomloader)

date.range.months = 12
ref.range.months = 3
n.ref.values = 100

############################## Westgard Rules ##############################
apply_westgard_rules <- function(df, mean, sd) {
    # input: dataframe with column 'value' and parameters mean and sd
    # ouput: dataframe with new column 'rule'
    df = df %>% mutate(m.above = 0, m.below = 0, s1.above = 0, s1.below = 0, s2.above = 0, s2.below = 0, s3.above = 0, s3.below = 0, r4 = 0)
    
    for (i in 2:nrow(df)) {
        if (df$value[i] > mean) {df$m.above[i] = df$m.above[i-1] + 1}  ### above mean
        if (df$value[i] < mean) {df$m.below[i] = df$m.below[i-1] + 1}  ### below mean
        if (df$value[i] > mean + 1*sd) {df$s1.above[i] = df$s1.above[i-1] + 1}  ### above 1s
        if (df$value[i] < mean - 1*sd) {df$s1.below[i] = df$s1.below[i-1] + 1}  ### below 1s
        if (df$value[i] > mean + 2*sd) {df$s2.above[i] = df$s2.above[i-1] + 1}  ### above 2s
        if (df$value[i] < mean - 2*sd) {df$s2.below[i] = df$s2.below[i-1] + 1}  ### below 2s
        if (df$value[i] > mean + 3*sd) {df$s3.above[i] = df$s3.above[i-1] + 1}  ### above 3s
        if (df$value[i] < mean - 3*sd) {df$s3.below[i] = df$s3.below[i-1] + 1}  ### below 3s
        if (df$s2.above[i-1] >= 1 & df$s2.below[i] >= 1) {df$r4[i] = 1} ### above 2s --> below 2s
        if (df$s2.below[i-1] >= 1 & df$s2.above[i] >= 1) {df$r4[i] = 1} ### bewlo 2s --> above 2s
    }
    
    df %>% mutate(rule = case_when(
            r4 >= 1 ~ "R-4s",  ### R-4s rule
            s3.above >= 1 ~ "1-3s", ### 1-3s rule
            s3.below >= 1 ~ "1-3s", ### 1-3s rule
            s2.above >= 2 ~ "2-2s", ### 2-2s rule
            s2.above >= 2 ~ "2-2s", ### 2-2s rule
            s2.above >= 1 ~ "1-2s", ### 1-2s alarm
            s2.above >= 1 ~ "1-2s", ### 1-2s alarm
            s1.above >= 3 ~ "3-1s", ### 3-1s alarm
            s1.above >= 3 ~ "3-1s", ### 3-1s alarm
            m.above >= 10 ~ "10x", ### 10 above mean alarm
            m.below >= 10 ~ "10x", ### 10 below mean alarm
            TRUE ~ "ok"))
    }


############################## Plot ##############################
plot_style <- list(geom_point(size = 3, shape = 16),
                   panel_border(),
                   background_grid(major = "xy", minor = "xy"),
                   theme(legend.position = "none"),
                   xlab("Date"),
                   ylab("Ct value"),
                   theme_set(theme_cowplot()),
                   theme(legend.position = "none"),
                   panel_border(),
                   background_grid(major = "xy", minor = "xy"))

plot_colors <- scale_colour_manual(values = c("ok" = "#228B22", ### grün
                               "10x"= "#FFA500", ### orange
                               "1-2s"= "#FFA500", ### orange
                               "3-1s"= "#FFA500", ### orange
                               "R-4s" = "#0000FF", ### blau
                               "2-2s" = "#FF0000", ### rot
                               "1-3s" = "#FF0000")) ### rot

plot_lines <- function(mean, sd) {
    list(geom_hline(yintercept = mean, color = "black", linetype = "dotted", size = 0.25), ### mean black
         geom_hline(yintercept = mean + 1*sd, color = "#228B22", linetype = "dotted", size = 0.25), ### 1s grün
         geom_hline(yintercept = mean - 1*sd, color = "#228B22", linetype = "dotted", size = 0.25), ### 1s grün
         geom_hline(yintercept = mean + 2*sd, color = "#FFA500", linetype = "dashed"), ### 2s orange
         geom_hline(yintercept = mean - 2*sd, color = "#FFA500", linetype = "dashed"), ### 2s orange
         geom_hline(yintercept = mean + 3*sd, color = "#FF0000"), ### 3s rot
         geom_hline(yintercept = mean - 3*sd, color = "#FF0000")) ### 3s rot
}


############################## UI ##############################
ui <- fluidPage(
    theme = shinytheme("yeti"),
    #titlePanel("IQC"),
    sidebarLayout(
        sidebarPanel(
            style = "position:fixed; width:inherit;",
            h1("IQC IMV"),
            fileInput("file", "PCR-Kontrollen", accept = "xlsx"),
            selectInput("sheet", "Sheet", c("DNA ViiA7"), selected = "DNA ViiA7"),
            uiOutput("target"),
            uiOutput("lot"),
            dateRangeInput("dateRange",
                           label = "Date range input: yyyy-mm-dd",
                           start = Sys.Date() - (date.range.months/12*365), end = Sys.Date(),
                           weekstart = 1),
            dateRangeInput("refRange",
                           label = "Reference range input: yyyy-mm-dd",
                           start = Sys.Date() - (ref.range.months/12*365), end = Sys.Date(),
                           weekstart = 1),
            numericInput("n.ref", "N reference", value = n.ref.values, min = 2, step = 10),
            width = 3
        ),

        mainPanel(
            tabsetPanel(
                tabPanel("DNA ViiA7",
                         #plotlyOutput("select_plot"),
                         withLoader(plotlyOutput("select_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("HSV_1_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("HSV_2_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("CMV_low_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("CMV_high_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("VZV_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("EBV_low_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("EBV_high_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("Parvo_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("Adeno_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("HHV6AB_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("JC_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("BK_low_plot"), type = "html", loader = "loader1"),
                         withLoader(plotlyOutput("BK_high_plot"), type = "html", loader = "loader1")),
                tabPanel("RNA ViiA7"),
                tabPanel("DNA Quant3"),
                tabPanel("RNA Quant3"),
                tabPanel("Ambion"),
                tabPanel("GIT"),
                tabPanel("Viia7 Multiplex"))
            )
        )
    )


############################## Server ##############################
server <- function(input, output) {
    
    raw_data = reactive({read_excel(input$file$datapath, sheet = input$sheet)[1:14] })
    # raw_data = reactive({read_excel("/Volumes/Diagnostic/Common/Labor/PCR-Kontrollen.xlsx", sheet = "DNA ViiA7")[1:14] }) ### direct loading
    
    clean_data = reactive({
        data = raw_data() %>%
            rename(date = 1) %>%
            filter(!date %in% c("Mittelwert", "Standardabw", "2s", "oberer GW", "unterer GW", "Lot:", "Datum")) %>%
            mutate(lot = ifelse(as.numeric(`HSV 2`) > 40000, `HSV 2`, NA)) %>%
            mutate(lot = ifelse(is.na(lot), lag(lot), lot))
        
        for (n in 2:nrow(data)) {
            if (is.na(data$lot[n]) | data$lot[n] == "") {
                data$lot[n] = data$lot[n-1]
            }
        }
        return(data)
    })
    
    tidy_data = reactive({
        clean_data() %>%
        filter(!(is.na(date))) %>%
        gather(key = "target", value = "value", -date, -lot) %>%
        mutate(date = as.Date(as.numeric(date), origin = "1899-12-30")) %>%
        mutate(lot = as.Date(as.numeric(lot), origin = "1899-12-30")) %>%
        mutate(value = as.numeric(value)) %>%
        filter(!(is.na(value))) %>%
        filter(value <= 50)
        })
    
    targets = reactive({tidy_data() %>% pull(target) %>% unique()})
    output$target = renderUI({
        req(input$file)
        selectInput("target", "Target", targets(), selectize = FALSE)
        })
    
    lots = reactive({tidy_data() %>% pull(lot) %>% unique()})
    output$lot = renderUI({
        req(input$file)
        selectInput("lot", "Lot", lots(), multiple = TRUE, selected = lots(), selectize = FALSE)
        })
    
    plot_data = reactive({
        tidy_data() %>%
            filter(date > input$dateRange[1]) %>%
            filter(date < input$dateRange[2]) %>%
            filter(as.character(lot) %in% input$lot)
    })
    
    
    output$select_plot = renderPlotly({
        req(input$file)
        targetn = input$target
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$HSV_1_plot = renderPlotly({
        targetn = "HSV 1"
        req(input$file)
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$HSV_2_plot = renderPlotly({
        targetn = "HSV 2"
        req(input$file)
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$CMV_low_plot = renderPlotly({
        req(input$file)
        targetn = "CMV low"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$CMV_high_plot = renderPlotly({
        req(input$file)
        targetn = "CMV high"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$EBV_low_plot = renderPlotly({
        req(input$file)
        targetn = "EBV low"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$EBV_high_plot = renderPlotly({
        req(input$file)
        targetn = "EBV high"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$VZV_plot = renderPlotly({
        req(input$file)
        targetn = "VZV"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$Parvo_plot = renderPlotly({
        req(input$file)
        targetn = "Parvo"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$Adeno_plot = renderPlotly({
        req(input$file)
        targetn = "Adeno"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$HHV6AB_plot = renderPlotly({
        req(input$file)
        targetn = "HHV6A+B"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$JC_plot = renderPlotly({
        req(input$file)
        targetn = "JC"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$BK_low_plot = renderPlotly({
        req(input$file)
        targetn = "BK low"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
    
    output$BK_high_plot = renderPlotly({
        req(input$file)
        targetn = "BK high"
        dfn = plot_data() %>% filter(target == targetn)
        mean = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% base::mean()
        sd = dfn %>% filter(date > input$refRange[1]) %>% filter(date < input$refRange[2]) %>% pull(value) %>% head(input$n.ref) %>% stats::sd()
        pn = dfn %>% apply_westgard_rules(mean, sd) %>% ggplot(aes(x = date, y = value, color = rule)) +
            ggtitle(paste0(targetn," (n.data = ", nrow(dfn),"; n.ref = ", input$n.ref, ")")) + plot_lines(mean, sd) + plot_colors + plot_style
        ggplotly(pn)
    })
}

############################## Run the application ##############################
shinyApp(ui = ui, server = server)
