#server section for "Patient Data Analyser" application
server <- function(input, output,session) {
  
  #observe event of demographic show visual toggle button 
  #to show and hide the demographic visual section
  observeEvent(input$demographic_show_visuals_togg,{
    if(isTRUE(input$demographic_show_visuals_togg)){
      shinyjs::show("demographic_dta_visuals_div")
    }else{
      shinyjs::hide("demographic_dta_visuals_div")
    }
  })
  
  #observe event of adverse event show visual toggle button 
  #to show and hide the adverse event visual section
  observeEvent(input$adv_event_show_visuals_togg,{
    if(isTRUE(input$adv_event_show_visuals_togg)){
      shinyjs::show("adv_event_dta_visuals_div")
    }else{
      shinyjs::hide("adv_event_dta_visuals_div")
    }
  })
  
  #render the rmarkdown file of demographic insights
  output$demographic_insights_ui <- renderUI({
    tagList(
      includeMarkdown("markdown_files/demographic_insights.md")
    )
  })
  
  #render the rmarkdown file of adverse event insights
  output$adverse_event_insights_ui <- renderUI({
    tagList(
      includeMarkdown("markdown_files/adverse_event_insights.md")
    )
  })
  
  #calling the server function of data preview module to show the demographic data
  DataPreviewServer("demographic_data_preview",dataset = demographic_dataset)
  #calling the server function of data preview module to show the adverse event data
  DataPreviewServer("adverse_event_data_preview",dataset = adverse_event_dataset)
  
  #generating the patient distribution bar plot for demographic data
  output$patient_distribution_bar <- renderPlot({
    #transforming data into desirable format
    plot_data <- demographic_dataset %>% 
      dplyr::group_by(!!sym(input$demographic_dim_toggle)) %>% 
      dplyr::summarise(FREQUENCY = n(),.groups = "drop") %>% 
      data.frame()
    #generating plot using ggplot2
    p <- ggplot(plot_data, aes(x = reorder(.data[[input$demographic_dim_toggle]],desc(FREQUENCY)),
                               y = FREQUENCY,
                               fill = reorder(.data[[input$demographic_dim_toggle]],desc(FREQUENCY)))) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = FREQUENCY),vjust = -0.5, colour = "black",size = 5) +
      scale_fill_manual(values = c("#3b5998","#8b9dc3","#dfe3ee","#f7f7f7")) +
      labs(title = "Patient Distribution", x = input$demographic_dim_toggle, y = "NUMBER OF PATIENTS") +
      guides(fill=guide_legend(title= paste0(input$demographic_dim_toggle))) +
      ylim(0,350) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "plain"),
            legend.position = "bottom",
            plot.background = element_rect(colour = "#8b9dc3", fill=NA, linewidth=1,linetype = "dotted"),
            plot.margin = unit(c(1,1,1,1), "cm"))
    # ggsave(plot = p, 
    #        filename = paste0("markdown_files/graphs/patient_distribution_",input$demographic_dim_toggle,".png"),
    #        width = 4.5, height = 2.5, scale = 1.5,bg = "white")
    p
  })
  
  #generating the age distribution box plot for demographic data
  output$age_distribution_boxplot <- renderPlot({
    #generating plot using ggplot2
    p <- ggplot(demographic_dataset,
                aes(x = reorder(.data[[input$demographic_dim_toggle]],AGE), 
                    y = AGE,
                    fill = reorder(.data[[input$demographic_dim_toggle]],AGE))) +
      geom_boxplot(outlier.color = "#ffa500",alpha = 0.8) +
      scale_fill_manual(values = c("#3b5998","#8b9dc3","#dfe3ee","#f7f7f7")) +
      geom_jitter(shape=1, position=position_jitter(0.2),alpha = 0.6,color = "blue")+
      labs(title = "Age Distribution",x = paste0(input$demographic_dim_toggle)) +
      guides(fill=guide_legend(title= paste0(input$demographic_dim_toggle))) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "plain"),
            legend.position = "bottom",
            plot.background = element_rect(colour = "#8b9dc3", fill=NA, linewidth=1,linetype = "dotted"),
            plot.margin = unit(c(1,1,1,1), "cm"))
    # ggsave(plot = p, 
    #        filename = paste0("markdown_files/graphs/age_distribution_",input$demographic_dim_toggle,".png"), 
    #        width = 4.5, height = 2.5, scale = 1.5,bg = "white")
    p
  })
  
  #generating the age groups ratio bar plot for demographic data
  output$age_groups_ratio_barplot <- renderPlot({
    #transforming data into desirable format
    plot_data <- demographic_dataset %>% 
      dplyr::group_by(!!sym(input$demographic_dim_toggle),AGEGROUPS) %>% 
      dplyr::summarise(FREQUENCY = n(),.groups = "drop") %>% 
      dplyr::group_by(!!sym(input$demographic_dim_toggle)) %>% 
      dplyr::mutate(PERCENTAGE = round((FREQUENCY / sum(FREQUENCY)) * 100,2)) %>% 
      data.frame()
    #generating plot using ggplot2
    p <- ggplot(plot_data, mapping = aes(fill= AGEGROUPS, 
                                         y= PERCENTAGE,
                                         x=.data[[input$demographic_dim_toggle]])) + 
      geom_bar(position="stack", stat="identity") +
      geom_text(mapping = aes(label = paste0(PERCENTAGE,"%")), position = 'stack',vjust = 1, size = 5,color="white") +
      scale_fill_manual(values = c("#f7f7f7","#dfe3ee","#8b9dc3","#3b5998")) +
      labs(title = "Age Group Ratio") +
      ylim(0,120) +
      theme_light() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "plain"),
            legend.position = "bottom",
            plot.background = element_rect(colour = "#8b9dc3", fill=NA, linewidth=1,linetype = "dotted"),
            plot.margin = unit(c(1,1,1,1), "cm"))
    # ggsave(plot = p,
    #        filename = paste0("markdown_files/graphs/age_group_ratio_",input$demographic_dim_toggle,".png"),
    #        width = 4.5, height = 2.5, scale = 1.5,bg = "white")
    p
  })
  
  #generating the age distribution density plot for adverse event data
  output$age_distribution_density_plot <- renderPlot({
    #generating plot using ggplot2 and ggridges
    p <- ggplot(adverse_event_dataset, aes(x = AGE, 
                                           y = LIFETHREATHENINGEVENT, 
                                           fill = LIFETHREATHENINGEVENT)) +
      ggridges::geom_density_ridges_gradient(rel_min_height = 0.01) +
      scale_fill_manual(values= c("#3b5998","#8b9dc3")) +
      labs(title = "Age Distribution Based On Life Threathening Event Flag",y = "LIFE THREATHENING EVENT FAG") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "plain"),
            legend.position = "bottom",
            plot.background = element_rect(colour = "#8b9dc3", fill=NA, linewidth=1,linetype = "dotted"),
            plot.margin = unit(c(1,1,1,1), "cm"))
    # ggsave(plot = p, 
    #        filename = paste0("markdown_files/graphs/age_distribution_flag.png"), 
    #        width = 4.5, height = 2.5, scale = 1.5,bg = "white")
    p
    
  })
  
  #generating the top 5 severe adverse event bar plot for adverse event data
  output$top5_severe_adv_evt_barplot <- renderPlot({
    #transforming data into desirable format
    plot_data <- adverse_event_dataset %>% 
      dplyr::filter(SEVERITY == "SEVERE") %>% 
      dplyr::group_by(ADVERSEEVENTTERM) %>% 
      dplyr::summarise(FREQUENCY = n(),.groups = "drop") %>% 
      arrange(desc(FREQUENCY)) %>% data.frame()
    plot_data <- plot_data[1:5,]
    #generating plot using ggplot2
    p <-ggplot(plot_data, mapping = aes(x = reorder(ADVERSEEVENTTERM,FREQUENCY), 
                                        y = FREQUENCY, 
                                        fill = factor(FREQUENCY,levels = c("4","3","2")))) +
      geom_bar(stat = "identity") + 
      geom_text(mapping = aes(label = FREQUENCY),vjust = 0.3 , hjust = -0.2,  size = 3) + 
      coord_flip() + 
      guides(fill=guide_legend(title= paste0("FREQUENCY"))) +
      scale_fill_manual(values= c("#3b5998","#8b9dc3","#dfe3ee")) +
      labs(title = "Top Five Severe Adverse Events",x = "ADVERSE EVENT") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "plain"),
            legend.position = "bottom",
            plot.background = element_rect(colour = "#8b9dc3", fill=NA, linewidth=1,linetype = "dotted"),
            plot.margin = unit(c(1,1,1,1), "cm"))
    # ggsave(plot = p, 
    #        filename = paste0("markdown_files/graphs/top5_adv_events.png"), 
    #        width = 4.5, height = 2.5, scale = 1.5,bg = "white")
    p
    
  })
  
  #generating the severity sex ratio bar plot for adverse event data
  output$severity_sex_ratio_barplot <- renderPlot({
    #transforming data into desirable format
    plot_data <- adverse_event_dataset %>% 
      dplyr::filter(SEVERITY %in% c("MODERATE","SEVERE")) %>% 
      dplyr::group_by(SEX,SEVERITY) %>% 
      dplyr::summarise(FREQUENCY = n(),.groups = "drop") %>% 
      dplyr::mutate(PERCENTAGE = round((FREQUENCY / sum(FREQUENCY)) * 100,2)) %>% 
      data.frame()
    #generating plot using ggplot2
    p <- ggplot(data = plot_data, mapping = aes(x = SEVERITY, y = PERCENTAGE, fill = SEX)) + 
      geom_bar(stat = "identity",color = "black", width = 0.7, position = position_dodge(0.9),
               linetype = "blank" ) + 
      geom_text(mapping = aes(label = paste0(PERCENTAGE,"% (",FREQUENCY,")")), position = position_dodge(width=1), 
                vjust = -0.5, hjust=0.5, size = 3.5) + 
      scale_fill_manual(values = c("#3b5998","#8b9dc3")) +
      labs(title = "Moderate and Severe Adverse Event Ratio By Sex",y = "PERCENTAGE") +
      theme_classic() +
      theme(plot.title = element_text(hjust = 0.5, size = 14, color = "black", face = "plain"),
            legend.position = "bottom",
            plot.background = element_rect(colour = "#8b9dc3", fill=NA, linewidth=1,linetype = "dotted"),
            plot.margin = unit(c(1,1,1,1), "cm"))
    # ggsave(plot = p, 
    #        filename = paste0("markdown_files/graphs/adv_events_ratio.png"), 
    #        width = 4.5, height = 2.5, scale = 1.5,bg = "white")
    p
  })
  
  #download handler to generate the word file for demographic visualization report
  output$demographic_report_download <- downloadHandler(
    filename = paste0("Demographic - Data Visualizaton Report ",Sys.Date(),".docx"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("markdown_files", "gemographic_visuals.Rmd", fsep="/")
      file.copy("gemographic_visuals.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  #download handler to generate the word file for adverse event visualization report
  output$adv_event_report_download <- downloadHandler(
    filename = paste0("Adverse Event - Data Visualizaton Report ",Sys.Date(),".docx"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path("markdown_files", "adverse_event_visuals.Rmd", fsep="/")
      file.copy("adverse_event_visuals.Rmd", tempReport, overwrite = TRUE)
      rmarkdown::render(tempReport, output_file = file,
                        envir = new.env(parent = globalenv())
      )
    }
  )
}