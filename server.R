library(readxl)
library(shiny)
library(ggplot2)
library(Dict)
library(stringr)
library(dplyr)
library(tidyselect)
library(tidyverse)
library(kableExtra)
library(rtf)
library(knitr)
library(gt)
library(gtsummary)
library(heatmaply)

ctcae <-
  server <- function(input, output, session) {
    # enable column and row selection
    info <- eventReactive(input$choice, {
      inFile <- input$file1
      req(inFile)
      df <- read.csv(
        input$file1$datapath,
        header = input$header,
        sep = input$sep,
        blank.lines.skip = TRUE
      )
      #names(df) <- str_replace_all(names(df), c(" " = "_"))
      # change all PT and SOC to lowercase
      df$PT <- tolower(df$PT)
      df$SOC <- tolower(df$SOC)
      # get list of unique items to filter
      subject_list <- c(unique(df$Subject.ID))
      site_list <- c(unique(df$Site))
      pt_list <- c(unique(df$PT))
      soc_list <- c(unique(df$SOC))
      #c_vars <- names(df)
      r_vars <- row.names(df)
      #updateSelectInput(session, "columns",
      #"Columns to show", choices = c_vars)
      updateSelectInput(session,
                        "rows",
                        "Rows to show",
                        choices = r_vars,
                        selected = r_vars)
      updatePickerInput(session,
                        "subject",
                        "Subject",
                        choices = subject_list,
                        selected = subject_list)
      updatePickerInput(session,
                        "site",
                        "Site",
                        choices = site_list,
                        selected = site_list)
      updatePickerInput(
        session,
        "preferred_term",
        "Preferred Term",
        choices = pt_list,
        selected = pt_list
      )
      updatePickerInput(
        session,
        "organ_class",
        "System Organ Class",
        choices = soc_list,
        selected = soc_list
      )
      return(df)
    })
    output$contents <- DT::renderDataTable({
      data <- info()
      data <- subset(data[input$rows, ])#, select = input$columns)
      # filtering according to user input
      data <- data[which(
        data$Subject.ID %in% input$subject &
          data$Site %in% input$site &
          data$PT %in% input$preferred_term &
          data$SOC %in% input$organ_class
      ),]
      return(data)
    })
    # pie chart for overall soc percentage
    output$pie_soc <- renderPlot({
      data <- info()
      soc_p <- data.frame(table(data$SOC))
      names(soc_p)[1] = "SOC"
      ggplot(soc_p, aes(x = "", y = Freq, fill = SOC)) +
        geom_bar(stat = "identity", width = 1) +
        coord_polar("y", start = 0) +
        theme(legend.position = "bottom")
    })
    
    output$summary_table <- function() {
      data1 <- info()
      data1 <-
        df %>% select(any_of(c(
          "Subject.ID", "SOC", "PT", "Treatment", "Grade"
        )))
      any_ae <- data1 %>%
        group_by(Subject.ID, Treatment) %>%
        arrange(Subject.ID, Treatment,-Grade) %>%
        slice_head(n = 1)
      any_soc <- data1 %>%
        group_by(Subject.ID, Treatment, SOC) %>%
        arrange(Subject.ID, Treatment, SOC,-Grade) %>%
        slice_head(n = 1)
      any_pt <- data1 %>%
        group_by(Subject.ID, Treatment, SOC, PT) %>%
        arrange(Subject.ID, Treatment, SOC, PT,-Grade) %>%
        slice_head(n = 1)
      
      any_ae1 <- any_ae %>%
        group_by(Treatment) %>%
        summarise(n = n(), .groups = "keep")
      any_ae1 <-
        any_ae1 %>% mutate(pct = n * 100 / sum(as.numeric(any_ae1$n)),
                           txt = "Total Subjects with an Event")
      
      any_soc1 <- any_soc %>%
        group_by(SOC, Treatment) %>%
        summarise(n = n(), .groups = "keep")
      any_soc1 <- cbind(any_soc1[, 1], any_soc1)
      any_soc1 <-
        any_soc1 %>% mutate(pct = n * 100 / sum(as.numeric(any_soc1$n)))
      colnames(any_soc1) <-
        c("Systemic_Organ_Class", "SOC", "Treatment", "n", "pct")
      
      any_pt1 <- any_pt %>%
        group_by(PT, SOC, Treatment) %>%
        summarise(n = n(), .groups = "keep")
      any_pt1 <-
        any_pt1 %>% mutate(pct = n * 100 / sum(as.numeric(any_pt1$n)),
                           Systemic_Organ_Class = " ")
      
      all_ae <- rbind(any_ae1, any_soc1, any_pt1)
      all_ae1 <-
        all_ae[order(all_ae$Treatment, all_ae$SOC, all_ae$PT),]
      all_ae2 <-
        all_ae1 %>% mutate(var1 = paste(n, "(", format(round(pct, digits = 2), nsmall = 2), ")"))
      t_ae <-
        all_ae2 %>% ungroup() %>% select(-n,-pct) %>% spread(Treatment, var1)
      clean_ae <-
        t_ae[order(t_ae$SOC, t_ae$Systemic_Organ_Class, t_ae$PT, decreasing = TRUE),] %>% select(-SOC)
      #t_ae1 <- t_ae[order(all_ae$SOC, all_ae$PT), ]
      clean_ae[is.na(clean_ae)] = ""
      tt <-
        clean_ae[clean_ae$txt == "Total Subjects with an Event",]
      clean_ae <-
        clean_ae[-grep("Total Subjects with an Event", clean_ae$txt),]
      clean_ae <- rbind(tt, clean_ae)
      #clean_ae <- clean_ae[order(clean_ae$txt), ]
      k_table <-
        clean_ae %>% knitr::kable("html", caption = "<center>Adverse Event Summary</center>") %>% kable_styling("striped", full_width = F)
      return(k_table)
    }
    
    output$heatmap <- renderPlot({
      data <- info()
      data <- df %>% select(c("SOC", "Treatment")) %>% 
        group_by(SOC, Treatment) %>% 
        summarise(n=n(), .groups = "keep")
      
      data2 <- data %>% mutate(pct=n*100/sum(as.numeric(data$n))) %>% select(- pct) %>% spread(Treatment, n)
      rownames(data2) <- data2$SOC
      data2 <- subset(data2, select = -c(SOC))
      data2[is.na(data2)] = 0
      m <- as.matrix(sapply(data2, as.numeric))
      rownames(m) <- unique(data$SOC)
      h_map <- heatmap(m)
      return(h_map)
    })
    output$soc_tr <- renderTable({
      data <- info()
      data <- df %>% select(c("SOC", "Treatment")) %>% 
        group_by(SOC, Treatment) %>% 
        summarise(n=n(), .groups = "keep")
      data2 <- data %>% mutate(pct=n*100/sum(as.numeric(data$n))) %>% 
        mutate(var1 = paste(n, "(", format(round(pct, digits = 2), nsmall = 2), ")")) %>%
        select(- pct) %>% 
        select(- n) %>%
        spread(Treatment, var1)
      rownames(data2) <- data2$SOC
      
      soc_only <- subset(data, select = -c(Treatment))
      soc_only <- soc_only %>% 
        summarise(total = sum(n), .groups = "keep")
      soc_only <- soc_only %>% 
        mutate(pct=total*100/sum(soc_only$total)) %>%
        mutate(Total = paste(total, "(", format(round(pct, digits = 2), nsmall = 2), ")")) %>%
        select(-c(total, pct))
      data2 <- cbind(data2, soc_only[2])
      return(data2)
    })
   # output$summary_table <- render_gt({
   #   data <- info()
   #   data <- data %>% select(any_of(c("SOC", "PT", "Treatment")))
   #   # data3 <-data %>%
   #   #   mutate(SOC = paste("SOC", SOC)) %>%
   #   #   tbl_strata(
   #   #     strata = SOC,
   #   #     .tbl_fun =
   #   #       ~ .x %>%
   #   #       tbl_summary(by = Treatment, missing = "no") %>%
   #   #       add_n(),
   #   #     .header = "**{strata}**, N = {n}"
   #   #   ) %>% as_gt
   #   data3 <- data %>% 
   #     tbl_summary(by = Treatment, missing = "no") %>%
   #     as_gt()
   # 
   #   # # sorting the events by categories and severity
   #   # data <- data[order(data$SOC, data$PT, -data$Grade), ]
   #   # # ae <- data %>%
   #   # #   group_by(Subject.ID, Treatment, SOC) %>%
   #   # #   arrange(Subject.ID, Treatment, SOC, -Grade) %>%
   #   # #   slice_head(1)
   #   # 
   #   # data <- data[order(data$Treatment, data$SOC, data$PT, -data$Grade), ]
   #   # N_pat <- n_distinct(data$Subject.ID)
   #   # data2 <- data %>% 
   #   #   group_by(Treatment, SOC, PT, Grade, Subject.ID) %>%
   #   #   summarise(n_ae = n()) %>%
   #   #   group_by(Treatment, SOC, PT, Grade) %>%
   #   #   summarise(n_pat = n(), n_ae = sum(n_ae)) %>%
   #   #   mutate(pct = round(n_ae/N_pat*100, digits = 1),
   #   #          txt = paste0("[", n_ae,"] ", n_pat, " (", pct, "%)"),
   #   #          arm = paste0("Group ", Treatment)) 
   # 
   #   # header_ae <- data %>%
   #   #   group_by(Treatment, Subject.ID) %>%
   #   #   summarise(n=n()) %>%
   #   #   group_by(Treatment) %>%
   #   #   summarise(n = n()) %>%
   #   #   ungroup() %>%
   #   #   mutate(armtxt = n_distinct(Treatment)) %>%
   #   #   mutate(txt = paste0(armtxt, " (N=", n, ")")) %>%
   #   #   select(txt) %>%
   #   #   deframe
   #   # 
   #   # ae_table_fns <- function(data, filtervar){
   #   #   
   #   #   filtervar = ensym(filtervar)
   #   #   
   #   #   data %>%
   #   #     group_by(Treatment) %>%
   #   #     mutate(N_pat = n_distinct(Subject.ID)) %>%
   #   #     filter(!!filtervar == 1)  %>%
   #   #     group_by(Subject.ID, Treatment, N_pat, SOC, PT) %>%
   #   #     summarise(n_ae = n()) %>%
   #   #     filter(!is.na(PT)) %>%
   #   #     group_by(Treatment, N_pat, SOC, PT) %>%
   #   #     summarise(n_pat = n(),
   #   #               n_ae = sum(n_ae)) %>%
   #   #     mutate(pct = round(n_pat/N_pat*100,digits = 1),
   #   #            txt = paste0("[", n_ae,"] ", n_pat, " (", pct, "%)"),
   #   #            arm = paste0("arm", Treatment)) %>%
   #   #     ungroup %>% select(arm, SOC, PT, txt) %>%
   #   #     pivot_wider(values_from = txt, names_from = arm) %>%
   #   #     mutate_at(vars(starts_with("arm")), ~if_else(is.na(.), "", .)) %>%
   #   #     arrange(SOC, PT) %>%  group_by(SOC2 = SOC) %>%
   #   #     mutate(SOC = if_else(row_number() != 1, "", SOC)) %>% ungroup() %>% select(-SOC2)
   #   # }
   #   # data3 <- adae %>%
   #   #   bind_rows(adae, .id="added") %>%
   #   #   filter(!is.na(PT)) %>% 
   #   #   mutate(PT = if_else(added == 2, "#Total", PT)) %>%
   #   #   mutate(all = 1) %>%
   #   #   ae_table_fns("all") %>%
   #   #   table(col.names = c("System Organ Class", "Preferred Term", header_ae),
   #   #                caPTion = " Adverse Events by System Organ Class and Preferred term*",
   #   #                booktabs = TRUE,
   #   #                longtable = TRUE)
   #   return(data3)
   #   
   # })
}




