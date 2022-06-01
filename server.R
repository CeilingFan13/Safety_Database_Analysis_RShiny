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
library(epitools)
library(lubridate)
library(grid)
library(shinyscreenshot)

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
         # unify date format, may be a point of weakness
         df$Onset.Date <- mdy(df$Onset.Date)
         df$AE.End.Date <- mdy(df$AE.End.Date)
         df$First.dose.date <- mdy(df$First.dose.date)
         df$previous.dose.date <- mdy(df$previous.dose.date)
         #names(df) <- str_replace_all(names(df), c(" " = "_"))
         # change all PT and SOC to lowercase
         df$PT <- tolower(df$PT)
         df$SOC <- tolower(df$SOC)
         # get list of unique items to filter
         subject_list <- c(unique(df$Subject.ID))
         site_list <- c(unique(df$Site))
         pt_list <- c(unique(df$PT))
         soc_list <- c(unique(df$SOC))
         treatment_list <- c(unique(df$Treatment)) %>% sort()
         
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
         updatePickerInput(session,
                           "treatment1",
                           "Control Group",
                           choices = treatment_list)
         updatePickerInput(session,
                           "treatment2",
                           "Treatment Group",
                           choices = treatment_list)
         
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
      filtered <- eventReactive (input$confirm, {
         df <- info()
         data1 <- df %>%
            select(Subject.ID, SOC, PT, Treatment) %>%
            distinct() %>%
            group_by(SOC, PT, Treatment) %>%
            summarise(n = n(), .groups = "keep")
         
         data2 <-
            data1 %>% mutate(pct = n * 100 / sum(as.numeric(data1$n))) %>%
            mutate(var1 = paste(n, "(", 
                                format(round(pct, digits = 2), 
                                       nsmall = 2), ")")) %>%
            mutate(cat = "Incidence Rate (%)")
         
         data2 <-
            data2[data2$Treatment %in% c(input$treatment1, input$treatment2),]
         return(data2)
      })
      #-------------------------------------------------------------------------
      # reactive for treatment group 2 on tab that performs wald test
      observeEvent(input$treatment1, {
         data1 <- info()
         updatePickerInput(
            session = session,
            inputId = "treatment2",
            choices = sort(unique(subset(data1$Treatment, data1$Treatment != input$treatment1)))
         )
      })
      observeEvent(input$report, {
         screenshot()
      })
    
      # dropdown input from page "EAIR"
      # observeEvent(input$treatment1, {
      #    data <- info()
      #    updatePickerInput(
      #       session = session,
      #       inputId = "interest_ae",
      #       choices = unique(data[data$Treatment == input$treatment1, "PT"])
      #    )
      # }, ignoreInit = TRUE)
      # 
      # observeEvent(input$treatment2, {
      #    data <- info()
      #    updatePickerInput(
      #       session = session,
      #       inputId = "interest_ae",
      #       choices = unique(data[data$Treatment == input$treatment2, "PT"])
      #    )
      # }, ignoreInit = TRUE)
      #-------------------------------------------------------------------------
      output$contents <- DT::renderDataTable({
         data <- info()
         data <- subset(data[input$rows,])#, select = input$columns)
         # filtering according to user input
         data <- data[which(
            data$Subject.ID %in% input$subject &
               data$Site %in% input$site &
               data$PT %in% input$preferred_term &
               data$SOC %in% input$organ_class
         ), ]
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
         data <- info()
         data1 <-
            data %>% select(any_of(c(
               "Subject.ID", "SOC", "PT", "Treatment", "Grade"
            )))
         any_ae <- data1 %>%
            group_by(Subject.ID, Treatment) %>%
            arrange(Subject.ID, Treatment, -Grade) %>%
            slice_head(n = 1)
         any_soc <- data1 %>%
            group_by(Subject.ID, Treatment, SOC) %>%
            arrange(Subject.ID, Treatment, SOC, -Grade) %>%
            slice_head(n = 1)
         any_pt <- data1 %>%
            group_by(Subject.ID, Treatment, SOC, PT) %>%
            arrange(Subject.ID, Treatment, SOC, PT, -Grade) %>%
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
            all_ae[order(all_ae$Treatment, all_ae$SOC, all_ae$PT), ]
         all_ae2 <-
            all_ae1 %>% mutate(var1 = paste(n, "(", format(round(pct, digits = 2), nsmall = 2), ")"))
         t_ae <-
            all_ae2 %>% ungroup() %>% select(-n, -pct) %>% spread(Treatment, var1)
         clean_ae <-
            t_ae[order(t_ae$SOC,
                       t_ae$Systemic_Organ_Class,
                       t_ae$PT,
                       decreasing = TRUE), ] %>% select(-SOC)
         #t_ae1 <- t_ae[order(all_ae$SOC, all_ae$PT), ]
         clean_ae[is.na(clean_ae)] = ""
         tt <-
            clean_ae[clean_ae$txt == "Total Subjects with an Event", ]
         clean_ae <-
            clean_ae[-grep("Total Subjects with an Event", clean_ae$txt), ]
         clean_ae <- rbind(tt, clean_ae)
         #clean_ae <- clean_ae[order(clean_ae$txt), ]
         k_table <-
            clean_ae %>% knitr::kable("html", caption = "<center>Adverse Event Summary</center>") %>% kable_styling("striped", full_width = F)
         return(k_table)
      }
      
      output$heatmap <- renderPlot({
         data0 <- info()
         data <- data0 %>% select(c("SOC", "Treatment")) %>%
            group_by(SOC, Treatment) %>%
            summarise(n = n(), .groups = "keep")
         
         data2 <-
            data %>% mutate(pct = n * 100 / sum(as.numeric(data$n))) %>% select(-pct) %>% spread(Treatment, n)
         rownames(data2) <- data2$SOC
         data2 <- subset(data2, select = -c(SOC))
         data2[is.na(data2)] = 0
         m <- as.matrix(sapply(data2, as.numeric))
         rownames(m) <- unique(data$SOC)
         h_map <- heatmap(m)
         return(h_map)
      })
      output$soc_tr <- renderTable({
         data0 <- info()
         data <- data0 %>% select(c("SOC", "Treatment")) %>%
            group_by(SOC, Treatment) %>%
            summarise(n = n(), .groups = "keep")
         data2 <-
            data %>% mutate(pct = n * 100 / sum(as.numeric(data$n))) %>%
            mutate(var1 = paste(n, "(", format(round(pct, digits = 2), nsmall = 2), ")")) %>%
            select(-pct) %>%
            select(-n) %>%
            spread(Treatment, var1)
         rownames(data2) <- data2$SOC
         
         soc_only <- subset(data, select = -c(Treatment))
         soc_only <- soc_only %>%
            summarise(total = sum(n), .groups = "keep")
         soc_only <- soc_only %>%
            mutate(pct = total * 100 / sum(soc_only$total)) %>%
            mutate(Total = paste(total, "(", format(round(pct, digits = 2), nsmall = 2), ")")) %>%
            select(-c(total, pct))
         data2 <- cbind(data2, soc_only[2])
         return(data2)
      })
      
      output$eair_incidence <- renderPlot({
         df <- info()
         time <- df %>% 
            filter(Treatment %in% c(input$treatment1, input$treatment2)) %>%
            select(Subject.ID, Treatment, PT, Onset.Date, First.dose.date, previous.dose.date) %>%
            mutate(duration1 = as.numeric((difftime(Onset.Date, First.dose.date, units = "days")) + 1) / 365.25) %>%
            mutate(duration2 = as.numeric((difftime(previous.dose.date, First.dose.date, units = "days")) + 1 + input$effect_days) / 365.25) %>%
            select(Treatment, PT, duration1, duration2)
            
         data1 <- df %>%
            select(Subject.ID, SOC, PT, Treatment) %>%
            distinct() %>%
            group_by(SOC, PT, Treatment) %>%
            summarise(n = n(), .groups = "keep") %>%
            filter(Treatment %in% c(input$treatment1, input$treatment2))
         person_time <- c()
         for (i in 1:length(data1$PT)) {
            trt <- data1$Treatment[i]
            ae <- data1$PT[i]
            yes_ae0 <- subset(time, time$PT == ae)
            no_ae0 <- subset(time, time$PT != ae)
            yes_ae1 <- subset(yes_ae0, yes_ae0$Treatment == trt)
            no_ae1 <- subset(no_ae0, no_ae0$Treatment == trt)
            person_time[i] <- data1$n[i] * 100 / (sum(yes_ae1$duration1)+sum(no_ae1$duration2))
            
         }
         
         data2 <-data1 %>% 
            cbind(person_time) %>%
            mutate(cat = "Incidence Rate (%)")
         colnames(data2[5]) <- "pct"
         
         p <-
            ggplot(data2, aes(
               x = unlist(data2[, 5]),
               y = PT,
               color = as.factor(Treatment)
            )) +
            geom_point(size = 3, position = position_jitter(h = 0.1, w = 0.1)) +
            facet_grid(
               rows = vars(SOC),
               cols = vars(cat),
               scales = "free",
               space = "free"
            ) +
            theme(legend.position = "bottom") +
            theme(strip.text.y = element_text(angle = 90, size = 8),
                  axis.title.y = element_blank()) +
            theme(axis.title.x = element_blank())
         
         output$eair_wald <- renderPlot({
            data5 <- data2
            # run through the AEs in treatment groups and return wald test with CI
            wald_table <- c()
            soc_list <- c()
            for (n in unique(data5$PT)) {
               data6 <- data5
               for (x in 1:length(data6$PT)) {
                  if (data6$PT[x] != n) {
                     data6$PT[x] <- 0
                  }
                  if (data6$Treatment[x] == input$treatment1){
                     data6$Treatment[x] <- 0
                  }
                  if (data6$Treatment[x] == input$treatment2){
                     data6$Treatment[x] <- 1
                  }
                 
               }
               # https://sphweb.bumc.bu.edu/otlt/mph-modules/ph717-quantcore/r-for-ph717/R-for-PH71714.html
               wald_table <- 
                  rbind(wald_table,
                        riskratio.wald(table(
                           data6$PT, 
                           data6$Treatment)
                        )$measure[2,])
               soc_list <-
                  append(soc_list, unique(data5[data5$PT == n,]$SOC))
            }
            rownames(wald_table) <- unique(data5$PT)
            wald_table1 <-
               as.data.frame(wald_table) %>% mutate(cat = "Risk Ratio with 95% CI")
            wald_table1$soc <- soc_list
            wald_table1[is.na(wald_table1)] = 0
            p_ci <-
               ggplot(wald_table1, 
                      aes(x = estimate,
                          y = rownames(wald_table1),)) +
               geom_point(
                  size = 3) +
               geom_text(data = wald_table1, 
                         aes(label = round(estimate, digits = 2),
                             group = x), 
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, 
                         hjust = -0.3) +
               facet_grid(
                  rows = vars(soc),
                  cols = vars(cat),
                  scales = "free",
                  space = "free"
               ) +
               theme(legend.position = "bottom") +
               theme(
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()
               ) +
               theme(strip.text.y = element_text(angle = 90, size = 8)) +
               geom_errorbarh(aes(xmin = lower, xmax = upper)) +
               xlim(min = -1 * max(wald_table1$upper) - 1,
                    max = max(wald_table1$upper) + 1) +
               annotate("segment", x = 0, xend = 5, y = 0.5, yend = 0.5, size = 1, 
                        colour = "red", arrow = arrow(type = "closed")) +
               annotate("text", x = 3, xend = 5, y= 0.5, yend = 0.5, label = "More Risk", fontface = "bold") +
               annotate("segment", x = 0, xend = -5, y = 0.5, yend = 0.5, size = 1, 
                        colour = "green", arrow = arrow(type = "closed")) +
               annotate("text", x = -3, xend = -5, y= 0.5, yend = 0.5, label = "Less Risk", fontface = "bold")
            return(p_ci)
            
         }, )
         
         return(p)
 
         # data <- info()
         # data0 <- data %>%
         #    mutate(duration = (as.numeric(
         #       difftime(previous.dose.date, First.dose.date)) + 1 + 7) / 365.25) %>%
         #    filter(Treatment %in% c(input$treatment_1, input$treatment_2))
         # data1 <- data %>%
         #    select(Subject.ID, Treatment, PT, Onset.Date, First.dose.date) %>%
         #    filter(Treatment %in% c(input$treatment_1, input$treatment_2)) %>%
         #    mutate(duration = (as.numeric(
         #       difftime(Onset.Date, First.dose.date)
         #    ) + 1) / 365.25)
         # output$txt <- renderText({
         #    calc <-
         #       length(unique(data1$Subject.ID)) * 100 / (sum(data0$duration) + sum(data1$duration))
         #    text <-
         #       paste(
         #          "The exposure-adjusted incidence rate estimates that if 100 patients were treated for 1 year, ",
         #          calc,
         #          " patients would experience the event of interest."
         #       )
         #    return(text)
         # })
         # return(data1)
         
      })
#-------------------------------------------------------------------------------      
      
      output$crude_incidence <- renderPlot({
         # df <- info()
         # data1 <- df %>%
         #    select(Subject.ID, SOC, PT, Treatment) %>%
         #    distinct() %>%
         #    group_by(SOC, PT, Treatment) %>%
         #    summarise(n = n(), .groups = "keep")
         # 
         # data2 <-
         #    data1 %>% mutate(pct = n * 100 / sum(as.numeric(data1$n))) %>%
         #    mutate(var1 = paste(n, "(", 
         #                        format(round(pct, digits = 2), 
         #                               nsmall = 2), ")")) %>%
         #    mutate(cat = "Incidence Rate (%)")
         # 
         # data2 <-
         #    data2[data2$Treatment %in% c(input$treatment1, input$treatment2),]
         data2 <- filtered()
      
         p <-
            ggplot(data2, aes(
               x = pct,
               y = PT,
               color = as.factor(Treatment)
            )) +
            geom_point(size = 3, position = position_jitter(h = 0.1, w = 0.1)) +
            facet_grid(
               rows = vars(SOC),
               cols = vars(cat),
               scales = "free",
               space = "free"
            ) +
            theme(legend.position = "bottom") +
            theme(strip.text.y = element_text(angle = 90, size = 8),
                  axis.title.y = element_blank()) +
            theme(axis.title.x = element_blank())
         return(p)
      })
         
#-------------------------------------------------------------------------------         
         output$crude_wald <- renderPlot({
            data5 <- filtered()
            wald_table <- c()
            soc_list <- c()
            #p_fisher <- c()
            #pt_list <- c()
            #rr <- c()
            # run through the AEs in treatment groups and return wald test with CI
            for (n in unique(data5$PT)) {
               data6 <- data5
               for (x in 1:length(data6$PT)) {
                  if (data6$PT[x] != n) {
                     data6$PT[x] <- 0
                  }
                  if (data6$Treatment[x] == input$treatment1){
                     data6$Treatment[x] <- 0
                  }
                  if (data6$Treatment[x] == input$treatment2){
                     data6$Treatment[x] <- 1
                  }
               }
               # https://sphweb.bumc.bu.edu/otlt/mph-modules/ph717-quantcore/r-for-ph717/R-for-PH71714.html
               rev_con <- table(data6$PT,
                                data6$Treatment)
               wald_table <- 
                  rbind(wald_table,
                        riskratio.wald(rev_con)$measure[2,])
               soc_list <-
                  append(soc_list, unique(data5[data5$PT == n,]$SOC))
               # calculate p value for each AE between groups
               #con_table <- table(data6$Treatment,data6$PT)
               #p_fisher <- rbind(p_fisher, fisher.test(con_table)$p)
               # calculate relative risk
               #if (rev_con[,1][2] / sum(rev_con[,1]) == 0 || rev_con[,1][2] / sum(rev_con[,1]) == Inf) {
                  relative_ratio <- round((rev_con[,2][2] / sum(rev_con[,2])) - (rev_con[,1][2] / sum(rev_con[,1])), digit=2)
               }
               #else {
               #    relative_ratio <- round((rev_con[,2][2] / sum(rev_con[,2])) / (rev_con[,1][2] / sum(rev_con[,1])), digit=2)
               # }
               # rr <- append(rr, relative_ratio)
               # pt_list <- append(pt_list, n)
            
           
            # output$volcano <- renderPlot({
            #    rr <- as.data.frame(rr)
            #    rr <- cbind(pt_list, rr)
            #    colnames(rr) <- c("pt", "rr")
            #    rr <- mutate(rr, group = "rr",.keep = "all" )
            #    vol <- cbind(rr, p_fisher)
            #    vol <- cbind(vol, soc_list)
            #    
            #    volc <- ggplot(data = vol, aes(x = rr, y = -log10(p_fisher), color = factor(soc_list))) +
            #       geom_point(size = 2, position = position_jitter(h = 0.2, w = 0.2, seed = 1)) +
            #       theme(legend.position = "bottom") +
            #       geom_label(aes(label = pt, fill = factor(soc_list)), color = "white", position = position_jitter(h = 0.2, w = 0.2, seed = 1), size = 4) +
            #       geom_hline(yintercept=-log10(0.05), linetype="dashed", color = "red") +
            #       geom_vline(xintercept = 1, linetype="dotted",color = "blue") +
            #       annotate("segment", x = 1, xend = 2, y = 0, yend = 0, size = 0.5, 
            #                colour = "blue", arrow = arrow(type = "closed")) +
            #       annotate("text", x = 1.5, y= 0.01, label = "Treatment Group", fontface = "bold") +
            #       annotate("segment", x = 1, xend = 0, y = 0, yend = 0, size = 0.5, 
            #                colour = "green", arrow = arrow(type = "closed")) +
            #       annotate("text", x = 0.5, y= 0.01,  label = "Control Group", fontface = "bold") +
            #       xlim(min = -1 * max(vol$rr), max = max(vol$rr) + 2)
            #    return(volc)
            # })
            rownames(wald_table) <- unique(data5$PT)
            
            # output$crude_wald_table <- renderTable({
            #    return(wald_table)
            # })
            wald_table1 <-
               as.data.frame(wald_table) %>% mutate(cat = "Risk Ratio with 95% CI")
            wald_table1$soc <- soc_list
            wald_table1[is.na(wald_table1)] = 0
            p_ci <-
               ggplot(wald_table1, 
                      aes(x = estimate,
                          y = rownames(wald_table1),)) +
               geom_point(
                  size = 3) +
               geom_text(data = wald_table1, 
                         aes(label = round(estimate, digits = 2),
                             group = x), 
                         position = position_dodge(width = 0.8),
                         vjust = -0.3, 
                         hjust = -0.3) +
               facet_grid(
                  rows = vars(soc),
                  cols = vars(cat),
                  scales = "free",
                  space = "free"
               ) +
               theme(legend.position = "bottom") +
               theme(
                  axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.title.x = element_blank()
               ) +
               theme(strip.text.y = element_text(angle = 90, size = 8)) +
               geom_errorbarh(aes(xmin = lower, xmax = upper)) +
               xlim(min = -1 * max(wald_table1$upper) - 1,
                    max = max(wald_table1$upper) + 1) +
               annotate("segment", x = 0, xend = 5, y = 0.5, yend = 0.5, size = 1, 
                        colour = "red", arrow = arrow(type = "closed")) +
               annotate("text", x = 3, xend = 5, y= 0.5, yend = 0.5, label = "More Risk", fontface = "bold") +
               annotate("segment", x = 0, xend = -5, y = 0.5, yend = 0.5, size = 1, 
                        colour = "green", arrow = arrow(type = "closed")) +
               annotate("text", x = -3, xend = -5, y= 0.5, yend = 0.5, label = "Less Risk", fontface = "bold")
            return(p_ci)
            
         })
      
      output$volcano <- renderPlot({
         data5 <- filtered()
         soc_list <- c()
         p_fisher <- c()
         pt_list <- c()
         rr <- c()
         # run through the AEs in treatment groups and return wald test with CI
         for (n in unique(data5$PT)) {
            data6 <- data5
            for (x in 1:length(data6$PT)) {
               if (data6$PT[x] != n) {
                  data6$PT[x] <- 0
               }
               if (data6$Treatment[x] == input$treatment1){
                  data6$Treatment[x] <- 0
               }
               if (data6$Treatment[x] == input$treatment2){
                  data6$Treatment[x] <- 1
               }
            }
            # https://sphweb.bumc.bu.edu/otlt/mph-modules/ph717-quantcore/r-for-ph717/R-for-PH71714.html
            rev_con <- table(data6$PT,
                             data6$Treatment)
            soc_list <-
               append(soc_list, unique(data5[data5$PT == n,]$SOC))
            # calculate p value for each AE between groups
            con_table <- table(data6$Treatment,data6$PT)
            p_fisher <- rbind(p_fisher, fisher.test(con_table)$p)
            # calculate relative risk
            if (rev_con[,1][2] / sum(rev_con[,1]) == 0 || rev_con[,1][2] / sum(rev_con[,1]) == Inf) {
               relative_ratio <- round((rev_con[,2][2] / sum(rev_con[,2])) - (rev_con[,1][2] / sum(rev_con[,1])), digit=2)
            }
            else {
               relative_ratio <- round((rev_con[,2][2] / sum(rev_con[,2])) / (rev_con[,1][2] / sum(rev_con[,1])), digit=2)
            }
            rr <- append(rr, relative_ratio)
            pt_list <- append(pt_list, n)
         }
         rr <- as.data.frame(rr)
         rr <- cbind(pt_list, rr)
         colnames(rr) <- c("pt", "rr")
         rr <- mutate(rr, group = "rr",.keep = "all" )
         vol <- cbind(rr, p_fisher)
         vol <- cbind(vol, soc_list)
         
         volc <- ggplot(data = vol, aes(x = rr, y = -log10(p_fisher), color = factor(soc_list))) +
            geom_point(size = 2, position = position_jitter(h = 0.2, w = 0.2, seed = 1)) +
            theme(legend.position = "bottom") +
            geom_label(aes(label = pt, fill = factor(soc_list)), color = "white", position = position_jitter(h = 0.2, w = 0.2, seed = 1), size = 4) +
            geom_hline(yintercept=-log10(0.05), linetype="dashed", color = "red") +
            geom_vline(xintercept = 1, linetype="dotted",color = "blue") +
            annotate("segment", x = 1, xend = 2, y = 0, yend = 0, size = 0.5, 
                     colour = "blue", arrow = arrow(type = "closed")) +
            annotate("text", x = 1.5, y= 0.01, label = "Treatment Group", fontface = "bold") +
            annotate("segment", x = 1, xend = 0, y = 0, yend = 0, size = 0.5, 
                     colour = "green", arrow = arrow(type = "closed")) +
            annotate("text", x = 0.5, y= 0.01,  label = "Control Group", fontface = "bold") +
            xlim(min = -1 * max(vol$rr), max = max(vol$rr) + 2)
         return(volc)
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




