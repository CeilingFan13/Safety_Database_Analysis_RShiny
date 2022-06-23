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
library(reshape)
library(plotly)
library("rjags")
library("coda")
library(shinycustomloader)
library(DiagrammeR)
source("Meta-Analysis.R")

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
                           choices = sort(site_list),
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
      # data for incidence plot
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
      # data for crude wald and volcano plot
      less_filtered <- eventReactive (input$confirm, {
         df <- info()
         data1 <- df %>%
            select(Subject.ID, SOC, PT, Treatment) %>%
            group_by(SOC, PT, Treatment)
         
         
         data2 <-
            data1[data1$Treatment %in% c(input$treatment1, input$treatment2),]
         return(data2)
      })
      inputgroup <- eventReactive(input$confirm, {
         df <- info()
         time <- df %>% 
            filter(Treatment %in% c(input$treatment1, input$treatment2))
         return(time)
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
      observeEvent(
         {input$site}, {
         data1 <- info()
         a <- filter(data1, data1$Site %in% input$site
                     )
         #site_item <- sort(unique(a$Site))
         subject_item <- sort(unique(a$Subject.ID))
         soc_item <- sort(unique(a$SOC))
         pt_item <- sort(unique(a$PT))

         # updatePickerInput(
         #    session = session,
         #    inputId = "site",
         #    choices = site_item,
         #    selected = site_item
         # )
         updatePickerInput(
            session = session,
            inputId = "subject",
            choices = subject_item,
            selected = subject_item
         )
         updatePickerInput(
            session = session,
            inputId = "organ_class",
            choices = soc_item,
            selected = soc_item

         )
         updatePickerInput(
            session = session,
            inputId = "preferred_term",
            choices = pt_item,
            selected = pt_item

         )
      })
     
      observeEvent(input$report, {
         screenshot()
      })
      observeEvent(input$report1, {
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
      
      observeEvent({input$prior_type
            input$study_type
            input$outcomes}, {
         if (input$prior_type != "Log-Normal") {
            hide("study_type")
            hide("outcomes")
         }
         
         if (input$prior_type == "Inverse-Gamma") {
            updateNumericInput(session = session,
                               inputId = "alpha",
                               value = 0.001)
            updateNumericInput(session = session,
                               inputId = "beta",
                               value = 0.001)
         }
         else if (input$prior_type == "Uniform") {
            updateNumericInput(session = session,
                               inputId = "alpha",
                               value = 0)
            updateNumericInput(session = session,
                               inputId = "beta",
                               value = 2)
         }
         else if (input$prior_type == "Half-Normal") {
            updateNumericInput(session = session,
                               inputId = "alpha",
                               value = 0)
            updateNumericInput(session = session,
                               inputId = "beta",
                               value = 0.1)
         }
         else if (input$prior_type == "Log-Normal") {
            shinyjs::show("study_type")
            shinyjs::show("outcomes")
            if(input$study_type == "Pharmacological vs. placebo/control comparison" & input$outcomes == "all-cause mortality") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -4.06)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.45)
            }
            if(input$study_type == "Pharmacological vs. placebo/control comparison" & input$outcomes == "semi-objective outcome") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -3.02)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.85)
            }
            if(input$study_type == "Pharmacological vs. placebo/control comparison" & input$outcomes == "subjective outcome") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -2.13)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.58)
            }
            if(input$study_type == "Pharmacological vs. pharmacological comparison" & input$outcomes == "all-cause mortality") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -4.27)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.48)
            }
            if(input$study_type == "Pharmacological vs. pharmacological comparison" & input$outcomes == "semi-objective outcome") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -3.23)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.88)
            }
            if(input$study_type == "Pharmacological vs. pharmacological comparison" & input$outcomes == "subjective outcome") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -2.34)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.62)
            }
            if(input$study_type == "Non-pharmacological comparison" & input$outcomes == "all-cause mortality") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -3.93)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.51)
            }
            if(input$study_type == "Non-pharmacological comparison" & input$outcomes == "semi-objective outcome") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -2.89)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.91)
            }
            if(input$study_type == "Non-pharmacological comparison" & input$outcomes == "subjective outcome") {
               updateNumericInput(session = session,
                                  inputId = "alpha",
                                  value = -2.01)
               updateNumericInput(session = session,
                                  inputId = "beta",
                                  value = 1.64)
            }
         }
      })
      
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
            clean_ae %>% 
            knitr::kable("html", caption = "<center>Adverse Event Summary</center>") %>% 
            kable_styling("striped", full_width = F)
         return(k_table)
      }
      
      output$heatmap <- renderPlot({
         data0 <- info()
         data <- data0 %>% select(c("SOC", "Treatment")) %>%
            group_by(SOC, Treatment) %>%
            summarise(n = n(), .groups = "keep")
         
         data1 <-data %>% mutate(pct = n * 100 / sum(as.numeric(data$n))) 
         data2 <- data1 %>% select(-pct) %>% spread(Treatment, n)
         rownames(data2) <- data2$SOC
         data2 <- subset(data2, select = -c(SOC))
         data2[is.na(data2)] = 0
         m <- as.matrix(sapply(data2, as.numeric))
         soc <- unique(data$SOC)
         rownames(m) <- soc
         #my_colors <- colorRampPalette(c("cyan", "deeppink3"))
         #h_map <- heatmap(m, Rowv = NA, col = my_colors(100))
         m <- as.data.frame(m)
         m <- cbind(m, soc)
         m_reshape <- melt(m)
         h_map <- ggplot(m_reshape, aes(variable, soc)) + 
            geom_tile(aes(fill = value)) + 
            scale_fill_gradient(low = "cyan", high = "blue") +
            theme(legend.position = "bottom", 
                  axis.title.x = element_blank(), 
                  axis.title.y = element_blank(),
                  axis.text.y = element_text(angle = 60)) +
            coord_fixed()
         
         output$soc_bar <- renderPlot({
            m_filter <- filter(m_reshape, m_reshape$value != 0)
            bar_soc <- ggplot(m_filter, aes(variable, value, fill = soc)) + 
               geom_bar(position = "dodge", stat = "identity") +
               theme(legend.position = "bottom", 
                     axis.title.x = element_blank()) 
               
            return(bar_soc)
         })
         
         return(h_map)
      })
      
      output$soc_tr <- DT::renderDataTable({
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
      }, width = "100%")
      
      output$eair_incidence <- renderPlot({
         df <- inputgroup()
         time <- df %>% 
            select(Subject.ID, Treatment, PT, Onset.Date, First.dose.date, previous.dose.date) %>%
            mutate(duration1 = as.numeric((difftime(Onset.Date, First.dose.date, units = "days")) + 1) / 365.25) %>%
            mutate(duration2 = as.numeric((difftime(previous.dose.date, First.dose.date, units = "days")) + 1 + input$effect_days) / 365.25) %>%
            select(Treatment, PT, duration1, duration2)
            
         data1 <- df %>%
            select(Subject.ID, SOC, PT, Treatment) %>%
            distinct() %>%
            group_by(SOC, PT, Treatment) %>%
            summarise(n = n(), .groups = "keep")
            #filter(Treatment %in% c(input$treatment1, input$treatment2))
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
               one <- sum(subset(data6, data6$PT == 0 & data6$Treatment == 0)[5])
               two <- sum(subset(data6, data6$PT == 0 & data6$Treatment == 1)[5])
               if (is.na(as.numeric(subset(data6, data6$PT != 0 & data6$Treatment == 0)[5]))){
                  three <- 0
               }
               else {
                  three <- as.numeric(subset(data6, data6$PT != 0 & data6$Treatment == 0)[5])
               }
               if (is.na(as.numeric(subset(data6, data6$PT != 0 & data6$Treatment == 1)[5]))){
                  four <- 0
               }
               else {
                  four <- as.numeric(subset(data6, data6$PT != 0 & data6$Treatment == 1)[5])
               }
               # https://sphweb.bumc.bu.edu/otlt/mph-modules/ph717-quantcore/r-for-ph717/R-for-PH71714.html
               rbl <- matrix(
                  c(one, two, three, four), ncol = 2, byrow=TRUE)
               print(rbl)
               wald_table <- 
                  rbind(wald_table,
                        riskratio.wald(rbl)$measure[2,])
               soc_list <-
                  append(soc_list, unique(data5[data5$PT == n,]$SOC))
            }
            rownames(wald_table) <- unique(data2$PT)
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
               # annotate("segment", x = 0, xend = 5, y = 0.5, yend = 0.5, size = 1, 
               #          colour = "red", arrow = arrow(type = "closed")) +
               annotate("text", x = 3, xend = 5, y= 0.5, yend = 0.5, label = "More Risk", fontface = "bold", colour = "red") +
               # annotate("segment", x = 0, xend = -5, y = 0.5, yend = 0.5, size = 1, 
               #          colour = "green", arrow = arrow(type = "closed")) +
               annotate("text", x = -3, xend = -5, y= 0.5, yend = 0.5, label = "Less Risk", fontface = "bold", colour = "green") +
               geom_vline(xintercept = 0,
                          linetype = 2,
                          color = "blue")
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
            data5 <- less_filtered()
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
               # annotate("segment", x = 0, xend = 5, y = 0.5, yend = 0.5, size = 1, 
               #          colour = "red", arrow = arrow(type = "closed")) +
               annotate("text", x = 3, xend = 5, y= 0.5, yend = 0.5, label = "More Risk", fontface = "bold", colour = "red") +
               # annotate("segment", x = 0, xend = -5, y = 0.5, yend = 0.5, size = 1, 
               #          colour = "green", arrow = arrow(type = "closed")) +
               annotate("text", x = -3, xend = -5, y= 0.5, yend = 0.5, label = "Less Risk", fontface = "bold", colour = "green") +
               geom_vline(xintercept = 0,
                          linetype = 2,
                          color = "blue")
            return(p_ci)
            
         })
      
      output$volcano <- renderPlot({
         data5 <- less_filtered()
         soc_list <- c()
         p_fisher <- c()
         pt_list <- c()
         rr <- c()
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
            # if (rev_con[,1][2] / sum(rev_con[,1]) == 0 || rev_con[,1][2] / sum(rev_con[,1]) == Inf) {
            #    relative_ratio <- round(((rev_con[,2][2] + 1) / (sum(rev_con[,2]) + 1 )), digit=2)
            #                            #- (rev_con[,1][2] / sum(rev_con[,1])), digit=2)
            # }
            if(sum(rev_con[,1]) == 0) {
               relative_ratio <- round((rev_con[,2][2] / sum(rev_con[,2])) / (rev_con[,1][2] / (sum(rev_con[,1]) + 0.5)), digit=2)
            }
            else if(rev_con[,1][2] == 0) {
               relative_ratio <- round((rev_con[,2][2] / sum(rev_con[,2])) / ((rev_con[,1][2] + 0.5) / sum(rev_con[,1])), digit=2)
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
         
         volc <- ggplot(data = vol, aes(x = log2(rr), y = -log10(p_fisher), color = factor(soc_list))) +
            geom_point(size = 2, position = position_jitter(h = 0.2, w = 0.2, seed = 1)) +
            theme(legend.position = "bottom") +
            geom_label(aes(label = pt, fill = factor(soc_list)), color = "white", position = position_jitter(h = 0.2, w = 0.2, seed = 1), size = 4) +
            geom_hline(yintercept=-log10(0.05), linetype="dashed", color = "red") +
            geom_vline(xintercept = 0, linetype="dotted",color = "blue") +
            annotate("segment", x = 0, xend = max(vol$rr), y = 0, yend = 0, size = 0.5, 
                     colour = "blue", arrow = arrow(type = "closed")) +
            annotate("text", x = max(vol$rr)/2, y= 0.01, label = "Treatment Group", fontface = "bold") +
            annotate("segment", x = 0, xend = -1 * max(vol$rr), y = 0, yend = 0, size = 0.5, 
                     colour = "green", arrow = arrow(type = "closed")) +
            annotate("text", x = -1 * max(vol$rr)/2, y= 0.01,  label = "Control Group", fontface = "bold") +
            xlim(min = -1 * max(vol$rr) - 1, max = max(vol$rr) + 1)
         return(volc)
      })
      
      output$flowchart <- renderGrViz({
         flowchart <- grViz("digraph flowchart {
      # node definitions with substituted label text
      node [fontname = Helvetica, shape = rectangle, color = SteelBlue] 
      edge [color = Gray, arrowhead = vee]
      graph[layout = dot]
      tab1 [label = '@@1']
      tab2 [label = '@@2']
      tab3 [label = '@@3']
      tab4 [label = '@@4']
      tab6 [label = '@@6']
      tab9 [label = '@@9']
      node [fontname = Helvetica, shape = oval, color = Firebrick]
      tab5 [label = '@@5']
      tab7 [label = '@@7']
      tab8 [label = '@@8']
      

      # edge definitions with the node IDs
      tab1 -> tab2 -> tab3 -> tab4 -> tab6 -> tab9;
      tab4 -> tab5;
      tab5 -> tab7 -> tab2 ;
      tab5 -> tab8 -> tab1;
      { rank=same; tab5 tab6;}
      { rank=same; tab7 tab8;}
      }

      [1]: 'Specify priors for hyperparameters and input data'
      [2]: 'Run burn-in period to draw posterior samples to \\n achieve the convergence and stablizationn of the Markov chains \\n (discarded before inference)'
      [3]: 'Additional iterations to draw posterior'
      [4]: 'Use trace plot and density plot to assess convergence'
      [5]: 'Bad convergence'
      [6]: 'Good convergence'
      [7]: 'Increase iterations for burn-in'
      [8]: 'Use informative priors'
      [9]: 'Obtain point estimation and confidence intervals for parameters of interest'
      
      ")
         return(flowchart)
      })
      
      refresh <- eventReactive(input$refresh, {
         df <- read.csv(
            input$file2$datapath,
            header = TRUE,
            sep = ",",
            blank.lines.skip = TRUE
         )
         data <- list(df$r1, df$r2, df$n1, df$n2)
         names(data) <- c("r1", "r2", "n1", "n2")
         prior_results(data = data, 
                       prior = input$prior_type, 
                       alpha = input$alpha, 
                       beta = input$beta,
                       n.burnin = input$burnin, 
                       n.iter = input$iteration, 
                       n.chains = input$chains) 
         
      })
    
      output$meta_table <- DT::renderDataTable({
         
         data0 <- refresh()[[1]]
         data <- data0[rownames(data0) %in% c("OR", "log_or", "tau2"),]
         return(data)
         
      })
      
      output$meta_trace <- renderPlot({
         coda.ma <- refresh()[[2]]
         par(mfcol = c(length(coda.ma), 1))
         for(k in 1:length(coda.ma)){
            temp <- as.vector(coda.ma[[k]][,"log_or"])
            plot(temp, type = "l", col = "red", cex.lab = 1.5, cex.main = 1.5,
                      xlab = "Iteration", ylab = "Log odds ratio",
                      main = paste("Chain", k))
         }
         
      })
      
      output$meta_den <- renderPlot({
         coda.ma <- refresh()[[2]]
         post.coda <- NULL
         for(k in 1:length(coda.ma)){
            post.coda <- rbind(post.coda, coda.ma[[k]][,"log_or"])
         }
         postden <- density(post.coda)
         p <- plot(postden, main = "Posterior density", xlab = "Log odds ratio")
         polygon(postden, col = "lightblue", border = "darkblue")
         v_50 <- summary(coda.ma)$quantiles[ , "50%"]["log_or"]
         colname <- c("lb", "median", "ub")
         abline(v = v_50, lty = 4)
         return(p)
         
      })
      
      output$logor <- renderPlot({
         data0 <- as.data.frame(refresh()[[1]])
         colnames(data0) <- c("two.five", "fifty", "nineseven")
         data <- data0[rownames(data0) %in% c("OR", "log_or", "tau2"),]
         p_ci <-
            ggplot(data, 
                   aes(x = fifty,
                       y = rownames(data),)) +
            geom_point(
               size = 3) +
            geom_text(data = data, 
                      aes(label = round(fifty, digits = 2)), 
                      position = position_dodge(width = 0.8),
                      vjust = -0.3, 
                      hjust = -0.3) +
            theme(strip.text.y = element_text(angle = 90, size = 8)) +
            geom_errorbarh(aes(xmin = two.five, xmax = nineseven)) +
            xlim(min = -1 * max(data$nineseven) - 1,
                 max = max(data$nineseven) + 1)
         return(p_ci)
      })
      
      output$equation <- renderUI({
         withMathJax(helpText('$$r_{Ci} \\sim binomial(n_{Ci}, \\pi_{Ci}),\\text{  } r_{Ti} \\sim binomial(n_{Ti}, \\pi_{Ti})$$ \n
                              where $$\\pi_{Ci} \\text{  }and\\text{  } \\pi_{Ti}$$ are true underlying event rate. \n
                              $$logit(\\pi_{Ci}) = \\mu_i,\\text{  } logit(\\pi_{Ti}) = \\mu_i + \\delta_i$$ \n
                              where $$\\mu_i \\text{  }and\\text{  }\\delta_i$$ are the baseline risk in each study and underlying true log ORs within studies, respectively. \n
                              $$\\delta_i \\sim Normal(\\theta, \\tau^2)$$ \n
                              where $$\\theta\\text{  }and\\text{  } \\tau^2$$ are overall log OR and between-studies variance, respectively. \n
                              $$mu_i,\\text{  } \\theta,\\text{  } \\tau \\sim priors$$ \n'))
         
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




