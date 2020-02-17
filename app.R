library(shiny)
library(semantic.dashboard)
library(ggplot2)
library(plotly)
library(DT)

ui <- dashboardPage(
  dashboardHeader(color = "blue",title = "Dashboard Demo", inverted = TRUE),
  dashboardSidebar(
    size = "thin", color = "teal",
    sidebarMenu(
      menuItem(tabName = "main", "Main", icon = icon("car")),
      menuItem(tabName = "real_time", "Real-Time-All", icon = icon("table")),
      menuItem(tabName = "Emergency_Room_Availability", "Emergency_Room", icon = icon("table")),
      menuItem(tabName = "Surgery_availability", "Surgery_Room", icon = icon("table")),
      menuItem(tabName = "Operabilty", "Operabilty", icon = icon("table")),
      menuItem(tabName = "Power", "Power", icon = icon("table")),
      menuItem(tabName = "Beds", "Beds", icon = icon("table")),
      menuItem(tabName = "Mortality", "Mortality", icon = icon("table")),
      menuItem(tabName = "Contamination", "Contamination", icon = icon("table")),
      menuItem(tabName = "Dialysis", "Dialysis", icon = icon("table")),
      menuItem(tabName = "Nutrition", "Nutrition", icon = icon("table")),
      menuItem(tabName = "Strike", "Strike", icon = icon("table")),
      menuItem(tabName = "extra", "Extra", icon = icon("table")),
      menuItem(tabName = "staff", "Staff", icon = icon("table"))
      
    )),
  
########## Dashboard Body  ###########
  dashboardBody(
    tabItems(
      selected = 1,
      
      ### 1st
      tabItem(
        tabName = "main",
        fluidRow(
          box(width = 8,
              title = "Graph 1",
              color = "green", ribbon = TRUE, title_side = "top right",
              column(width = 8,
                     plotOutput("boxplot1")
              )
          ),
          box(width = 8,
              title = "Graph 2",
              color = "red", ribbon = TRUE, title_side = "top right",
              column(width = 8,
                     plotlyOutput("dotplot1")
              )))),
      
      tabItem(tabName = "real_time",fluidRow(dataTableOutput("real_time"))),
      tabItem(tabName = "Emergency_Room_Availability",fluidRow(dataTableOutput("Emergency_Room_Availability"))),
      tabItem(tabName = "Surgery_availability",fluidRow(dataTableOutput("Surgery_availability"))),
      tabItem(tabName = "Operabilty",fluidRow(dataTableOutput("Operabilty"))),
      tabItem(tabName = "Power",fluidRow(dataTableOutput("Power"))),
      tabItem(tabName = "Beds",fluidRow(dataTableOutput("Beds"))),
      tabItem(tabName = "Mortality",fluidRow(dataTableOutput("Mortality"))),
      tabItem(tabName = "Contamination",fluidRow(dataTableOutput("Contamination"))),
      tabItem(tabName = "Dialysis",fluidRow(dataTableOutput("Dialysis"))),
      tabItem(tabName = "Nutrition",fluidRow(dataTableOutput("Nutrition"))),
      tabItem(tabName = "Strike",fluidRow(dataTableOutput("Strike"))),
      tabItem(tabName = "staff",fluidRow(dataTableOutput("staff")))
    )
  ), theme = "cerulean"
)



server <- shinyServer(function(input, output, session) {
#  data("mtcars")
#  colscale <- c(semantic_palette[["red"]], semantic_palette[["green"]], semantic_palette[["blue"]])
#  mtcars$am <- factor(mtcars$am,levels=c(0,1), labels=c("Automatic","Manual"))
  
  
  ####################
  library(ggplot2)
  
  table <- read_excel('chart.xlsx')
  table <- as.data.frame(table)
  
  a <- data.frame('power_outage_deaths_count' = table$power_outage_deaths_count,
                  'mortality_hospital_failure_cardiovascular_count' =table$mortality_hospital_failure_cardiovascular_count,
                  'mortality_hospital_failure_trauma_count'= table$mortality_hospital_failure_trauma_count,
                  'Region'=table$federal_entity)
  a['fac'] <- as.integer(a$power_outage_deaths_count) +
    as.integer(a$mortality_hospital_failure_cardiovascular_count) + 
    as.integer(a$mortality_hospital_failure_trauma_count)
  a['sum']<-sum(a['fac'])
  a['Mortality_rate'] <- as.integer(a$fac) / as.integer(a$sum)
  
  
  b <- data.frame('Daily_Power_outage' = table$power_outage_avg_failures_per_day,
                  'Region'=table$federal_entity)
  
  chart_2<-ggplot()+geom_bar(data = b,aes(x = Region, y= Daily_Power_outage),stat = "identity")+coord_flip()
  chart_1<-ggplot()+geom_bar(data = a,aes(x = Region, y=Mortality_rate),stat = "identity")+coord_flip()
  
  
  
  ########
  library(readxl)
  real_time <- read_xlsx("real_time.xlsx")
  #View(real_time)
  Emergency_Room_Availability <- data.frame('Location' = real_time$federal_entity,
                                            'hospital_name' =real_time$hospital,
                                            'Last Update'= real_time$date,
                                            'Adrenalin' = real_time$er_avail_adrenalin,
                                            'Aminoglycosides'=real_time$er_avail_aminoglycosides_quinolone,
                                            'Atropine'=real_time$er_avail_atropine,
                                            'Dopamine'=real_time$er_avail_dopamine,
                                            'Cephalosporins'=real_time$er_avail_cephalosporins_betalactams,
                                            'Vancomycin'=real_time$er_avail_vancomycin_clindamycin,
                                            'Steroids'=real_time$er_avail_steroids,
                                            'Lidocaine'=real_time$er_avail_lidocaine,
                                            'Opioids (minor)'=real_time$er_avail_minor_opioids,
                                            'Opioids (major)'=real_time$er_avail_major_opioids,
                                            'IV Fluids'=real_time$er_avail_iv_fluids,
                                            'Diazepam'=real_time$er_avail_diazepam_dph,
                                            'Heparin'=real_time$er_avail_heparin,
                                            'Insulin'=real_time$er_avail_insulin,
                                            'Asthma'=real_time$er_avail_asthma,
                                            'Blood Preassure'=real_time$er_avail_blood_pressure,
                                            'Defibrilator'=real_time$er_avail_defibrillator,
                                            'Intubation'=real_time$er_avail_ott_intubation,
                                            'Catheter'=real_time$er_avail_catheter,
                                            'Oxygen'=real_time$er_avail_oxygen_suction)
  #View(Emergency_Room_Availability)
  
  Surgery_availability <- data.frame('Location' = real_time$federal_entity,
                                     'hospital_name' =real_time$hospital,
                                     'Last Update'= real_time$date,
                                     'Opioids (minor)' = real_time$sx_avail_minor_opioids,
                                     'Opioids (major)'=real_time$sx_avail_major_opioids,
                                     'Anesthetic Gases'=real_time$sx_avail_anesthetic_gases,
                                     'Anesthetic IV'=real_time$sx_avail_anesthetics_iv,
                                     'Relaxants'=real_time$sx_avail_relaxants,
                                     'Intubation'=real_time$sx_avail_ott_intubation,
                                     'Patient Lingere Kit'=real_time$sx_avail_patient_lingerie_kit,
                                     'Oxygen Suction'=real_time$sx_avail_oxygen_suction,
                                     'Disposables'=real_time$sx_avail_disposables_mask_gloves_gown)
  
  #View(Surgery_availability)
  
  Operabilty <- data.frame('Location' = real_time$federal_entity,
                           'hospital_name' =real_time$hospital,
                           'Last Update'= real_time$date,
                           'Intensive Care' = real_time$operability_icu,
                           'Intensive Care Pavillion'=real_time$operability_icu_p,
                           'Emergency Room'=real_time$operability_er,
                           'Surgery'=real_time$operability_sx,
                           'Lab'=real_time$operability_lab,
                           'ULS'=real_time$operability_uls,
                           'CT/MRI'=real_time$operability_ct_mri,
                           'XRAY'=real_time$operability_xr)
  
  
  # View(Operabilty)
  
  Power <- data.frame('Location' = real_time$federal_entity, 
                      'hospital_name' =real_time$hospital,
                      'Last Update'= real_time$date,
                      'Generator' = real_time$power_generator_available,
                      'Outage Mortality'=real_time$power_outage_mortatility,
                      'Outage Death Count'=real_time$power_outage_deaths_count,
                      'Otage'=real_time$power_outage,
                      'Failure per Day (avg)'=real_time$power_outage_avg_failures_per_day,
                      'Outage Day Count'=real_time$power_outage_days_count,
                      'Equipment Failure'=real_time$power_outage_equipment_failure)
  #  View(Power)
  
  Beds <- data.frame('Location' = real_time$federal_entity, 
                     'hospital_name' =real_time$hospital,
                     'Last Update'= real_time$date,
                     'OP Beds Count' = real_time$op_beds_count,
                     'OP Beds Emergency Room'=real_time$op_beds_er_count,
                     'Pavilions'=real_time$op_pavilions_count,
                     'Arch Beds'=real_time$arch_beds_count)
  
  #  View(Beds)
  
  Mortality <- data.frame('Location' = real_time$federal_entity, 
                          'hospital_name' =real_time$hospital,
                          'Last Update'= real_time$date,
                          'Cardiovascular'= real_time$mortality_hospital_failure_cardiovascular_count,
                          'Trauma'= real_time$mortality_hospital_failure_trauma_count)
  
  #  View(Mortality)
  
  Contamination <- data.frame('Location' = real_time$federal_entity, 
                              'hospital_name' =real_time$hospital,
                              'Last Update'= real_time$date,
                              'Suspected Desease'= real_time$epidemiological_emergency_suspected_diseases,
                              'Isolation Area'= real_time$nCoV_isolation_area_avail,
                              'Isolation Protocol'= real_time$nCoV_respiratory_isolation_protocol_avail,
                              'Face Mask'= real_time$nCoV_face_mask_avail)
  
  
  #  View(Contamination)
  
  Dialysis <- data.frame('Location' = real_time$federal_entity, 
                         'hospital' =real_time$hospital,
                         'Last Update'= real_time$date,
                         'Catheters (High Flow)' = real_time$rrt_avail_high_flow_catheters,
                         'Blood Test'=real_time$rrt_avail_blood_tests_hiv_hvb_hvc_vdr,
                         'Immediate Acces'=real_time$rrt_avail_immediate_access_urea_reduction_bun,
                         'Dialysis'=real_time$rrt_avail,
                         'Dialysis Operability'=real_time$rrt_operability,
                         'Date Stopped'=real_time$rrt_date_stopped_operability,
                         'Daily Patients (avg)'=real_time$rrt_avg_daily_patients,
                         'Peritoneal Count'=real_time$rrt_peritoneal_count,
                         'Hemodialysis'=real_time$rrt_num_hemodialysis,
                         'Acute'=real_time$rrt_num_hemodialysis_acute,
                         'Chronic'=real_time$rrt_num_hemodialysis_chronic,
                         'Equipments'=real_time$rrt_num_hemodialysis_equipments,
                         'Equipments Operability'=real_time$rrt_num_hemodialysis_equipments_operability,
                         'Lines Avalaibility'=real_time$rrt_hemodialysis_avail_lines,
                         'Kit'=real_time$rrt_hemodialysis_avail_kit_hemodialysis,
                         'Iron'=real_time$rrt_hemodialysis_avail_iron,
                         'B Complex'=real_time$rrt_hemodialysis_avail_b_complex,
                         'Calcium'=real_time$rrt_hemodialysis_avail_calcium,
                         'Zemblar'=real_time$rrt_hemodialysis_avail_zemblar,
                         'Osmosis Operability'=real_time$rrt_reverse_osmosis_unit_operability
  )
  #  View(Dialysis)
  Nutrition <- data.frame('Location' = real_time$federal_entity, 
                          'hospital' =real_time$hospital,
                          'Last Update'= real_time$date,
                          'Nutrition'= real_time$nutr_avail,
                          'Operability'= real_time$nutr_operability,
                          'Operability Stop'= real_time$nutr_date_stopped_operability)
  #  View(Nutrition)
  Strike <- data.frame('Location' = real_time$federal_entity, 
                       'hospital' =real_time$hospital,
                       'Last Update'= real_time$date,
                       'Med Staff'= real_time$strike_medical_staff_affected,
                       'Nurse'= real_time$strike_nurses_affected,
                       'Other Staff'= real_time$strike_other_staff_affected,
                       'Patients'= real_time$strike_patients_affected,
                       'Others'= real_time$strike_other_affected)
  #  View(Strike)
  staff <- data.frame('Location' = real_time$federal_entity, 
                      'hospital' =real_time$hospital,
                      'Last Update'= real_time$date,
                      'ER(doc)'= real_time$er_staff_mic_day_on_call,
                      'ER(noc)'= real_time$er_staff_mic_night_on_call,
                      'Res&rural (doc)'= real_time$er_staff_residents_and_rural_day_on_call,
                      'ER(noc)'= real_time$er_staff_mic_night_on_call,
                      'Nurse(doc)'= real_time$er_staff_non_professional_nurse_day_on_call,
                      'Nurse(noc)'= real_time$er_staff_non_professional_nurse_night_on_call,
                      'Specialist (doc)'= real_time$er_staff_specialist_day_on_call,
                      'Specialist (noc)'= real_time$er_staff_specialist_night_on_call,
                      'Res&Rural (noc)'= real_time$er_staff_residents_and_rural_night_on_call)
  
  
  output$boxplot1 <- renderPlot({chart_1})
  
  output$dotplot1 <- renderPlotly({chart_2})
  
  
  
  output$real_time <- renderDataTable(real_time,options = list(scrollX = TRUE))
  output$Emergency_Room_Availability <- renderDataTable(Emergency_Room_Availability,options = list(scrollX = TRUE))
  output$Surgery_availability <- renderDataTable(Surgery_availability,options = list(scrollX = TRUE))
  output$Operabilty <- renderDataTable(Operabilty,options = list(scrollX = TRUE))
  output$Power <- renderDataTable(Power,options = list(scrollX = TRUE))
  output$Beds <- renderDataTable(Beds,options = list(scrollX = TRUE))
  output$Mortality <- renderDataTable(Mortality,options = list(scrollX = TRUE))
  output$Contamination <- renderDataTable(Contamination,options = list(scrollX = TRUE))
  output$Dialysis <- renderDataTable(Dialysis,options = list(scrollX = TRUE))
  output$Nutrition <- renderDataTable(Nutrition,options = list(scrollX = TRUE))
  output$Strike <- renderDataTable(Strike,options = list(scrollX = TRUE))
  output$staff <- renderDataTable(staff,options = list(scrollX = TRUE))

})



shinyApp(ui, server)
