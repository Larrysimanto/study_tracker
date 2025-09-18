#install.packages(c("shiny", "shinydashboard", "dplyr", "DT", "plotly", "shinyjs"))


StudyID<- c('PROJ101', 'PROJ101', 'PROJ202', 'PROJ202')
Dataset<- c('ADSL', 'ADAE', 'DM','AE' )
Type<- c('ADaM', 'ADaM', 'SDTM', 'SDTM')
Status<- c('Completed', 'In Progress', 'Completed', 'Not Started')
Owner<- c('Alex', 'Alex', 'Maria', 'Maria')
PlannedDate<-  c('2025-07-20', '2025-08-15', '2025-06-10', '2025-08-25')
ActualDate<- c('2025-07-29','', '2025-06-10', '')

dsets<-data.frame(StudyID,  Dataset, Type, Status, Owner, PlannedDate, ActualDate)
write.csv(dsets, "datasets_tracker.csv",row.names = FALSE)    


StudyID<- c('PROJ101', 'PROJ101', 'PROJ202', 'PROJ202')
Tfl_ID<- c('t-14', 'f-15', 't-16','f-18' )
Description<- c('Table', 'figure', 'table', 'table')
Status<- c('Completed', 'In Progress', 'Completed', 'Not Started')
Owner<- c('John', 'Ian', 'Larry', 'Larry')
PlannedDate<-  c('2025-07-20', '2025-08-15', '2025-06-10', '2025-08-25')
ActualDate<- c('2025-07-29','', '2025-06-10', '')

outputs<-data.frame(StudyID,  Tfl_ID, Description, Status, Owner, PlannedDate, ActualDate)
write.csv(dsets, "tfls_tracker.csv",row.names = FALSE)    

