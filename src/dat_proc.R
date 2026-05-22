## DMSTAr development and comparison
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2026-05-22

## Input data formatting procedure

## Libraries
library(AnalystHelper)
library(plyr)
library(reshape2)
library(readxl)

# Modeling
library(DMSTAr)

## Paths
wd <- "C:/Julian_LaCie/_GitHub/DMSTAr_manu"
paths <- paste0(wd,c("/Plots/","/Data/"))

plot.path <- paths[1]
data.path <- paths[2]

# Functions
source(paste0(wd,"/src/func.R"))

# Restoration Strategies --------------------------------------------------
dmsta_ver_val <- "2E"
## Data --------------------------------------------------------------------
RS_dat_path # path on local machine 

STA_sheet <- c("FEBS5A_N","FEB_S5A", "FEBS5A_OUT",
               "STA1_DW", "STA1W","STA1E","STA2B","FEB_34","FEB34_OUT", "STA34",
               "FEB_56","STA56_DW", "STA5", "STA6")
sheet_name_pre <- c(rep("/PROJECT_SFWMD_EC_01MAR2012_NET_EAA_",10),
                    rep("/PROJECT_SFWMD_W_01MAR2012_NET_56_",4))


RS_dmsta_rslt <- list()
for(k in seq_along(STA_sheet)){
  wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
  dmsta_output<- readxl::read_xlsx(wksht_path,sheet = "Series_Overall",skip = 9,
                                   col_names = FALSE,progress=FALSE)|>as.data.frame()
  names(dmsta_output) <- series_colnames
  dmsta_output$Date <- as.Date(dmsta_output$Date,origin = "1900-01-01")-2
  dmsta_output$WY <- WY(dmsta_output$Date)
  dmsta_output$STA <- STA_sheet[k]
  
  RS_dmsta_rslt[[k]] <- dmsta_output
  print(k)
}
RS_dmsta_rslt[[1]]|>head()

### FEBS5A_N ----------------------------------------------------------------
k <- 1
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_FEBS5A_N_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,
                                                 dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_FEBS5A_N_cell <- build_case_cells(RS_FEBS5A_N_params)

RS_FEBS5A_N_input <- read_series_input(wksht_path)
RS_FEBS5A_N_input <- RS_FEBS5A_N_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = cfs_to_hm3d(REL_FARM),
         Zcontrol = 0,
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### FEB_S5A ------------------------------------------------------------------
k <- 2
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_FEB_S5A_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1)|>
  mutate(CaseName = STA_sheet[k])
RS_FEB_S5A_cell <- build_case_cells(RS_FEB_S5A_params)

RS_FEB_S5A_input <- read_series_input(wksht_path)
RS_FEB_S5A_input$Rel_opt[is.na(RS_FEB_S5A_input$Rel_opt)] <- 0
RS_FEB_S5A_input <- RS_FEB_S5A_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = cfs_to_hm3d(Rel_opt),
         Qr2 = 0,
         Zcontrol = ft.to.m(FEB_REG),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### FEBS5A_OUT ------------------------------------------------------------------
k <- 3
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_FEBS5A_out_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1,
                                                   dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_FEBS5A_out_cell <- build_case_cells(RS_FEBS5A_out_params)

RS_FEBS5A_out_input <- read_series_input(wksht_path,base=c("Date"))

RS_FEBS5A_out_input <- RS_FEBS5A_out_input|>
  mutate(
    Flow = 0, # Series_Input is blank
    Conc = 0, # Series_Input is blank
    Rainfall = 0, # Series_Input is blank
    ET = 0, # Series_Input is blank
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = STA_sheet[k]
  )

### STA1_DW -----------------------------------------------------------------
k <- 4
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA1_DW_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1,
                                                dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA1_DW_cell <- build_case_cells(RS_STA1_DW_params)

RS_STA1_DW_input <- read_series_input(wksht_path,base=c("Date","Flow","Conc"))
RS_STA1_DW_input <- RS_STA1_DW_input|>
  mutate(
    Rainfall = 0, # Series_Input is blank
    ET = 0, # Series_Input is blank
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = STA_sheet[k]
  )
summary(RS_STA1_DW_input)
### STA1W -------------------------------------------------------------------
k <- 5
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA1W_params <- dmstar_read_parameters_tab(wksht_path,ncells=10,DutyCycle = 0.95,
                                              dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA1W_cell <- build_case_cells(RS_STA1W_params)

RS_STA1W_input <- read_series_input(wksht_path)
RS_STA1W_input <- RS_STA1W_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = cm_to_m(0),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### STA1E -------------------------------------------------------------------
k <- 6
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA1E_params <- dmstar_read_parameters_tab(wksht_path,ncells=8,DutyCycle = 0.95,
                                              dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA1E_cell <- build_case_cells(RS_STA1E_params)

RS_STA1E_input <- read_series_input(wksht_path)
RS_STA1E_input <- RS_STA1E_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = cm_to_m(0),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### STA2B -------------------------------------------------------------------
k <- 7
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA2B_params <- dmstar_read_parameters_tab(wksht_path,ncells=11,DutyCycle = 0.95,
                                              dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA2B_cell <- build_case_cells(RS_STA2B_params)

RS_STA2B_input <- read_series_input(wksht_path)
RS_STA2B_input <- RS_STA2B_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = 0,#cm_to_m(0),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### FEB_34 ------------------------------------------------------------------
k <- 8
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_FEB_34_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1,
                                               dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_FEB_34_cell <- build_case_cells(RS_FEB_34_params)

RS_FEB_34_input <- read_series_input(wksht_path)
RS_FEB_34_input <- RS_FEB_34_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0, 
         Qr2 = 0, 
         Zcontrol = 0,
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### FEB34_OUT ---------------------------------------------------------------
k <- 9
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_FEB34_out_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1,
                                                  dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_FEB34_out_cell <- build_case_cells(RS_FEB34_out_params)

RS_FEB34_out_input <- read_series_input(wksht_path,base = c("Date","Flow","Conc"))
RS_FEB34_out_input <- RS_FEB34_out_input|>
  mutate(
    Rainfall = 0, # Serie_Input is blank
    ET = 0, # Serie_Input is blank
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = STA_sheet[k]
  )

### STA34 -------------------------------------------------------------------
k <- 10
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA34_params <- dmstar_read_parameters_tab(wksht_path,ncells=8,DutyCycle = 0.95,
                                              dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA34_cell <- build_case_cells(RS_STA34_params)

RS_STA34_input <- read_series_input(wksht_path)
RS_STA34_input <- RS_STA34_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = cm_to_m(0),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )
## Time series longer than others? 
RS_STA34_input <- subset(RS_STA34_input,Date%in%unique(RS_FEB_34_input$Date))
range(RS_STA34_input$Date)

### FEB_56 ------------------------------------------------------------------
k <- 11
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_FEB_56_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1,
                                               dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_FEB_56_cell <- build_case_cells(RS_FEB_56_params)

RS_FEB_56_input <- read_series_input(wksht_path)
RS_FEB_56_input <- RS_FEB_56_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0, 
         Qr2 = 0, 
         Zcontrol = 0,
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### STA56_DW ----------------------------------------------------------------
k <- 12
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA56_DW_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 1,
                                                 dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA56_DW_cell <- build_case_cells(RS_STA56_DW_params)

RS_STA56_DW_input <- read_series_input(wksht_path,base = c("Date"))
RS_STA56_DW_input <- RS_STA56_DW_input|>
  mutate(
    Flow = 0, # not in input data (TS_NULL)
    Conc = 0, # not in input data (TS_NULL)
    Rainfall = 0,
    ET = 0,
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0, 
    Qr2 = 0, 
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = STA_sheet[k]
  )

### STA5 --------------------------------------------------------------------
k <- 13
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA5_params <- dmstar_read_parameters_tab(wksht_path,ncells=10,DutyCycle = 0.95,
                                             dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA5_cell <- build_case_cells(RS_STA5_params)

RS_STA5_input <- read_series_input(wksht_path)
RS_STA5_input <- RS_STA5_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = cm_to_m(0),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )

### STA6 --------------------------------------------------------------------
k <- 14
STA_sheet[k]
wksht_path <- paste0(RS_dat_path,sheet_name_pre[k],STA_sheet[k],".xls")
##
RS_STA6_params <- dmstar_read_parameters_tab(wksht_path,ncells=10,DutyCycle = 0.95,
                                             dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = STA_sheet[k])
RS_STA6_cell <- build_case_cells(RS_STA6_params)

RS_STA6_input <- read_series_input(wksht_path)
RS_STA6_input <- RS_STA6_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = cm_to_m(0),
         Date = as.Date(Date),
         STA = STA_sheet[k]
  )
## Time series longer than others? 
RS_STA6_input <- subset(RS_STA6_input,Date%in%unique(RS_FEB_56_input$Date))
range(RS_STA6_input$Date)

# WERP --------------------------------------------------------------------
# For DMSTA modeling, ALTHR is same as WALTH/ALTH that was originally posted on February 8, 2019.
WERP_dat_path # path on local machine 

wksht_path <- paste0(WERP_dat_path,"/PROJECT_WERP_WALT_H_NFC139A.XLSX_NET_STA_STA_NF.xlsx")
dmsta_output<- readxl::read_xlsx(wksht_path,sheet = "Series_Overall",skip = 9,
                                 col_names = FALSE,progress=FALSE)|>as.data.frame()
names(dmsta_output) <- series_colnames
dmsta_output$Date <- as.Date(dmsta_output$Date,origin = "1900-01-01")-2
dmsta_output$WY <- WY(dmsta_output$Date)
dmsta_output$STA <- "NF"

WERP_dmsta_rslt <- dmsta_output
vars <- c("tot_bypass","release1","release2","seep_out","treated_out")
WERP_dmsta_rslt$Q_out_total <- rowSums(WERP_dmsta_rslt[,paste(vars,"Q",sep="_")],na.rm=T)
WERP_dmsta_rslt$L_out_total <- rowSums(WERP_dmsta_rslt[,paste(vars,"L",sep="_")],na.rm=T)

##
dmsta_ver_val <- "2C2B"
WERP_NF_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,DutyCycle = 0.95,
                                             dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = "NF STA")
WERP_NF_cell <- build_case_cells(WERP_NF_params)

WERP_NF_input <- read_series_input(wksht_path)
WERP_NF_input <- WERP_NF_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = cm_to_m(0),
         Date = as.Date(Date),
         STA = "NF STA"
  )

# CEPP PACR ---------------------------------------------------------------
dmsta_ver_val <- "2C2B"
## Data ------------------------------------------------------------
CEPP_dat_path # path on local machine 
CEPP_STA_sheet <- c("A1","A2","A2_DW","A2_DW2","STA_A2","STA2B","STA34")
sheet_name_pre <- "/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_"

CEPP_dmsta_rslt <- list()
for(k in seq_along(CEPP_STA_sheet)){
  wksht_path <- paste0(CEPP_dat_path,sheet_name_pre,CEPP_STA_sheet[k],".xlsx")
  dmsta_output<- readxl::read_xlsx(wksht_path,sheet = "Series_Overall",skip = 9,
                                   col_names = FALSE,progress=FALSE)|>as.data.frame()
  names(dmsta_output) <- series_colnames
  dmsta_output$Date <- as.Date(dmsta_output$Date,origin = "1900-01-01")
  dmsta_output$WY <- WY(dmsta_output$Date)
  dmsta_output$STA <- CEPP_STA_sheet[k]
  
  CEPP_dmsta_rslt[[k]] <- dmsta_output
  print(k)
}

### CEPP A1 -----------------------------------------------------------------
k <- 1
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
## 
CEPP_A1_params <- dmstar_read_parameters_tab(wksht_path,ncells=1,
                                             dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_A1_cell <- build_case_cells(CEPP_A1_params)

CEPP_A1_input <- read_series_input(wksht_path)
CEPP_A1_input <- CEPP_A1_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = cfs_to_hm3d(Rel_34),
         Qr2 = cfs_to_hm3d(Rel_2B),
         Zcontrol = 0,
         Date = as.Date(Date),
         STA = CEPP_STA_sheet[k]
  )


### CEPP A2 -----------------------------------------------------------------
k <- 2
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
##
CEPP_A2_params <- dmstar_read_parameters_tab(wksht_path,ncells=1,
                                             dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_A2_cell <- build_case_cells(CEPP_A2_params)

CEPP_A2_input <- read_series_input(wksht_path)
CEPP_A2_input <- CEPP_A2_input |>
  mutate(
    Qi  = cfs_to_hm3d(Flow),
    Ci  = Conc,
    Rain = in_to_m(Rainfall),
    Et   = in_to_m(ET),
    Qr0 =  cfs_to_hm3d(Rel_AG),
    Qr1 =  cfs_to_hm3d(Rel_A2DW),
    Qr2 = cfs_to_hm3d(Rel_A2DW2),
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = CEPP_STA_sheet[k]
  )

### CEPP A2_DW -----------------------------------------------------------------
k <- 3
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
##
CEPP_A2_DW_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,
                                                dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_A2_DW_cell <- build_case_cells(CEPP_A2_DW_params)

CEPP_A2_DW_input <- read_series_input(wksht_path,base = "Date")
CEPP_A2_DW_input <- CEPP_A2_DW_input|>
  mutate(
    Flow = 0, # not in input data (TS_NULL)
    Conc = 0, # not in input data (TS_NULL)
    Rainfall = NA,
    ET = NA,
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = CEPP_STA_sheet[k]
  )

### CEPP A2_DW2 -----------------------------------------------------------------
k <- 4
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
##
CEPP_A2_DW2_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,
                                                 dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_A2_DW2_cell <- build_case_cells(CEPP_A2_DW2_params)

CEPP_A2_DW2_input <- read_series_input(wksht_path,base = "Date")
CEPP_A2_DW2_input <- CEPP_A2_DW2_input|>
  mutate(
    Flow = 0, # not in input data (TS_NULL)
    Conc = 0, # not in input data (TS_NULL)
    Rainfall = NA,
    ET = NA,
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = CEPP_STA_sheet[k]
  )

### CEPP STA_A2 -----------------------------------------------------------------
k <- 5
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
##
CEPP_STA_A2_params <- dmstar_read_parameters_tab(wksht_path,
                                                 ncells=4,
                                                 DutyCycle = 0.95,
                                                 dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_STA_A2_cell <- build_case_cells(CEPP_STA_A2_params)

CEPP_STA_A2_input <- read_series_input(wksht_path)
CEPP_STA_A2_input <- CEPP_STA_A2_input|>
  mutate(
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = CEPP_STA_sheet[k]
  )

### CEPP STA2B -----------------------------------------------------------------
k <- 6
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
##
CEPP_STA2B_params <- dmstar_read_parameters_tab(wksht_path,
                                                ncells=11,
                                                DutyCycle = 0.95,
                                                dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_STA2B_cell <- build_case_cells(CEPP_STA2B_params)

CEPP_STA2B_input <- read_series_input(wksht_path)
CEPP_STA2B_input <- CEPP_STA2B_input|>
  mutate(
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = CEPP_STA_sheet[k]
  )

### CEPP STA34 -----------------------------------------------------------------
k <- 7
CEPP_STA_sheet[k]
wksht_path <- paste0(CEPP_dat_path,"/PROJECT_SFWMD_C240TSP_20180312.XLSM_NET_Central_",CEPP_STA_sheet[k],".xlsx")
##
CEPP_STA34_param <- dmstar_read_parameters_tab(wksht_path,
                                               ncells=6,
                                               DutyCycle = 0.95,
                                               dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = CEPP_STA_sheet[k])
CEPP_STA34_cell <- build_case_cells(CEPP_STA34_param)

CEPP_STA34_input <- read_series_input(wksht_path)
CEPP_STA34_input <- CEPP_STA34_input|>
  mutate(
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = CEPP_STA_sheet[k]
  )


# LOSOM PA25 --------------------------------------------------------------
dmsta_ver_val <- "2C2B"
## Data ------------------------------------------------------------
LOSOM_dat_path # path on local machine 
LOSOM_STA_sheet <- c("FEB_34","FEB34_OUT","STA2B","STA34","STA_A2")
sheet_name_pre <- "PROJECT_PA25.XLSM_NET_Central_"

LOSOM_dmsta_rslt <- list()
for(k in seq_along(LOSOM_STA_sheet)){
  wksht_path <- paste0(LOSOM_dat_path,sheet_name_pre,LOSOM_STA_sheet[k],".xlsx")
  dmsta_output<- readxl::read_xlsx(wksht_path,sheet = "Series_Overall",skip = 9,col_names = FALSE)|>as.data.frame()
  names(dmsta_output) <- series_colnames
  dmsta_output$Date <- as.Date(dmsta_output$Date,origin = "1900-01-01")
  dmsta_output$WY <- WY(dmsta_output$Date)
  dmsta_output$STA <- LOSOM_STA_sheet[k]
  
  LOSOM_dmsta_rslt[[k]] <- dmsta_output
  print(k)
}

### LOSOM FEB_34 -----------------------------------------------------------------
k <- 1
LOSOM_STA_sheet[k]
wksht_path <- paste0(LOSOM_dat_path,sheet_name_pre,LOSOM_STA_sheet[k],".xlsx")
## 
LOSOM_FEB34_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,
                                                 dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = LOSOM_STA_sheet[k])
LOSOM_FEB34_cell <- build_case_cells(LOSOM_FEB34_params)

LOSOM_FEB34_input <- read_series_input(wksht_path)
LOSOM_FEB34_input <- LOSOM_FEB34_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = cfs_to_hm3d(Rel_opt34),
         Qr2 = cfs_to_hm3d(Rel_opt2B),
         Zcontrol = 0,
         Date = as.Date(Date),
         STA = LOSOM_STA_sheet[k]
  )

### LOSOM FEB34_OUT -----------------------------------------------------------------
k <- 2
LOSOM_STA_sheet[k]
wksht_path <- paste0(LOSOM_dat_path,sheet_name_pre,LOSOM_STA_sheet[k],".xlsx")
# shell.exec(wksht_path)
##
LOSOM_FEB34_out_params <- dmstar_read_parameters_tab(wksht_path,ncells=2,
                                                     dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = LOSOM_STA_sheet[k])
LOSOM_FEB34_out_cell <- build_case_cells(LOSOM_FEB34_out_params)

LOSOM_FEB34_out_input <- read_series_input(wksht_path,base=c("Date","Flow","Conc"))
LOSOM_FEB34_out_input[,c("Rainfall","ET")] <- 0
LOSOM_FEB34_out_input <- LOSOM_FEB34_out_input|>
  mutate(Qi = cfs_to_hm3d(Flow),
         Ci = Conc,
         Rain = in_to_m(Rainfall),
         Et = in_to_m(ET),
         Qr0 = 0,
         Qr1 = 0,
         Qr2 = 0,
         Zcontrol = 0,
         Date = as.Date(Date),
         STA = LOSOM_STA_sheet[k]
  )

### LOSOM STA2B -----------------------------------------------------------------
k <- 3
LOSOM_STA_sheet[k]
wksht_path <- paste0(LOSOM_dat_path,sheet_name_pre,LOSOM_STA_sheet[k],".xlsx")
# shell.exec(wksht_path)
##
LOSOM_STA2B_param <- dmstar_read_parameters_tab(wksht_path,ncells=11,
                                                DutyCycle = 0.95,
                                                dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = LOSOM_STA_sheet[k])
LOSOM_STA2B_cell <- build_case_cells(LOSOM_STA2B_param)

LOSOM_STA2B_input <- read_series_input(wksht_path)
LOSOM_STA2B_input <- LOSOM_STA2B_input|>
  mutate(
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = LOSOM_STA_sheet[k]
  )

### LOSOM STA34 -----------------------------------------------------------------
k <- 4
LOSOM_STA_sheet[k]
wksht_path <- paste0(LOSOM_dat_path,sheet_name_pre,LOSOM_STA_sheet[k],".xlsx")
# shell.exec(wksht_path)
##
LOSOM_STA34_param <- dmstar_read_parameters_tab(wksht_path,ncells=6,
                                                DutyCycle = 0.95,
                                                dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = LOSOM_STA_sheet[k])
LOSOM_STA34_cell <- build_case_cells(LOSOM_STA34_param)

LOSOM_STA34_input <- read_series_input(wksht_path)
LOSOM_STA34_input <- LOSOM_STA34_input|>
  mutate(
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = LOSOM_STA_sheet[k]
  )

### LOSOM STA_A2 -----------------------------------------------------------------
k <- 5
LOSOM_STA_sheet[k]
wksht_path <- paste0(LOSOM_dat_path,sheet_name_pre,LOSOM_STA_sheet[k],".xlsx")
# shell.exec(wksht_path)
##
LOSOM_STA_A2_param <- dmstar_read_parameters_tab(wksht_path,ncells=6,
                                                 DutyCycle = 0.95,
                                                 dmsta_version = dmsta_ver_val)|>
  mutate(CaseName = LOSOM_STA_sheet[k])
LOSOM_STA_A2_cell <- build_case_cells(LOSOM_STA_A2_param)

LOSOM_STA_A2_input <- read_series_input(wksht_path)
LOSOM_STA_A2_input <- LOSOM_STA_A2_input|>
  mutate(
    Qi = cfs_to_hm3d(Flow),
    Ci = Conc,
    Rain = in_to_m(Rainfall),
    Et = in_to_m(ET),
    Qr0 = 0,
    Qr1 = 0,
    Qr2 = 0,
    Zcontrol = 0,
    Date = as.Date(Date),
    STA = LOSOM_STA_sheet[k]
  )


# Numeric Stability -------------------------------------------------------

## DMSTA -------------------------------------------------------------------
dmsta_dat_path # path on local machine 
sheet_name_pre <- "PROJECT_SFWMD_C240TSP_20180312.XLSM_STA34_"
runs <- paste0("RUN",1:5)

CEPP_STA34_numstab <- list()
for(k in seq_along(runs)){
  wksht_path <- paste0(dmsta_dat_path,sheet_name_pre,runs[k],".xlsx")
  dmsta_output<- readxl::read_xlsx(wksht_path,sheet = "Series_Overall",skip = 9,
                                   col_names = FALSE,progress=FALSE)|>as.data.frame()
  names(dmsta_output) <- series_colnames
  dmsta_output$Date <- as.Date(dmsta_output$Date,origin = "1900-01-01")
  dmsta_output$WY <- WY(dmsta_output$Date)
  dmsta_output$STA <- "STA34"
  dmsta_output$RUN <- runs[k]
  
  CEPP_STA34_numstab[[k]] <- dmsta_output
  print(k)
}
CEPP_STA34_numstab_da <- do.call(rbind,CEPP_STA34_numstab)

vars <- c("tot_bypass","release1","release2","seep_out","treated_out")
CEPP_STA34_numstab_da$Q_out_total <- rowSums(CEPP_STA34_numstab_da[,paste(vars,"Q",sep="_")],na.rm=T)
CEPP_STA34_numstab_da$L_out_total <- rowSums(CEPP_STA34_numstab_da[,paste(vars,"L",sep="_")],na.rm=T)
CEPP_STA34_numstab_da <- merge(CEPP_STA34_numstab_da,
                               data.frame(RUN = runs,
                                          Nstep = c(1L,2L,4L,8L,16L)),
                               "RUN")
vars <- c("RUN","Nstep","WY","STA","Date","Q_out_total","L_out_total","Depth")
CEPP_STA34_numstab_da_test <- CEPP_STA34_numstab_da[,vars]

# Export Files ------------------------------------------------------------
## DMSTA_results -----------------------------------------------------------
RS_dmsta_rslt2 <- do.call(rbind,RS_dmsta_rslt)
RS_dmsta_rslt2$PROJECT <- "RS"

werp_vars <- names(WERP_dmsta_rslt)[!(names(WERP_dmsta_rslt)%in%c( "Q_out_total",  "L_out_total"))]
WERP_dmsta_rslt2 <- WERP_dmsta_rslt[,werp_vars]
WERP_dmsta_rslt2$PROJECT <- "WERP"

CEPP_dmsta_rslt2 <- do.call(rbind,CEPP_dmsta_rslt)
CEPP_dmsta_rslt2$PROJECT <- "CEPP"

LOSOM_dmsta_rslt2 <- do.call(rbind,LOSOM_dmsta_rslt)
LOSOM_dmsta_rslt2$PROJECT <- "LOSOM"

dmsta_rslt_all <- rbind(
  RS_dmsta_rslt2,
  WERP_dmsta_rslt2,
  CEPP_dmsta_rslt2,
  LOSOM_dmsta_rslt2
)
# write.csv(dmsta_rslt_all,paste0(export.path,"dmsta_overallcase_all.csv"),row.names = FALSE)

## Input params ------------------------------------------------------------
params_files <- ls(pattern = ".*_param*")
lst <- mget(params_files)
lst <- Filter(is.data.frame,lst)
combined <- do.call(rbind,
                    lapply(names(lst), function(n) {
                      tmp <- lst[[n]]
                      tmp$PROJECT <- sapply(strsplit(n,"_"),"[",1)
                      tmp$source <- n
                      tmp
                    })
)
# write.csv(combined,paste0(export.path,"dmsta_inputparams_all.csv"),row.names = FALSE)

## Input data --------------------------------------------------------------
input_files <- ls(pattern = ".*input$")

lst <- mget(input_files)
lst <- Filter(is.data.frame,lst)

inputs_out <- lapply(names(lst), function(n) {
  tmp <- lst[[n]]
  tmp$PROJECT <- strsplit(n, "_")[[1]][1]
  tmp$source <- n
  tmp
})
names(inputs_out) <- names(lst)

# saveRDS(inputs_out, file = paste0(data.path,"dmstar_inputs.rds"))

## Numeric Stability -------------------------------------------------------



# END ---------------------------------------------------------------------


