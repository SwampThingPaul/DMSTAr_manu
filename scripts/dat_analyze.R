## DMSTAr development and comparison
## Created by: Paul Julian (pjulian@evergladesfoundation.org)
## Created on: 2026-05-22

AnalystHelper:::clear_session()

## Libraries
library(AnalystHelper)
library(plyr)
library(reshape2)
library(readxl)

# Modeling
library(DMSTAr)

## Extra plotting and tables
library(patchwork)
library(ggplot2)
theme_set(theme_minimal(base_size = 16)+
            theme_bw()+
            theme(panel.spacing = grid::unit(0.8, "lines"),
                  panel.border = element_rect("black",fill=NA,linewidth=1)))
library(flextable)

## Paths
wd <- "C:/Julian_LaCie/_GitHub/DMSTAr_manu"
paths <- paste0(wd,c("/Plots/","/Data/"))

plot.path <- paths[1]
data.path <- paths[2]

# Functions
source(paste0(wd,"/src/func.R"))


# Read all data -----------------------------------------------------------

# input parameters for all simulations
dmsta_inputparam <- read.csv(paste0(data.path,"dmsta_inputparams_all.csv"))
# input data for all simulations as a list
input_dat <- readRDS(paste0(data.path,"dmstar_inputs.rds"))
# DMSTA results 
dmsta_results_all <- read.csv(paste0(data.path,"dmsta_overallcase_all.csv"))
# DMSTAr results
dmstar_results_all <- read.csv(paste0(data.path,"dmstar_overallcase_all.csv"))

# DMSTA Numeric stability REsults 
CEPP_STA34_numstab_da <- read.csv(paste0(data.path,"dmsta_numstab.csv"))
dmstar_numstab_runs <- read.csv(paste0(data.path,"dmstar_numstab.csv"))

# Restoration Strategies --------------------------------------------------
dmsta_ver_val <- "2E"

STA_sheet <- c("FEBS5A_N","FEB_S5A", "FEBS5A_OUT",
               "STA1_DW", "STA1W","STA1E","STA2B","FEB_34","FEB34_OUT", "STA34",
               "FEB_56","STA56_DW", "STA5", "STA6")

### FEBS5A_N ----------------------------------------------------------------
RS_FEBS5A_N_params <- subset(dmsta_inputparam,CaseName =="FEBS5A_N"&PROJECT=="RS")
RS_FEBS5A_N_cell <- build_case_cells(RS_FEBS5A_N_params)

RS_FEBS5A_N_input <- input_dat[["RS_FEBS5A_N_input"]]

### FEB_S5A ------------------------------------------------------------------
RS_FEB_S5A_params <- subset(dmsta_inputparam,CaseName =="FEB_S5A"&PROJECT=="RS")
RS_FEB_S5A_cell <- build_case_cells(RS_FEB_S5A_params)

RS_FEB_S5A_input <- input_dat[["RS_FEB_S5A_input"]]

### FEBS5A_OUT ------------------------------------------------------------------
RS_FEBS5A_out_params <- subset(dmsta_inputparam,CaseName =="FEBS5A_OUT"&PROJECT=="RS")
RS_FEBS5A_out_cell <- build_case_cells(RS_FEBS5A_out_params)

RS_FEBS5A_out_input <- input_dat[["RS_FEBS5A_out_input"]]

### STA1_DW -----------------------------------------------------------------
RS_STA1_DW_params <- subset(dmsta_inputparam,CaseName =="STA1_DW"&PROJECT=="RS")
RS_STA1_DW_cell <- build_case_cells(RS_STA1_DW_params)

RS_STA1_DW_input <- input_dat[["RS_STA1_DW_input"]]

### STA1W -------------------------------------------------------------------
RS_STA1W_params <- subset(dmsta_inputparam,CaseName =="STA1W"&PROJECT=="RS")
RS_STA1W_cell <- build_case_cells(RS_STA1W_params)

RS_STA1W_input <- input_dat[["RS_STA1W_input"]]

### STA1E -------------------------------------------------------------------
RS_STA1E_params <- subset(dmsta_inputparam,CaseName =="STA1E"&PROJECT=="RS")
RS_STA1E_cell <- build_case_cells(RS_STA1E_params)

RS_STA1E_input <- input_dat[["RS_STA1E_input"]]

### STA2B -------------------------------------------------------------------
RS_STA2B_params <- subset(dmsta_inputparam,CaseName =="STA2B"&PROJECT=="RS")
RS_STA2B_cell <- build_case_cells(RS_STA2B_params)

RS_STA2B_input <- input_dat[["RS_STA2B_input"]]

### FEB_34 ------------------------------------------------------------------
RS_FEB_34_params <- subset(dmsta_inputparam,CaseName =="FEB_34"&PROJECT=="RS")
RS_FEB_34_cell <- build_case_cells(RS_FEB_34_params)

RS_FEB_34_input <- input_dat[["RS_FEB_34_input"]]

### FEB34_OUT ---------------------------------------------------------------
RS_FEB34_out_params <- subset(dmsta_inputparam,CaseName =="FEB34_OUT"&PROJECT=="RS")
RS_FEB34_out_cell <- build_case_cells(RS_FEB34_out_params)

RS_FEB34_out_input <- input_dat[["RS_FEB34_out_input"]]

### STA34 -------------------------------------------------------------------
RS_STA34_params <- subset(dmsta_inputparam,CaseName =="STA34"&PROJECT=="RS")
RS_STA34_cell <- build_case_cells(RS_STA34_params)

RS_STA34_input <- input_dat[["RS_STA34_input"]]

### FEB_56 ------------------------------------------------------------------
RS_FEB_56_params <- subset(dmsta_inputparam,CaseName =="FEB_56"&PROJECT=="RS")
RS_FEB_56_cell <- build_case_cells(RS_FEB_56_params)

RS_FEB_56_input <- input_dat[["RS_FEB_56_input"]]

### STA56_DW ----------------------------------------------------------------
RS_STA56_DW_params <- subset(dmsta_inputparam,CaseName =="STA56_DW"&PROJECT=="RS")
RS_STA56_DW_cell <- build_case_cells(RS_STA56_DW_params)

RS_STA56_DW_input <- input_dat[["RS_STA56_DW_input"]]



### STA5 --------------------------------------------------------------------
RS_STA5_params <- subset(dmsta_inputparam,CaseName =="STA5"&PROJECT=="RS")
RS_STA5_cell <- build_case_cells(RS_STA5_params)

RS_STA5_input <- input_dat[["RS_STA5_input"]]

### STA6 --------------------------------------------------------------------
RS_STA6_params <- subset(dmsta_inputparam,CaseName =="STA6"&PROJECT=="RS")
RS_STA6_cell <- build_case_cells(RS_STA6_params)

RS_STA6_input <- input_dat[["RS_STA6_input"]]

## EC Network Case Sim -----------------------------------------------------------------
# 1) Build routes from a DMSTA-style network table
RS_EC_net_table_ls <- list()
RS_EC_net_table_ls[[1]] <- data.frame(CaseName = "FEBS5A_N", Bypass_to = "FEB_S5A", 
                                      Release1_to = "FEB_S5A" , Release2_to = 5, Outflow_to  = "FEB_S5A", Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[2]] <- data.frame(CaseName = "FEB_S5A", Bypass_to = "STA1_DW",
                                      Release1_to = "STA1_DW", Release2_to = 5, Outflow_to  = "FEBS5A_OUT", Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[3]] <- data.frame(CaseName = "FEBS5A_OUT", Bypass_to = "STA1_DW",
                                      Release1_to = NA, Release2_to = NA, Outflow_to  = "STA2B", Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[4]] <- data.frame(CaseName = "STA1_DW", Bypass_to = "STA1E",
                                      Release1_to = NA, Release2_to = NA, Outflow_to  = "STA1W", Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[5]] <- data.frame(CaseName = "STA1W", Bypass_to = 1,
                                      Release1_to = NA, Release2_to = NA, Outflow_to  = 1, Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[6]] <- data.frame(CaseName = "STA1E", Bypass_to = 2,
                                      Release1_to = NA, Release2_to = NA, Outflow_to  = 2, Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[7]] <- data.frame(CaseName = "FEB_34", Bypass_to = "STA34",
                                      Release1_to = "STA34", Release2_to = "STA2B", Outflow_to  = "FEB34_OUT", Seepage_to  = "FEB34_OUT",
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[8]] <- data.frame(CaseName = "FEB34_OUT", Bypass_to = "STA34",
                                      Release1_to = NA, Release2_to = NA, Outflow_to  = "STA2B", Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[9]] <- data.frame(CaseName = "STA2B", Bypass_to = 3,
                                      Release1_to = NA, Release2_to = NA, Outflow_to  = 3, Seepage_to  = NA,
                                      stringsAsFactors = FALSE)
RS_EC_net_table_ls[[10]] <- data.frame(CaseName = "STA34", Bypass_to = 4,
                                       Release1_to = NA, Release2_to = NA, Outflow_to  = 4, Seepage_to  = NA,
                                       stringsAsFactors = FALSE)
RS_EC_net_table <- do.call(rbind,RS_EC_net_table_ls)

RS_EC_routes <- build_routes_from_net_table(RS_EC_net_table, outlet_count = 5)

# test_block_date <- seq(as.Date("1965-05-01"),(as.Date("1965-05-01")+(2*365))-1,"1 days")
# test_block_val <- 1:length(test_block_date)
# [test_block_val,]
# 2) build case table (input and cell definitions)
RS_EC_cases <- list(
  FEBS5A_N =   list(series_base = RS_FEBS5A_N_input,   cells = RS_FEBS5A_N_cell),
  FEB_S5A =    list(series_base = RS_FEB_S5A_input,    cells = RS_FEB_S5A_cell),
  FEBS5A_OUT = list(series_base = RS_FEBS5A_out_input, cells = RS_FEBS5A_out_cell),
  STA2B =      list(series_base = RS_STA2B_input,      cells = RS_STA2B_cell),
  STA1_DW =    list(series_base = RS_STA1_DW_input,    cells = RS_STA1_DW_cell),
  STA1E =      list(series_base = RS_STA1E_input,      cells = RS_STA1E_cell),
  STA1W =      list(series_base = RS_STA1W_input,      cells = RS_STA1W_cell),
  FEB_34 =     list(series_base = RS_FEB_34_input,     cells = RS_FEB_34_cell),
  FEB34_OUT =  list(series_base = RS_FEB34_out_input,  cells = RS_FEB34_out_cell),
  STA34 =      list(series_base = RS_STA34_input,      cells = RS_STA34_cell)
)

EC_NStep_case <- c(
  FEBS5A_N = 12L
  # FEB_S5A = 4L,
  # FEBS5A_OUT = 4L,
  # STA2B = 4L,
  # STA1_DW = 4L,
  # STA1E = 4L,
  # STA1W = 4L,
  # FEB_34 = 4L,
  # FEB34_OUT = 4L,
  # STA34 = 4L
)
# 3) Run the network
ptm <- proc.time()
RS_EC_net_res <- run_network_of_cases(
  cases = RS_EC_cases,
  routes = RS_EC_routes,
  outlet_count = 5,
  Nsteps = 4L,       # passed through to dmsta_flowP_case()
  max_iter = 1L,      # passed through to dmsta_flowP_case()
  Nsteps_case = EC_NStep_case
)
proc.time() - ptm
beepr::beep(4)
## Full EC simulation 
# user  system elapsed 
# 6379.83  383.72 6800.05 
# user = CPU spent executing code ~1.77 hrs
# system = OS working ~ 6.4 minute
# elaspse = wall-clock time ~1.88 hrs

## W Network Case Sim ------------------------------------------------------
RS_W_net_table_ls <- list()
RS_W_net_table_ls[[1]] <- data.frame(CaseName = "FEB_56", Bypass_to = "STA56_DW", 
                                     Release1_to = "STA56_DW" , Release2_to = 5, Outflow_to  = "STA56_DW", Seepage_to  = "STA56_DW",
                                     stringsAsFactors = FALSE)
RS_W_net_table_ls[[2]] <- data.frame(CaseName = "STA56_DW", Bypass_to = "STA6", 
                                     Release1_to = NA , Release2_to = NA, Outflow_to  = "STA5", Seepage_to  = NA,
                                     stringsAsFactors = FALSE)
RS_W_net_table_ls[[3]] <- data.frame(CaseName = "STA5", Bypass_to = 1, 
                                     Release1_to = NA , Release2_to = NA, Outflow_to  = 1, Seepage_to  = NA,
                                     stringsAsFactors = FALSE)
RS_W_net_table_ls[[4]] <- data.frame(CaseName = "STA6", Bypass_to = 2, 
                                     Release1_to = NA , Release2_to = NA, Outflow_to  = 2, Seepage_to  = NA,
                                     stringsAsFactors = FALSE)
RS_W_net_table <- do.call(rbind,RS_W_net_table_ls)

RS_W_routes <- build_routes_from_net_table(RS_W_net_table, outlet_count = 5)
# 2) build case table (input and cell definitions)
RS_W_cases <- list(
  FEB_56   = list(series_base = RS_FEB_56_input,    cells = RS_FEB_56_cell),
  STA56_DW = list(series_base = RS_STA56_DW_input,  cells = RS_STA56_DW_cell),
  STA5     = list(series_base = RS_STA5_input,      cells = RS_STA5_cell),
  STA6     = list(series_base = RS_STA6_input,      cells = RS_STA6_cell)
)

W_NStep_case <- c(
  # FEB_56 = 4L,
  STA56_DW = 12L
  # STA5 = 4L,
  #STA6 = 4L
)

ptm <- proc.time()
# 3) Run the network
RS_W_net_res <- run_network_of_cases(
  cases = RS_W_cases,
  routes = RS_W_routes,
  outlet_count = 5,
  Nsteps = 4L,       # passed through to dmsta_flowP_case()
  max_iter = 1L,      # passed through to dmsta_flowP_case()
  Nsteps_case = W_NStep_case
)
proc.time() - ptm
beepr::beep(4)
## Full W simulation 
# user  system elapsed 
# 4521.04  272.25 4819.85 
# user = CPU spent executing code 1.26 hrs
# system = OS working 4 min
# elaspse = wall-clock time 1.34 

#### DMSTAr ------------------------------------------------------------------
sum_STAs <- c("STA1W","STA1E","STA2B","STA34","FEB_34","FEB_S5A")
RS_EC_dmstar_rslt_ls <- lapply(seq_along(sum_STAs),function(i){
  
  tmp <- RS_EC_net_res$case_results[[sum_STAs[i]]]
  tmp <- tmp$results$case
  tmp$WY <- WY(tmp$Date)
  tmp$STA <- sum_STAs[i]
  tmp
  # tmp$Q_out_treated <- with(tmp,ifelse(Q_out_treated<0.00025,0,Q_out_treated))# tinyflow
})
RS_EC_dmstar_rslt <- do.call(rbind,RS_EC_dmstar_rslt_ls)

sum_STAs <- c("FEB_56","STA5","STA6")
RS_W_dmstar_rslt_ls <- lapply(seq_along(sum_STAs),function(i){
  
  tmp <- RS_W_net_res$case_results[[sum_STAs[i]]]
  tmp <- tmp$results$case
  tmp$WY <- WY(tmp$Date)
  tmp$STA <- sum_STAs[i]
  tmp
})
RS_W_dmstar_rslt <- do.call(rbind,RS_W_dmstar_rslt_ls)

RS_dmstar_rslt <- rbind(RS_EC_dmstar_rslt,RS_W_dmstar_rslt)

RS_dmstar_WY <- ddply(RS_dmstar_rslt,c("WY","STA"),summarise,
                      NFlow = N.obs(Q_out_total),
                      TFlow = sum(Q_out_total,na.rm=T),
                      TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTAr",
         project = "RS")|>
  subset(NFlow>=365)
#### DMSTA -------------------------------------------------------------------
RS_dmsta_rslt_da <- subset(dmsta_results_all,
                           STA%in%c("STA1W","STA1E","STA2B","STA34","FEB_34","FEB_S5A","FEB_56","STA5","STA6")&
                             PROJECT == "RS")
vars <- c("tot_bypass","release1","release2","seep_out","treated_out")
RS_dmsta_rslt_da$Q_out_total <- rowSums(RS_dmsta_rslt_da[,paste(vars,"Q",sep="_")],na.rm=T)
RS_dmsta_rslt_da$L_out_total <- rowSums(RS_dmsta_rslt_da[,paste(vars,"L",sep="_")],na.rm=T)

RS_dmsta_WY <- RS_dmsta_rslt_da|>
  ddply(c("WY","STA"),summarise,
        NFlow = N.obs(Q_out_total),
        TFlow = sum(Q_out_total,na.rm=T),
        TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTA",
         project = "RS")|>
  subset(NFlow>=365)

# WERP --------------------------------------------------------------------
dmsta_ver_val <- "2C2B"
WERP_NF_params <- subset(dmsta_inputparam,CaseName =="NF STA"&PROJECT=="WERP")
WERP_NF_cell <- build_case_cells(WERP_NF_params)

WERP_NF_input <- input_dat[["WERP_NF_input"]]

## Simualtion (no network)
WERP_NF_res <- dmsta_flowP_case(
  series = WERP_NF_input,
  cells  = WERP_NF_cell,
  Nsteps = 4L,
  max_iter = 1L
)
WERP_dmstar_rslt <- WERP_NF_res$results$case
WERP_dmstar_rslt$WY <- WY(WERP_dmstar_rslt$Date)
WERP_dmstar_rslt$STA <- "NF"

## DMSTA --------------------------------------------------------
WERP_dmsta_rslt <- subset(dmsta_results_all,STA=="NF"&PROJECT == "WERP")
vars <- c("tot_bypass","release1","release2","seep_out","treated_out")
WERP_dmsta_rslt$Q_out_total <- rowSums(WERP_dmsta_rslt[,paste(vars,"Q",sep="_")],na.rm=T)
WERP_dmsta_rslt$L_out_total <- rowSums(WERP_dmsta_rslt[,paste(vars,"L",sep="_")],na.rm=T)

WERP_dmsta_WY <- ddply(WERP_dmsta_rslt,c("WY","STA"),summarise,
                       NFlow = N.obs(Q_out_total),
                       TFlow = sum(Q_out_total,na.rm=T),
                       TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTA",
         project = "WERP")|>
  subset(NFlow>=365)

## DMSTAr ------------------------------------------------------------------
WERP_dmstar_WY <- ddply(WERP_dmstar_rslt,c("WY","STA"),summarise,
                        NFlow = N.obs(Q_out_total),
                        TFlow = sum(Q_out_total,na.rm=T),
                        TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTAr",
         project = "WERP")|>
  subset(NFlow>=365)

# CEPP PACR ---------------------------------------------------------------
dmsta_ver_val <- "2C2B"

CEPP_STA_sheet <- c("A1","A2","A2_DW","A2_DW2","STA_A2","STA2B","STA34")

### CEPP A1 -----------------------------------------------------------------
CEPP_A1_params <- subset(dmsta_inputparam,CaseName =="A1"&PROJECT=="CEPP")
CEPP_A1_cell <- build_case_cells(CEPP_A1_params)

CEPP_A1_input <- input_dat[["CEPP_A1_input"]]

### CEPP A2 -----------------------------------------------------------------
CEPP_A2_params <- subset(dmsta_inputparam,CaseName =="A2"&PROJECT=="CEPP")
CEPP_A2_cell <- build_case_cells(CEPP_A2_params)

CEPP_A2_input <- input_dat[["CEPP_A2_input"]]

### CEPP A2_DW -----------------------------------------------------------------
CEPP_A2_DW_params <- subset(dmsta_inputparam,CaseName =="A2_DW"&PROJECT=="CEPP")
CEPP_A2_DW_cell <- build_case_cells(CEPP_A2_DW_params)

CEPP_A2_DW_input <- input_dat[["CEPP_A2_DW_input"]]

### CEPP A2_DW2 -----------------------------------------------------------------
CEPP_A2_DW2_params <- subset(dmsta_inputparam,CaseName =="A2_DW2"&PROJECT=="CEPP")
CEPP_A2_DW2_cell <- build_case_cells(CEPP_A2_DW2_params)

CEPP_A2_DW2_input <- input_dat[["CEPP_A2_DW2_input"]]

### CEPP STA_A2 -----------------------------------------------------------------
CEPP_STA_A2_params <- subset(dmsta_inputparam,CaseName =="STA_A2"&PROJECT=="CEPP")
CEPP_STA_A2_cell <- build_case_cells(CEPP_STA_A2_params)

CEPP_STA_A2_input <- input_dat[["CEPP_STA_A2_input"]]

### CEPP STA2B -----------------------------------------------------------------
CEPP_STA2B_params <- subset(dmsta_inputparam,CaseName =="STA2B"&PROJECT=="CEPP")
CEPP_STA2B_cell <- build_case_cells(CEPP_STA2B_params)

CEPP_STA2B_input <- input_dat[["CEPP_STA2B_input"]]

### CEPP STA34 -----------------------------------------------------------------
CEPP_STA34_param <- subset(dmsta_inputparam,CaseName =="STA34"&PROJECT=="CEPP")
CEPP_STA34_cell <- build_case_cells(CEPP_STA34_param)

CEPP_STA34_input <- input_dat[["CEPP_STA34_input"]]

## CEPP - Network Sim ------------------------------------------------------
CEPP_net_table_ls <- list()
CEPP_net_table_ls[[1]] <- data.frame(
  CaseName = "A2", Bypass_to = "A1", 
  Release1_to = "A2_DW" , Release2_to = "A2_DW2", Outflow_to  = 5, Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table_ls[[2]] <- data.frame(
  CaseName = "A2_DW", Bypass_to = "A1", 
  Release1_to = NA , Release2_to = NA, Outflow_to  = "STA_A2", Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table_ls[[3]] <- data.frame(
  CaseName = "A1", Bypass_to = "A2_DW2", 
  Release1_to = "STA34" , Release2_to = "STA2B", Outflow_to  = NA, Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table_ls[[4]] <- data.frame(
  CaseName = "A2_DW2", Bypass_to = "STA34", 
  Release1_to = NA , Release2_to = NA, Outflow_to  = "STA2B", Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table_ls[[5]] <- data.frame(
  CaseName = "STA_A2", Bypass_to = "STA34", 
  Release1_to = NA , Release2_to = NA, Outflow_to  = 3, Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table_ls[[6]] <- data.frame(
  CaseName = "STA34", Bypass_to = 4, 
  Release1_to = NA , Release2_to = NA, Outflow_to  = 2, Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table_ls[[7]] <- data.frame(
  CaseName = "STA2B", Bypass_to = 4, 
  Release1_to = NA , Release2_to = NA, Outflow_to  = 1, Seepage_to  = NA,
  stringsAsFactors = FALSE)
CEPP_net_table <- do.call(rbind,CEPP_net_table_ls)

CEPP_routes <- build_routes_from_net_table(CEPP_net_table, outlet_count = 5)

# 2) build case table (input and cell definitions)
CEPP_cases <- list(
  A2     = list(series_base = CEPP_A2_input,     cells = CEPP_A2_cell),
  A2_DW  = list(series_base = CEPP_A2_DW_input,  cells = CEPP_A2_DW_cell),
  A1     = list(series_base = CEPP_A1_input,     cells = CEPP_A1_cell),
  A2_DW2 = list(series_base = CEPP_A2_DW2_input, cells = CEPP_A2_DW2_cell),
  STA_A2 = list(series_base = CEPP_STA_A2_input, cells = CEPP_STA_A2_cell),
  STA34  = list(series_base = CEPP_STA34_input,  cells = CEPP_STA34_cell),
  STA2B  = list(series_base = CEPP_STA2B_input,  cells = CEPP_STA2B_cell)
)

# 3) Run the network
ptm <- proc.time()
CEPP_net_res <- run_network_of_cases(
  cases = CEPP_cases,
  routes = CEPP_routes,
  outlet_count = 5,
  Nsteps = 4L,
  max_iter = 1L
)
proc.time() - ptm
beepr::beep(4)
## Full EC simulation 
# user  system elapsed 
# 3808.55  422.40 4260.39 
# user = CPU spent executing code ~1.05 hrs
# system = OS working ~ 7.04 minute
# elaspse = wall-clock time ~1.18 hrs

#### DMSTAr ------------------------------------------------------------------
sum_STAs <- c("A1","A2","STA_A2","STA2B","STA34")
CEPP_dmstar_rslt_ls <- lapply(seq_along(sum_STAs),function(i){
  
  tmp <- CEPP_net_res$case_results[[sum_STAs[i]]]
  tmp <- tmp$results$case
  tmp$WY <- WY(tmp$Date)
  tmp$STA <- sum_STAs[i]
  tmp
})
CEPP_dmstar_rslt <- do.call(rbind,CEPP_dmstar_rslt_ls)

CEPP_dmstar_WY <- ddply(CEPP_dmstar_rslt,c("WY","STA"),summarise,
                        NFlow = N.obs(Q_out_total),
                        TFlow = sum(Q_out_total,na.rm=T),
                        TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTAr",
         project = "CEPP")|>
  subset(NFlow>=365)
range(CEPP_dmstar_WY$WY)

#### DMSTA -------------------------------------------------------------------
CEPP_dmsta_rslt_da <- subset(dmsta_results_all,
                           STA%in%c("A1","A2","STA_A2","STA2B","STA34")&
                             PROJECT == "CEPP")
vars <- c("tot_bypass","release1","release2","seep_out","treated_out")
CEPP_dmsta_rslt_da$Q_out_total <- rowSums(CEPP_dmsta_rslt_da[,paste(vars,"Q",sep="_")],na.rm=T)
CEPP_dmsta_rslt_da$L_out_total <- rowSums(CEPP_dmsta_rslt_da[,paste(vars,"L",sep="_")],na.rm=T)

CEPP_dmsta_WY <- CEPP_dmsta_rslt_da|>
  ddply(c("WY","STA"),summarise,
        NFlow = N.obs(Q_out_total),
        TFlow = sum(Q_out_total,na.rm=T),
        TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTA",
         project = "CEPP")|>
  subset(NFlow>=365)
range(CEPP_dmsta_WY$WY)

# LOSOM PA25 --------------------------------------------------------------
dmsta_ver_val <- "2C2B"

LOSOM_STA_sheet <- c("FEB_34","FEB34_OUT","STA2B","STA34","STA_A2")

### LOSOM FEB_34 -----------------------------------------------------------------
LOSOM_FEB34_params <- subset(dmsta_inputparam,CaseName =="FEB_34"&PROJECT=="LOSOM")
LOSOM_FEB34_cell <- build_case_cells(LOSOM_FEB34_params)

LOSOM_FEB34_input <- input_dat[["LOSOM_FEB34_input"]]

### LOSOM FEB34_OUT -----------------------------------------------------------------
LOSOM_FEB34_out_params <- subset(dmsta_inputparam,CaseName =="FEB34_OUT"&PROJECT=="LOSOM")
LOSOM_FEB34_out_cell <- build_case_cells(LOSOM_FEB34_out_params)

LOSOM_FEB34_out_input <- input_dat[["LOSOM_FEB34_out_input"]]

### LOSOM STA2B -----------------------------------------------------------------
LOSOM_STA2B_param <- subset(dmsta_inputparam,CaseName =="STA2B"&PROJECT=="LOSOM")
LOSOM_STA2B_cell <- build_case_cells(LOSOM_STA2B_param)

LOSOM_STA2B_input <- input_dat[["LOSOM_STA2B_input"]]

### LOSOM STA34 -----------------------------------------------------------------
LOSOM_STA34_param <- subset(dmsta_inputparam,CaseName =="STA34"&PROJECT=="LOSOM")
LOSOM_STA34_cell <- build_case_cells(LOSOM_STA34_param)

LOSOM_STA34_input <- input_dat[["LOSOM_STA34_input"]]


### LOSOM STA_A2 -----------------------------------------------------------------
LOSOM_STA_A2_param <- subset(dmsta_inputparam,CaseName =="STA_A2"&PROJECT=="LOSOM")
LOSOM_STA_A2_cell <- build_case_cells(LOSOM_STA_A2_param)

LOSOM_STA_A2_input <- input_dat[["LOSOM_STA_A2_input"]]

## LOSOM - Network Sim ------------------------------------------------------
LOSOM_net_table_ls <- list()
LOSOM_net_table_ls[[1]] <- data.frame(
  CaseName = "FEB_34", Bypass_to = "STA34", 
  Release1_to = "STA34" , Release2_to = "STA2B", Outflow_to  = "STA34", Seepage_to  = "STA34",
  stringsAsFactors = FALSE)
LOSOM_net_table_ls[[2]] <- data.frame(
  CaseName = "FEB34_OUT", Bypass_to = "STA34", 
  Release1_to = NA , Release2_to = NA, Outflow_to  = "STA2B", Seepage_to  = NA,
  stringsAsFactors = FALSE)
LOSOM_net_table_ls[[3]] <- data.frame(
  CaseName = "STA2B", Bypass_to = 3, 
  Release1_to = NA , Release2_to = NA, Outflow_to  = 1, Seepage_to  = NA,
  stringsAsFactors = FALSE)
LOSOM_net_table_ls[[4]] <- data.frame(
  CaseName = "STA34", Bypass_to = 4, 
  Release1_to = NA , Release2_to = NA, Outflow_to  = 2, Seepage_to  = NA,
  stringsAsFactors = FALSE)
LOSOM_net_table_ls[[5]] <- data.frame(
  CaseName = "STA_A2", Bypass_to = 5, 
  Release1_to = NA , Release2_to = NA, Outflow_to  = 5, Seepage_to  = NA,
  stringsAsFactors = FALSE)
LOSOM_net_table <- do.call(rbind,LOSOM_net_table_ls)

LOSOM_routes <- build_routes_from_net_table(LOSOM_net_table, outlet_count = 5)

# 2) build case table (input and cell definitions)
LOSOM_cases <- list(
  FEB_34    = list(series_base = LOSOM_FEB34_input,     cells = LOSOM_FEB34_cell),
  FEB34_OUT = list(series_base = LOSOM_FEB34_out_input, cells = LOSOM_FEB34_out_cell),
  STA2B     = list(series_base = LOSOM_STA2B_input,     cells = LOSOM_STA2B_cell),
  STA34     = list(series_base = LOSOM_STA34_input,     cells = LOSOM_STA34_cell),
  STA_A2    = list(series_base = LOSOM_STA_A2_input,    cells = LOSOM_STA_A2_cell)
)

# 3) Run the network
ptm <- proc.time()
LOSOM_net_res <- run_network_of_cases(
  cases = LOSOM_cases,
  routes = LOSOM_routes,
  outlet_count = 5,
  Nsteps = 4L,
  max_iter = 1L
)
proc.time() - ptm
beepr::beep(4)
## Full EC simulation 
# user  system elapsed 
# 8239.54  704.58 9012.22 
# user = CPU spent executing code ~2.29 hrs
# system = OS working ~ 11.74 minute
# elaspse = wall-clock time ~2.50 hrs


#### DMSTAr ------------------------------------------------------------------
sum_STAs <- c("FEB_34","STA2B","STA34","STA_A2")
LOSOM_dmstar_rslt_ls <- lapply(seq_along(sum_STAs),function(i){
  tmp <- LOSOM_net_res$case_results[[sum_STAs[i]]]
  tmp <- tmp$results$case
  tmp$WY <- WY(tmp$Date)
  tmp$STA <- sum_STAs[i]
  tmp
})
LOSOM_dmstar_rslt <- do.call(rbind,LOSOM_dmstar_rslt_ls)

LOSOM_dmstar_WY <- ddply(LOSOM_dmstar_rslt,c("WY","STA"),summarise,
                         NFlow = N.obs(Q_out_total),
                         TFlow = sum(Q_out_total,na.rm=T),
                         TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTAr",
         project = "LOSOM")|>
  subset(NFlow>=365)
range(LOSOM_dmstar_WY$WY)
#### DMSTA -------------------------------------------------------------------
LOSOM_dmsta_rslt_da <- subset(dmsta_results_all,
                             STA%in%c("FEB_34","STA2B","STA34","STA_A2")&
                               PROJECT == "CEPP")
vars <- c("tot_bypass","release1","release2","seep_out","treated_out")
LOSOM_dmsta_rslt_da$Q_out_total <- rowSums(LOSOM_dmsta_rslt_da[,paste(vars,"Q",sep="_")],na.rm=T)
LOSOM_dmsta_rslt_da$L_out_total <- rowSums(LOSOM_dmsta_rslt_da[,paste(vars,"L",sep="_")],na.rm=T)

LOSOM_dmsta_WY <- LOSOM_dmsta_rslt_da|>
  ddply(c("WY","STA"),summarise,
        NFlow = N.obs(Q_out_total),
        TFlow = sum(Q_out_total,na.rm=T),
        TLoad = sum(L_out_total,na.rm=T))|>
  mutate(FWM = TLoad/TFlow,
         method = "DMSTA",
         project = "LOSOM")|>
  subset(NFlow>=365)
range(LOSOM_dmsta_WY$WY)

# WY results --------------------------------------------------------------
dmstar_WY <- rbind(
  RS_dmstar_WY,
  WERP_dmstar_WY,
  CEPP_dmstar_WY,
  LOSOM_dmstar_WY
)

dmsta_WY <- rbind(
  RS_dmsta_WY,
  WERP_dmsta_WY,
  CEPP_dmsta_WY,
  LOSOM_dmsta_WY
)

dmsta_dmstar_WY <- rbind(dmstar_WY,dmsta_WY)
dmsta_dmstar_WY_melt <- melt(dmsta_dmstar_WY,id.vars = c("WY","STA","method", "project"))

dmsta_dmstar_WY2 <- subset(dmsta_dmstar_WY_melt, variable!="NFlow")|>
  dcast(STA+project+WY+variable~method,id.vars = "value",mean)


## Time Series Plots -------------------------------------------------------

### RS ----------------------------------------------------------------------
## STAs
tmp1 <- subset(dmsta_dmstar_WY2,
               project == "RS"&
                 STA%in%c("STA1W","STA1E","STA2B","STA34","STA5","STA6"))|>
  mutate(
    STA = factor(STA, levels = c("STA1E", "STA1W", "STA2B", "STA34", "STA5", "STA6")),
    variable = factor(variable, levels = c("TFlow","TLoad","FWM"))
  )

RS1_flow <- make_model_plot(tmp1,"TFlow",
                            y_lab = expression(Total~Discharge~(hm^3~yr^{-1})),
                            show_legend = FALSE
)+
  labs(title = "Restoration Strategies")+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

RS1_load <- make_model_plot(tmp1,"TLoad",
                            y_lab = expression(Total~TP~Load~(kg~yr^{-1})),
                            show_legend = FALSE
)+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

RS1_fwm <- make_model_plot(tmp1,"FWM",
                           y_lab = expression(TP~FWM~(mu*g~L^{-1})),
                           show_legend = TRUE
) +
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )+
  scale_y_continuous(
    limits = c(5,20),
    breaks = seq(5,20,5),
    labels = seq(5,20,5)
  )

RS_STA_timeseries <- RS1_flow + RS1_load + RS1_fwm + plot_layout(ncol = 3, widths = c(1, 1, 1))
RS_STA_timeseries

## FEBs
tmp2 <- subset(dmsta_dmstar_WY2,
               project == "RS"&
                 STA%in%c("FEB_S5A","FEB_34","FEB_56"))|>
  mutate(
    STA = factor(STA, levels = c("FEB_S5A","FEB_34","FEB_56")),
    variable = factor(variable, levels = c("TFlow","TLoad","FWM"))
  )

RS2_flow <- make_model_plot(tmp2,"TFlow",
                            y_lab = expression(Total~Discharge~(hm^3~yr^{-1})),
                            show_legend = FALSE
)+
  labs(title = "Restoration Strategies")+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

RS2_load <- make_model_plot(tmp2,"TLoad",
                            y_lab = expression(Total~TP~Load~(kg~yr^{-1})),
                            show_legend = FALSE
)+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

RS2_fwm <- make_model_plot(tmp2,"FWM",
                           y_lab = expression(TP~FWM~(mu*g~L^{-1})),
                           show_legend = TRUE
) +
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

RS_FEBs_timeseries <- RS2_flow + RS2_load + RS2_fwm + plot_layout(ncol = 3, widths = c(1, 1, 1))

### WERP --------------------------------------------------------------------
tmp_WERP <- subset(dmsta_dmstar_WY2,
                   project == "WERP")|>
  mutate(
    variable = factor(variable, levels = c("TFlow","TLoad","FWM"))
  )

werp_flow <- make_model_plot(tmp_WERP,"TFlow",
                             y_lab = expression(Total~Discharge~(hm^3~yr^{-1})),
                             show_legend = FALSE
)+
  labs(title = "WERP")+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

werp_load <- make_model_plot(tmp_WERP,"TLoad",
                             y_lab = expression(Total~TP~Load~(kg~yr^{-1})),
                             show_legend = FALSE
)+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

werp_fwm <- make_model_plot(tmp_WERP,"FWM",
                            y_lab = expression(TP~FWM~(mu*g~L^{-1})),
                            show_legend = TRUE
) +
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )+
  scale_y_continuous(
    limits = c(5,20),
    breaks = seq(5,20,5),
    labels = seq(5,20,5)
  )

WERP_timeseries <- werp_flow + werp_load + werp_fwm + plot_layout(ncol = 3, widths = c(1, 1, 1))

### CEPP --------------------------------------------------------------------
tmp_CEPP <- subset(dmsta_dmstar_WY2,
                   project == "CEPP"&
                     STA%in% c("A1","A2","STA_A2","STA2B","STA34"))|>
  mutate(
    STA = factor(STA, levels =  c("A1","A2","STA_A2","STA2B","STA34")),
    variable = factor(variable, levels = c("TFlow","TLoad","FWM"))
  )

cepp_flow <- make_model_plot(tmp_CEPP, "TFlow",
                             y_lab = expression(Total~Discharge~(hm^3~yr^{-1})),
                             show_legend = FALSE
)+
  labs(title = "CEPP PACR - C240")+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

cepp_load <- make_model_plot(tmp_CEPP, "TLoad",
                             y_lab = expression(Total~TP~Load~(kg~yr^{-1})),
                             show_legend = FALSE
)+
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

cepp_fwm <- make_model_plot(tmp_CEPP, "FWM",
                            y_lab = expression(TP~FWM~(mu*g~L^{-1})),
                            show_legend = TRUE
) +
  scale_x_continuous(
    breaks = seq(1966,2005,10),
    labels = seq(1966,2005,10)
  )

CEPP_STA_timeseries <- cepp_flow + cepp_load + cepp_fwm + plot_layout(ncol = 3, widths = c(1, 1, 1))

### LOSOM -------------------------------------------------------------------
tmp_LOSOM <- subset(dmsta_dmstar_WY2,
                    project == "LOSOM"&
                      STA%in%c("FEB_34","STA2B","STA34","STA_A2"))|>
  mutate(
    STA = factor(STA, levels = c("FEB_34","STA2B","STA34","STA_A2")),
    variable = factor(variable, levels = c("TFlow","TLoad","FWM"))
  )

x_bks <- seq(1966,2016,15)
losom_flow <- make_model_plot(tmp_LOSOM,"TFlow",
                              y_lab = expression(Total~Discharge~(hm^3~yr^{-1})),
                              show_legend = FALSE
)+
  labs(title = "LOSOM - PA25")+
  scale_x_continuous(breaks = x_bks)

losom_load <- make_model_plot(tmp_LOSOM,"TLoad",
                              y_lab = expression(Total~TP~Load~(kg~yr^{-1})),
                              show_legend = FALSE
)+
  scale_x_continuous(breaks = x_bks)

losom_fwm <- make_model_plot(tmp_LOSOM,"FWM",
                             y_lab = expression(TP~FWM~(mu*g~L^{-1})),
                             show_legend = TRUE
) +
  scale_x_continuous(breaks = x_bks)

LOSOM_STA_timeseries <- losom_flow + losom_load + losom_fwm + plot_layout(ncol = 3, widths = c(1, 1, 1))

## WY comparison -----------------------------------------------------------
dmsta_dmstar_WY2 <- dmsta_dmstar_WY2|>
  mutate(
    mean_val = (DMSTAr + DMSTA)/2,
    diff_val = (DMSTAr - DMSTA),
    project = factor(project, levels = c("RS","CEPP","WERP","LOSOM"))
  )
dmsta_dmstar_WY2$diff_pct <- with(dmsta_dmstar_WY2,
                                  ifelse(mean_val == 0, NA, 100 * (DMSTAr - DMSTA) / mean_val)
)

tab_params <- c("project","variable","n","bias","mad","rmse","mean_pct","pct_within_1pct")
## by variable (globally)
vars <- c("TFlow","TLoad","FWM")
agreement_by_variable <- lapply(seq_along(vars),function(i){
  tmp <- subset(dmsta_dmstar_WY2,variable==vars[i])
  rslt <- agreement_metrics_fun(tmp)
  rslt$variable  <-  vars[i]
  rslt
}
)
agreement_by_variable <- do.call(rbind,agreement_by_variable)
agreement_by_variable$project <-  "Overall"
agreement_by_variable[,tab_params]

vars <- unique(dmsta_dmstar_WY2[,c("variable","project")])
agreement_by_project_variable <- lapply(seq_along(vars[,1]),function(i){
  tmp <- subset(dmsta_dmstar_WY2,variable == vars[i,"variable"]&project == vars[i,"project"] )
  rslt <- agreement_metrics_fun(tmp)
  rslt$variable  <-  vars[i,"variable"]
  rslt$project <- vars[i,"project"]
  rslt
}
)
agreement_by_project_variable <- do.call(rbind,agreement_by_project_variable)
agreement_by_project_variable[,tab_params]

sumtab1 <- rbind(agreement_by_variable[,tab_params],
                 agreement_by_project_variable[,tab_params])

sumtab1$project <- factor(sumtab1$project,levels = c("Overall","RS","CEPP","WERP","LOSOM"))
sumtab1[order(sumtab1$project),]


# Make explicit grouping variable
dmsta_dmstar_WY2$project_variable <- interaction(
  dmsta_dmstar_WY2$project,
  dmsta_dmstar_WY2$variable,
  drop = TRUE,
  sep = "___"
)
# Split by project-variable combination
ba_list <- split(
  dmsta_dmstar_WY2,
  dmsta_dmstar_WY2$project_variable
)
# Apply function to each group
ba_mat <- do.call(
  rbind,
  lapply(ba_list, ba_stats_fun)
)
# Build data.frame without relying on rownames()
ba_stats <- data.frame(
  project_variable = names(ba_list),
  ba_mat,
  row.names = NULL
)
# Split project_variable back into project and variable
tmp <- strsplit(as.character(ba_stats$project_variable), "___", fixed = TRUE)

ba_stats$project <- vapply(tmp, `[`, character(1), 1)
ba_stats$variable <- vapply(tmp, `[`, character(1), 2)
ba_stats$project <- factor(
  ba_stats$project,
  levels = c("RS", "CEPP", "WERP", "LOSOM")
)
# Reorder columns
ba_stats <- ba_stats[
  c("project", "variable", "bias_pct", "loa_low_pct", "loa_high_pct")
]
ba_stats

### Bland Altman -----------------------------------------------------
vars <- unique(dmsta_dmstar_WY2[,c("variable","project","STA")])
length(vars[vars$variable=="TFlow",]$STA)

sta_project_list <- unique(dmsta_dmstar_WY2[,c("project","STA")])

sta_project_list$project_sta <- interaction(
  sta_project_list$project,
  sta_project_list$STA,
  sep = " | ",
  drop = TRUE
)

sta_project_list$project <- factor(sta_project_list$project,levels = c("RS","CEPP","WERP","LOSOM"))
sta_project_list <- sta_project_list[order(sta_project_list$project),] 

n_cols <- length(project_sta_levels)
project_sta_cols <- grDevices::hcl.colors(
  n = n_cols,
  palette = "Set 3"
)|>
  adjustcolor(0.5)
names(project_sta_cols) <- sta_project_list$project_sta
project_sta_cols

dmsta_dmstar_WY2$project_sta <- interaction(
  dmsta_dmstar_WY2$project,
  dmsta_dmstar_WY2$STA,
  sep = " | ",
  drop = TRUE
)
dmsta_dmstar_WY2$project_sta <- factor(
  dmsta_dmstar_WY2$project_sta,
  levels = sta_project_list$project_sta
)
dmsta_dmstar_WY2$plot_col <- project_sta_cols[
  dmsta_dmstar_WY2$project_sta
]



var_vals <- c("TFlow","TLoad","FWM")
proj_vals <- c("RS","CEPP","WERP","LOSOM")
proj_vals_labs <- c("Restoration Strategies","CEPP","WERP","LOSOM")
STA_vals <- list()
STA_vals[["RS"]] <- c("STA1E","STA1W","STA2B","STA34","STA5",'STA6',"FEB_S5A","FEB_34","FEB_56")
STA_vals[["CEPP"]] <- c("A1","A2","STA2B","STA34","STA_A2")
STA_vals[["WERP"]] <- c("NF")
STA_vals[["LOSOM"]] <- c("STA2B","STA34","STA_A2","FEB_34")

par(mar=c(1.5,1.75,0.25,0.5),oma=c(2,2,0.5,0.25));
layout(matrix(c(1:12,rep(13,4)),4,4,byrow = FALSE),widths = c(1,1,1,0.75))
ylim.val <- c(-2,2);by.y <- 1; 
ymaj <- seq(ylim.val[1],ylim.val[2],by.y); ymin <- seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val <- c(10,2000); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")

for(i in seq_along(proj_vals)){
  tmp <- subset(dmsta_dmstar_WY2,project==proj_vals[i] & variable == var_vals[1])
  mean_CI <- ba_stats_fun(tmp)
  
  plot(diff_pct~mean_val,tmp,ylim=ylim.val,xlim = xlim.val,
       type = "n",ann = FALSE, axes = FALSE,log="x")
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  abline(h=0)
  points(diff_pct~mean_val,tmp,pch=21,lwd = 0.1, cex= 1.25,
         bg = tmp$plot_col,col=adjustcolor("black",0.5))
  abline(h = mean_CI,col = c("red","grey25","grey25"),lty = c(1,2,2))
  axis_fun(1,xmaj,xmin,xmaj,line = -0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==4){mtext(side = 1,line=2,cex = 0.8,
                 expression("Total Discharge (hm"^"3"~"yr"^"-1"*")"))}
}
mtext(side = 2,outer = TRUE, "Percent Difference (%)")

ylim.val <- c(-4,4);by.y <- 2; 
ymaj <- seq(ylim.val[1],ylim.val[2],by.y); ymin <- seq(ylim.val[1],ylim.val[2],by.y/2)
xlim.val <- c(50,30000); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")
for(i in seq_along(proj_vals)){
  tmp <- subset(dmsta_dmstar_WY2,project==proj_vals[i] & variable == var_vals[2])
  mean_CI <- ba_stats_fun(tmp)
  
  plot(diff_pct~mean_val,tmp,ylim=ylim.val,xlim = xlim.val,
       type = "n",ann = FALSE, axes = FALSE,log="x")
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  abline(h=0)
  points(diff_pct~mean_val,tmp,pch=21,lwd = 0.1, cex= 1.25,
         bg = tmp$plot_col,col=adjustcolor("black",0.5))
  abline(h = mean_CI,col = c("red","grey25","grey25"),lty = c(1,2,2))
  axis_fun(1,xmaj,xmin,xmaj,line = -0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==4){mtext(side = 1,line=2,cex = 0.8,
                 expression("Total Load (kg"~"yr"^"-1"*")"))}
}

xlim.val <- c(2,200); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")
for(i in seq_along(proj_vals)){
  tmp <- subset(dmsta_dmstar_WY2,project==proj_vals[i] & variable == var_vals[3])
  mean_CI <- ba_stats_fun(tmp)
  
  plot(diff_pct~mean_val,tmp,ylim=ylim.val,xlim = xlim.val,
       type = "n",ann = FALSE, axes = FALSE,log="x")
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  abline(h=0)
  points(diff_pct~mean_val,tmp,pch=21,lwd = 0.1, cex= 1.25,
         bg = tmp$plot_col,col=adjustcolor("black",0.5))
  abline(h = mean_CI,col = c("red","grey25","grey25"),lty = c(1,2,2))
  axis_fun(1,xmaj,xmin,xmaj,line = -0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side = 4, line = 0.25,adj = 0.5,
        cex = c(0.7,0.8,0.8,0.8)[i],
        proj_vals_labs[i])
  if(i==4){mtext(side = 1,line=2,cex = 0.8,
                 expression("TP FWM ("*mu*"g L"^"-1"*")"))}
}

plot(0:1,0:1,type="n",ann = FALSE, axes = FALSE)
legend("center",
       legend = c(as.character(sta_project_list$project_sta),"Mean","95% CI"),
       pch = c(rep(21,length(sta_project_list$project_sta)),NA,NA),
       lty = c(rep(0,length(sta_project_list$project_sta)),1,2),
       lwd = c(rep(0.5,length(sta_project_list$project_sta)),1.5,1.5),
       col = c(rep(adjustcolor("black",0.5),length(sta_project_list$project_sta)),"red","grey25"),
       pt.bg=c(project_sta_cols,NA,NA),
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1
)


par(mar=c(1.5,2.5,0.5,1),oma=c(2,2,1.5,0.25));
layout(matrix(c(1:12,rep(13,4)),4,4,byrow = FALSE),widths = c(1,1,1,0.75))

xlim.val <- c(10,3000); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")
ylim.val <- xlim.val; ymaj <- xmaj; ymin <- xmin
for(i in seq_along(proj_vals)){
  tmp <- subset(dmsta_dmstar_WY2,project==proj_vals[i] & variable == var_vals[1])
  
  plot(DMSTAr~DMSTA,tmp,ylim=ylim.val,xlim = xlim.val,
       type = "n",ann = FALSE, axes = FALSE,log="xy")
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(DMSTAr~DMSTA,tmp,pch=21,lwd = 0.1, cex= 1.25,
         bg = tmp$plot_col,col=adjustcolor("black",0.5))
  abline(0,1,col="red",lty = 2)
  axis_fun(1,xmaj,xmin,xmaj,line = -0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i==1){mtext(side = 3,cex = 0.8,
                 expression("Total Discharge (hm"^"3"~"yr"^"-1"*")"))}
}
mtext(side = 2,outer = TRUE,line=0.5, "DMSTAr Value",cex = 0.8)

xlim.val <- c(50,50000); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")
ylim.val <- xlim.val; ymaj <- xmaj; ymin <- xmin
for(i in seq_along(proj_vals)){
  tmp <- subset(dmsta_dmstar_WY2,project==proj_vals[i] & variable == var_vals[2])
  
  plot(DMSTAr~DMSTA,tmp,ylim=ylim.val,xlim = xlim.val,
       type = "n",ann = FALSE, axes = FALSE,log="xy")
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(DMSTAr~DMSTA,tmp,pch=21,lwd = 0.1, cex= 1.25,
         bg = tmp$plot_col,col=adjustcolor("black",0.5))
  abline(0,1,col="red",lty = 2)
  axis_fun(1,xmaj,xmin,xmaj,line = -0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  if(i == 1){mtext(side = 3,cex = 0.8,
                   expression("Total Load (kg"~"yr"^"-1"*")"))}
  if(i == 4){
    mtext(side = 1,line=2,cex = 0.8,"DMSTA Value")
  }
}

xlim.val <- c(2,200); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")
ylim.val <- xlim.val; ymaj <- xmaj; ymin <- xmin
for(i in seq_along(proj_vals)){
  tmp <- subset(dmsta_dmstar_WY2,project==proj_vals[i] & variable == var_vals[3])
  
  plot(DMSTAr~DMSTA,tmp,ylim=ylim.val,xlim = xlim.val,
       type = "n",ann = FALSE, axes = FALSE,log="xy")
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(DMSTAr~DMSTA,tmp,pch=21,lwd = 0.1, cex= 1.25,
         bg = tmp$plot_col,col=adjustcolor("black",0.5))
  abline(0,1,col="red",lty = 2)
  axis_fun(1,xmaj,xmin,xmaj,line = -0.5)
  axis_fun(2,ymaj,ymin,ymaj);box(lwd=1)
  mtext(side = 4, line = 0.25,adj = 0.5,
        cex = c(0.7,0.8,0.8,0.8)[i],
        proj_vals_labs[i])
  if(i == 1){mtext(side = 3,cex = 0.8,
                   expression("TP FWM ("*mu*"g L"^"-1"*")"))}
  
}
par(mar=c(1.5,1,0.25,1))
plot(0:1,0:1,type="n",ann = FALSE, axes = FALSE)
legend("center",
       legend = c(as.character(sta_project_list$project_sta),"1:1 Line"),
       pch = c(rep(21,length(sta_project_list$project_sta)),NA),
       lty = c(rep(0,length(sta_project_list$project_sta)),2),
       lwd = c(rep(0.5,length(sta_project_list$project_sta)),1.5),
       col = c(rep(adjustcolor("black",0.5),length(sta_project_list$project_sta)),"red"),
       pt.bg=c(project_sta_cols,NA,NA),
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1
)
dev.off()


# Numeric stability -------------------------------------------------------
## Using CEPP PACR modeling we tested the numeric stability of DMSTA (to compare to DMSTAr)
## For this effort we used STA-3/4, limited the simulation to a 10 WY simulation (full sim period Jan 1 1965 to Dec 31 1976) and varied integration step per day
## STA34_Run1 = 1 steps per day (1 day relative time) Purpose: Coarsest practical daily integration (euler like integration)
## STA34_Run2 = 2 steps per day (0.5 day relative time) Purpose: Intermediate
## STA34_Run3 (base) = 4 steps per day (0.25 day relative time) Purpose: Current configuration
## STA34_Run4 = 8 steps per day (0.125 day relative time) Purpose: Finer Integration
## STA34_Run5 = 16 steps per day (0.0625 day relative time) Purpose: Near-reference solution (?)


## DMSTAr ------------------------------------------------------------------
numstab_CEPP_STA34_input <- subset(CEPP_STA34_input,Date<as.Date("1977-01-01"))
range(numstab_CEPP_STA34_input$Date)

Nsteps_vec <- c(1L,2L,4L,8L,16L)

Nsteps_vec <- c(1L, 2L, 4L, 8L, 16L)

dmstar_numstab_runs_ls <- lapply(Nsteps_vec, function(n) {
  tmp <- dmsta_flowP_case(
    series = numstab_CEPP_STA34_input,
    cells  = CEPP_STA34_cell,
    Nsteps = n
  )
  tmp <- tmp$results$case
  tmp$STA <- "STA34"
  tmp$Nstep <- n
  tmp
}
)
dmstar_numstab_runs <- do.call(rbind,dmstar_numstab_runs_ls)
dmstar_numstab_runs <- merge(dmstar_numstab_runs,
                             data.frame(RUN = runs,
                                        Nstep = c(1L,2L,4L,8L,16L)),
                             "Nstep")
dmstar_numstab_runs$WY <- WY(dmstar_numstab_runs$Date)

## DMSTA -------------------------------------------------------------------
CEPP_STA34_numstab_da
vars <- c("RUN","Nstep","WY","STA","Date","Q_out_total","L_out_total","Depth")
CEPP_STA34_numstab_da_test <- CEPP_STA34_numstab_da[,vars]
range(CEPP_STA34_numstab_da_test$WY); # focus on 1966 - 1977

## 
DMSTA_daily <- subset(CEPP_STA34_numstab_da_test,WY%in%seq(1966,1976,1))
DMSTA_conv_results <- convergence_order(DMSTA_daily,reference_steps = 4,
                                        dat_vars = c("Date", "Q_out_total", "L_out_total","Depth"))

DMSTA_WY <- ddply(DMSTA_daily,c("WY","Nstep"),summarise,
                  TFlow = sum(Q_out_total),
                  TLoad = sum(L_out_total),
                  AvgDepth = mean(Depth))
DMSTA_WY_conv_results <- convergence_order(DMSTA_WY,reference_steps = 4,
                                           by.var = "WY",
                                           dat_vars = c("WY", "TFlow", "TLoad","AvgDepth"),
                                           ref_vars = c("WY", "Q_ref", "load_ref","depth_ref"),
                                           test_vars = c("WY", "Q_test", "load_test","depth_test"))

## 
DMSTAr_daily <- subset(dmstar_numstab_runs,WY%in%seq(1966,1976,1))
DMSTAr_conv_results <- convergence_order(DMSTAr_daily,reference_steps = 4,
                                         dat_vars = c("Date","Q_out_total","L_out_total","Z_end_cm"))

DMSTA_conv_results$method <- "DMSTA"
DMSTAr_conv_results$method <- "DMSTAr"
DMSTA_DMSTAr_conv_results <- rbind(DMSTA_conv_results,DMSTAr_conv_results)

DMSTA_DMSTAr_conv_results <- melt(DMSTA_DMSTAr_conv_results,id.vars = c("steps_per_day","method"))
DMSTA_DMSTAr_conv_results$variable <- as.character(DMSTA_DMSTAr_conv_results$variable)

DMSTA_DMSTAr_conv_results$metric <- sub("_(?=[^_]+$).*", "", DMSTA_DMSTAr_conv_results$variable, perl = TRUE)
DMSTA_DMSTAr_conv_results$var <- sub(".*_(?=[^_]+$)", "", DMSTA_DMSTAr_conv_results$variable, perl = TRUE)


var_val_lab <- c("Discharge","Load","Depth")
par(mar=c(1.5,2.5,0.5,1),oma=c(2,2,1.5,0.25));
layout(matrix(1:9,3,3,byrow = TRUE))
xlim.val <- c(1,16); xmaj <- log.scale.fun(xlim.val,"major"); xmin <- log.scale.fun(xlim.val,"minor")

for(i in seq_along(var_vals)){
  tmp <- subset(DMSTA_DMSTAr_conv_results,var == var_vals[i]&metric == "nrmse")
  ylim.val <- range(tmp$value)*c(0.7,1.3); ymaj <- log.scale.fun(ylim.val,"major"); ymin <- log.scale.fun(ylim.val,"minor")

  plot(value~steps_per_day,tmp, type="n",
       log="xy",ann = FALSE,axes = FALSE,ylim = ylim.val,xlim=xlim.val)
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(value~steps_per_day,subset(tmp,method == "DMSTA"),pch=19,col = adjustcolor("black",0.5),cex = 1.25)
  points(value~steps_per_day,subset(tmp,method == "DMSTAr"),pch=19,col = adjustcolor("red",0.5),cex = 1.25)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  mtext(side=3,var_val_lab[i])
  if(i==1){
    mtext(side=2,line=2,"NRMSE")
    legend("bottomleft",
           legend = c("DMSTA","DMSTAr"),
           pch = c(19),
           lty = c(0),
           lwd = c(0.1),
           col = adjustcolor(c("black","red"),0.5),
           pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1.1,x.intersp=0.75,xpd=NA,xjust=0,yjust=1
    )
  }
  mtext(side=3,adj=0.99,line=-1.25,LETTERS[i],font=2,cex = 1)
  
}
for(i in seq_along(var_vals)){
  tmp <- subset(DMSTA_DMSTAr_conv_results,var == var_vals[i]&metric == "mae")
  ylim.val <- range(tmp$value)*c(0.7,1.3); ymaj <- log.scale.fun(ylim.val,"major"); ymin <- log.scale.fun(ylim.val,"minor")
  
  plot(value~steps_per_day,tmp, type="n",
       log="xy",ann = FALSE,axes = FALSE,ylim = ylim.val,xlim=xlim.val)
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(value~steps_per_day,subset(tmp,method == "DMSTA"),pch=19,col = adjustcolor("black",0.5),cex = 1.25)
  points(value~steps_per_day,subset(tmp,method == "DMSTAr"),pch=19,col = adjustcolor("red",0.5),cex = 1.25)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  if(i==1){
    mtext(side=2,line=2,"MAE")
  }
  mtext(side=3,adj=0.99,line=-1.25,LETTERS[i+3],font=2,cex = 1)
  
}
for(i in seq_along(var_vals)){
  tmp <- subset(DMSTA_DMSTAr_conv_results,var == var_vals[i]&metric == "max_error")
  ylim.val <- range(tmp$value)*c(0.7,1.3); ymaj <- log.scale.fun(ylim.val,"major"); ymin <- log.scale.fun(ylim.val,"minor")
  
  plot(value~steps_per_day,tmp, type="n",
       log="xy",ann = FALSE,axes = FALSE,ylim = ylim.val,xlim=xlim.val)
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(value~steps_per_day,subset(tmp,method == "DMSTA"),pch=19,col = adjustcolor("black",0.5),cex = 1.25)
  points(value~steps_per_day,subset(tmp,method == "DMSTAr"),pch=19,col = adjustcolor("red",0.5),cex = 1.25)
  axis_fun(2,ymaj,ymin,ymaj)
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  if(i==1){
    mtext(side=2,line=2,"Max Error")
  }
  mtext(side=3,adj=0.99,line=-1.25,LETTERS[i+6],font=2,cex = 1)
  
}
mtext(side=1,outer=T,"Steps Per Day")
dev.off()

## compare DMSTA and DMSTAr
DMSTA_daily2 <- DMSTA_daily[,c("Date","WY","Nstep", "Q_out_total", "L_out_total","Depth")]
names(DMSTA_daily2) <- c("Date","WY","Nstep", "Q", "TPL","Depth")
DMSTA_daily2$method <- "DMSTA"

DMSTAr_daily2 <- DMSTAr_daily[,c("Date","WY","Nstep","Q_out_total","L_out_total","Z_end_cm")]
names(DMSTAr_daily2) <- c("Date","WY","Nstep", "Q", "TPL","Depth")
DMSTAr_daily2$method <- "DMSTAr"

DMSTA_DMSTAr_daily <- rbind(DMSTA_daily2,DMSTAr_daily2)|>
  melt(id.vars = c("Date","WY","Nstep","method"))|>
  dcast(Date+WY+Nstep+variable~method,value.var = "value",mean)

DMSTA_DMSTAr_daily$abs_error <- with(DMSTA_DMSTAr_daily,abs(DMSTAr - DMSTA))

error_Nstep_dmsta_dmstar <- ddply(DMSTA_DMSTAr_daily,c("variable","Nstep"),summarise,
                                  nrmse = 100 * sqrt(mean(abs_error^2))/mean(DMSTAr),
                                  mae = mean(abs_error),
                                  mad = median(abs_error),
                                  max_error = max(abs_error))

var_vals <- c("Q","TPL","Depth")
var_val_lab <- c("Discharge","Load","Depth")

par(mar=c(1.5,2.5,0.5,1),oma=c(2,2,1.5,0.25));
layout(matrix(1:9,3,3,byrow = TRUE))

xlim.val <- c(0,17);by.x <- 4; xmaj <- seq(xlim.val[1],xlim.val[2],by.x); xmin <- seq(xlim.val[1],xlim.val[2],by.x/2)
for(i in seq_along(var_vals)){
  tmp <- subset(error_Nstep_dmsta_dmstar,variable == var_vals[i])
  ymaj <- labeling::heckbert(min(tmp$nrmse),max(tmp$nrmse),3); ymin <- seq(min(ymaj),max(ymaj),min(diff(ymaj))/2)
  ylim.val <- range(ymaj)
  
  plot(nrmse~Nstep,tmp, type="n",
       ann = FALSE,axes = FALSE,ylim = ylim.val,xlim=xlim.val)
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(nrmse~Nstep,tmp,pch=21,bg = adjustcolor("dodgerblue1",0.5),cex = 1.25)
  axis_fun(2,ymaj,ymin,format(ymaj))
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  mtext(side=3,var_val_lab[i])
  if(i==1){
    mtext(side=2,line=3,"NRMSE")
  }
  mtext(side=3,adj=0.99,line=-1.25,LETTERS[i],font=2,cex = 1)
}
for(i in seq_along(var_vals)){
  tmp <- subset(error_Nstep_dmsta_dmstar,variable == var_vals[i])
  ymaj <- labeling::heckbert(min(tmp$mae),max(tmp$mae),3); ymin <- seq(min(ymaj),max(ymaj),min(diff(ymaj))/2)
  ylim.val <- range(ymaj)
  
  plot(mae~Nstep,tmp, type="n",
       ann = FALSE,axes = FALSE,ylim = ylim.val,xlim=xlim.val)
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(mae~Nstep,tmp,pch=21,bg = adjustcolor("dodgerblue1",0.5),cex = 1.25)
  axis_fun(2,ymaj,ymin,format(ymaj*1000))
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  if(i==1){
    mtext(side=2,line=3,"MAE (x1000)")
  }
  mtext(side=3,adj=0.99,line=-1.25,LETTERS[i+3],font=2,cex = 1)
  
}
for(i in seq_along(var_vals)){
  tmp <- subset(error_Nstep_dmsta_dmstar,variable == var_vals[i])
  ymaj <- labeling::heckbert(min(tmp$max_error)*0.9,max(tmp$max_error)*1.1,4); ymin <- seq(min(ymaj),max(ymaj),min(diff(ymaj))/2)
  ylim.val <- range(ymaj)
  
  plot(mae~Nstep,tmp, type="n",
       ann = FALSE,axes = FALSE,ylim = ylim.val,xlim=xlim.val)
  abline(h = ymaj,v = xmaj, lty=3,col="grey",lwd=0.5)
  points(max_error~Nstep,tmp,pch=21,bg = adjustcolor("dodgerblue1",0.5),cex = 1.25)
  axis_fun(2,ymaj,ymin,format(round(ymaj*10,2)))
  axis_fun(1,xmaj,xmin,xmaj,line=-0.5)
  box(lwd=1)
  if(i==1){
    mtext(side=2,line=3,"Max Error (x10)")
  }
  mtext(side=3,adj=0.99,line=-1.25,LETTERS[i+6],font=2,cex = 1)
  
}
mtext(side=1,outer=T,"Steps Per Day")
dev.off()
