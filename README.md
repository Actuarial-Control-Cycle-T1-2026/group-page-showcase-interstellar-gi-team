
# Actuarial Theory and Practice A

_"We Love Control Cycle." – George Washington_

---

## Instructions: How to Run the Code

The project is structured into three main folders:
- `00_DataPrep`
- `01_Pricing`
- `02_Capital`

Please follow the steps below in order.

---

# Folder 1: 00_DataPrep

## Step 1: Input Data
Download the input Excel files from **SOA_2026_Case_Study_Materials** and place them in: `00_DataPrep/RawData`

---

## Step 2: Raw to Intermediate Data

In: `00_DataPrep/Scripts` 
run: `Raw_to_Intermediate.R`
- This script converts raw data into intermediate datasets used in later stages.

---

## Step 3: Intermediate Processing

Still in: `00_DataPrep/Scripts`
- run the remaining **four R / Rmd scripts**, which further process the intermediate data for pricing and capital modelling.

---

# Folder 2: 01_Pricing

The order of subfolders (BI, Cargo, EQF, WC) does not matter.

---

## Business Interruption (BI)

Download the following scripts in the BI folder:

- `BI_freq_reconciliation.R`
- `BI_functions.R`  
- `BI_sev_reconciliation.R`  

Then run: `BusinessInterruption.Rmd`

---

## Cargo

Run: `Cargo.Rmd`

- All required functions are already included in the script.

---

## Equipment Failure (EQF)

Run: `EquipmentFailure.Rmd`

- All required functions are self-contained.

---

## Workers’ Compensation (WC)

Download the following scripts in the WC folder:

- `WC_NB_data_generator.R`  
- `WC_NB_data_generator_all.R`  
- `WC_NB_mapping_table.R`  
- `WC_freq_reconciliation.R`  
- `WC_functions.R`  
- `WC_sev_reconciliation.R`  

Then run: `WorkersComp.Rmd`

---

# Folder 3: 02_Capital

## Step 1: Input Files

Ensure aggregate loss files generated in Folder 2 follow this naming convention:

- `BI_base_agg_loss.rds` 
- `Cargo_base_agg_loss.rds`  
- `EF_base_agg_loss.rds`  
- `WC_base_agg_loss.rds`  

---

## Step 2: Run Capital Model

Run: `Copula.R`

This script:
- reads aggregate loss files
- models dependence using copulas
- produces capital and portfolio risk outputs




