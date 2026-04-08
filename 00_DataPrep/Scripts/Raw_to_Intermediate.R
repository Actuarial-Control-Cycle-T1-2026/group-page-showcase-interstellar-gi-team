########## Raw_to_Bronze.R ##########
# -----------------------------------------------------------------------------
# This script split different sheets from xlsx file and save tables individually in the intermediate output folder.
# -----------------------------------------------------------------------------

# 0. Import Packages
library(readxl)

# 1. Set Directory
raw_path <- "00_DataPrep/RawData/"
intermediate_path <- "00_DataPrep/IntermediateOutput/"

# 2 Import Data
## 2.1. Business Interruption Claim Data 
bi_claim_freq <- read_excel(paste0(raw_path, "srcsc-2026-claims-business-interruption.xlsx"), sheet = "freq")
bi_claim_sev <- read_excel(paste0(raw_path, "srcsc-2026-claims-business-interruption.xlsx"), sheet = "sev")

## 2.2. Cargo Claim Data 
cargo_claim_freq <- read_excel(paste0(raw_path, "srcsc-2026-claims-cargo.xlsx"), sheet = "freq")
cargo_claim_sev <- read_excel(paste0(raw_path, "srcsc-2026-claims-cargo.xlsx"), sheet = "sev")

## 2.3. Equipment Failure Claim Data 
eqf_claim_freq <- read_excel(paste0(raw_path, "srcsc-2026-claims-equipment-failure.xlsx"), sheet = "freq")
eqf_claim_sev <- read_excel(paste0(raw_path, "srcsc-2026-claims-equipment-failure.xlsx"), sheet = "sev")

## 2.4. Workers Comp Claim Data 
wc_claim_freq <- read_excel(paste0(raw_path, "srcsc-2026-claims-workers-comp.xlsx"), sheet = "freq")
wc_claim_sev <- read_excel(paste0(raw_path, "srcsc-2026-claims-workers-comp.xlsx"), sheet = "sev")

## 2.5 Inventory Data
inventory_equip <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A4:D10")

inventory_HC <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A14:G20")
inventory_BS <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A22:G28")
inventory_OD <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A30:G36")

inventory_risk_index <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A50:D56")
inventory_cargo <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A61:E67")

inventory_use_HC <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A40:C46")
inventory_use_BS <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A40:C46")
inventory_use_OD <- read_excel(paste0(raw_path, "srcsc-2026-cosmic-quarry-inventory.xlsx"),sheet = 1,range = "A40:C46")

inventory <- read_excel(paste0(raw_path, "inventory.xlsx"))


## 2.6. Personnel Data
personnel <- read_excel(paste0(raw_path, "personnel.xlsx"))

## 2.7. Economic Index Data
macro_econ_index <- read_excel(paste0(raw_path, "srcsc-2026-interest-and-inflation.xlsx"),sheet = 1,range = "A3:E18")

# 3. Export Data
write.csv(bi_claim_freq,
          paste0(getwd(), "/", intermediate_path, "bi_claim_freq.csv"),
          row.names = FALSE)

write.csv(bi_claim_sev,
          paste0(getwd(), "/", intermediate_path, "bi_claim_sev.csv"),
          row.names = FALSE)

write.csv(cargo_claim_freq,
          paste0(getwd(), "/", intermediate_path, "cargo_claim_freq.csv"),
          row.names = FALSE)

write.csv(cargo_claim_sev,
          paste0(getwd(), "/", intermediate_path, "cargo_claim_sev.csv"),
          row.names = FALSE)

write.csv(eqf_claim_freq,
          paste0(getwd(), "/", intermediate_path, "eqf_claim_freq.csv"),
          row.names = FALSE)

write.csv(eqf_claim_sev,
          paste0(getwd(), "/", intermediate_path, "eqf_claim_sev.csv"),
          row.names = FALSE)

write.csv(wc_claim_freq,
          paste0(getwd(), "/", intermediate_path, "wc_claim_freq.csv"),
          row.names = FALSE)

write.csv(wc_claim_sev,
          paste0(getwd(), "/", intermediate_path, "wc_claim_sev.csv"),
          row.names = FALSE)

write.csv(inventory_equip,
          paste0(getwd(), "/", intermediate_path, "inventory_equip.csv"),
          row.names = FALSE)

write.csv(inventory_HC,
          paste0(getwd(), "/", intermediate_path, "inventory_HC.csv"),
          row.names = FALSE)

write.csv(inventory_BS,
          paste0(getwd(), "/", intermediate_path, "inventory_BS.csv"),
          row.names = FALSE)

write.csv(inventory_OD,
          paste0(getwd(), "/", intermediate_path, "inventory_OD.csv"),
          row.names = FALSE)

write.csv(inventory_risk_index,
          paste0(getwd(), "/", intermediate_path, "inventory_risk_index.csv"),
          row.names = FALSE)

write.csv(inventory_cargo,
          paste0(getwd(), "/", intermediate_path, "inventory_cargo.csv"),
          row.names = FALSE)

write.csv(inventory_use_HC,
          paste0(getwd(), "/", intermediate_path, "inventory_use_HC.csv"),
          row.names = FALSE)

write.csv(inventory_use_BS,
          paste0(getwd(), "/", intermediate_path, "inventory_use_BS.csv"),
          row.names = FALSE)

write.csv(inventory_use_OD,
          paste0(getwd(), "/", intermediate_path, "inventory_use_OD.csv"),
          row.names = FALSE)

write.csv(inventory,
          paste0(getwd(), "/", intermediate_path, "inventory.csv"),
          row.names = FALSE)

write.csv(personnel,
          paste0(getwd(), "/", intermediate_path, "personnel.csv"),
          row.names = FALSE)

write.csv(macro_econ_index,
          paste0(getwd(), "/", intermediate_path, "macro_econ_index.csv"),
          row.names = FALSE)

