library(magrittr)

# download ----
url_12 <- "https://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/xls/CdeR2013_MIP_12x12.xlsx"
xlsx_12 <- "data-raw/mip_2013_bcch_12x12.xlsx"

if (!file.exists(xlsx_12)) {
  download.file(url_12, xlsx_12)
}

# input-output matrix ----
io12 <- readxl::read_excel(xlsx_12,
  sheet = "1", range = "C12:AD23",
  col_names = FALSE
) %>%
  janitor::remove_empty("cols") %>%
  janitor::clean_names()

wages12 <- readxl::read_excel(xlsx_12,
  sheet = "1", range = "C34:N34",
  col_names = FALSE
) %>%
  janitor::clean_names() %>%
  purrr::as_vector()

names_12 <- c(
  "agriculture_fishing", "mining", "manufacturing_industry", "electricity_gas_water",
  "construction", "retail_hotels_restaurants", "transport_communications_information",
  "financial_services", "real_estate", "business_services", "personal_services",
  "public_administration"
)

names2_12 <- c(
  "intermediate_total_demand", "household_consumption", "non_profit_consumption",
  "government_consumption", "gross_fixed_capital_formation", "change_in_inventories",
  "exports", "final_total_demand"
)

names(io12) <- c(names_12, names2_12)

X <- io12 %>%
  dplyr::select(names_12) %>%
  data.matrix()

X[is.na(X)] <- 0

rownames(X) <- names_12

input_output_12x12 <- X

# wage and demand ----

Y <- io12 %>%
  dplyr::select(names2_12) %>%
  data.matrix()

Y <- cbind(wages12, Y)

Y[is.na(Y)] <- 0
colnames(Y) <- c("wage", names2_12)

transaction_matrix <- X
usethis::use_data(transaction_matrix, overwrite = T)

wage_demand_matrix <- Y
usethis::use_data(wage_demand_matrix, overwrite = T)
