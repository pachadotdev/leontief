library(magrittr)

# download ----
url_12 <- "http://si3.bcentral.cl/estadisticas/Principal1/Excel/CCNN/cdr/xls/CdeR2008_MIP_12x12.xlsx"
xlsx_12 <- "data-raw/mip_2008_bcch_12x12.xlsx"

if(!file.exists(xlsx_12)) {
  download.file(url_12, xlsx_12)
}

# input-output matrix ----
io12 <- readxl::read_excel("data-raw/mip_2008_bcch_12x12.xlsx",
                     sheet = "12x12_2008", range = "C12:AC23",
                     col_names = FALSE) %>% 
  janitor::remove_empty("cols") %>% 
  janitor::clean_names()

# names_12 <- readxl::read_excel("data-raw/mip_2008_bcch_12x12.xlsx",
#                         sheet = "Glosas", range = "C10:C21",
#                         col_names = FALSE) %>% 
#   janitor::clean_names() %>% 
#   dplyr::mutate(
#     x1 = stringr::str_to_lower(iconv(x1, from = "", to = "ASCII//TRANSLIT", sub = "")),
#     x1 = stringr::str_replace_all(x1, " |, |-", "_")
#   ) %>% 
#   purrr::as_vector()

names_12 <- c("agriculture", "fishing", "mining", "manufacturing_industry", "electricity_gas_water",
              "construction", "retail_hotels_restaurants", "transport_and_communications",
              "financial_and_business_services", "housing_services", "personal_services", "public_administration")

# names2_12 <- c("total_demanda_intermedia", "consumo_de_hogares", "consumo_de_ipsfl",
#                 "consumo_de_gobierno", "formacion_bruta_de_capital_fijo", "variacion_de_existencias",
#                 "exportaciones", "total_demanda_final")

names2_12 <- c("intermediate_total_demand", "household_consumption", "np_consumption",
               "government_consumption", "gross_fixed_capital_formation", "change_in_inventories",
               "exports", "total_final_demand")

names(io12) <- c(names_12, names2_12)

X <- io12 %>% 
  dplyr::select(names_12) %>% 
  data.matrix()

X[is.na(X)] <- 0

colnames(X) <- names_12
rownames(X) <- colnames(X)

input_output_12x12 <- X

# # intermediate demand ----
Xi <- io12 %>%
  dplyr::select(intermediate_total_demand) %>%
  data.matrix()

Xi[is.na(Xi)] <- 0

rownames(Xi) <- names_12

# # final demand ----
Xf <- io12 %>%
  dplyr::select(total_final_demand) %>%
  data.matrix()

Xf[is.na(Xf)] <- 0

rownames(Xf) <- names_12
 
# direct coefficients matrix ----
## demand table
Xt <- cbind(X, Xi, Xf)
Xt[is.na(Xt)] <- 0

demand_matrix_12x12 <- Xt
usethis::use_data(demand_matrix_12x12)

## identity
I <- diag(12)

## direct coefficients table
A <- X / t(matrix(Xf, 12, 12))
A[is.na(A)] <- 0
colnames(A) <- names_12
rownames(A) <- colnames(A)
 
# leontief inverse matrix ----
B <- solve(I-A)
B[is.na(B)] <- 0
colnames(B) <- names_12
rownames(B) <- colnames(B)

## system solution: $x = (I-A)^{-1}b = Lb$
Xe <- solve(B,Xf)
