load("data/transaction_matrix.rda")

d <- wage_demand_matrix[, "final_total_demand"]

employment_matrix <- data.frame(
  employees = c(728257, 254724, 886182, 72168, 673366, 1804381,
                646006, 173259, 62611, 399681, 1677874, 407815),
  # just to check that the data is the same, it is
  # production = c(11304108, 26338440, 47308394, 9579204, 21102837, 30659058,
  #                26820947, 11225202, 12634289, 21681241, 21337323, 9026177),
  employment_coefficient = NA
)

employment_matrix <- as.matrix(employment_matrix)
employment_matrix[,2] <- employment_matrix[,1] / d
rownames(employment_matrix) <- rownames(transaction_matrix)

usethis::use_data(employment_matrix, overwrite = T)
