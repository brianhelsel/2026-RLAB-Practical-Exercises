data <- read.csv("RLABRForLifestyleAnd-ContactInformation_DATA_2026-06-05_1029.csv")

data$firstName <- purrr::map_chr(strsplit(data$name, " |  "), ~ .x[[1]])
data$lastName <- purrr::map_chr(strsplit(data$name, " |  "), ~ .x[[2]])


to_proper <- function(x) {
  s <- strsplit(x, " ")[[1]]
  x <- purrr::map_chr(
    s,
    ~ paste0(toupper(substr(.x, 1, 1)), substr(.x, 2, nchar(.x)))
  )
  paste(x, collapse = " ")
}

data <- data |>
  tibble::as.tibble() |>
  dplyr::arrange(lastName) |>
  dplyr::mutate(
    title = purrr::map_chr(title, to_proper),
    institution = ifelse(
      institution == "KUMC",
      "University of Kansas Medical Center",
      institution
    ),
    role = case_when(
      role == 1 ~ "Graduate Student",
      role == 2 ~ "Postdoctoral Fellow",
      TRUE ~ title
    )
  )

paste0(
  data$name,
  "\n", data$firstName, "is a ", data$role, " with the ", data$department, " at the ", data$institution
)

new_names <- data$name[!grepl("Brian Helsel|Julianne Clina", data$name)]

purrr::walk(
  gsub(" ", "", new_names),
  ~ {
    path <- file.path("/Users/bhelsel/Documents/GitHub/2026-RLAB-Practical-Exercises/attendees", .x, "code")
    dir.create(path)
  }
)
