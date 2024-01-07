#' Generate a data frame with random age, sex, and group variables
#'
#' This function generates a data frame with random age, sex, and group variables.
#'
#' @param n An integer value indicating the number of observations to generate.
#' @param create_errors A logical value indicating whether to generate errors in the data.
#'
#' @return A data frame with the generated variables.
#'
#' @examples
#' create_data()
#'
#' @import dplyr
#' @importFrom stats runif
#' @importFrom stats rnorm
#'
#' @export
create_data <- function(n = 100, create_errors = FALSE) {

  if(!create_errors){
    # Generate random age between 0 and 80
    unadjusted_age <- round(rnorm(n, mean = 35, sd = 20))
    age <- round(ifelse(unadjusted_age < 0 | unadjusted_age > 100, runif(length(unadjusted_age), 0, 100), unadjusted_age))

    # Randomly assign sex (Male or Female)
    sex <- sample(c("Male", "Female"), n, replace = TRUE)

    # Assign a group (Group A, Group B, or Group C)
    group <- sample(c("Group A", "Group B", "Group C"), n, replace = TRUE)

    # Return a list with the created variables
    data <- data.frame(age, sex, group)
  }

  if(create_errors){
    # Generate random age between -5 and 140
    age <- sample(-5:140, n, replace = TRUE)

    # Randomly assign sex (Male, Female, NA, or Wrongentry)
    sex <- sample(c("Männlich", "Male", "F", "Female", NA, "Wrongentry"), n, replace = TRUE)

    # Assign a group (Group A, Group B, Group C, or NA)
    group <- sample(c("Group A", "Group B", "Group C", NA), n, replace = TRUE)

    # Return a list with the created variables
    data <- data.frame(age, sex, group)
  }

  data
}


#' Plot a population pyramid
#'
#' This function creates a population pyramid using the ggplot2 package.
#'
#' @param age A numeric vector of ages.
#' @param sex A character vector of sexes.
#' @param agegroup A logical value indicating whether to group ages into chunks.
#' @param agegroup_size An integer value indicating the size of age groups. Defaults to 10.
#' @param filter_NA An logical value indicating wether to exclude NA values. Defaults to TRUE.
#'
#' @return A ggplot object.
#'
#' @examples
#' plot_agepyramide()
#'
#' @import ggplot2
#' @importFrom dplyr count
#' @importFrom dplyr mutate
#'
#' @export
plot_agepyramide <- function(age = create_data()$age,
                             sex = create_data()$sex,
                             agegroup = TRUE,
                             agegroup_size = 10,
                             filter_NA = TRUE) {



  sex = ifelse(sex %in% c("Male",
                          "male",
                          "Männlich",
                          "männlich",
                          "Maennlich",
                          "maennlich",
                          "M",
                          "m"), "Male", sex)

  sex = ifelse(sex %in% c("Female",
                          "female",
                          "weiblich",
                          "Weiblich",
                          "W",
                          "w",
                          "F",
                          "f"), "Female", sex)

  sex = ifelse(sex %in% c("Male", "Female"), sex, NA)


  if(agegroup){
    data <- data.frame(age, sex) %>%
      mutate(agegroup = cut(age, breaks = seq(0, 100, by = agegroup_size), right = FALSE)) %>%
      count(sex, age = agegroup)
  }

  if(!agegroup){
    data <- data.frame(age, sex) %>%
      count(sex, age)
  }


  if(filter_NA){
    data <- data %>%
      dplyr::filter(!is.na(sex)) %>%
      dplyr::filter(!is.na(age))
  }


  ggplot(data) +
    aes(x = age,
        fill = sex,
        y = ifelse(test = sex == "Male", yes = -n, no = n)) +
    geom_bar(stat = "identity") +
    scale_y_continuous(labels = abs, limits = max(data$n) * c(-1,1)) +
    scale_colour_manual(values = c("brown3", "steelblue"), aesthetics = c("colour", "fill")) +
    labs(title = "Population Pyramid", x = "Age", y = "Count") +
    coord_flip() +
    theme_classic()


}



