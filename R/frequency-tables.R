
#' Summarise coding frequency
#'
#' @description calculate frequency table for coding frequency.
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'

summarise_code_freq <- function(data) {

  # Validation checks
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq")
  }

  data$code_freq <- factor(data$code_freq, levels = c("Never",
                                                      "Rarely",
                                                      "Sometimes",
                                                      "Regularly",
                                                      "All the time"))

  freqs <- data.frame(table(data$code_freq))

  colnames(freqs) <- c("Coding frequency", "Count")
  return(freqs)
}


#' Summarise data operations
#'
#' @description calculate frequency table for data operations
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom rlang .data

summarise_operations <- function(data) {

  questions <- c("ops_analysis", "ops_cleaning", "ops_linking", "ops_transfer_migration", "ops_vis", "ops_machine_learning", "ops_modelling", "ops_QA")
  responses <- c("I do some or all of this by coding", "I do this without coding", "I don't do this")
  labels <- c("Data analysis", "Data cleaning", "Data linking", "Data transfer / migration", "Data visualisation", "Machine learning", "Modelling", "Quality assurance")

  frequencies <- create_tidy_freq_table(data, questions, responses, labels, order=TRUE)

  return(frequencies)

}


#' Summarise coding tools
#'
#' @description calculate frequency table coding tools (knowledge or access)
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#' @param type type of table (knowledge or access)
#'
#' @return frequency table (data.frame)
#'

summarise_coding_tools <- function(data, type = list("knowledge", "access")) {
  type <- match.arg(type, several.ok = TRUE)

  selected_data <- data[grepl(paste0(type, "_"), colnames(data))]

  frequencies <- data.frame(apply(selected_data, 2, function(x) {
    x <- factor(x, levels = c("Yes", "Don't Know", "No"))

    table(x)
  }))

  frequencies <- frequencies[order(colnames(frequencies))]

  languages <- c(
    "C++ / C#",
    "Java / Scala",
    "Javascript / Typescript",
    "Matlab",
    "Python",
    "R",
    "SAS",
    "SPSS",
    "SQL",
    "Stata",
    "VBA"
  )

  frequencies <- data.frame("Programming language" = languages, t(frequencies), check.names = FALSE)

  rownames(frequencies) <- NULL

  frequencies <- frequencies %>%
    tidyr::pivot_longer(Yes:No, names_to = "Response", values_to = "Count") %>%
    dplyr::arrange("Programming language", "Response")

  return(frequencies)
}


#' @title Summarise where respondents learned to code
#'
#' @description calculate frequency table of where respondents learned to code
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'

summarise_where_learned_code <- function(data){

  # Validation checks
  if (!"first_learned" %in% colnames(data)) {
    stop("unexpected_input: no column called 'first_learned")
  }
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq")
  }
  if (!"prev_coding_experience" %in% colnames(data)) {
    stop("unexpected_input: no column called 'prev_coding_experience")
  }

  #
  data$first_learned[(is.na(data$prev_coding_experience) | (data$prev_coding_experience == "No")) &
                               data$code_freq != "Never"] <- "In current role"

  levels = c("In current role",
             "In education",
             "In private sector employment",
             "In public sector employment",
             "Self-taught",
             "Other")

  data$first_learned[!is.na(data$first_learned) &
                               !(data$first_learned %in% levels)] <- "Other"

  data$first_learned <- factor(data$first_learned, levels = levels)

  freqs <- data.frame(table(data$first_learned))

  colnames(freqs) <- c("First coding experience", "Count")

  return(freqs)
}


#' @title Summarise data practices questions
#'
#' @description calculate frequency table for data practices
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom rlang .data

summarise_coding_practices <- function(data) {

  selected_data <- dplyr::select(data, .data$prac_use_open_source:.data$prac_AQUA_book)

  levels <- c("I don't understand this question",
              "Never",
              "Rarely",
              "Sometimes",
              "Regularly",
              "All the time")

  labels <- c("I use open source software when programming",
              "My team open sources its code",
              "I use a source code version control system e.g. Git",
              "Code my team writes is reviewed by a colleague",
              "I write repetitive elements in my code as functions",
              "I unit test my code",
              "I collect my code and supporting material into packages",
              "I follow a standard directory structure when programming",
              "I follow coding guidelines or style guides when programming",
              "I write code to automatically quality assure data",
              "My team applies the principles set out in the Aqua book when carrying out analysis as code")

  frequencies <- calc_multi_col_freqs(data = selected_data, levels = levels, labels = labels)

  colnames(frequencies) <- c("Question", levels)

  frequencies <- frequencies %>%
    tidyr::pivot_longer("I don't understand this question":"All the time", names_to = "Response", values_to = "Count") %>%
    dplyr::arrange("Question", "Response")

  return(frequencies)

}


#' @title Knowledge of RAP
#'
#' @description Create a frequency table of knowledge of RAP
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_rap_knowledge <- function(data){

  # Validation checks
  if (!"heard_of_RAP" %in% colnames(data)) {
    stop("unexpected_input: no column called 'heard_of_RAP")
  }

  data$RAP_knowledge[data$heard_of_RAP == "No"] <- "Have not heard of RAP"

  data$RAP_knowledge <- factor(data$RAP_knowledge, levels = c(
    "Have not heard of RAP",
    "I don't know what a RAP champion is",
    "I know what a RAP champion is but don't know who the RAP champion in my department is",
    "I know what a RAP champion is and there is no RAP champion in my department",
    "I know who the RAP champion in my department is"
  ))

  rap_knowledge <- data.frame(table(data$RAP_knowledge))

  colnames(rap_knowledge) <- c("RAP champion knowledge", "Count")

  rap_knowledge[1] <- c("Have not heard of RAP",
                        "Heard of RAP, have not heard of RAP champions",
                        "Heard of RAP, does not know department champion",
                        "Heard of RAP champions, no champion in department",
                        "Knows department RAP champion")

  rap_knowledge[[1]] <- factor(rap_knowledge[[1]], levels = rap_knowledge[[1]])

  return(rap_knowledge)
}


#' @title Opinions of RAP
#'
#' @description Create frequency table of opinions of RAP
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_rap_opinions <- function(data) {

  # Validation checks
  if (!"heard_of_RAP" %in% colnames(data)) {
    stop("unexpected_input: no column called 'heard_of_RAP")
  }

  opinion_rap_data <- data[data$heard_of_RAP == "Yes", ]
  opinion_rap_data <- dplyr::select(opinion_rap_data, "RAP_confident":"RAP_planning_to_implement")

  levels = c("Strongly Disagree",
             "Disagree",
             "Neutral",
             "Agree",
             "Strongly Agree")

  labels = c("I feel confident implementing RAP in my work",
             "I feel supported to implement RAP in my work",
             "I know where to find resources to help me implement RAP",
             "I understand what the key components of the RAP methodology are",
             "I think it is important to implement RAP in my work",
             "I and/or my team are currently implementing RAP",
             "I or my team are planning on implementing RAP in the next 12 months")

  freq_rap_opinions <- calc_multi_col_freqs(opinion_rap_data, levels = levels, labels = labels)

  colnames(freq_rap_opinions) <- c("Question",
                                   "Strongly disagree",
                                   "Disagree",
                                   "Neutral",
                                   "Agree",
                                   "Strongly agree")

  freq_rap_opinions <- freq_rap_opinions %>%
    tidyr::pivot_longer("Strongly disagree":"Strongly agree", names_to = "Response", values_to = "Count") %>%
    dplyr::arrange("Question", "Response")

  return(freq_rap_opinions)

}


#' @title Frequency of documentation use
#'
#' @description Create frequency table of documentation use
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_doc <- function(data) {

  # Validation checks
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq")
  }

  documentation_data <- data[data$code_freq != "Never", ]
  documentation_data <- dplyr::select(documentation_data, "doc_comments":"doc_flow_charts")

  levels = c("I don't understand this question",
             "Never",
             "Rarely",
             "Sometimes",
             "Regularly",
             "All the time")

  labels = c("Code comments",
             "Documentation for each function or class",
             "README files",
             "Desk notes",
             "Analytical Quality Assurance (AQA) logs",
             "Data or assumptions registers",
             "Flow charts")


  freq_documentation_data <- calc_multi_col_freqs(documentation_data, levels = levels, labels = labels)

  colnames(freq_documentation_data) <- c("Question", levels)

  freq_documentation_data <- freq_documentation_data %>%
    tidyr::pivot_longer("I don't understand this question":"All the time", names_to = "Response", values_to = "Count") %>%
    dplyr::arrange("Question", "Response")

  return(freq_documentation_data)

}


#' @title Summarise continuous integration frequency
#'
#' @description calculate frequency table for continuous integration
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_ci <- function(data) {

  # Validation checks
  if (!"CI" %in% colnames(data)) {
    stop("unexpected_input: no column called 'CI")
  }

  data$CI <- factor(data$CI, levels = c("Yes",
                                        "No",
                                        "I don't know what continuous integration is"))

  freqs <- data.frame(table(data$CI))

  colnames(freqs) <- c("Continuous Integration Frequency", "Count")
  return(freqs)
}


#' @title Summarise dependency management frequency
#'
#' @description calculate frequency table for dependency management.
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_dep_man <- function(data) {

  # Validation checks
  if (!"dep_management" %in% colnames(data)) {
    stop("unexpected_input: no column called 'dep_management")
  }

  data$dep_management <- factor(data$dep_management, levels = c("Yes",
                                                                "No",
                                                                "I don't know what dependency management is"))

  freqs <- data.frame(table(data$dep_management))

  colnames(freqs) <- c("Use dependency management software", "Count")

  return(freqs)

}


#' @title Summarise dependency_management frequency
#'
#' @description calculate frequency table for dependency_management.
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_rep_workflow <- function(data) {

  # Validation checks
  if (!"reproducible_workflow" %in% colnames(data)) {
    stop("unexpected_input: no column called 'reproducible_workflow")
  }

  data$reproducible_workflow <- factor(data$reproducible_workflow, levels = c("Yes",
                                                                              "No",
                                                                              "I don't know what reproducible workflows are"))

  freqs <- data.frame(table(data$reproducible_workflow))

  colnames(freqs) <- c("Use reproducible workflow packages", "Count")

  return(freqs)
}


#' @title Summarise ability change frequency
#'
#' @description calculate frequency table for ability change
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_ability_change <- function(data) {

  # Validation checks
  if (!"coding_ability_change" %in% colnames(data)) {
    stop("unexpected_input: no column called 'coding_ability_change")
  }

  data$coding_ability_change <- factor(data$coding_ability_change,levels = c("Significantly worse",
                                                               "Slightly worse",
                                                               "No change",
                                                               "Slightly better",
                                                               "Significantly better"))

  freqs <- data.frame(table(data$coding_ability_change))

  colnames(freqs) <- c("Ability Change", "Count")
  return(freqs)
}


#' @title Summarise manage someone who codes
#'
#' @description calculate frequency table for if someone line manages someone who codes
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_line_manage <- function(data){

  data$management <- factor(data$management,levels = c("Yes",
                                                       "No - I manage people who do not write code",
                                                       "No - I don't line manage anyone"))

  table <- data.frame(table(data$management))
  colnames(table) <- c("Line manage anyone who writes codes",
                       "count")
  return(table)

}


#' @title Summarise capability change by coding frequency
#'
#' @description calculate the cross tab of coding frequency by capability change
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_cap_change_by_freq <- function(data){

  freq_order <- c(
    "Never",
    "Rarely",
    "Sometimes",
    "Regularly",
    "All the time"
  )

  change_order <- c(
    "Significantly worse",
    "Slightly worse",
    "No change",
    "Slightly better",
    "Significantly better"
  )

  data$code_freq <- factor(data$code_freq, levels = freq_order)
  data$coding_ability_change <- factor(data$coding_ability_change, levels = change_order)

  capability_change <- data.frame(table(data$code_freq, data$coding_ability_change))
  capability_change <- data.frame(capability_change, check.names = FALSE)

  colnames(capability_change) <- c("Coding frequency", "Coding ability change", "Count")

  return(capability_change)
}


#' Summarise programming language knowledge by profession
#'
#' @description only used the main summary page. Needs to be turned into wide data for html table.
#'
#' @param data CARS data (preprocessed)
#'
#' @return data.frame
#'
#' @export

summarise_languages_by_prof <- function(data) {
  data <- data[complete.cases(data),]
  data <- data[data$code_freq != "Never", ]

  profs <- c("prof_DS", "prof_DDAT", "prof_GAD", "prof_GES", "prof_geog",
             "prof_GORS", "prof_GSR", "prof_GSG")
  langs <- c("knowledge_R", "knowledge_SQL", "knowledge_python", "knowledge_SAS",
             "knowledge_SPSS", "knowledge_VBA", "knowledge_matlab", "knowledge_stata",
             "knowledge_JS", "knowledge_java_scala", "knowledge_C")
  lang_names <- c("R", "SQL", "Python", "SAS", "SPSS", "VBA", "Matlab", "Stata",
                  "JavaScript", "Scala", "C#/C++")

  prof_counts <- colSums(data[profs] == "Yes")

  prof_langs <- sapply(profs, function(prof) {
    filtered_data <- data[data[prof] == "Yes", ]

    freqs <- as.vector(colSums(filtered_data[langs] == "Yes"))

    return(freqs)
  }) %>% data.frame

  prof_langs <- cbind(lang = lang_names, prof_langs)

  colnames(prof_langs) <- c("lang", "Data scientists", "Digital and data (DDAT)", "Actuaries", "Economists (GES)",
                            "Geographers", "Operational researchers (GORS)", "Social researchers (GSR)", "Statisticians (GSG)")

  prof_langs_long <- tidyr::pivot_longer(prof_langs, cols = colnames(prof_langs)[2:9]) %>% data.frame
  prof_langs_long[[1]] <- factor(prof_langs_long[[1]], levels = unique(prof_langs_long[[1]]))
  prof_langs_long[[2]] <- factor(prof_langs_long[[2]], levels = unique(prof_langs_long[[2]]))

  return(prof_langs_long)

}
