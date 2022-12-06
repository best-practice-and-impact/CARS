
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

  selected_data <- data %>% dplyr::select(ops_analysis:ops_other)

  frequencies <- apply(selected_data, 2, function(x) {
    x <- factor(x, levels = c("I do some or all of this by coding", "I do this without coding", "I don't do this"))

    table(x)
  })

  labels <- c("Data analysis", "Data cleaning", "Data linking", "Data transfer / migration", "Data visualisation", "Machine learning", "Modelling", "Quality assurance", "Other data operations")

  frequencies <- data.frame("Data operation" = labels, t(frequencies))

  # Replace full stops with spaces
  colnames(frequencies) <- gsub("[.]", " ", colnames(frequencies))

  rownames(frequencies) <- NULL

  frequencies <- frequencies %>%
    pivot_longer("I do some or all of this by coding":"I don t do this", names_to = "Proportion of task done by coding", values_to = "Count") %>%
    arrange("Data operation", "Frequency")

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
    pivot_longer(Yes:No, names_to = "Response", values_to = "Count") %>%
    arrange("Programming language", "Response")

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

  frequencies <- calc_multi_col_freqs(data = selected_data, levels = levels, labels = labels, calc_props = TRUE)

  colnames(frequencies) <- c("Question", levels)

  return(frequencies)

}

#' @title Summarise basic rap score
#'
#' @description calculate frequency table for basic rap scores
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'

#' summarise_rap_basic <- function(data){
#'
#'   data <- data[data$code_freq != "Never", ]
#'
#'   basic_freqs <- data.frame(table(data$basic_rap_score))
#'   colnames(basic_freqs) <- c("Basic RAP score", "Count")
#'
#'   return(basic_freqs)
#'
#' }
#'
#' #' @title Summarise Advanced rap score
#' #'
#' #' @description calculate frequency table for Advanced rap scores
#' #'
#' #' @param data full CARS wave 3 data.frame after preprocessing
#' #'
#' #' @return frequency table (data.frame)
#' #'
#'
#' summarise_rap_advanced <- function(data){
#'
#'   data <- data[data$code_freq != "Never", ]
#'
#'   advanced_freqs <- data.frame(table(data$advanced_rap_score))
#'   colnames(advanced_freqs) <- c("Advanced RAP score", "Count")
#'
#'   return(advanced_freqs)
#
# }

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

  freq_rap_opinions <- calc_multi_col_freqs(opinion_rap_data, levels = levels, labels = labels, calc_props = TRUE)

  colnames(freq_rap_opinions) <- c("Question",
                                   "Strongly disagree",
                                   "Disagree",
                                   "Neutral",
                                   "Agree",
                                   "Strongly agree")

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
  documentation_data <- dplyr::select(documentation_data, "code_comments":"flow_charts")

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


  freq_documentation_data <- calc_multi_col_freqs(documentation_data, levels = levels, labels = labels, calc_props = TRUE)

  colnames(freq_documentation_data) <- c("Question", levels)

  return(freq_documentation_data)

}

#' @title RAP score components
#'
#' @description Create frequency table of basic and advanced RAP score components
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_rap_comp <- function(data){

  labels <- c("use_open_source_score" = "Use open source software",
              "open_code_score" = "Team open source code",
              "version_control_score" = "Version control",
              "peer_review_score" = "Peer review",
              "AQUA_book_score" = "AQUA book guidance",
              "doc_score" = "Documentation",
              "function_score" = "Functions",
              "unit_test_score" = "Unit testing",
              "function_doc_score" = "Function documentation",
              "package_score" = "Code packages",
              "code_style_score" = "Follow code style guidelines",
              "cont_integreation_score" = "Continuous integration",
              "dep_management_score" = "Dependency management")

  rap_score <- data[grepl("_score", colnames(data))]

  components <- rap_score[!colnames(rap_score) %in% c("basic_rap_score", "advanced_rap_score")]
  components[is.na(components)] <- 0
  components <- data.frame(Component = labels,
                           Type = c(rep("Basic", 6), rep("Advanced", 7)),
                           Count = unname(colSums(components))
  )

  rownames(components) <- NULL
  components <- components[order(-rank(components$Type),components$Component),]
  components$Component <- factor(components$Component, levels = components$Component)

  return(components)

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
  if (!"dependency_management" %in% colnames(data)) {
    stop("unexpected_input: no column called 'dependency_management")
  }

  data$dependency_management <- factor(data$dependency_management, levels = c("Yes",
                                                                              "No",
                                                                              "I don't know what dependency management is"))

  freqs <- data.frame(table(data$dependency_management))

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
  if (!"ability_change" %in% colnames(data)) {
    stop("unexpected_input: no column called 'ability_change")
  }

  data$ability_change <- factor(data$ability_change,levels = c("Significantly worse",
                                                               "Slightly worse",
                                                               "No change",
                                                               "Slightly better",
                                                               "Significantly better"))

  freqs <- data.frame(table(data$ability_change))

  colnames(freqs) <- c("Ability Change", "Count")
  return(freqs)
}

#' @title Summarise programming language status
#'
#' @description calculate counts of responents reporting access to, knowledge of, or both for each programming language.
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom rlang .data

summarise_language_status <- function(data) {
  selected_data <- dplyr::select(data, .data$status_R:.data$status_matlab)

  select_data <- selected_data[order(colnames(selected_data))]

  frequencies <- data.frame(apply(selected_data, 2, function(x) {
    x <- factor(x, levels = c("access", "both", "knowledge"))

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

  frequencies <- data.frame(languages, t(frequencies), check.names = FALSE)

  colnames(frequencies) <- c("Programming language", "Access only", "Access and knowledge", "Knowledge only")

  frequencies
}

#' @title Summarise manage someone who codes
#'
#' @description calculate frequency table for if someone line manages someone who codes
#'
#' @param data full CARS wave 3 data.frame after preprocessing
#'
#' @return frequency table (data.frame)

summarise_line_manage <- function(data){

  data$code_manage <- factor(data$code_manage,levels = c("Yes",
                                                         "No",
                                                         "I don't line manage anyone"))

  table <- data.frame(table(data$code_manage))
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
  data$ability_change <- factor(data$ability_change, levels = change_order)

  capability_change <- data.frame(table(data$code_freq, data$ability_change)) %>% tidyr::pivot_wider(names_from = Var2, values_from = Freq)
  capability_change <- data.frame(capability_change, check.names = FALSE)
  capability_change[2:6] <- t(apply(capability_change[2:6], 1, function(x) x / sum(x)))

  colnames(capability_change)[1] <- "Coding frequency"

  return(capability_change)
}


#' @title Compare basic RAP score to implementation of RAP
#'
#' @description calculate frequency table for basic rap score compared with implementation of RAP
#'
#' @param implementing_data carsurvey data filter to people who have heard of RAP and code at least rarely
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_basic_score_by_imp <- function(implementing_data){

  rap_score_by_implementing <- data.frame(table(implementing_data$RAP_implementing, implementing_data$basic_rap_score)) %>% tidyr::pivot_wider(names_from = Var2, values_from = Freq)
  rap_score_by_implementing %<>% data.frame(check.names = FALSE)
  rap_score_by_implementing[2:8] <- t(apply(rap_score_by_implementing[2:8], 1, function(x) x / sum(x) * 100))

  colnames(rap_score_by_implementing)[1] <- "I am Currently Implementing RAP"

  return(rap_score_by_implementing)
}

#' @title Compare advanced RAP score to implementation of RAP
#'
#' @description calculate frequency table for advanced rap score compared with implementation of RAP
#'
#' @param implementing_data carsurvey data filtered to people who have heard of RAP and code at least rarely
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_adv_score_by_imp <- function(implementing_data){

  advanced_score_by_implementing <- data.frame(table(implementing_data$RAP_implementing, implementing_data$advanced_rap_score)) %>% tidyr::pivot_wider(names_from = Var2, values_from = Freq)
  advanced_score_by_implementing %<>% data.frame(check.names = FALSE)
  advanced_score_by_implementing[2:9] <- t(apply(advanced_score_by_implementing[2:9], 1, function(x) x / sum(x) * 100))

  colnames(advanced_score_by_implementing)[1] <- "I am Currently Implementing RAP"

  return(advanced_score_by_implementing)
}

#' @title Compare basic RAP score to understanding of key RAP components
#'
#' @description calculate frequency table for basic rap score compared with understanding of key RAP components
#'
#' @param implementing_data carsurvey data filter to people who have heard of RAP and code at least rarely
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_basic_score_by_understanding <- function(implementing_data){

  rap_score_by_understanding <- data.frame(table(implementing_data$RAP_understand_key_components, implementing_data$basic_rap_score)) %>% tidyr::pivot_wider(names_from = Var2, values_from = Freq)
  rap_score_by_understanding %<>% data.frame(check.names = FALSE)
  rap_score_by_understanding[2:8] <- t(apply(rap_score_by_understanding[2:8], 1, function(x) x / sum(x) * 100))

  colnames(rap_score_by_understanding)[1] <- "I Understand Key concepts of RAP"

  return(rap_score_by_understanding)
}

#' @title Compare advanced RAP score to understanding of key RAP components
#'
#' @description calculate frequency table for advanced rap score compared with understanding of key RAP components
#'
#' @param implementing_data carsurvey data filtered to people who have heard of RAP and code at least rarely
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_adv_score_by_understanding <- function(implementing_data){

  advanced_score_by_understanding <- data.frame(table(implementing_data$RAP_understand_key_components, implementing_data$advanced_rap_score)) %>% tidyr::pivot_wider(names_from = Var2, values_from = Freq)
  advanced_score_by_understanding %<>% data.frame(check.names = FALSE)
  advanced_score_by_understanding[2:9] <- t(apply(advanced_score_by_understanding[2:9], 1, function(x) x / sum(x) * 100))

  colnames(advanced_score_by_understanding)[1] <- "I Understand Key concepts of RAP"

  return(advanced_score_by_understanding)
}

#' @title  Compare coding frequency of ONS to other departments
#'
#' @description calculate frequency table for coding frequency for ONS and all other departments combined
#'
#' @param ons_data carsurvey data filter to ONS department
#' @param other_deps_data lcarsurvey  data filter to all other department
#' @param ons_tables list of tables made from summaries_all from carsurvey data filter to ONS department
#' @param other_deps_tables list of tables made from summaries_all from carsurvey  data filter to all other department
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_coding_freq_comparison <- function(ons_data, other_deps_data, ons_tables, other_deps_tables){

  ons_tables$code_freq[2] <- ons_tables$code_freq[2] / nrow(ons_data) * 100
  other_deps_tables$code_freq[2] <- other_deps_tables$code_freq[2] / nrow(other_deps_data) * 100


  freq_combined <- rbind(data.frame(other_deps_tables$code_freq, department = "Other departments", check.names = FALSE),
                         data.frame(ons_tables$code_freq, department = "ONS", check.names = FALSE))

  freq_combined <- freq_combined[c(1,3,2)]

  freq_combined$department <- factor(freq_combined$department, levels = c("Other departments", "ONS"))

  return(freq_combined)
}

#' @title  Compare coding tool frequency of ONS to other departments
#'
#' @description calculate frequency table for coding tool for ONS and all other departments combined
#'
#' @param ons_data carsurvey data filter to ONS department
#' @param other_deps_data lcarsurvey  data filter to all other department
#' @param ons_tables list of tables made from summaries_all from carsurvey data filter to ONS department
#' @param other_deps_tables list of tables made from summaries_all from carsurvey  data filter to all other department
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_coding_tools_comparison <- function(ons_data, other_deps_data, ons_tables, other_deps_tables){

  langs <- grep("knowledge_", colnames(data), value = TRUE)
  lang_names <- c("R", "SQL", "SAS", "VBA", "Python", "SPSS", "Stata", "JavaScript", "Java/Scala", "C++/C#", "Matlab")

  ons_tables$knowledge[2] <- ons_tables$knowledge[2] / nrow(ons_data) * 100
  other_deps_tables$knowledge[2] <- other_deps_tables$knowledge[2] / nrow(other_deps_data) * 100

  langs_combined <- rbind(data.frame(ons_tables$knowledge[1:2], department = "ONS", check.names = FALSE),
                          data.frame(other_deps_tables$knowledge[1:2], department = "Other departments",  check.names = FALSE))

  langs_combined <- langs_combined[c(1,3,2)]

  return(langs_combined)
}

#' @title  Compare basic RAP scores frequency of ONS to other departments
#'
#' @description calculate frequency table for coding tool for ONS and all other departments combined
#'
#' @param ons_data carsurvey data filter to ONS department
#' @param other_deps_data lcarsurvey  data filter to all other department
#' @param ons_tables list of tables made from summaries_all from carsurvey data filter to ONS department
#' @param other_deps_tables list of tables made from summaries_all from carsurvey  data filter to all other department
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_baisc_rap_scores_comparison <- function(ons_data, other_deps_data, ons_tables, other_deps_tables){

  ons_tables$basic_rap_scores[2] <- ons_tables$basic_rap_scores[2] / sum(ons_data$code_freq != "Never") * 100
  other_deps_tables$basic_rap_scores[2] <- other_deps_tables$basic_rap_scores[2] / sum(other_deps_data$code_freq != "Never") * 100

  basic_scores_combined <- rbind(
    data.frame(other_deps_tables$basic_rap_scores, department = "Other departments", check.names = FALSE),
    data.frame(ons_tables$basic_rap_scores, department = "ONS", check.names = FALSE)
  )

  basic_scores_combined <- basic_scores_combined[c(1,3,2)]
  basic_scores_combined$department <- factor(basic_scores_combined$department, levels = c("Other departments", "ONS"))

  return(basic_scores_combined)
}

#' @title  Compare advanced RAP scores frequency of ONS to other departments
#'
#' @description calculate frequency table for coding tool for ONS and all other departments combined
#'
#' @param ons_data carsurvey data filter to ONS department
#' @param other_deps_data lcarsurvey  data filter to all other department
#' @param ons_tables list of tables made from summaries_all from carsurvey data filter to ONS department
#' @param other_deps_tables list of tables made from summaries_all from carsurvey  data filter to all other department
#'
#' @return frequency table (data.frame)
#'
#' @export

summarise_adv_rap_scores_comparison <- function(ons_data, other_deps_data, ons_tables, other_deps_tables){

  ons_tables$advanced_rap_scores[2] <- ons_tables$advanced_rap_scores[2] / sum(ons_data$code_freq != "Never") * 100
  other_deps_tables$advanced_rap_scores[2] <- other_deps_tables$advanced_rap_scores[2] / sum(other_deps_data$code_freq != "Never") * 100

  advanced_scores_combined <- rbind(
    data.frame(other_deps_tables$advanced_rap_scores, department = "Other departments", check.names = FALSE),
    data.frame(ons_tables$advanced_rap_scores, department = "ONS", check.names = FALSE)
  )

  advanced_scores_combined <- advanced_scores_combined[c(1,3,2)]
  advanced_scores_combined$department <- factor(advanced_scores_combined$department, levels = c("Other departments", "ONS"))

  return(advanced_scores_combined)
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
  data <- data[data$code_freq <= "Never", ]

  data$prof_DS <- ifelse(data$prof_DS_GSG_GORS == "Yes" | data$prof_DS_other == "Yes", "Yes", "No")

  profs <- c("prof_GAD", "prof_DDAT",  "prof_DS", "prof_GES", "prof_GORS", "prof_GSR", "prof_GSG")
  langs <- c("knowledge_R", "knowledge_SQL", "knowledge_python", "knowledge_SAS", "knowledge_SPSS",
             "knowledge_VBA", "knowledge_matlab", "knowledge_stata")
  lang_names <- c("R", "SQL", "Python", "SAS", "SPSS", "VBA", "Matlab", "Stata")

  prof_counts <- colSums(data[profs] == "Yes")

  prof_langs <- sapply(profs, function(prof) {
    filtered_data <- data[data[prof] == "Yes", ]

    freqs <- as.vector(colSums(filtered_data[langs] == "Yes")) / prof_counts[prof] * 100

    return(freqs)
  }) %>% data.frame

  prof_langs <- cbind(lang = lang_names, prof_langs)

  colnames(prof_langs) <- c("lang", "Actuaries", "Digital and data (DDAT)", "Data scientists", "Economists (GES)",
                            "Operational researchers (GORS)", "Social researchers (GSR)", "Statisticians (GSG)")

  prof_langs_long <- tidyr::pivot_longer(prof_langs, cols = colnames(prof_langs)[2:8]) %>% data.frame
  prof_langs_long[[1]] <- factor(prof_langs_long[[1]], levels = unique(prof_langs_long[[1]]))
  prof_langs_long[[2]] <- factor(prof_langs_long[[2]], levels = unique(prof_langs_long[[2]]))

  return(prof_langs_long)

}