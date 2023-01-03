#' @title Summarise all
#'
#' @description Produce all summary tables and return as a named list.
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#' @param all_tables logical: whether to produce all summary output tables. Defaults to FALSE.
#'
#' @return list of frequency tables
#'
#' @export

summarise_all <- function(data, all_tables = FALSE) {

  output_list <- list(
    code_freq = summarise_code_freq(data),
    operations = summarise_operations(data),
    knowledge = summarise_coding_tools(data, "knowledge"),
    access = summarise_coding_tools(data, "access"),
    language_status = summarise_language_status(data),
    where_learned = summarise_where_learned_code(data),
    ability_change = summarise_ability_change(data),
    coding_practices = summarise_coding_practices(data),
    doc = summarise_doc(data),
    rap_knowledge = summarise_rap_knowledge(data),
    rap_opinions = summarise_rap_opinions(data),
    basic_rap_scores = summarise_rap_basic(data),
    advanced_rap_scores = summarise_rap_advanced(data),
    rap_components = summarise_rap_comp(data),
    ci = summarise_ci(data),
    dependency_management = summarise_dep_man(data),
    rep_workflow = summarise_rep_workflow(data),
    line_manage = summarise_line_manage(data)
  )

  if (all_tables) {

    output_list <- c(output_list,
                     list(
                       capability_change_by_freq = summarise_cap_change_by_freq(data),
                       basic_score_by_implementation = summarise_basic_score_by_imp(data),
                       adv_score_by_implementation = summarise_adv_score_by_imp(data),
                       basic_score_by_understanding = summarise_basic_score_by_understanding(data),
                       adv_score_by_understanding = summarise_adv_score_by_understanding(data),
                       languages_by_prof = summarise_languages_by_prof(data) # Needs refactoring
                     ))

  }

  return(output_list)
}



#' @title Summarise coding frequency
#'
#' @description calculate frequency table for coding frequency.
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_code_freq <- function(data) {

  # Validation checks
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq'")
  }

  questions <- "code_freq"

  levels <- c("Never",
              "Rarely",
              "Sometimes",
              "Regularly",
              "All the time")

  labels <- "Coding frequency"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)
}


#' @title Summarise data operations
#'
#' @description calculate frequency table for data operations
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_operations <- function(data) {

  questions <- c("ops_analysis", "ops_cleaning", "ops_linking",
                 "ops_transfer_migration", "ops_vis", "ops_machine_learning",
                 "ops_modelling", "ops_QA")

  levels <- c("I do some or all of this by coding",
                 "I do this without coding", "I don't do this")

  labels <- c("Data analysis", "Data cleaning", "Data linking",
              "Data transfer / migration", "Data visualisation",
              "Machine learning", "Modelling", "Quality assurance")

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise coding tools
#'
#' @description calculate frequency table coding tools (knowledge or access)
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#' @param type type of table (knowledge or access)
#'
#' @return frequency table (data.frame)

summarise_coding_tools <- function(data, type = list("knowledge", "access")) {

  questions <- c("knowledge_R", "access_R", "knowledge_SQL", "access_SQL",
                 "knowledge_SAS", "access_SAS", "knowledge_VBA", "access_VBA",
                 "knowledge_python", "access_python", "knowledge_SPSS",
                 "access_SPSS", "knowledge_stata", "access_stata", "knowledge_JS",
                 "access_JS", "knowledge_java", "access_java", "knowledge_C",
                 "access_C", "knowledge_matlab", "access_matlab")

  levels <- c("Yes", "No", "Don't Know")

  labels <- c("R", "SQL", "SAS", "VBA", "Python", "SPSS", "Stata",
              "Javascript / Typescript", "Java / Scala", "C++ / C#", "Matlab")

  type <- match.arg(type, several.ok = TRUE)

  questions <- questions[grepl(paste0(type, "_"), questions)]

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)
}


#' @title Summarise where respondents learned to code
#'
#' @description calculate frequency table of where respondents learned to code
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_where_learned_code <- function(data){

  # Validation checks
  if (!"first_learned" %in% colnames(data)) {
    stop("unexpected_input: no column called 'first_learned'")
  }
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq'")
  }
  if (!"prev_coding_experience" %in% colnames(data)) {
    stop("unexpected_input: no column called 'prev_coding_experience'")
  }

  questions <- "first_learned"

  levels <- c("In current role",
              "In education",
              "In private sector employment",
              "In public sector employment",
              "Self-taught",
              "Other")

  labels <- "First coding experience"

  data <- data %>%
    dplyr::select(first_learned, prev_coding_experience, code_freq) %>%
    dplyr::mutate(
      first_learned = dplyr::case_when((is.na(data$prev_coding_experience) |
                                        (data$prev_coding_experience == "No")) &
                                          data$code_freq != "Never" ~ "In current role",
                                       !is.na(data$first_learned) & !(data$first_learned %in% levels) ~ "Other",
                                       TRUE ~ first_learned))

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)
}


#' @title Summarise data practices questions
#'
#' @description calculate frequency table for data practices
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_coding_practices <- function(data) {

  questions <- c("prac_use_open_source", "prac_open_source_own",
                 "prac_version_control", "prac_review", "prac_functions",
                 "prac_unit_test", "prac_package", "prac_dir_structure",
                 "prac_style", "prac_automated_QA", "prac_AQUA_book")

  levels <- c("I don't understand this question", "Never", "Rarely",
                 "Sometimes", "Regularly", "All the time")

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

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise basic rap score
#'
#' @description calculate frequency table for basic rap scores
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)
#'

summarise_rap_basic <- function(data){

  data <- data[data$code_freq != "Never", ]

  questions <- "basic_rap_score"

  levels <- 0:6

  labels <- "Basic RAP score"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise Advanced rap score
#'
#' @description calculate frequency table for Advanced rap scores
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)
#'

summarise_rap_advanced <- function(data){

  data <- data[data$code_freq != "Never", ]

  questions <- "advanced_rap_score"

  levels <- 0:7

  labels <- "Advanced RAP score"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Knowledge of RAP
#'
#' @description Create a frequency table of knowledge of RAP
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_rap_knowledge <- function(data){

  # Validation checks
  if (!"heard_of_RAP" %in% colnames(data)) {
    stop("unexpected_input: no column called 'heard_of_RAP'")
  }

  questions <- "know_RAP_champ"

  levels <- c("Have not heard of RAP",
              "I don't know what a RAP champion is",
              "I know what a RAP champion is but don't know who the RAP champion in my department is",
              "I know what a RAP champion is and there is no RAP champion in my department",
              "I know who the RAP champion in my department is")

  labels <- "RAP champion knowledge"

  data$know_RAP_champ[data$heard_of_RAP == "No"] <- "Have not heard of RAP"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)
}


#' @title Opinions of RAP
#'
#' @description Create frequency table of opinions of RAP
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_rap_opinions <- function(data) {

  # Validation checks
  if (!"heard_of_RAP" %in% colnames(data)) {
    stop("unexpected_input: no column called 'heard_of_RAP'")
  }

  opinion_rap_data <- data[data$heard_of_RAP == "Yes", ]

  questions <- c("RAP_confident",
                 "RAP_supported",
                 "RAP_resources",
                 "RAP_components",
                 "RAP_important",
                 "RAP_implementing",
                 "RAP_planning")

  levels <- c("Strongly Disagree",
              "Disagree",
              "Neutral",
              "Agree",
              "Strongly Agree")

  labels <- c("I feel confident implementing RAP in my work",
              "I feel supported to implement RAP in my work",
              "I know where to find resources to help me implement RAP",
              "I understand what the key components of the RAP methodology are",
              "I think it is important to implement RAP in my work",
              "I and/or my team are currently implementing RAP",
              "I or my team are planning on implementing RAP in the next 12 months")


  frequencies <- create_tidy_freq_table(opinion_rap_data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Frequency of documentation use
#'
#' @description Create frequency table of documentation use
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_doc <- function(data) {

  # Validation checks
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq'")
  }

  documentation_data <- data[data$code_freq != "Never", ]

  questions <- c("doc_comments",
                 "doc_functions",
                 "doc_readme",
                 "doc_desk_notes",
                 "doc_registers",
                 "doc_AQA_logs",
                 "doc_flow_charts")

  levels <- c("I don't understand this question",
              "Never",
              "Rarely",
              "Sometimes",
              "Regularly",
              "All the time")

  labels <- c("Code comments",
              "Documentation for each function or class",
              "README files",
              "Desk notes",
              "Analytical Quality Assurance (AQA) logs",
              "Data or assumptions registers",
              "Flow charts")


  frequencies <- create_tidy_freq_table(documentation_data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title RAP score components
#'
#' @description Create frequency table of basic and advanced RAP score components
#'
#' @param data full CARS wave 3 data.frame after pre-processing
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

  components <- rap_score %>%
    dplyr::select(!c("basic_rap_score", "advanced_rap_score")) %>%
    dplyr::summarise(across(everything(), ~ sum(., is.na(.), 0))) %>%
    tidyr::pivot_longer(everything(), names_to = "Component", values_to = "Count") %>%
    dplyr::mutate(Component = dplyr::recode(Component, !!!labels)) %>%
    dplyr::mutate(Type = c(rep("Basic", 6), rep("Advanced", 7))) %>%
    dplyr::relocate(Count, .after = Type) %>%
    dplyr::arrange(desc(Type), Component) %>%
    dplyr::mutate(Component = factor(Component, levels = Component)) %>%
    tidyr::drop_na() %>%
    data.frame

  return(components)

}


#' @title Summarise continuous integration frequency
#'
#' @description calculate frequency table for continuous integration
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_ci <- function(data) {

  # Validation checks
  if (!"CI" %in% colnames(data)) {
    stop("unexpected_input: no column called 'CI'")
  }

  questions <- "CI"

  levels <- c("Yes",
              "No",
              "I don't know what continuous integration is")

  labels <- "Continuous Integration Frequency"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise dependency management frequency
#'
#' @description calculate frequency table for dependency management.
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_dep_man <- function(data) {

  # Validation checks
  if (!"dep_management" %in% colnames(data)) {
    stop("unexpected_input: no column called 'dep_management'")
  }

  questions <- "dep_management"

  levels <- c("Yes",
              "No",
              "I don't know what dependency management is")

  labels <- "Use dependency management software"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise dependency_management frequency
#'
#' @description calculate frequency table for dependency_management.
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_rep_workflow <- function(data) {

  # Validation checks
  if (!"reproducible_workflow" %in% colnames(data)) {
    stop("unexpected_input: no column called 'reproducible_workflow'")
  }

  questions <- "reproducible_workflow"

  levels <- c("Yes",
              "No",
              "I don't know what reproducible workflows are")

  labels <- "Use reproducible workflow packages"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise ability change frequency
#'
#' @description calculate frequency table for ability change
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_ability_change <- function(data) {

  # Validation checks
  if (!"coding_ability_change" %in% colnames(data)) {
    stop("unexpected_input: no column called 'coding_ability_change'")
  }

  questions <- "coding_ability_change"

  levels <- c("Significantly worse",
              "Slightly worse",
              "No change",
              "Slightly better",
              "Significantly better")

  labels <- "Ability Change"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise programming language status
#'
#' @description calculate counts of responents reporting access to, knowledge of, or both for each programming language.
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom rlang .data

summarise_language_status <- function(data) {

  questions <- c("status_R",
                 "status_SQL",
                 "status_SAS",
                 "status_VBA",
                 "status_python",
                 "status_SPSS",
                 "status_stata",
                 "status_JS",
                 "status_java",
                 "status_C",
                 "status_matlab")

  levels <- c("both", "access", "knowledge", "neither")

  labels <- c("R",
              "SQL",
              "SAS",
              "VBA",
              "Python",
              "SPSS",
              "Stata",
              "Javascript / Typescript",
              "Java / Scala",
              "C++ / C#",
              "Matlab")

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise manage someone who codes
#'
#' @description calculate frequency table for if someone line manages someone who codes
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_line_manage <- function(data){

  questions <- "management"

  levels <- c("Yes",
              "No - I manage people who do not write code",
              "No - I don't line manage anyone")

  labels <- "Line manage anyone who writes codes"

  frequencies <- create_tidy_freq_table(data, questions, levels,
                                        labels)

  return(frequencies)

}


#' @title Summarise capability change by coding frequency
#'
#' @description calculate the cross tab of coding frequency by capability change
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_cap_change_by_freq <- function(data){

  col1 <- "code_freq"

  col2 <- "coding_ability_change"

  levels1 <- c(
    "Never",
    "Rarely",
    "Sometimes",
    "Regularly",
    "All the time")

  levels2 <- c(
    "Significantly worse",
    "Slightly worse",
    "No change",
    "Slightly better",
    "Significantly better")

  frequencies <- create_tidy_cross_table(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare basic RAP score to implementation of RAP
#'
#' @description calculate frequency table for basic rap score compared with implementation of RAP
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_basic_score_by_imp <- function(data){

  col1 <- "RAP_implementing"

  col2 <- "basic_rap_score"

  levels1 <- c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree")

  levels2 <- c(0, 1, 2, 3, 4, 5, 6)

  frequencies <- create_tidy_cross_table(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare advanced RAP score to implementation of RAP
#'
#' @description calculate frequency table for advanced rap score compared with implementation of RAP
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_adv_score_by_imp <- function(data){

  col1 <- "RAP_implementing"

  col2 <- "advanced_rap_score"

  levels1 <- c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree")

  levels2 <- c(0, 1, 2, 3, 4, 5, 6, 7)

  frequencies <- create_tidy_cross_table(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare basic RAP score to understanding of key RAP components
#'
#' @description calculate frequency table for basic rap score compared with understanding of key RAP components
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_basic_score_by_understanding <- function(data){

  col1 <- "RAP_components"

  col2 <- "basic_rap_score"

  levels1 <- c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree")

  levels2 <- c(0, 1, 2, 3, 4, 5, 6)

  frequencies <- create_tidy_cross_table(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare advanced RAP score to understanding of key RAP components
#'
#' @description calculate frequency table for advanced rap score compared with understanding of key RAP components
#'
#' @param data full CARS wave 3 data.frame after pre-processing
#'
#' @return frequency table (data.frame)

summarise_adv_score_by_understanding <- function(data){

  col1 <- "RAP_components"

  col2 <- "advanced_rap_score"

  levels1 <- c(
    "Strongly disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly agree")

  levels2 <- c(0, 1, 2, 3, 4, 5, 6, 7)

  frequencies <- create_tidy_cross_table(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Summarise programming language knowledge by profession
#'
#' @description only used the main summary page. Needs to be turned into wide data for html table.
#'
#' @param data CARS data (pre-processed)
#'
#' @return data.frame

summarise_languages_by_prof <- function(data) {

  data <- data[!(data$code_freq %in% c("Never", NA)), ]

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

  prof_langs_long <- prof_langs %>%
    tidyr::pivot_longer(cols = colnames(prof_langs)[2:9]) %>%
    dplyr::group_by(lang) %>%
    dplyr::mutate(lang = factor(lang, levels = unique(lang))) %>%
    dplyr::mutate(name = factor(name, levels = unique(name))) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(value = round((value / sum(value)), 2)) %>%
    data.frame

  return(prof_langs_long)

}
