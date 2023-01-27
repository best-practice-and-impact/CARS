#' @title Summarise all
#'
#' @description Produce all summary tables and return as a named list.
#'
#' @param data full CARS dataset after pre-processing
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


#' @title Sample sizes for table/plot outputs
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return list of sample sizes
#'
#' @export

sample_sizes <- function(data) {
  list(
    all = nrow(data),
    code_at_work = sum(data$code_freq != "Never"),
    can_code = sum(data$code_freq != "Never" | (data$other_coding_experience == "Yes" & data$prev_coding_experience != "No")),
    other_code_experience = sum(data$other_coding_experience == "Yes"),
    heard_of_RAP = sum(data$heard_of_RAP == "Yes"),
    not_RAP_champ = sum(is.na(data$know_RAP_champ) | data$know_RAP_champ != "I am a RAP champion")
  )
}


#' @title Summarise coding frequency
#'
#' @description calculate frequency table for coding frequency.
#'
#' @param data full CARS dataset after pre-processing
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

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)
}


#' @title Summarise data operations
#'
#' @description calculate frequency table for data operations
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_operations <- function(data) {

  questions <- c("ops_analysis", "ops_cleaning", "ops_linking",
                 "ops_transfer_migration", "ops_vis", "ops_machine_learning",
                 "ops_modelling", "ops_QA")

  levels <- c("I do some or all of this by coding", "I do this without coding")

  labels <- c("Data analysis", "Data cleaning", "Data linking",
              "Data transfer / migration", "Data visualisation",
              "Machine learning", "Modelling", "Quality assurance")

  frequencies <- calculate_freqs(data, questions, levels, labels)

  return(frequencies)

}


#' @title Summarise coding tools
#'
#' @description calculate frequency table coding tools (knowledge or access)
#'
#' @param data full CARS dataset after pre-processing
#' @param type type of table (knowledge or access)
#' @param prop whether to return proportion data (0-1). TRUE by default. Assumes mutually exclusive response options.
#'
#' @return frequency table (data.frame)

summarise_coding_tools <- function(data, type = list("knowledge", "access"), prop = TRUE) {

  questions <- c("knowledge_R", "access_R", "knowledge_SQL", "access_SQL",
                 "knowledge_SAS", "access_SAS", "knowledge_VBA", "access_VBA",
                 "knowledge_python", "access_python", "knowledge_SPSS",
                 "access_SPSS", "knowledge_stata", "access_stata", "knowledge_JS",
                 "access_JS", "knowledge_java", "access_java", "knowledge_C",
                 "access_C", "knowledge_matlab", "access_matlab")

  levels <- c("Yes", "Don't Know", "No")

  labels <- c("R", "SQL", "SAS", "VBA", "Python", "SPSS", "Stata",
              "Javascript / Typescript", "Java / Scala", "C++ / C#", "Matlab")

  type <- match.arg(type, several.ok = TRUE)

  questions <- questions[grepl(paste0(type, "_"), questions)]

  frequencies <- calculate_freqs(data, questions, levels, labels, prop = prop)

  return(frequencies)
}


#' @title Summarise where respondents learned to code
#'
#' @description calculate frequency table of where respondents learned to code
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom dplyr select mutate case_when

summarise_where_learned_code <- function(data){

  # Validation checks
  if (!"first_learned" %in% colnames(data)) {
    stop("unexpected_input: no column called 'first_learned'")
  }
  if (!"code_freq" %in% colnames(data)) {
    stop("unexpected_input: no column called 'code_freq'")
  }
  if (!"other_coding_experience" %in% colnames(data)) {
    stop("unexpected_input: no column called 'other_coding_experience'")
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

  data <- data %>%
    select(.data$first_learned, .data$prev_coding_experience, .data$code_freq) %>%
    mutate(
      first_learned = case_when((data$other_coding_experience == "No" |
                                   data$prev_coding_experience == "No") &
                                  data$code_freq != "Never" ~ "In current role",
                                !is.na(data$first_learned) & !(data$first_learned %in% levels) ~ "Other",
                                TRUE ~ .data$first_learned))

  data$prev_coding_experience[data$other_coding_experience == "No"] <- "No"

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)
}


#' @title Summarise data practices questions
#'
#' @description calculate frequency table for data practices
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_coding_practices <- function(data) {

  questions <- c("prac_use_open_source", "prac_open_source_own",
                 "prac_version_control", "prac_review", "prac_functions",
                 "prac_unit_test", "prac_package", "prac_dir_structure",
                 "prac_style", "prac_automated_QA", "prac_AQUA_book")

  levels <- c("I don't understand this question", "Never", "Rarely",
                 "Sometimes", "Regularly", "All the time")

  labels <- c("Use open source software",
              "Open source own code",
              "Version control",
              "Code review",
              "Functions",
              "Unit testing",
              "Packaging code",
              "Standard directory structure",
              "Coding guidelines / Style guides",
              "Automated data quality assurance",
              "Apply AQUA book principles with analysis code")

  frequencies <- calculate_freqs(data, questions, levels, labels)

  return(frequencies)

}


#' @title Summarise basic rap score
#'
#' @description calculate frequency table for basic rap scores
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)
#'

summarise_rap_basic <- function(data){

  data <- data[data$code_freq != "Never", ]

  questions <- "basic_rap_score"

  levels <- 0:6

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Summarise Advanced rap score
#'
#' @description calculate frequency table for Advanced rap scores
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)
#'

summarise_rap_advanced <- function(data){

  data <- data[data$code_freq != "Never", ]

  questions <- "advanced_rap_score"

  levels <- 0:7

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Knowledge of RAP
#'
#' @description Create a frequency table of knowledge of RAP
#'
#' @param data full CARS dataset after pre-processing
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

  data$know_RAP_champ[data$heard_of_RAP == "No"] <- "Have not heard of RAP"

  frequencies <- calculate_freqs(data, questions, levels)
  return(frequencies)
}


#' @title Opinions of RAP
#'
#' @description Create frequency table of opinions of RAP
#'
#' @param data full CARS dataset after pre-processing
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


  frequencies <- calculate_freqs(opinion_rap_data, questions, levels, labels)

  return(frequencies)

}


#' @title Frequency of documentation use
#'
#' @description Create frequency table of documentation use
#'
#' @param data full CARS dataset after pre-processing
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


  frequencies <- calculate_freqs(documentation_data, questions, levels, labels)

  return(frequencies)

}

#' @title RAP score components
#'
#' @description Create frequency table of basic and advanced RAP score components
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom dplyr mutate arrange

summarise_rap_comp <- function(data) {

  labels <- c("Use open source software",
              "Team open source code",
              "Version control",
              "Peer review",
              "AQUA book guidance",
              "Documentation",
              "Functions",
              "Unit testing",
              "Function documentation",
              "Code packages",
              "Follow code style guidelines",
              "Continuous integration",
              "Dependency management")

  questions <- c("use_open_source_score",
                 "open_code_score",
                 "version_control_score",
                 "peer_review_score",
                 "AQUA_book_score",
                 "doc_score",
                 "function_score",
                 "unit_test_score",
                 "function_doc_score",
                 "package_score",
                 "code_style_score",
                 "cont_integration_score",
                 "dep_management_score")

  levels <- c(1)

  components <- calculate_freqs(data, questions, levels, labels)

  components <- components %>%
    mutate(name = factor(.data$name, levels = labels)) %>%
    arrange(.data$name) %>%
    mutate(value = c(rep("Basic", 6), rep("Advanced", 7))) %>%
    mutate(n = colSums(data[questions], na.rm = TRUE) / ifelse(sum(colSums(data[questions], na.rm = TRUE))==0,
                                                               1,
                                                               sum(colSums(data[questions], na.rm = TRUE))))

  names(components$n) <- NULL

  return(components)

}


#' @title Summarise continuous integration frequency
#'
#' @description calculate frequency table for continuous integration
#'
#' @param data full CARS dataset after pre-processing
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

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Summarise dependency management frequency
#'
#' @description calculate frequency table for dependency management.
#'
#' @param data full CARS dataset after pre-processing
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

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Summarise dependency_management frequency
#'
#' @description calculate frequency table for dependency_management.
#'
#' @param data full CARS dataset after pre-processing
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

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Summarise ability change frequency
#'
#' @description calculate frequency table for ability change
#'
#' @param data full CARS dataset after pre-processing
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

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Summarise programming language status
#'
#' @description calculate counts of responents reporting access to, knowledge of, or both for each programming language.
#'
#' @param data full CARS dataset after pre-processing
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

  levels <- c("Access Only", "Both", "Knowledge Only")

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

  frequencies <- calculate_freqs(data, questions, levels, labels)

  return(frequencies)

}


#' @title Summarise manage someone who codes
#'
#' @description calculate frequency table for if someone line manages someone who codes
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_line_manage <- function(data){

  questions <- "management"

  levels <- c("Yes",
              "No - I manage people who do not write code",
              "No - I don't line manage anyone")

  frequencies <- calculate_freqs(data, questions, levels)

  return(frequencies)

}


#' @title Summarise capability change by coding frequency
#'
#' @description calculate the cross tab of coding frequency by capability change
#'
#' @param data full CARS dataset after pre-processing
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

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare basic RAP score to implementation of RAP
#'
#' @description calculate frequency table for basic rap score compared with implementation of RAP
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_basic_score_by_imp <- function(data){

  col1 <- "RAP_implementing"

  col2 <- "basic_rap_score"

  levels1 <- c(
    "Strongly Disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly Agree")

  levels2 <- 0:6

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare advanced RAP score to implementation of RAP
#'
#' @description calculate frequency table for advanced rap score compared with implementation of RAP
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_adv_score_by_imp <- function(data){

  col1 <- "RAP_implementing"

  col2 <- "advanced_rap_score"

  levels1 <- c(
    "Strongly Disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly Agree")

  levels2 <- c(0, 1, 2, 3, 4, 5, 6, 7)

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare basic RAP score to understanding of key RAP components
#'
#' @description calculate frequency table for basic rap score compared with understanding of key RAP components
#'
#' @param data full CARS datasetafter pre-processing
#'
#' @return frequency table (data.frame)

summarise_basic_score_by_understanding <- function(data){

  col1 <- "RAP_components"

  col2 <- "basic_rap_score"

  levels1 <- c(
    "Strongly Disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly Agree")

  levels2 <- 0:6

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Compare advanced RAP score to understanding of key RAP components
#'
#' @description calculate frequency table for advanced rap score compared with understanding of key RAP components
#'
#' @param data full CARS datasetafter pre-processing
#'
#' @return frequency table (data.frame)

summarise_adv_score_by_understanding <- function(data){

  col1 <- "RAP_components"

  col2 <- "advanced_rap_score"

  levels1 <- c(
    "Strongly Disagree",
    "Disagree",
    "Neutral",
    "Agree",
    "Strongly Agree")

  levels2 <- 0:7

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Summarise programming language knowledge by profession
#'
#' @description only used the main summary page. Needs to be turned into wide data for html table.
#'
#' @param data CARS data (pre-processed)
#'
#' @return data.frame
#'
#' @importFrom dplyr recode

summarise_languages_by_prof <- function(data) {

  profs <- c("prof_DS", "prof_DDAT", "prof_GAD", "prof_GES", "prof_geog",
             "prof_GORS", "prof_GSR", "prof_GSG")

  prof_names <- c("Data scientists",
                  "Digital and data (DDAT)",
                  "Actuaries",
                  "Economists (GES)",
                  "Geographers",
                  "Operational researchers (GORS)",
                  "Social researchers (GSR)",
                  "Statisticians (GSG)")

  names(prof_names) <- profs

  outputs <- lapply(profs, function(prof) {
    filtered_data <- data[data[prof] == "Yes", ]

    output <- summarise_coding_tools(filtered_data, "knowledge", prop = FALSE)

    # Retain frequencies for "Yes" responses only
    output <- output[output[[2]] == "Yes", ]

    output$value <- prof

    output$n <- output$n / ifelse(sum(output$n)==0, 1, sum(output$n))

    return(output)
  })

  outputs <- do.call(rbind, outputs)

  colnames(outputs) <- c("lang", "prof", "n")
  outputs <- outputs[, c("prof", "lang", "n")]
  rownames(outputs) <- NULL

  outputs$prof <- recode(outputs$prof, !!!prof_names)

  return(outputs)
}


#' @title Calculate frequencies
#'
#' @description Returns a frequency table in tidy data format.
#'
#' @param data full CARS data frame  after pre-processing
#' @param questions columns to filter data on
#' @param levels all possible factor values in the filtered columns
#' @param labels labels to rename the column headers. Only needed for multi-column frequencies
#' @param prop whether to return proportion data (0-1). TRUE by default. Assumes mutually exclusive response options.
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom dplyr select all_of group_by count mutate recode arrange
#' @importFrom tidyr pivot_longer drop_na

calculate_freqs <- function(data, questions, levels, labels = NULL, prop = TRUE){

  if (!is.null(labels)) {
    labels_list <- as.list(labels)
    names(labels_list) <- questions
  } else if (length(questions) > 1) {
    stop("Missing input: labels needed for mutli-column frequencies.")
  }

  selected_data <- data %>% select(all_of(questions))


  selected_data[] <- lapply(selected_data, factor, levels = levels)

  if (length(questions) == 1) {
    frequencies <- data.frame(table(selected_data[questions]))

    colnames(frequencies) <- c("value", "n")

    if (prop) {

      frequencies$n <- frequencies$n / ifelse(sum(frequencies$n, na.rm = TRUE)==0,
                                              1,
                                              sum(frequencies$n, na.rm = TRUE))
    }

  } else {
    frequencies <- selected_data %>%
      pivot_longer(cols = questions,
                   names_to = "name",
                   values_to = "value") %>%
      group_by(.data$name) %>%
      count(.data$value, .drop=FALSE) %>%
      mutate(name = recode(.data$name, !!!labels_list)) %>%
      arrange(.data$name, by_group=TRUE) %>%
      drop_na() %>%
      data.frame()

    colnames(frequencies) <- c("name", "value", "n")

    if (prop) {
      frequencies <- prop_by_group(frequencies)
    }
  }

  return(frequencies)
}

#' @title Create tidy cross table
#'
#' @description Returns a cross table in tidy data format.
#'
#' @param data pre-processed CARS data set
#' @param col1 first column of interest
#' @param col2 column to cross-tabulate first column against
#' @param levels1 factor levels for col1
#' @param levels2 factor levels for col2
#' @param prop whether to return proportion data (0-1). TRUE by default. Assumes mutually exclusive response options.
#'
#' @return data.frame
#'
#' @importFrom dplyr all_of across

calculate_multi_table_freqs <- function(data, col1, col2, levels1, levels2, prop = TRUE){

  selected_data <- data %>% dplyr::select(all_of(c(col1, col2)))

  selected_data[col1] <- factor(selected_data[[col1]], levels = levels1)

  selected_data[col2] <- factor(selected_data[[col2]], levels = levels2)

  frequencies <- selected_data %>%
    count(across(c(col1, col2)), .drop=FALSE) %>%
    drop_na() %>%
    data.frame()

  if(prop){
    frequencies <- prop_by_group(frequencies)
  }

  return(frequencies)

}

#' @title Convert frequencies to proportions
#'
#' @param data frequency table with three columns (can be of any name): name, value and count
#'
#' @return input data with the third column as proportion (0-1)
#'
#' @importFrom dplyr group_by_at mutate

prop_by_group <- function(data) {

  data %>%
    group_by_at(1) %>%
    mutate(n = .data$n / ifelse(sum(.data$n)==0, 1, sum(.data$n))) %>%
    data.frame()

}
