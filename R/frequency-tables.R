#' @title Summarise all
#'
#' @description Produce all summary tables and return as a named list.
#'
#' @param data full CARS dataset after pre-processing
#' @param config CARS config
#' @param all_tables logical: whether to produce all summary output tables. Defaults to FALSE.
#' @param sample additionally returns count and sample size for selected tables for QA. FALSE by default
#'
#' @return list of frequency tables
#'
#' @export

summarise_all <- function(data, config, all_tables = FALSE, sample = TRUE) {

  output_list <- list(
    code_freq = summarise_data(data, config, question = "code_freq", sample = sample),
    knowledge = summarise_coding_tools(data, config, question = "coding_tools_knowledge", sample = sample),
    access = summarise_coding_tools(data, config, question = "coding_tools_access", sample = sample),
    first_learned = summarise_data(data, config, question = "first_learned", sample = sample),
    ability_change = summarise_data(data, config, question = "ability_change" , sample = sample),
    coding_years = summarise_data(data, config, question = "coding_years" , sample = sample),
    coding_practices = summarise_multi_col_data(data, config, question = "coding_practices" , sample = sample),
    working_practices = summarise_multi_col_data(data, config, question = "working_practices" , sample = sample),
    doc = summarise_multi_col_data(data, config, question = "doc" , sample = sample),
    rap_knowledge = summarise_data(data, config, question = "heard_of_rap" , sample = sample),
    rap_opinions = summarise_rap_opinions(data, config, question = "rap_opinions" , sample = sample),
    git_knowledge = summarise_git(data, config, question = "coding_tools_knowledge" , sample = sample),
    access_git = summarise_git(data, config, question = "coding_tools_access" , sample = sample),
    rap_components = summarise_rap_comp(data, config, question = "rap_components", sample = sample)

  )

  if (all_tables) {

    output_list <- c(output_list,
                     list(
                       coding_exp = summarise_data(data, config, question = "coding_exp", sample = sample),
                       team = summarise_data(data, config, question = "team", sample = sample),
                       management = summarise_data(data, config, question = "management", sample = sample),
                       code_leisure = summarise_data(data, config, question = "code_leisure", sample = sample),
                       ai = summarise_data(data, config, question = "ai" , sample = sample),
                       ai_tools = summarise_data(data, config, question = "ai_tools" , sample = sample),
                       ai_use = summarise_data(data, config, question = "ai_use" , sample = sample),
                       ai_trust = summarise_data(data, config, question = "ai_trust" , sample = sample),
                       qs_aware = summarise_data(data, config, question = "qs_aware" , sample = sample),
                       qs_comply = summarise_data(data, config, question = "qs_comply" , sample = sample),
                       qq_aware = summarise_data(data, config, question = "qq_aware" , sample = sample),
                       capability_change_by_freq = summarise_cap_change_by_freq(data, config, question1 = "code_freq", question2 = "ability_change", sample = sample),
                       languages_by_prof = summarise_languages_by_prof(data, config, question = "professions" , sample = sample)
                       # capability_change_by_line_manage = summarise_cap_change_by_line_manage(data),
                       # capability_change_by_CS_grade = summarise_cap_change_by_CS_grade(data),
                       # basic_score_by_implementation = summarise_basic_score_by_imp(data),
                       # adv_score_by_implementation = summarise_adv_score_by_imp(data),
                       # basic_score_by_understanding = summarise_basic_score_by_understanding(data),
                       # adv_score_by_understanding = summarise_adv_score_by_understanding(data),
                       # language_status = summarise_language_status(data),
                       # open_source_by_prof = summarise_open_source_by_prof(data),
                       # heard_of_RAP_by_prof = summarise_heard_of_RAP_by_prof(data)
                       ))
  }

  return(output_list)
}

#' Generic summarise data into frequency table
#'
#'
#' @param data cleaned CARS dataset
#' @param config CARS config
#' @param question question name taken from config
#' @param prop additionally returns value as a proportion. TRUE by default
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return frequency table df
#' @export

summarise_data <- function(data, config, question, prop = TRUE, sample = TRUE) {

  list2env(get_question_data(config, question), envir = environment())

  data[] <- lapply(data, factor, levels = levels)

  frequencies <- calculate_freqs(data, cols, labels, prop = prop, sample = sample)

  return(frequencies)
}


#' @title Generic summarise multi-column data into frequency table
#'
#' @description calculate frequency table for multi-column data where no additional operations are needed
#'
#' @param data cleaned CARS dataset
#' @param config CARS config
#' @param question question name taken from config
#' @param prop additionally returns value as a proportion. TRUE by default
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return frequency table (data.frame)
#' @export

summarise_multi_col_data <- function(data, config, question, prop = TRUE, sample = TRUE) {

  list2env(get_question_data(config, question), envir = environment())

  labels <- config[[question]][["cols"]]

  data[] <- lapply(data, factor, levels = levels)

  frequencies <- calculate_freqs(data, cols, labels, prop = prop, sample = sample)

  frequencies <- dplyr::arrange(frequencies, name, match(value, levels))

  return(frequencies)

}


#' @title Summarise coding tools
#'
#' @description calculate frequency table coding tools (knowledge or access)
#'
#' @param data full CARS dataset after pre-processing
#' @param config CARS config
#' @param question question name taken from config
#' @param prop whether to return proportion data (0-1). TRUE by default. Assumes mutually exclusive response options.
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return frequency table (data.frame)
#' @export

summarise_coding_tools <- function(data, config, question, prop = TRUE, sample = TRUE) {

  list2env(get_question_data(config, question), envir = environment())

  cols <- cols[!grepl("git|other", cols)]

  data[] <- lapply(data, factor, levels = levels)

  frequencies <- calculate_freqs(data, cols, labels, prop = prop, sample = sample)

  frequencies <- dplyr::arrange(frequencies, factor(name, levels = labels))

  return(frequencies)

}


#' @title Opinions of RAP
#'
#' @description Create frequency table of opinions of RAP
#'
#' @param data full CARS dataset after pre-processing
#' @param config CARS config
#' @param question rap opinions question, taken from config
#' @param prop returns proportion, TRUE by default
#' @param sample additionally returns count and sample size. TRUE by default
#'
#' @return frequency table (data.frame)
#' @export
#'
summarise_rap_opinions <- function(data, config, question, prop = TRUE, sample = TRUE) {

  list2env(get_question_data(config, question), envir = environment())

  labels <- config[[question]][["cols"]]

  cols <- cols[!grepl("other", cols)]

  data[] <- lapply(data, factor, levels = levels)

  frequencies <- calculate_freqs(data, cols, labels, prop = prop, sample = sample)

  frequencies$name <- factor(frequencies$name, levels = labels)

  frequencies <- dplyr::arrange(frequencies, name, match(value, levels))

  frequencies$name <- as.character(frequencies$name)

  return(frequencies)

}



#' @title Summarise programming language status
#'
#' @description calculate counts of responents reporting knowledge of, use of, or both for each programming language.
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
                 "status_matlab",
                 "status_dax",
                 "status_spark")

  levels <- c("Use Only", "Both", "Knowledge Only")

  labels <- c("R",
              "SQL",
              "SAS",
              "VBA",
              "Python",
              "SPSS",
              "Stata",
              "Matlab",
              "DAX",
              "Spark")

  data[] <- lapply(data, factor, levels = levels)

  frequencies <- calculate_freqs(data, questions, labels, prop = TRUE, sample = FALSE)

  return(frequencies)

}




#' @title Summarise access to git and knowledge of git
#'
#' @description calculate frequency table for if someone has access to git and calculate frequency table for if someone knows how to version control using git
#'
#' @param data full CARS dataset after pre-processing
#' @param config CARS config
#' @param question coding tools knowledge or access question, taken from config
#' @param prop returns proportion, TRUE by default
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return frequency table (data.frame)
#' @export

summarise_git <- function(data, config, question, prop = TRUE, sample = TRUE) {

  list2env(get_question_data(config, question), envir = environment())

  cols <- cols[grepl("git", cols)]

  data[] <- lapply(data, factor, levels = levels)

  frequencies <- calculate_freqs(data, cols, labels, prop = TRUE, sample = sample)

  return(frequencies)

}


#' @title Summarise capability change by coding frequency
#'
#' @description calculate the cross tab of coding frequency by capability change
#'
#' @param data full CARS dataset after pre-processing
#' @param config CARS config
#' @param question1 code freq question, string, taken from config
#' @param question2 capability change question, string, taken from config
#' @param prop returns proportion, TRUE by default
#' @param sample returns count and sample size. FALSE by default
#'
#' @return frequency table (data.frame)

summarise_cap_change_by_freq <- function(data, config, question1, question2, prop = TRUE, sample = FALSE){

  q1 <- get_question_data(config, question1)
  col1 <- q1[["cols"]]
  levels1 <- q1[["levels"]][-1]

  q2 <- get_question_data(config, question2)
  col2 <- q2[["cols"]]
  levels2 <- q2[["levels"]]

  data <- dplyr::filter(data, (coding_exp == "Yes" & data$first_learned != "Current role"))

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2, prop, sample)

  return(frequencies)

}

#' @title RAP score components
#'
#' @description Create frequency table of basic and advanced RAP score components
#'
#' @param data full CARS dataset after pre-processing
#' @param config CARS config
#' @param question rap components question, taken from config
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return frequency table (data.frame)
#'
#' @importFrom dplyr mutate arrange

summarise_rap_comp <- function(data, config, question, sample = TRUE) {

  list2env(get_question_data(config, question), envir = environment())

  labels <- config[[question]][["cols"]]

  levels <- c(1)
  selected_data <- data
  selected_data[] <- lapply(data, factor, levels = levels)

  components <- calculate_freqs(selected_data, cols, labels, prop = FALSE, sample = FALSE)

  components <- components %>%
    dplyr::mutate(name = factor(name, levels = labels)) %>%
    dplyr::arrange(name) %>%
    dplyr::mutate(value = c(rep("Basic", 6), rep("Advanced", 8))) %>%
    dplyr::mutate(n = colSums(data[cols], na.rm = TRUE) / sum(data$code_freq != "Never", na.rm = TRUE))

  names(components$n) <- NULL

  if (sample == TRUE) {
    components <- components %>%
      dplyr::mutate(count = colSums(data[cols], na.rm = TRUE))

    components$sample <- sum(data$code_freq != "Never", na.rm = TRUE)
  }

  return(components)

}

#' @title Summarise capability change by management responsibility
#'
#' @description calculate the cross tab of capability change by management responsibility
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_cap_change_by_line_manage <- function(data){

  col1 <- "management"

  col2 <- "coding_ability_change"

  levels1 <- c("Yes",
               "No - I manage people who do not write code",
               "No - I don't line manage anyone")

  levels2 <- c(
    "It has become significantly worse",
    "It has become slightly worse",
    "It has stayed the same",
    "It has become slightly better",
    "It has become significantly better")

  frequencies <- calculate_multi_table_freqs(data, col1, col2, levels1, levels2)

  return(frequencies)

}


#' @title Summarise capability change by CS grade
#'
#' @description calculate the cross tab of capability change by CS grade
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)

summarise_cap_change_by_CS_grade <- function(data){

  col1 <- "CS_grade"

  col2 <- "coding_ability_change"

  levels1 <- c("Higher Executive Officer (or equivalent)",
               "Senior Executive Officer (or equivalent)",
               "Grade 6 and 7")

  levels2 <- c(
    "It has become significantly worse",
    "It has become slightly worse",
    "It has stayed the same",
    "It has become slightly better",
    "It has become significantly better")

  selected_data <- data |>
    dplyr::select(CS_grade, coding_ability_change) |>
    dplyr::mutate(CS_grade = dplyr::case_when(CS_grade %in% c("Grade 7 (or equivalent)",
                                                              "Grade 6 (or equivalent)") ~ "Grade 6 and 7",
                                              TRUE ~ CS_grade))

  frequencies <- calculate_multi_table_freqs(selected_data, col1, col2, levels1, levels2)

  return(frequencies)

}

#' @title Summarise programming language knowledge by profession
#'
#' @description only used the main summary page. Needs to be turned into wide data for html table.
#'
#' @param data CARS data (pre-processed)
#' @param config CARS config
#' @param question professions question, taken from config
#' @param prop returns proportion, TRUE by default
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return data.frame
#'
#' @importFrom dplyr recode

summarise_languages_by_prof <- function(data, config, question, prop = TRUE, sample = FALSE) {

  list2env(get_question_data(config, question), envir = environment())
  cols <- cols[!grepl("none|other", cols)]
  labels <- config[[question]][["cols"]]
  labels <- labels[!grepl("no profession|Other", labels)]

  outputs <- lapply(cols, function(cols) {
    filtered_data <- dplyr::filter(data, get(cols) == "Yes")

    if(nrow(filtered_data) > 0) {

      output <- summarise_coding_tools(filtered_data, config, question = "coding_tools_knowledge", prop = prop, sample = sample)

      # Retain frequencies for "Yes" responses only
      output <- output[output[[2]] == "Yes", ]

      output$value <- cols

      return(output)
    }
  })

  outputs <- do.call(rbind, outputs)

  colnames(outputs) <- c("Language", "Profession", "n")
  rownames(outputs) <- NULL

  outputs$Profession <- dplyr::recode(outputs$Profession, !!!labels)

  return(outputs)
}


#' @title Summarise open source practice by profession
#'
#' @description only used the main summary page. Needs to be turned into wide data for html table.
#'
#' @param data CARS data (pre-processed)
#'
#' @return data.frame
#'
#' @importFrom dplyr recode

summarise_open_source_by_prof <- function(data) {

  profs <- c("prof_DE", "prof_DS", "prof_DDAT", "prof_GAD", "prof_GES", "prof_geog",
             "prof_GORS", "prof_GSR", "prof_GSG")

  prof_names <- c("Data engineers",
                  "Data scientists",
                  "Digital and data (DDAT)",
                  "Actuaries",
                  "Economists (GES)",
                  "Geographers",
                  "Operational researchers (GORS)",
                  "Social researchers (GSR)",
                  "Statisticians (GSG)")

  names(prof_names) <- profs

  outputs <- lapply(profs, function(prof) {
    filtered_data <- dplyr::filter(data, get(prof) == "Yes")

    if(nrow(filtered_data) > 0) {

      output <- summarise_coding_practices(filtered_data)

      output <- output[output[[1]] == "Use open source software",]

      output$name <- prof

      return(output)
    }
  })

  outputs <- do.call(rbind, outputs)

  rownames(outputs) <- NULL

  outputs$name <- recode(outputs$name, !!!prof_names)

  return(outputs)
}


#' @title Summarise heard of RAP by profession
#'
#' @description Create frequency table of RAP awareness for professions
#'
#' @param data full CARS dataset after pre-processing
#'
#' @return frequency table (data.frame)
#'
#' @importFrom dplyr filter mutate case_match arrange

summarise_heard_of_RAP_by_prof <- function(data) {

  filtered_data <- dplyr::filter(data, workplace == "Civil service, including devolved administrations")
  filtered_RAP_data <- dplyr::filter(filtered_data, heard_of_RAP == "Yes")

  questions <- c("heard_of_RAP")

  profs <- c("prof_DE", "prof_DS", "prof_DDAT", "prof_GAD", "prof_GES", "prof_geog",
             "prof_GORS", "prof_GSR", "prof_GSG")

  prof_names <- c("Data engineers",
                  "Data scientists",
                  "Digital and data (DDAT)",
                  "Actuaries",
                  "Economists (GES)",
                  "Geographers",
                  "Operational researchers (GORS)",
                  "Social researchers (GSR)",
                  "Statisticians (GSG)")

  names(prof_names) <- profs

  frequencies <- calculate_freqs(filtered_data, questions, profs)

  frequencies <- frequencies |>
    dplyr::mutate(value = factor(value, levels = profs)) |>
    dplyr::arrange(value) |>
    dplyr::mutate(n = colSums(filtered_RAP_data[profs] == "Yes") / ifelse(colSums(filtered_data[profs] == "Yes") != 0,
                                                                          colSums(filtered_data[profs] == "Yes"),
                                                                          1))

  rownames(frequencies) <- NULL
  names(frequencies$n) <- NULL

  frequencies$value <- recode(frequencies$value, !!!prof_names)

  return(frequencies)

}

#' @title Summarise open source vs proprietary capability
#'
#' @description Calculate proportion of respondents who have capability in R/Python vs SAS/SPSS/stata
#'
#' @param data Full CARS dataset including previous waves
#'
#' @return data.frame
#'
#' @export

summarise_os_vs_prop <- function(data) {


  data$open_source_lang_knowledge <- ifelse(
    data$knowledge_python == "Yes" | data$knowledge_R == "Yes",
    TRUE, FALSE
  )

  data$prop_lang_knowledge <- ifelse(
    data$knowledge_SAS == "Yes" |
      data$knowledge_SPSS == "Yes" |
      data$knowledge_stata == "Yes",
    TRUE, FALSE
  )

  os_freqs <- data |>
    dplyr::group_by(year) |>
    dplyr::summarise(Freq = sum(open_source_lang_knowledge, na.rm = TRUE), sample = dplyr::n()) |>
    data.frame() |>
    CARS::get_ci(freq_col = 2, n_col = 3)

  os_freqs <- cbind(lang_type = "Open Source", os_freqs)

  prop_freqs <- data |>
    dplyr::group_by(year) |>
    dplyr::summarise(Freq = sum(prop_lang_knowledge, na.rm = TRUE), sample = dplyr::n()) |>
    data.frame() |>
    CARS::get_ci(freq_col = 2, n_col = 3)

  prop_freqs <- cbind(lang_type = "Proprietary", prop_freqs)

  grouped_lang_freqs <- rbind(os_freqs, prop_freqs)
  grouped_lang_freqs$year <- as.character(grouped_lang_freqs$year)
  grouped_lang_freqs$lang_type <- factor(grouped_lang_freqs$lang_type, levels = c("Open Source", "Proprietary"))

  return(grouped_lang_freqs)
}

#' @title RAP awareness over time
#'
#' @param data all wave data
#'
#' @return data frame
#' @export

summarise_rap_awareness_over_time <- function(data) {

    data <- data[data$code_freq != "Never", ]

    RAP_awareness <- table(data$heard_of_RAP, data$year) |>
    data.frame() |>
    dplyr::group_by(Var2) |>
    dplyr::mutate(Count = sum(Freq)) |>
    dplyr::filter(Var1 == "Yes") |>
    data.frame()  |>
    CARS::get_ci(freq_col = 3, n_col = 4) |>
    dplyr::rename(n = "percent")

    return(RAP_awareness)
}


#' @title Get question metadata
#'
#' @param config CARS config file
#' @param question question name as written in config file
#'
#' @return List of data cols, labels (if multiple cols), and factor levels for the specified question
#' @export

get_question_data <- function(config, question){

  if (!is.null(config[[question]][["cols"]])){

    cols <- names(config[[question]][["cols"]])
    labels <- config[[question]][["cols"]]

  } else {

    cols <- question
    labels <- NULL
  }

  levels <- config[[question]][["levels"]]
  full_question <- config[[question]][["question"]]

  return(list(cols = cols, labels = labels, levels = levels, full_question = full_question))

}

#' @title Calculate frequencies
#'
#' @description Returns a frequency table in tidy data format.
#'
#' @param data full CARS data frame  after pre-processing
#' @param cols columns to filter data on
#' @param labels labels to rename the column headers. Only needed for multi-column frequencies
#' @param prop whether to return proportion data (0-1). TRUE by default. Assumes mutually exclusive response options.
#' @param sample additionally returns count and sample size. FALSE by default
#'
#' @return data.frame
#'
#' @export
#'
#' @importFrom dplyr select all_of group_by count mutate recode arrange
#' @importFrom tidyr pivot_longer drop_na

calculate_freqs <- function(data, cols, labels, prop = TRUE, sample = FALSE){

  if (sum(!cols %in% colnames(data)) > 0) {
    stop("unexpected_input: check column names")
  }

  if (is.null(labels) & (length(cols) > 1)) {
    stop("Missing input: labels needed for mutli-column frequencies.")
  }

  data <- data |>
    dplyr::select(dplyr::any_of(cols))

  if (length(cols) == 1) {
    frequencies <- data.frame(table(data[cols]))

    colnames(frequencies) <- c("value", "n")

  } else {
    frequencies <- data |>
      tidyr::pivot_longer(cols = dplyr::any_of(cols),
                          names_to = "name",
                          values_to = "value") |>
      dplyr::group_by(name) |>
      dplyr::count(value, .drop=FALSE) |>
      dplyr::mutate(name = dplyr::recode(name, !!!labels)) |>
      dplyr::arrange(name, by_group=TRUE) |>
      tidyr::drop_na() |>
      data.frame()

    colnames(frequencies) <- c("name", "value", "n")

  }


  if (sample) {

    frequencies <- add_sample_size(frequencies, prop)
  }

  if (prop) {

    frequencies <- count_to_prop(frequencies)
  }



  return(frequencies)
}


#' @title Convert frequencies to proportions
#'
#' @param data frequency table with three columns (can be of any name): name, value and count
#'
#' @return input data with the third column as proportion (0-1)
#'
#' @importFrom dplyr group_by mutate

count_to_prop <- function(data){

  data <- data |>
    dplyr::group_by(dplyr::across(dplyr::any_of("name"))) |>
    dplyr::mutate(n = n / ifelse(sum(n, na.rm = TRUE)==0, 1, sum(n, na.rm = TRUE))) |>
    data.frame()

  return(data)

}

#' @title Add sample size column
#'
#' @param frequencies frequency table with three columns (can be of any name): name, value and count
#' @param prop whether to return proportion data (0-1).
#'
#' @return input data with the third column as sample size (total number of responses)
#'
#' @import dplyr

add_sample_size <- function(frequencies, prop){

  frequencies <- frequencies |>
    dplyr::group_by(dplyr::across(dplyr::any_of("name"))) |>
    dplyr::mutate(count = n) |>
    dplyr::mutate(sample = sum(n)) |>
    data.frame()

  if (!prop) {
    frequencies <- dplyr::select(frequencies, -count)
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
#' @param sample returns proportion, count and, group size and sample size. FALSE by default
#'
#' @return data.frame
#'
#' @importFrom dplyr all_of across

calculate_multi_table_freqs <- function(data, col1, col2, levels1, levels2, prop = TRUE, sample = FALSE){

  selected_data <- data |> dplyr::select(dplyr::all_of(c(col1, col2)))

  selected_data[col1] <- factor(selected_data[[col1]], levels = levels1)

  selected_data[col2] <- factor(selected_data[[col2]], levels = levels2)

  frequencies <- selected_data |>
    dplyr::count(dplyr::across(dplyr::all_of(c(col1, col2))), .drop=FALSE) |>
    tidyr::drop_na() |>
    data.frame()

  if (sample) {
    frequencies <- add_sample_size(frequencies, prop)
  }

  if (prop) {
    frequencies <- count_to_prop(frequencies)
  }

  return(frequencies)

}
