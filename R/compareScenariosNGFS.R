#' Render CompareScenariosNGFS
#'
#' Renders the *.Rmd-files associated to CompareScenariosNGFS, based on
#' CompareScenarios2. In the Rmds,
#' scenario- and historical .mif-files are loaded. Then plots are created from
#' this data. The result may be rendered to PDF or HTML. Alternatively one can
#' choose Rmd as output format and obtain a copy of the *.Rmd-files.
#'
#' @param snapshot \code{character(n)}, Path to NGFS snapshot.
#' @param mifHist \code{character(1)}. Path to historical mif.
#' @param outputFile \code{character(1)}. File name (without extension) of the
#'   output document to be created.
#' @param outputDir \code{character(1)}. The directory where the output document
#'   and intermediary files are created.
#' @param outputFormat \code{character(1)}, not case-sensitive. \code{"html"},
#'   \code{"pdf"}, or \code{"rmd"}.
#' @param envir \code{environment}. The environment in which the code chunks are
#'   to be evaluated. See the argument of the same name in
#'   \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#'   Set this to \code{globalenv()} and \code{sections} to \code{NULL} to load
#'   an preprocess data in your global environment during development.
#' @param ... YAML parameters, see below.
#' @return The value returned by \code{\link[rmarkdown:render]{rmarkdown::render()}}.
#' @section YAML Parameters:
#' \describe{
#'   \item{\code{cfgScen}}{
#'     \code{character(n) or NULL}.
#'     Paths to config.Rdata files containing the \code{cfg} object for each
#'     scenario. The paths must be provided in the same order as \code{snapshot}.
#'     If provided, some information gathered from these files is
#'     shown at the beginning of the output document.}
#'   \item{\code{cfgDefault}}{
#'     \code{character(1) or NULL}.
#'     Path to default.cfg, which creates a \code{cfg} object with default
#'     values. If provided, some information gathered from this file is
#'     shown at the beginning of the output document.}
#'   \item{\code{yearsScen}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(2005, 2060, 5), seq(2070, 2100, 10))}.
#'     Years to show for scenario data.}
#'   \item{\code{yearsHist}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(seq(1960, 2020, 1), seq(2025, 2100, 5))}.
#'     Years to show for historical data.}
#'   \item{\code{yearsBarPlot}}{
#'     \code{numeric(n)}.
#'     Default: \code{c(2010, 2030, 2050, 2100)}.
#'     Years to show in bar plots of scenario data.}
#'   \item{\code{yearRef}}{
#'     \code{numeric(1)}.
#'     Default: \code{2020}.
#'     A reference year used to show relative values in Kaya decomposition.}
#'   \item{\code{reg}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Regions to show. \code{NULL} means all.}
#'   \item{\code{sections}}{
#'     \code{character(n) or numeric(n) or NULL}.
#'     Default: \code{"all"}.
#'     Names or numbers of sections to include. For names subset of
#'     \code{c("00_info", "01_summary", "02_macro", "03_emissions",
#'     "04_energy_supply", "05_energy_demand", "06_energy_services",
#'     "07_climate", "08_sdp", "09_carbon_management", "99_further_info")}.
#'     Use \code{"all"} to include all available sections.
#'     Use \code{NULL} to not include any section
#'     (useful in combination with parameter \code{envir}).}
#'   \item{\code{userSectionPath}}{
#'     \code{NULL} or \code{character(n)}.
#'     Default: \code{NULL}.
#'     Path to a *.Rmd-file that may be included as additional section.}
#'   \item{\code{mainReg}}{
#'     \code{character(1)}.
#'     Default: \code{"World"}.
#'     A region for which larger plots are shown.}
#'   \item{\code{figWidth, figHeight}}{
#'     \code{numeric(1)}.
#'     Default: \code{15} and \code{10}, respectively.
#'     Size of plots in inches.}
#'   \item{\code{warning}}{
#'     \code{logical(1)}.
#'     Default: \code{TRUE}.
#'     Show warnings in output?}
#' }
#' @author Christof Schoetz, Pascal Weigmann
#' @examples
#' \dontrun{
#' # Simple use. Creates PDF:
#' compareScenariosNGFS(
#'   snapshot = "path/to/snapshotFolder",
#'   mifHist = "path/to/historical.mif",
#'   outputFile = "CompareScenariosNGFSExample")
#' # More complex use. Creates folder with Rmds:
#' compareScenariosNGFS(
#'   snapshot = "path/to/snapshotFolder",
#'   mifHist = "path/to/historical.mif",
#'   cfgScen = c("path/to/scen1/config.RData", "path/to/scen2/config.RData"),
#'   cfgDefault = "path/to/default.cfg",
#'   outputDir = "path/to/output",
#'   outputFormat = "Rmd",
#'   outputFile = format(Sys.time(), "compScen_%Y%m%d-%H%M%S"),
#'   warning = FALSE,
#'   sections = c(0, 2, 3, 99),
#'   userSectionPath = "path/to/myPlots.Rmd")
#' # Use in development. Load data into global environment:
#' compareScenariosNGFS(
#'   snapshot = "path/to/snapshotFolder",
#'   mifHist = "path/to/historical.mif",
#'   outputFile = format(Sys.time(), "csN_load_%Y%m%d-%H%M%S"),
#'   sections = NULL,
#'   envir = globalenv())
#' }
#' @export
compareScenariosNGFS <- function(
  snapshot, mifHist,
  outputDir = getwd(),
  outputFile = "CompareScenariosNGFS",
  outputFormat = "PDF",
  envir = new.env(),
  ...
  ) {
  # Set yaml parameters and convert relative to absolute paths.
  yamlParams <- c(
    list(
      snapshot = normalizePath(snapshot, mustWork = TRUE),
      mifHist = normalizePath(mifHist, mustWork = TRUE)),
    list(...))
  if (!is.null(yamlParams[["cfgScen"]])) {
    yamlParams$cfgScen <- normalizePath(yamlParams$cfgScen, mustWork = TRUE)
  }
  if (!is.null(yamlParams[["cfgDefault"]])) {
    yamlParams$cfgDefault <- normalizePath(yamlParams$cfgDefault, mustWork = TRUE)
  }
  if (!is.null(yamlParams[["userSectionPath"]])) {
    yamlParams$userSectionPath <- normalizePath(yamlParams$userSectionPath, mustWork = TRUE)
  }

  outputFormat <- tolower(outputFormat)[[1]]
  if (outputFormat == "pdf") {
    outputFormat <- "pdf_document"
  } else if (outputFormat == "html") {
    outputFormat <- "html_document"
  } else if (outputFormat == "rmd") {
    return(.compareScenariosNGFSRmd(yamlParams, outputDir, outputFile))
  }
  rmarkdown::render(
    system.file("markdown/compareScenariosNGFS/csN_main.Rmd", package = "remind2"),
    intermediates_dir = outputDir,
    output_dir = outputDir,
    output_file = outputFile,
    output_format = outputFormat,
    params = yamlParams,
    envir = envir)
}

# Copies the CompareScenariosNGFS-Rmds to the specified location and modifies
# their YAML header according to \code{yamlParams}.
.compareScenariosNGFSRmd <- function(yamlParams, outputDir, outputFile) {
  pathMain <- system.file("markdown/compareScenariosNGFS/csN_main.Rmd", package = "remind2")
  linesMain <- readLines(pathMain)
  delimiters <- grep("^(---|\\.\\.\\.)\\s*$", linesMain)
  headerMain <- linesMain[(delimiters[1]):(delimiters[2])]
  yml <- yaml::yaml.load(
    headerMain,
    handlers = list(r = function(x) ymlthis::yml_params_code(!!rlang::parse_expr(x))))
  baseYaml <- ymlthis::as_yml(yml)
  newYamlParams <- baseYaml$params
  newYamlParams[names(yamlParams)] <- yamlParams
  # if (!is.null(names(yamlParams$snapshot))) {  # naming of snapshots not supported
  #   newYamlParams$snapshotNames <- names(yamlParams$snapshot)
  # }
  newYaml <- ymlthis::yml_replace(
    baseYaml,
    params = newYamlParams,
    date = format(Sys.Date()))
  pathDir <- file.path(outputDir, paste0(outputFile, "_Rmd"))
  if (!dir.exists(pathDir)) dir.create(pathDir)
  dirFiles <- dir(
    system.file("markdown/compareScenariosNGFS", package = "remind2"),
    full.names = TRUE)
  rmdDirFiles <- grep(
    dirFiles,
    pattern = "csN_main\\.Rmd$",
    invert = TRUE, value = TRUE)
  file.copy(rmdDirFiles, pathDir)
  ymlthis::use_rmarkdown(
    newYaml,
    path = file.path(pathDir, "csN_main.Rmd"),
    template = system.file(
      "markdown/compareScenariosNGFS/csN_main.Rmd",
      package = "remind2"),
    include_yaml = FALSE)
}
