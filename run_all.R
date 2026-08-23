# ==============================================================================
# FILE:    run_all.R
# AUTHOR:  Chan Jun Jie
# PURPOSE: Run the pipeline in order, timing each stage and stopping at the
#          first failure. Replaces following the README's numbered list by hand.
#
# USAGE:   Rscript run_all.R              # every stage
#          Rscript run_all.R 06 07 08     # only these stages
#          Rscript run_all.R --from 06    # this stage onward
#          Rscript run_all.R --list       # show the stages and exit
#
# OUTPUTS: output/pipeline_timings.csv
# ==============================================================================

STAGES <- c(
  "scripts/01_data_cleaning.R",
  "scripts/02_geocoding.R",
  "scripts/02b_validate_geocoding.R",
  "scripts/03_feature_engineering.R",
  "scripts/04_build_modelling_data.R",
  "scripts/05_eda.R",
  "scripts/06_model_linear.R",
  "scripts/07_model_regularised.R",
  "scripts/08_model_advanced.R",
  "scripts/09_compare_models.R",
  "scripts/10_build_app_bundle.R"
)

# Leading number of each filename: "scripts/02b_validate_geocoding.R" -> "02b".
stage_ids <- sub("^([0-9]+[a-z]?)_.*$", "\\1", basename(STAGES))

args <- commandArgs(trailingOnly = TRUE)

if ("--list" %in% args) {
  cat(paste0("  ", stage_ids, "  ", STAGES, collapse = "\n"), "\n")
  quit(status = 0)
}

selected <- if (length(args) == 0) {
  STAGES
} else if (args[1] == "--from") {
  start <- match(args[2], stage_ids)
  if (is.na(start)) stop("unknown stage: ", args[2], call. = FALSE)
  STAGES[start:length(STAGES)]
} else {
  unknown <- setdiff(args, stage_ids)
  if (length(unknown) > 0) {
    stop("unknown stage(s): ", paste(unknown, collapse = ", "), call. = FALSE)
  }
  STAGES[stage_ids %in% args]
}

# Each stage runs in its own R session. Sourcing them into one session would let
# a variable defined in an early script satisfy a later one that forgot to
# create it, so the pipeline would pass here and fail when run stage by stage.
rscript <- file.path(R.home("bin"), "Rscript")

fmt <- function(seconds) {
  if (seconds < 60) return(sprintf("%.1fs", seconds))
  sprintf("%dm %02ds", as.integer(seconds) %/% 60, as.integer(seconds) %% 60)
}

results <- data.frame()
started <- Sys.time()

for (stage in selected) {
  cat("\n", strrep("=", 78), "\n", sep = "")
  cat("RUN  ", stage, "   (", format(Sys.time(), "%H:%M:%S"), ")\n", sep = "")
  cat(strrep("=", 78), "\n", sep = "")

  t0 <- Sys.time()
  status <- system2(rscript, shQuote(stage))
  elapsed <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  results <- rbind(results, data.frame(
    stage = stage, status = status, seconds = round(elapsed, 1)
  ))

  if (status != 0) {
    cat("\nFAILED  ", stage, " exited ", status, " after ", fmt(elapsed),
        "\nStopping; later stages would run on stale inputs.\n", sep = "")
    dir.create("output", showWarnings = FALSE)
    write.csv(results, "output/pipeline_timings.csv", row.names = FALSE)
    quit(status = 1)
  }

  cat("\nOK   ", stage, "  ", fmt(elapsed), "\n", sep = "")
}

cat("\n", strrep("=", 78), "\n", sep = "")
cat("PIPELINE COMPLETE   total ",
    fmt(as.numeric(difftime(Sys.time(), started, units = "secs"))), "\n\n",
    sep = "")
for (i in seq_len(nrow(results))) {
  cat(sprintf("  %-40s %10s\n", results$stage[i], fmt(results$seconds[i])))
}

dir.create("output", showWarnings = FALSE)
write.csv(results, "output/pipeline_timings.csv", row.names = FALSE)
cat("\nTimings written to output/pipeline_timings.csv\n")
