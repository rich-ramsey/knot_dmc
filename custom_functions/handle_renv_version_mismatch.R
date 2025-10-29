# Complete renv workflow for R version differences
# Handles both directions: newer R and older R
# Fixed to handle partial restore failures properly

handle_renv_version_mismatch <- function() {
  
  cat("=== renv R Version Handler ===\n")
  
  # Check if we're in an renv project
  if (!file.exists("renv.lock")) {
    stop("No renv.lock file found. Are you in an renv project?")
  }
  
  # Read lockfile and compare R versions
  lockfile <- renv::lockfile_read()
  lockfile_r_version <- lockfile$R$Version
  current_r_version <- paste(R.version$major, R.version$minor, sep = ".")
  
  cat(sprintf("📦 Lockfile R version: %s\n", lockfile_r_version))
  cat(sprintf("💻 Your R version: %s\n", current_r_version))
  
  # Compare versions
  lockfile_numeric <- as.numeric_version(lockfile_r_version)
  current_numeric <- as.numeric_version(current_r_version)
  
  if (lockfile_numeric == current_numeric) {
    cat("✅ R versions match! Trying standard restore...\n")
  } else if (current_numeric > lockfile_numeric) {
    cat("🆙 You have NEWER R version - will update project forward\n")
  } else {
    cat("⬇️ You have OLDER R version - will install latest compatible packages\n")
  }
  
  # Get initial state
  initial_packages <- row.names(installed.packages())
  target_packages <- names(lockfile$Packages)
  
  # Try restore first (may partially succeed)
  cat("\n=== Attempting renv::restore() ===\n")
  restore_result <- try(renv::restore(prompt = FALSE), silent = TRUE)
  
  # Check what got installed after restore attempt
  post_restore_packages <- row.names(installed.packages())
  newly_installed <- setdiff(post_restore_packages, initial_packages)
  still_missing <- setdiff(target_packages, post_restore_packages)
  
  if (!inherits(restore_result, "try-error") && length(still_missing) == 0) {
    cat("✅ renv::restore() completely successful!\n")
    return(invisible(TRUE))
  }
  
  if (length(newly_installed) > 0) {
    cat(sprintf("✅ renv::restore() partially successful - installed %d packages from lockfile\n", length(newly_installed)))
    cat("📋 Packages installed from lockfile:\n")
    for (pkg in newly_installed) {
      if (pkg %in% target_packages) {
        lockfile_version <- lockfile$Packages[[pkg]]$Version
        actual_version <- as.character(packageVersion(pkg))
        cat(sprintf("  ✅ %s: %s\n", pkg, actual_version))
      }
    }
  } else {
    cat("❌ renv::restore() failed to install any packages\n")
  }
  
  if (length(still_missing) == 0) {
    cat("✅ All packages now installed!\n")
    return(invisible(TRUE))
  }
  
  cat(sprintf("\n❌ %d packages still missing after restore\n", length(still_missing)))
  
  # Separate missing packages by type
  github_missing <- character(0)
  cran_missing <- character(0)
  
  for (pkg in still_missing) {
    pkg_info <- lockfile$Packages[[pkg]]
    if (!is.null(pkg_info$RemoteType) && pkg_info$RemoteType == "github") {
      github_missing <- c(github_missing, pkg)
    } else {
      cran_missing <- c(cran_missing, pkg)
    }
  }
  
  cat(sprintf("📊 CRAN packages to install: %d\n", length(cran_missing)))
  cat(sprintf("🐙 GitHub packages to install: %d\n", length(github_missing)))
  
  lockfile_failures <- character(0)
  successful_lockfile <- character(0)
  
  # Try individual installation of missing CRAN packages (lockfile versions first)
  if (length(cran_missing) > 0) {
    cat("\n=== Installing Missing CRAN Packages (Lockfile Versions) ===\n")
    
    for (pkg in cran_missing) {
      lockfile_version <- lockfile$Packages[[pkg]]$Version
      cat(sprintf("Trying %s v%s (lockfile version)...", pkg, lockfile_version))
      
      # Try lockfile version with renv (which should respect the lockfile)
      install_result <- try(renv::install(pkg, prompt = FALSE), silent = TRUE)
      
      if (!inherits(install_result, "try-error") && pkg %in% row.names(installed.packages())) {
        actual_version <- as.character(packageVersion(pkg))
        successful_lockfile <- c(successful_lockfile, pkg)
        cat(sprintf(" ✅ (got %s)\n", actual_version))
      } else {
        lockfile_failures <- c(lockfile_failures, pkg)
        cat(" ❌ (lockfile version failed)\n")
      }
    }
  }
  
  # Try individual installation of missing GitHub packages (lockfile versions)
  if (length(github_missing) > 0) {
    cat("\n=== Installing Missing GitHub Packages (Lockfile Versions) ===\n")
    
    # Ensure remotes is available
    if (!requireNamespace("remotes", quietly = TRUE)) {
      cat("Installing remotes package...\n")
      install.packages("remotes", quiet = TRUE)
    }
    
    for (pkg in github_missing) {
      pkg_info <- lockfile$Packages[[pkg]]
      
      if (!is.null(pkg_info$RemoteUsername) && !is.null(pkg_info$RemoteRepo)) {
        remote_ref <- paste0(pkg_info$RemoteUsername, "/", pkg_info$RemoteRepo)
        
        # Add specific commit if available
        if (!is.null(pkg_info$RemoteSha)) {
          remote_ref <- paste0(remote_ref, "@", pkg_info$RemoteSha)
          cat(sprintf("Trying %s (lockfile commit %s)...", pkg, substr(pkg_info$RemoteSha, 1, 8)))
        } else {
          cat(sprintf("Trying %s from %s...", pkg, remote_ref))
        }
        
        github_result <- try(remotes::install_github(remote_ref, quiet = TRUE), silent = TRUE)
        
        if (!inherits(github_result, "try-error") && pkg %in% row.names(installed.packages())) {
          successful_lockfile <- c(successful_lockfile, pkg)
          cat(" ✅\n")
        } else {
          lockfile_failures <- c(lockfile_failures, pkg)
          cat(" ❌ (lockfile version failed)\n")
        }
      } else {
        lockfile_failures <- c(lockfile_failures, pkg)
        cat(sprintf("❌ %s: Incomplete GitHub info in lockfile\n", pkg))
      }
    }
  }
  
  # Install latest versions for packages that failed lockfile installation
  latest_successful <- character(0)
  complete_failures <- character(0)
  
  if (length(lockfile_failures) > 0) {
    cat(sprintf("\n=== Installing Latest Versions for %d Failed Packages ===\n", length(lockfile_failures)))
    
    # Separate failed packages by type for latest installation
    failed_github <- intersect(lockfile_failures, github_missing)
    failed_cran <- intersect(lockfile_failures, cran_missing)
    
    # Install latest CRAN packages
    for (pkg in failed_cran) {
      lockfile_version <- lockfile$Packages[[pkg]]$Version
      cat(sprintf("Installing %s (latest, lockfile was %s)...", pkg, lockfile_version))
      
      # Try binary first
      install_result <- try(install.packages(pkg, quiet = TRUE), silent = TRUE)
      
      if (inherits(install_result, "try-error") || !pkg %in% row.names(installed.packages())) {
        # Try source if binary fails
        cat(" (trying source)")
        source_result <- try(install.packages(pkg, type = "source", quiet = TRUE), silent = TRUE)
        
        if (!inherits(source_result, "try-error") && pkg %in% row.names(installed.packages())) {
          latest_successful <- c(latest_successful, pkg)
          actual_version <- as.character(packageVersion(pkg))
          cat(sprintf(" ✅ (got %s from source)\n", actual_version))
        } else {
          complete_failures <- c(complete_failures, pkg)
          cat(" ❌ (latest version also failed)\n")
        }
      } else {
        latest_successful <- c(latest_successful, pkg)
        actual_version <- as.character(packageVersion(pkg))
        cat(sprintf(" ✅ (got %s)\n", actual_version))
      }
    }
    
    # Install latest GitHub packages
    for (pkg in failed_github) {
      pkg_info <- lockfile$Packages[[pkg]]
      if (!is.null(pkg_info$RemoteUsername) && !is.null(pkg_info$RemoteRepo)) {
        remote_ref <- paste0(pkg_info$RemoteUsername, "/", pkg_info$RemoteRepo)
        cat(sprintf("Installing %s from %s (latest)...", pkg, remote_ref))
        
        github_result <- try(remotes::install_github(remote_ref, quiet = TRUE), silent = TRUE)
        
        if (!inherits(github_result, "try-error") && pkg %in% row.names(installed.packages())) {
          latest_successful <- c(latest_successful, pkg)
          cat(" ✅\n")
        } else {
          complete_failures <- c(complete_failures, pkg)
          cat(" ❌ (latest version also failed)\n")
        }
      }
    }
  }
  
  # Final status check and reporting
  final_installed <- row.names(installed.packages())
  final_missing <- setdiff(target_packages, final_installed)
  
  cat("\n=== Installation Summary ===\n")
  cat(sprintf("✅ From initial restore: %d packages\n", length(newly_installed)))
  cat(sprintf("✅ From lockfile versions (individual): %d packages\n", length(successful_lockfile)))
  cat(sprintf("🔄 From latest versions: %d packages\n", length(latest_successful)))
  cat(sprintf("❌ Complete failures: %d packages\n", length(complete_failures)))
  
  if (length(complete_failures) > 0) {
    cat("\nPackages that completely failed to install:\n")
    for (pkg in complete_failures) {
      lockfile_version <- lockfile$Packages[[pkg]]$Version
      cat(sprintf("  ❌ %s (wanted version %s)\n", pkg, lockfile_version))
    }
    cat("\nThese may need manual attention or system dependencies.\n")
  }
  
  # Check for version differences
  cat("\n=== Version Analysis ===\n")
  lockfile_matches <- 0
  version_differences <- character(0)
  
  for (pkg in intersect(target_packages, final_installed)) {
    lockfile_version <- lockfile$Packages[[pkg]]$Version
    current_version <- as.character(packageVersion(pkg))
    
    if (lockfile_version != current_version) {
      version_differences <- c(version_differences, pkg)
      if (pkg %in% latest_successful) {
        cat(sprintf("🔄 %s: %s → %s (upgraded due to lockfile failure)\n", pkg, lockfile_version, current_version))
      } else {
        cat(sprintf("📝 %s: %s → %s (version mismatch)\n", pkg, lockfile_version, current_version))
      }
    } else {
      lockfile_matches <- lockfile_matches + 1
    }
  }
  
  cat(sprintf("✅ %d packages match lockfile versions exactly\n", lockfile_matches))
  if (length(version_differences) > 0) {
    cat(sprintf("📊 %d packages have different versions\n", length(version_differences)))
  }
  
  # Offer to update lockfile
  cat("\n=== Update Lockfile? ===\n")
  
  should_update <- (current_numeric != lockfile_numeric) || 
    (length(version_differences) > 0) || 
    (length(complete_failures) > 0)
  
  if (should_update) {
    cat("The following changes would be recorded in the lockfile:\n")
    if (current_numeric != lockfile_numeric) {
      cat(sprintf("📦 R version: %s → %s\n", lockfile_r_version, current_r_version))
    }
    if (length(latest_successful) > 0) {
      cat(sprintf("🔄 Packages updated to latest: %d\n", length(latest_successful)))
    }
    if (length(version_differences) > 0) {
      cat(sprintf("📝 Total version changes: %d\n", length(version_differences)))
    }
    if (length(complete_failures) > 0) {
      cat(sprintf("➖ Packages to remove from lockfile: %d\n", length(complete_failures)))
    }
    
    response <- readline("\nUpdate renv.lock with these changes? (y/n): ")
    if (tolower(trimws(response)) == "y") {
      cat("Updating lockfile...\n")
      renv::snapshot()
      cat("✅ Lockfile updated!\n")
      
      # Generate commit message suggestion
      cat("\n=== Suggested Commit Message ===\n")
      if (current_numeric > lockfile_numeric) {
        cat(sprintf("Update renv.lock for R %s compatibility\n\n", current_r_version))
        cat("- Updated R version requirement\n")
        if (length(latest_successful) > 0) {
          cat(sprintf("- Updated %d packages to latest versions (lockfile versions failed)\n", length(latest_successful)))
        }
        if (length(version_differences) - length(latest_successful) > 0) {
          cat(sprintf("- Other version updates: %d packages\n", length(version_differences) - length(latest_successful)))
        }
      } else {
        cat("Update renv.lock with current package versions\n\n")
        cat("- Synchronized with available package versions\n")
      }
      
    } else {
      cat("📝 Lockfile unchanged\n")
    }
  } else {
    cat("✅ No lockfile updates needed\n")
  }
  
  cat("\n=== Complete! ===\n")
  return(invisible(list(
    r_version_updated = current_numeric != lockfile_numeric,
    from_restore = newly_installed,
    from_lockfile = successful_lockfile,
    from_latest = latest_successful,
    complete_failures = complete_failures,
    version_changes = version_differences
  )))
}

# Install latest versions of all packages in lockfile
install_latest_from_lockfile <- function() {
  
  cat("=== Installing Latest Versions from Lockfile ===\n")
  
  if (!file.exists("renv.lock")) {
    stop("No renv.lock file found")
  }
  
  lockfile <- renv::lockfile_read()
  all_packages <- names(lockfile$Packages)
  
  cat(sprintf("📦 Found %d packages in lockfile\n", length(all_packages)))
  
  # Separate CRAN and GitHub packages
  github_packages <- character(0)
  cran_packages <- character(0)
  
  for (pkg in all_packages) {
    pkg_info <- lockfile$Packages[[pkg]]
    if (!is.null(pkg_info$RemoteType) && pkg_info$RemoteType == "github") {
      github_packages <- c(github_packages, pkg)
    } else {
      cran_packages <- c(cran_packages, pkg)
    }
  }
  
  # Install latest CRAN packages
  if (length(cran_packages) > 0) {
    cat(sprintf("\n=== Installing %d CRAN packages (latest versions) ===\n", length(cran_packages)))
    install.packages(cran_packages)
  }
  
  # Install latest GitHub packages
  if (length(github_packages) > 0) {
    cat(sprintf("\n=== Installing %d GitHub packages (latest versions) ===\n", length(github_packages)))
    
    if (!requireNamespace("remotes", quietly = TRUE)) {
      install.packages("remotes")
    }
    
    for (pkg in github_packages) {
      pkg_info <- lockfile$Packages[[pkg]]
      if (!is.null(pkg_info$RemoteUsername) && !is.null(pkg_info$RemoteRepo)) {
        remote_ref <- paste0(pkg_info$RemoteUsername, "/", pkg_info$RemoteRepo)
        cat(sprintf("Installing %s from %s (latest)...\n", pkg, remote_ref))
        remotes::install_github(remote_ref)
      }
    }
  }
  
  cat("\n✅ Latest versions installed!\n")
  cat("Run renv::snapshot() if you want to update the lockfile\n")
}

# Usage:
# handle_renv_version_mismatch()
# install_latest_from_lockfile()