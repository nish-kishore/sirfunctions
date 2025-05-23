#### Functions for communicating within the CDC Microsoft infrastructure

#' Send a message on Microsoft Teams
#'
#' Helper function to send message to validated MS Teams interface.
#'
#' @param msg `str` Message to be sent.
#' @param team_id `str` Teams ID. Defaults to `"CGH-GID-PEB-SIR"`.
#' @param channel `str` Channel where message should be sent.
#' @param attach `str` Local path of files to be attached in message.
#' @param type `str` Type of message to be sent. Either `"text"` or `"html"`.
#' @returns Status message whether the operation was a success or an error message.
#' @examples
#' \dontrun{
#' message <- "this is a test"
#' send_teams_message(message)
#' }
#'
#' @export
send_teams_message <- function(msg, team_id = "CGH-GID-PEB-SIR", channel = "CORE 2.0", attach = NULL, type = "text") {
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop(
      'Package "Microsoft365R" must be installed to use this function.',
      call. = FALSE
    )
  }

  team <- Microsoft365R::get_team(team_id)

  channel <- team$get_channel(channel)

  channel$send_message(
    body = msg,
    attachments = attach,
    content_type = type
  )
}

#' Upload file to Sharepoint
#'
#' Helper function to upload file to MS SharePoint
#'
#' @param file_to_upload `str` Local path of files to be uploaded.
#' @param sharepoint_file_loc `str` Location in SharePoint to upload file.
#' Must include the file name and extension (i.e., folder/file_name.csv).
#' @param site `str` SharePoint site location. Defaults to `"CGH-GID-PEB"`
#' or the site URL: `"https://cdc.sharepoint.com/teams/CGH-GID-PEB-SIR283"`.
#' @param drive `str` SharePoint drive to upload data to.
#' @returns Status message whether the operation was a success or an error message.
#' @examples
#' \dontrun{
#' file_path <- "C:/Users/ABC1/df1.csv"
#' sp_path <- "test_folder/df1.csv"
#' upload_to_sharepoint(file_path, sp_path)
#' }
#'
#' @export
upload_to_sharepoint <- function(file_to_upload, sharepoint_file_loc, site = "https://cdc.sharepoint.com/teams/CGH-GID-PEB-SIR283", drive = "Documents") {
  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop(
      'Package "Microsoft365R" must be installed to use this function.',
      call. = FALSE
    )
  }

  tokens <- AzureAuth::list_azure_tokens()

  token_hash_names <- tokens |> names()

  if (length(token_hash_names) > 0) {
    token_list <- lapply(1:length(token_hash_names), function(x) {
      obj <- tokens[[token_hash_names[x]]]

      dplyr::tibble(
        "token" = token_hash_names[x],
        "resource" = obj$resource,
        "scope" = obj$scope
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(grepl("Sites.ReadWrite.All", scope))
  } else {
    token_list <- dplyr::tibble()
  }

  if (nrow(token_list) == 0) {
    site <- Microsoft365R::get_sharepoint_site(site_url = site)
  } else {
    site <- Microsoft365R::get_sharepoint_site(site_url = site, token = tokens[[pull(token_list, token)]])
  }

  drv <- site$get_drive(drive)

  drv$upload_file(src = file_to_upload, dest = sharepoint_file_loc)
}

#' Send email through Outlook
#'
#' Function to send an email through Outlook from R.
#' @param title `str` Subject of message to be sent.
#' @param body `str` Long string of body of message to be sent.
#' @param recipient `str` A semicolon separated list of recipients.
#' @param attachment `str` Path to local document to be attached to email. Defaults to `NULL`.
#' @returns Status message whether the operation was a success or an error message.
#' @examples
#' \dontrun{
#' title_message <- "Test"
#' body_message <- "this is a test"
#' recipient_list <- c("ab123@email.com")
#' send_outlook_email(title_message, body_message, recipient_list)
#' }
#'
#' @export
send_outlook_email <- function(title, body, recipient, attachment = NULL) {
  if (!requireNamespace("blastula", quietly = TRUE)) {
    stop('Package "blastula" must be installed to use this function.',
      .call = FALSE
    )
  }

  if (!requireNamespace("Microsoft365R", quietly = TRUE)) {
    stop(
      'Package "Microsoft365R" must be installed to use this function.',
      call. = FALSE
    )
  }

  tokens <- AzureAuth::list_azure_tokens()

  token_hash_names <- tokens |> names()

  if (length(token_hash_names) > 0) {
    token_list <- lapply(1:length(token_hash_names), function(x) {
      obj <- tokens[[token_hash_names[x]]]

      dplyr::tibble(
        "token" = token_hash_names[x],
        "resource" = obj$resource,
        "scope" = obj$scope
      )
    }) |>
      dplyr::bind_rows() |>
      dplyr::filter(grepl("Mail.ReadWrite", scope))
  } else {
    token_list <- dplyr::tibble()
  }

  if (nrow(token_list) == 0) {
    cli::cli_alert_warning(paste0(
      "Authenticating Azure connection. ",
      "If local host refuses to connect, ",
      "then unable to authenticate with Azure Active Directory and use this function."
    ))
    outl <- Microsoft365R::get_business_outlook()
  } else {
    outl <- Microsoft365R::get_business_outlook(token = tokens[[dplyr::pull(token_list, token)]])
  }

  bl_em <- blastula::compose_email(
    body = blastula::md(body),
    footer = blastula::md("This is an automated message produced by the SIR team")
  )
  em <- outl$create_email(bl_em, subject = title, to = recipient)

  if (!is.null(attachment)) {
    # add an attachment and send it
    em$add_attachment(attachment)
  }

  em$send()
}
