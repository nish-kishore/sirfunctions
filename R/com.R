#### Functions for communicating within the CDC Microsoft infrastructure

#' Send a message on Microsoft Teams
#'
#' @description
#' Helper function to send message to validated MS Teams interface
#' @import Microsoft365R AzureAuth
#' @param msg str: message to be sent
#' @param team_id str: Team id, defaults to "CGH-GID-PEB-SIR"
#' @param channel str: channel where message should be sent
#' @param attach str: local path of files to be attached in message
#' @returns Success or error message
send_teams_message <- function(msg, team_id = "CGH-GID-PEB-SIR", channel = "CORE 2.0", attach = NULL){

  team <- Microsoft365R::get_team(team_id)

  channel <- team$get_channel(channel)

  channel$send_message(body = msg,
                       attachments = attach)

}

#' Upload file to Sharepoint
#'
#' @description
#' Helper function to upload file to MS Sharepoint
#' @import Microsoft365R AzureAuth
#' @param file_to_upload str: local path of files to be uploaded
#' @param sharepoint_file_loc str: location in sharepoint to upload file
#' @param site str: Sharepoint site location, defaults to "CGH-GID-PEB" or the site URL: "https://cdc.sharepoint.com/teams/CGH-GID-PEB-SIR283"
#' @param drive str: Sharepoint drive to upload data to
#' @returns Success or error message
upload_to_sharepoint <- function(file_to_upload, sharepoint_file_loc, site = "https://cdc.sharepoint.com/teams/CGH-GID-PEB-SIR283", drive = "Documents"){

  tokens <- AzureAuth::list_azure_tokens()

  token_hash_names <- tokens |> names()

  token_list <- lapply(1:length(token_hash_names), function(x){

    obj <- tokens[[token_hash_names[x]]]

    dplyr::tibble(
      "token" = token_hash_names[x],
      "resource" = obj$resource,
      "scope" = obj$scope
    )
  }) |>
    dplyr::bind_rows() |>
    filter(grepl("Sites.ReadWrite.All", scope))

  if(nrow(token_list == 0)){
    site <- Microsoft365R::get_sharepoint_site(site_url = site)

  }else{
    site <- Microsoft365R::get_sharepoint_site(site_url = site, token = tokens[[pull(token_list, token)]])

  }

  drv <- site$get_drive(drive)

  drv$upload_file(src = file_to_upload, dest = sharepoint_file_loc)

}
