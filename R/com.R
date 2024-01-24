#### Functions for communicating within the CDC Microsoft infrastructure

#' Send a message on Microsoft Teams
#'
#' @description
#' Helper function to send message to validated MS Teams interface
#' @import Microsoft365R
#' @param msg str: message to be sent
#' @param team_id str: Team id, defaults to "CGH-GID-PEB-SIR"
#' @param channel str: channel where message should be sent
#' @param attach str: local path of files to be attached in message
#' @returns Success or error message
send_teams_message <- function(msg, team_id = "CGH-GID-PEB-SIR", channel = "CORE 2.0", attach = NULL){

  team <- Microsoft365R::get_team(team_id)

  channel <- team$get_channel(channel)

  channel$send_message(body = msg,
                       attachments = NULL)

}

#' Upload file to Sharepoint
#'
#' @description
#' Helper function to upload file to MS Sharepoint
#' @import Microsoft365R
#' @param upload str: local path of files to be uploaded
#' @param sp_folder str: location in sharepoint to upload file
#' @param site str: Sharepoint site location, defaults to "CGH-GID-PEB"
#' @param drive str: Sharepoint drive to upload data to
#' @returns Success or error message
upload_to_sharepoint <- function(upload, sp_folder, site = "CGH-GID-PEB-SIR", drive = "Documents"){

  site <- get_sharepoint_site(site)

  drv <- site$get_drive(drive)

  drv$upload_file(src = upload, dest = sp_folder)

}
