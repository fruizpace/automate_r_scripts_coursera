
###########################################################
###########################################################
## Automate R scripts with GitHub Actions: Deploy a model
###########################################################
###########################################################


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 7: Load the readmission model
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##


##---------------------------------------------------------------------------##
## Load required packages and readmission model
##---------------------------------------------------------------------------##

## Load required packages

library(googlesheets4)
library(readr)
library(dplyr)
library(parsnip)
library(recipes)
library(glmnet)
library(workflows)
library(emayili)
library(gargle)
library(tidyr)


## Load the prediction model
readmit_model <- read_rds("inst/final_readmission_model.rds")

##---------------------------------------------------------------------------##
## Decrpyt authorization to Google Drive and Google Sheet
##---------------------------------------------------------------------------##

## googledrive De-auth setup
googledrive::drive_auth(
  path = gargle::secret_decrypt_json("inst/googledrive_encrypt.json",
                                     "GOOGLEDRIVE_KEY"
                                     )
)

## googlesheet De-auth setup
googlesheets4::gs4_auth(
  path = gargle::secret_decrypt_json("inst/googledrive_encrypt.json",
                                     "GOOGLEDRIVE_KEY"
  )
)

##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 8: Load the data and make predictions
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Read in patient data and make predictions 
##---------------------------------------------------------------------------##

## Load the googlesheet where data is stored
sheet_id <- "https://docs.google.com/spreadsheets/d/1fDAJOAWpq4_rvaL_QLPMJA1kqLe81xXqT-207QbehK0/edit?usp=sharing"

## Load the patient raw data from the first sheet
patient_data <- read_sheet(sheet_id, sheet = 1)

## Read the second sheet that will contain the patient data and prediction
patient_data_with_pred <- read_sheet(sheet_id, sheet = 2)

## Make predictions for the patient data (sheet 1)
pred_data <- augment(readmit_model, patient_data) |>
  select(all_of(colnames(patient_data)),
  "Prediction" = ".pred_class",
  "Prob No Readmit" = ".pred_No",
  "Prob Readmit" = ".pred_Yes"
         )
         
## Save/write the prediction on the second sheet
sheet_write(ss = sheet_id, data = pred_data, sheet = "Patient Data with Prediction")

##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 9: Format prediction columns as percentages
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Helper function to format the N and O columns as percentages
##---------------------------------------------------------------------------##

## Helper function to help format googlesheet (0.456 -> 45%)
format_prediction_to_percent <- function(sheet, pos){
  
  ## Get the Google Sheet
  x <- gs4_get(sheet)
  
  ## Determine the targeted sheet
  range_spec <- googlesheets4:::as_range_spec(
    pos,
    sheet = 2,
    sheets_df = x$sheets, nr_df = x$named_ranges
  )
  
  ## Form request
  range_req <- googlesheets4:::as_GridRange(range_spec)
  
  ## Specify the cell formatting
  cell_req <- list(
    userEnteredFormat = list(
      numberFormat = list(
        type = "NUMBER",
        pattern = "0%"
      )
    )
  )
  
  field_req <- "userEnteredFormat.numberFormat"
  
  ## Generate and send the API request
  req <- request_generate(
    "sheets.spreadsheets.batchUpdate",
    params = list(
      spreadsheetId = as_sheets_id(sheet),
      requests = list(
        repeatCell = list(
          range = range_req,
          cell = cell_req,
          fields = field_req
        )
      )
    )
  )
  
  
  ## Execute the request and send the response
  resp_raw <- request_make(req)
  gargle::response_process(resp_raw)
}

##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 10: Write an automation script for prediction
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Automated script for prediction
##---------------------------------------------------------------------------##

## Capture the data of new patients in sheet 1
new_patients <- patient_data |>
  anti_join(patient_data_with_pred |>
              select(patient_id))


## If there is a new patient, then make prediction and 
## append to sheet 2

if(nrow(new_patients) > 0) {
  
  ## Apply model on new patient data
  pred_data <-
    augment(readmit_model, new_patients) |>
    select(all_of(colnames(new_patients)),
           "Prediction" = ".pred_class",
           "Prob No Readmit" = ".pred_No",
           "Prob Readmit" = ".pred_Yes")
  
  ## Instead of overwriting the data, we need to append new data
  
  ## Before appending, let's confirm that the column names are still the same.
  ## They should be, but it best practice to confirm.
  
  if(!all(colnames(patient_data_with_pred) == colnames(pred_data))) {
    stop("Patient data and prediction database don't match")
  }
  
  ## Append new patient and prediction to sheet 2
  sheet_append(ss = sheet_id,
               data = pred_data,
               sheet = "Patient Data with Prediction")
  
  ## Format the text col (i.e 0.564 -> 56%)
  format_prediction_to_percent(sheet_id, pos = "N") # columna N en google sheet
  format_prediction_to_percent(sheet_id, pos = "O") # Columna O en google sheet
}


##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##
## Task 12: Write automation script to send emails
##---------------------------------------------------------------------------##
##---------------------------------------------------------------------------##

##---------------------------------------------------------------------------##
## Automation script for emails
##---------------------------------------------------------------------------##

if(nrow(new_patients) > 0){
  ## Prepare email message
  smtp <- emayili::server(
    host = "smtp.gmail.com",
    port = 465,
    username = Sys.getenv("GMAIL_USERNAME"),
    password = Sys.getenv("GMAIL_PASSWORD")
  )
  
  send_to <- c("antoniolacasta@gmail.com")
  
  ## Create and send email message
  emayili <- envelope() %>% # la funcion envelope crea mensajes de email
    from("fruizpace@gmail.com") %>% 
    to(send_to) %>% 
    subject("New Readmission Prediction") %>% 
    emayili::render(
      input = "R/automate_r_email_content.Rmd", # doc markdown que contiene el correo
      params = list( # parametros que espera recibir el doc markdown para hacer la tabla
        patient_id = pred_data$patient_id,
        pred = pred_data$Prediction,
        no_readmit = pred_data$`Prob No Readmit`,
        yes_readmit = pred_data$`Prob Readmit`
      ),
      squish = F, # limpiar blank space
      include_css = "bootstrap" # nicer!
    )
  
  ## Show as the email sends
  smtp(emayili, verbose = TRUE)
}


