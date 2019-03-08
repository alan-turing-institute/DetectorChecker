#!/bin/bash

#Add Azure credentails before building docker container
export AZURE_STORAGE_ACCOUNT=""
export AZURE_STORAGE_ACCESS_KEY=""
export AZURE_CONTAINER=""

#Add email account and password before building docker container
export GMAIL_ACCOUNT=""
export GMAIL_PASSWORD=""

Rscript run.R
