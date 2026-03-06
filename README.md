Synthetic Risk Transfer (SRT) Analytics Dashboard

A modular R Shiny dashboard for modelling Synthetic Risk Transfer (SRT) and securitisation transactions.
The application enables structured finance and risk teams to simulate portfolio structures, evaluate regulatory capital impact, and analyze transaction economics through interactive analytics.

The system is designed with a modular Shiny architecture where the master application coordinates independent modules responsible for portfolio inputs, tranche configuration, regulatory modelling, and analytical outputs. 

App

Overview

This platform provides a computational framework for evaluating securitisation structures under regulatory capital frameworks.

Users can configure:

Portfolio characteristics

Risk parameters (PD, LGD, W)

Tranche structures

Regulatory assumptions

Capital components and cost inputs

The dashboard then generates detailed analytics including regulatory tests, economic summaries, and risk-weight calculations.

Key Features
Portfolio & Risk Configuration

Portfolio exposure inputs

Risk parameters (PD, LGD, W)

Currency and risk-weighting settings

Provision and FX parameters

Regulatory Modeling

Asset RW method (STD / IRB)

Securitisation RW method (SEC-ERBA / SEC-SA / SEC-IRBA)

STS classification

Pool type configuration

Transaction Structure

Tranche inputs and capital structure modeling

Attachment / detachment point calculation

Amortisation and loss vector generation

Analytics Outputs

Capital ratio analysis

EBA regulatory tests

Economics summaries

Risk-weighted exposure calculations

Cost of protection analysis

ERBA risk-weight tables

SEC-IRBA input modelling

Dashboard Modules

The application uses a modular architecture where each module handles a specific analytical component.

Module	Purpose
App.R	Master application that initializes UI, loads modules, and manages event communication
Mod90	Core SRT modelling module and dashboard outputs
Mod95	Tranche configuration and portfolio structure inputs
About Module	Informational placeholder module

Modules communicate through a session event bus (pub/sub architecture) which allows clean separation between input configuration and analytics calculations. 

App

Project Structure
project-root
│
├── App.R
├── GlobalFunctions.R
├── GlobalText.R
│
├── Mod90.R
├── Mod95.R
├── ModAbout.R
│
├── www/
│   ├── mod90_support/
│   │   ├── KeysSheet.R
│   │   ├── EconCore.R
│   │   ├── ERBA.R
│   │   ├── SEC-IRBA.R
│   │   └── Output.R
│   ├── mod90.css
│   └── mod90.js

Support modules under www/mod90_support/ provide the financial modelling logic used by the SRT dashboard.

Technologies Used

R

Shiny

rhandsontable

dplyr

data.table

xts

JavaScript (UI enhancements)

Installation

Clone the repository:

git clone https://github.com/yourusername/srt-dashboard.git

Install required R packages:

install.packages(c(
  "shiny",
  "shinyWidgets",
  "rhandsontable",
  "dplyr",
  "data.table",
  "xts"
))

Run the application:

shiny::runApp("App.R")
Use Cases

This tool is designed for:

Banks and structured finance teams

Regulatory capital analysts

Risk management teams

Securitisation structuring specialists

Typical use cases include:

Synthetic Risk Transfer structuring

Capital efficiency analysis

Risk-weighted asset optimization

Securitisation deal evaluation

Future Improvements

Potential extensions for the platform include:

Scenario simulation engine

Monte Carlo loss modeling

Market data integration

Automated regulatory reporting

AI-assisted interpretation of results
