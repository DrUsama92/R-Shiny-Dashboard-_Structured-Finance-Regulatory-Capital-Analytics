# Synthetic Risk Transfer (SRT) Analytics Dashboard

A modular **R Shiny dashboard** for modelling **Synthetic Risk Transfer (SRT)** and securitisation transactions.

The application enables structured finance and risk teams to simulate portfolio structures, evaluate regulatory capital impact, and analyse transaction economics through interactive analytics.

The system is built with a **modular Shiny architecture**, where the master application coordinates independent modules responsible for portfolio inputs, tranche configuration, regulatory modelling, and analytical outputs.

---

## Overview

This platform provides a computational framework for evaluating securitisation structures under regulatory capital frameworks.

Users can configure:

- Portfolio characteristics
- Risk parameters (**PD, LGD, W**)
- Tranche structures
- Regulatory assumptions
- Capital components and cost inputs

The dashboard then generates detailed analytics including regulatory tests, economic summaries, and risk-weight calculations.

---

## Key Features

### Portfolio & Risk Configuration
- Portfolio exposure inputs
- Risk parameters (**PD, LGD, W**)
- Currency and risk-weighting settings
- Provision and FX parameters

### Regulatory Modeling
- Asset RW method (**STD / IRB**)
- Securitisation RW method (**SEC-ERBA / SEC-SA / SEC-IRBA**)
- STS classification
- Pool type configuration

### Transaction Structure
- Tranche inputs and capital structure modelling
- Attachment / detachment point calculation
- Amortisation and loss vector generation

### Analytics Outputs
- Capital ratio analysis
- EBA regulatory tests
- Economics summaries
- Risk-weighted exposure calculations
- Cost of protection analysis
- ERBA risk-weight tables
- SEC-IRBA input modelling

---

## Dashboard Modules

The application uses a modular architecture where each module handles a specific analytical component.

| Module | Purpose |
|--------|---------|
| `App.R` | Master application that initializes UI, loads modules, and manages event communication |
| `Mod90.R` | Core SRT modelling module and dashboard outputs |
| `Mod95.R` | Tranche configuration and portfolio structure inputs |
| `ModAbout.R` | Informational placeholder module |

Modules communicate through a **session event bus (pub/sub architecture)**, allowing clean separation between input configuration and analytics calculations.

---

## Project Structure

```text
project-root/
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
