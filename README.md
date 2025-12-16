# Cluster Sampling Simulator

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=for-the-badge&logo=r&logoColor=white)
![Shiny](https://img.shields.io/badge/Shiny-blue?style=for-the-badge&logo=RStudio&logoColor=white)

An interactive R Shiny application to simulate cluster sampling, visualize population designs, and calculate survey costs and bias in real-time.



## Overview

**Cluster Sampling Simulator** is a statistical tool designed to demonstrate how cluster sampling works compared to simple random sampling. 

Users can generate synthetic populations, perform sampling in real-time, and instantly see the impact on **estimation accuracy**, **survey cost**, and **time efficiency**.

##  Key Features

### 1. Dynamic Population Generation
* **Customizable Parameters:** Control the total number of clusters ($N$), units per cluster ($M$), population mean ($\mu$), and standard deviation ($\sigma$).
* **Realistic Variation:** The app simulates realistic clusters where units within a specific group are more similar to each other than to the general population.

### 2. Interactive Sampling Design
* **Visual Sampling:** Use intuitive sliders to select the number of clusters to sample ($k$).
* **Visual Feedback:** Instantly see which clusters are selected for the survey and which are ignored.

### 3. Real-Time Metrics Dashboard
The app automatically calculates and updates:
* **True vs. Estimated Mean:** Compare your sample result against the ground truth.
* **Sampling Bias:** Quantify the difference between the estimate and the true parameter.
* **Cost Analysis:** Calculate total survey cost based on fixed overhead, per-cluster costs, and per-unit costs.
* **Time Estimation:** Estimate total surveyor hours required.

### 4. Advanced Visualization
* **Cluster Plot:** Uses `ggplot2` to visualize the entire population. Sampled units are highlighted in **Red**, while unsampled units remain **Grey**.
* **Distribution Analysis:** Overlaying boxplots display the variance within every cluster.

### 5. Sample Size Planner
* **Reverse Calculation:** Input your target **Margin of Error**, and the app calculates exactly how many clusters ($k$) you need to sample to achieve that precision based on current population variance ($S_b^2$).

---

##  Prerequisites

To run this application, you need **R** installed. You will also need the following R packages:

```r
install.packages(c("shiny", "ggplot2", "dplyr", "DT", "bslib"))
