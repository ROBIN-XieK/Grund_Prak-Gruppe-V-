# Weltweite Entwicklung von Fruchtbarkeitsraten und Kindersterblichkeit

**Author: **Kaiyan Xie, Yixiang Wang, Yidan Jin, Yuheng Yang

Responsible for content: Yuheng Yang

## General Instruction

This is the repository for Weltweite Entwicklung von Fruchtbarkeitsraten und Kindersterblichkeit Project.
To ensure the performance of our programs, please read this instruction before running the codes.

We use R Version 4.4.2. Some of our packages are dependent on the latest version of R.
If you are using other versions, you may encounter errors while installing them.

For a detailed list of loaded packages, please refer to "setup.R" under the
root directory of our project.

## Usage

First of all, please source the "setup.R" file. This will install all the
necessary packages.

Then, for single programs, simply source the corresponding R file under the
"Program" directory. For generating the final report, open and render the "presentation.qmd" file 
under the root directory.

## Directory Structure

### Root Directory

setup.R

presentation.qmd

### Data

This directory contains all the original data sets we used from ourworldindata.org.

### Program

This directory contains all the programs for generating charts.Each program includes 
its own data cleaning and organization processes.

The program names correspond to the names of the generated charts.

### Results

This directory contains the results (charts) of our analysis. They are all saved in png format.
