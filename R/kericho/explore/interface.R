# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 20/02/2022


# functios
source(file = 'R/kericho/StudyData.R')
source(file = 'R/kericho/explore/ExplorationGraph.R')


# the data
instances <- StudyData()


# excercise 1.1: relationships
# ln(cases) vs. maxT, minT, Rain
ExplorationGraphNL(instances = instances)