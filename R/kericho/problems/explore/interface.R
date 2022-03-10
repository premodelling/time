# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 20/02/2022


# functions
source(file = 'R/kericho/StudyData.R')
source(file = 'R/kericho/problems/explore/ExplorationGraph.R')


# the data
instances <- StudyData()


# ln(cases) vs. maxT, minT, Rain
ExplorationGraphNL(instances = instances)