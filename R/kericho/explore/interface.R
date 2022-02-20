# Title     : interface.R
# Objective : Interface
# Created by: greyhypotheses
# Created on: 20/02/2022

source(file = 'R/kericho/StudyData.R')
source(file = 'R/kericho/explore/ExplorationGraph.R')

instances <- StudyData()

ExplorationGraphNL(instances = instances)