# ABM_SocKult
Agent-based modelling for social and cultural dynamics

This is a repository for an exam called Social and Cultural Dynamics in Cognitive Science.

It contains the code for running Agent-based models that the formation of echo chambers in social networks.
It also contains the code for analysis of the models. It mainly consists of plots, as statistical analysis doesn't 
too much sense in simualtion. 

The ABM itself is written in a Java based program called "Netlogo" meant for running ABMs. We have modified the code to
include an adittional censorship intervention, and we have removed the code for an intervention using "Socratic Agents".

The ABM is controlled using R in the scrips called "job.r" and "jobShort.r"

job.r is the script that runs the modes with the normal parameter settings. 

jobShort.r is the same script as job.r, but it runs a shorter paremeter list, to to focus on the instances with harsh censorship and narrowminded agents.

echoPlots.md contains the scripts for the plots used in the written report.

We have also included the code from a programming assignment on building an ABM that investigates social learning strategies. 


