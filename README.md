# GEOG-563-Lab-1
Coding Challenge: develop R code to simulate a Lotka-volterra predator prey-system. Use Euler integration, which is basically a for-loop, to simulate changes in the predator and prey populations over time. 

The Lotka-Volterra predator-prey model describes how two populations (a prey species and a predator species) interact over time.

Workflow: 
We prompted chatgpt to give use code using the prompt "Could you help me develop R code to simulate a latka volterra predator prey system. Use forward euler integration which is basically a for loop to simulate changes in the predator prey populations  over time. Save population time series as separate csv files, then develop a separate R-script for plotting these time series, save these plots as png figures." 

It spit out some code on how to do this.

Predatory-prey dynamics for model populations: 
![image](https://github.com/user-attachments/assets/526ee79f-af8a-4328-959b-0fbdec0fe0bf)

Summary of equations used to produce figures:

prey[t] = prey[t-1] + (a * prey[t-1] - b * prey[t-1] * pred[t-1]) * dt (prey populations change equation)
pred[t] = pred[t-1] + (d * prey[t-1] * pred[t-1] - c * pred[t-1]) * dt (predator population change equation)

This simulation shows how a prey population (like rabbits) and a predator population (like wolves) change over time, based on a classic set of equations called the Lotka-Volterra predator-prey model.

The idea is:

Prey naturally grow in number over time, but some get eaten by predators.

Predators depend on catching prey to survive and reproduce. If there’s not enough food, the predator population shrinks.

We used a simple method to simulate these changes step-by-step over time (called Euler’s method). At each small time step, we update the population sizes based on how many were born, died, or eaten.

The simulation helps us understand how predator and prey numbers can cycle up and down in response to each other.
