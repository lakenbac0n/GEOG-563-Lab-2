# GEOG-563-Lab-1
Coding Challenge: develop R code to simulate a Lotka-volterra predator prey-system. Use Euler integration, which is basically a for-loop, to simulate changes in the predator and prey populations over time. 

The Lotka-Volterra predator-prey model describes how two populations (a prey species and a predator species) interact over time.

Predatory-prey dynamics for model populations: 
![image](https://github.com/user-attachments/assets/526ee79f-af8a-4328-959b-0fbdec0fe0bf)

Summary of equations used to produce figures:
prey[t] = prey[t-1] + (a * prey[t-1] - b * prey[t-1] * pred[t-1]) * dt (prey populations change equation)
pred[t] = pred[t-1] + (d * prey[t-1] * pred[t-1] - c * pred[t-1]) * dt (predator population change equation)
