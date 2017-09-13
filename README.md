# Lab 3: Schelling's model of segregation

In this lab, we will implement Schelling's model of segregation. Most of the implementation has already been done for you, in the `schelling-model.R` file. We'll go over this implementation at the start of class, and collectively write the final piece of the model.

After we implement Schelling's version, it'll be up to you to come up with a variation on the model and make the changes necessary to explore that variation. This variation should explore how segregation does or does not change when compared to the base Schelling model. You'll want to run a range of parameter values and see if there are differences in the equilibrium points or the speed with which the model reaches equilibrium. You'll do this work in the `my-variation.R` file. You should use comments in this file to narrate what you are doing and what you are learning.

Some ideas for variations:

1. Expand the concept of a neighborhood, so that people are looking not just at the immediate neighbors, but at some larger section of the grid.
2. Add some additional characteristic of agents, that may or may not vary across groups, to capture some other aspect of the real world that is not captured in the original model.
3. Allow agents to vary in their preferences.
4. Explore what happens if agents seek out some diversity instead of merely avoiding being the extreme minority.

