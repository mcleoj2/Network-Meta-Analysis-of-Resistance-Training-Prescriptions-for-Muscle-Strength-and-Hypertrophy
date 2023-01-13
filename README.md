# Network-Meta-Analysis-of-Resistance-Training-Prescriptions-for-Muscle-Strength-and-Hypertrophy

A top-level README is a good way to describe the gist of the code that's contained in the repo. Please feel free to delete most of this text, and/or replace with your own, once you've made use of the edits. 

Great work, and enjoy!

## Suggested usage of this repo
I definitely do not recommend blindly merging this with your repo, but rather using a comparison of the edits to see how my style might differ from yours. I really didn't 'fix' anything, as it was good to begin with. Rather, I just made some slight tweaks in the style that I've adopted over time (which may not be everyone's preferences)

## No data, so I'm blind-coding here :) 
The scripts are good-to-go, I assume... I have no data so couldn't actually see that everything works, but assume it does :).

## Spirit of repo changes
I've basically only made changes to code style for readability and some small changes to remove redundancies. I've added a bit of structure to the repo to help organize the outputs (see below). I've also created a top-level notebook that should serve as an organizing document that runs all the code in one go. No idea if that's even preferred, but I went for that option in the past.

## Repo structure
In general, the repo should now be structures like this:
 
    - README.md
    - Top_level_notebook.qmd (idea is to open this only and have it run all the other scripts, as well as render all the images and summaries you use through out for exploring the data)
    - R/
        - (all of your scripts)
    - outputs/
        - (all of your saved images)
        - (output tables can also go here)