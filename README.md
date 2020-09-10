# WaterBalance
R package for implementing Dave Thoma's water balance spreadsheet model


#TO REINSTALL WATERBALANCE PACKAGE AFTER R IS UPDATED USING THE COPY-PASTE METHOD IN WINDOWS

* These instructions pertain to R updates in which the WaterBalance package is copied from 
the previous version's win-library folder and pasted into the current version's win-library folder.

If the WaterBalance package does not work after R is updated, the user *may* need to do the following:

1. In R 4.0.x, check to see that the "Maintainer" field in the package DESCRIPTION is formatted as follows:

```Annie Kellner Dillon <anne_dillon@nps.gov>```

2. The WaterBalance package may need to be rebuilt. To do this, follow these steps:

a. Open the R terminal. Make sure R opens new terminals with Command Prompt. If you see dollar signs ($) in the prompt, you are likely using Git Bash to open terminals. To switch, go to Tools --> Global Options --> Terminal. Where it says "New terminals open with", select "Command Prompt" from the drop-down menu. Apply. (NOTE: you can also execute this workflow from the Command Prompt outside the R session by running the R.exe file). 

b. Once you have opened a new terminal in your R session using Command Prompt, change directory to the win-library associated with the current R version. 
The "change directory" command is ```cd ~/path/to/folder```.

c. Enter the following code into the Terminal: ```R CMD build WaterBalance```

d. Open the console (or the Installing_WB_package.R script)

e. Type or execute the following code: 
```
library("devtools")
install("WaterBalance")
```
