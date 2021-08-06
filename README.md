# Initial installation of the WaterBalance package

This repository consists of (1) scripts and data pertaining to the development of the Water Balance package and (2) the most recent working version of the Water Balance package. The Water Balance package is a custom-built R package implementing Dave Thoma's water balance spreadsheet model. The package is contained within in the following folder: `"./WaterBalance/WaterBalance"`

In order to use the package within the R environment, the package must be copied and pasted into your R package library. The location of this library can vary based on user preferences. An example location may be `C:/Users/yourname/Documents/R/win-library/4.0`. The package can be copied and pasted directly from this repository into your local folder. Once the package is copied and pasted in the same folder as your other packages, it can be installed in R using the following code:

```
library("devtools")
install("WaterBalance")
```

The package should then install and can be called into the workspace via the `library()` function. 

##If the installation does not work, try this:

Navigate to [this location on the shared drive](https://doimspp.sharepoint.com/sites/NPS-CCRP-FCScienceAdaptation/Shared%20Documents/Forms/AllItems.aspx?xsdata=MDN8MDF8fGI4ODE0YTI1NTJkZDRiOGVhNGY0NWE1ZTZkN2RlNGU4fDA2OTNiNWJhNGIxODRkN2I5MzQxZjMyZjQwMGE1NDk0fDF8MHwzMTU1Mzc4OTc1OTk5OTk5OTk5fEdvb2R8VkdWaGJYTlRaV04xY21sMGVWTmxjblpwWTJWOGV5SldJam9pTUM0d0xqQXdNREFpTENKUUlqb2lJaXdpUVU0aU9pSWlMQ0pYVkNJNk1USjk&sdata=MnRsRW5BOGZlMWNwa1NsM0NlOUV0Y2xtaTUzdm5FanF5UVFWakh2WTBzVT0&ovuser=0693b5ba%2D4b18%2D4d7b%2D9341%2Df32f400a5494%2Cadillon%40nps%2Egov&viewid=54c972dc%2D7b2e%2D4eb7%2Da737%2D42792988c0b3&id=%2Fsites%2FNPS%2DCCRP%2DFCScienceAdaptation%2FShared%20Documents%2FGeneral%2FWater%2FWB%2FWB%20Package%201%2E1%2E0) and download `WaterBalance_1.1.0.tar.gz` into your package library. Now repeat the steps above as follows:

```
library("devtools")
install("WaterBalance")

```

## If that *still* doesn't work, try this code:

```
install.packages("path/to/WaterBalance.tar.gz", repos = NULL, type = "source")
```

*Note: this method worked on a Mac.*


##If the installation *STILL* does not work, the package may need to be rebuilt following instructions below:


#Rebuilding the WaterBalance package using the copy-paste method in Windows 

These instructions should be followed when the WaterBalance package needs to be rebuilt. This may occur upon initial installation, or after R is updated to a major new version. The WaterBalance package should already be copied and pasted into the win-library folder per instructions above. 


If the WaterBalance package does not work upon initial installation or after R is updated, the user *may* need to do the following:

1. In R version >= 4.0.x, check to see that the "Maintainer" field in the package DESCRIPTION is formatted as follows:

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
