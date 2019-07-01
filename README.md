# Introduction
FLUAM: The repo is about Florida Land Use Allocation Model
The model was originally developed by RSG back in mid 90's and later in mid 2000's (2006 - 2007) was implemented to FL under FTE. The 2007 implementation is referred as FLUAM.
Between 2007 and 2009, the model was updated a few times.  Now the same code is brought into R and with some C++ functionality as required.  

# History
v1.0.0.0 = FLUAM estimated with aggregate historical data  
v2.0.1.0 = FLUAM estimated with disaggregated data in specified counties  
v2.1.1.0 = FLUAM which also exports FRATAR input file  

Versions :
Nov,  1994: LUAM Concept  
April 2006: v1.0   Florida Land Use Allocation (Adoption)  
June 2007: v2.0   FLUAM from Parcel Data  
Sept 2007: v2.0.1 County Specific Inputs  
July 2009 : v2.1.1 exports FRATAR input file  

We cannot compile these executables anymore.

***
>
From: Stephen Lawe [mailto:Stephen.Lawe@rsginc.com]
Sent: Monday, July 1, 2019 4:59 PM
To: Segovia, Cesar <Cesar.Segovia@dot.state.fl.us>
Subject: RE: NextGen TSM progress update  
*Hi Cesar,
It was nice to see you in Orlando. There are certainly some interesting opportunities ahead and we look forward to working with you. As promised, please find the land use code attached. In this file you will see a module called “RunFloridaLandUse” which is just what it says. From this, you should be able to establish the logic for the model.   
Please note that I first wrote this in 1994 and made both minor and major revisions for several years thereafter but has not been touched in quite some time. So if we are planning on making changes we would want to modify the code. Please also note that the attached code is part of a much larger software package that performs many modeling functions and has many supporting code modules. For example, the program reads and writes shape files which I wrote at a time when this was not terribly easy so I have IEEE bit-swapping and stuff like that in the code. What I’m sending you is just the land use model and not the extra overhead needed to compile the app.
Let me know if you want to consider any changes. It does feel like the FLUAM has been a useful part of the model and may still have a purpose.
Best,
Stephen*  
………………………………………………  
STEPHEN LAWE
Chief Executive Officer
RSG
55 Railroad Row | White River Junction, VT 05001
o 802.359.6452 | m 802.299.7370
www.rsginc.com
***

# FLUAM 3.0
New codebase written in RCPP (R & C++), basically R but have C++ functions embedded for computationally intense parts. One example is finding the closest ramp node to every taz.
