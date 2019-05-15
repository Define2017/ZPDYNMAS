# ZPDYNMAS
SAP HCM Dynamic Actions

With the old maintenance view V_T588Z you can't switch fast between several infotypes, subtypes, etc. And if you maintain the table T588Z directly it can get unclear where exactly you are. 
To change this I created a report which let you change the T588Z entries and save them into a (customizing-)transport request. To have a better overview about the infotypes, subtypes, etc. I added a tree view where you can choose which area you want to maintain. 
Another feature which is added, is the "rebuild numbers" button. With this button, the report automatically changes the numbers in the "order" or "sequence" column. 

Old SM30 View: 

![alt text](/img/SM30-Dynamic-Actions-V_T588Z.png "Old SM30 View")

New Report: 

![alt text](/img/ZPDYNMAS.png "New Report ZPDYNMAS")

## Installation and prerequisite

+ Use ABAPGit to install [Transport-Services](https://github.com/DennstedtB/Transport-Services) first
+ Use ABAPGIT to install this report. 

If you don't want to use the transport services libary for it ( maybe because it isn't finished yet ) then please wait for Release 1.0 where the transport management is done via function modules. From Release 2.0 on the transport services libary is the only one which will be used. 
