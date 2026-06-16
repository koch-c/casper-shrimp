This project has all it's R depdencies installed in `./r_libs`. Only use libs rom this folder. You can use them by adding 
```r
.libPaths(c("./r_libs", .libPaths()))
```
To the top of the file

Note the application will in production be running in a restricted environment. Therefore there are some restrictions to the app 
- it must be contained within one file, so it can easly be shared
- the amount of dependencies should be limited
 - Currently shiny and RODBC is installed on the machine and in `./r_libs`

The machine you are developing os is not the same machine that the application will be running on. 

Your development environment has R 4.6 installed and the production environment has R 4.5.2 installed.

Generally it is not possible to test the application against the database and no development database exist. 

The production environment is a windows 11 machine
