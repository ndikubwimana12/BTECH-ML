#initialization
#you difine your folder with automaticaly creator
folders <- c("data","script","output/polts","output/tables","docs")
#create folders it they do not exist already
for (f in folders){
  if (!dir.exists(f)) 
    dir.create(f,recursive=TRUE)
}
cat("project folders ready :",paste(folders,collapse = "",),"n\n")
#Create the isolated project 
install.packages('renv',dependencies = TRUE)
#initialization of project
renv::init()
# 1. create private project libraries(renv/library)where your project packages will live
# 2. create the renv.lock file , with tracks the exact versions of R packages used 
# 3.  adds a Rprofile file to ensure R automatically uese this private library every time 
# install 
install.packages (c("tidyverse","broom","class","ISLR2","gapminder"))
#or  meeans you can run one of them
renv::install (c("tidyverse","broom","class","ISLR2","gapminder"))
# installing new packages , tables snapshot to save the state of your project
renv::snapshot()
# usefull maintenance 
renv::status()
renv::update()
