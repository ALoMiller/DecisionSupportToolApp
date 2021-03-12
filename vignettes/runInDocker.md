# Run the Decision Support Tool in Docker

1.  Install docker desktop and launch it

1.  Open windows powershell

1.  Navigate to DST project folder

1.  Type:

    > docker build -t dstool .

Note: Don't forget the period!

1.  The build process will take 15-20 mins. (To see what it is doing edit the `Dockerfile`)

1.  Create a folder called `logs` in the projects root

1.  Once built, type

    > docker run --rm -d -p 3838:3838 -v $pwd/logs:/var/log/shiny-server dstool

1.  The container is now running locally. 

1.  Open a web browser and in the address bar type

    > localhost:3838

1.  Your app should run as if hosted on a shiny server with R 3.6.0 running.

The logs will be written to the `logs` folder

# Workflow when developing the app

1.  Make changes to code and save

2.  Kill any running containers

    > docker stop \$(docker ps -q)

3.  Build image (since the image copies the project into the container). Eventually we wont have to do this since we will mount the project to the container at run time

    > docker build -t dstool .

4.  This will take a few seconds

5.  Run the container

    > docker run --rm -d -p 3838:3838 -v $pwd/logs:/var/log/shiny-server dst1

1.  Open a web browser and in the address bar type

    > localhost:3838
