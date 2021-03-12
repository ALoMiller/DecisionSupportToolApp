# Run the Decision Support Tool in Docker

1.  Install docker desktop and launch it

2.  Open windows powershell

3.  Navigate to DST project folder

4.  Type:

    > docker build -t dstool .

Note: Don't forget the period!

5.  The build process will take 15-20 mins. (To see what it is doing edit the `Dockerfile`)

6.  Once built, type

    > docker run --rm -d -p 3838:3838 dstool

7.  The container is now running locally

8.  Open a web browser and in the address bar type

    > localhost:3838

9.  Your app should run as if hosted on a shiny server with R 3.6.0 running.

# Workflow when developing the app

1.  Make changes to code and save

2.  Kill any running containers

    > docker stop \$(docker ps -q)

3.  Build image (since the image copies the project into the container) Eventually we wont have to do this since we will mount the project to the cotainer at run time

    > docker build -t dstool .

4.  This will take a few seconds

5.  Run the container

> docker run --rm -d -p 3838:3838 dstool

1.  Open a web browser and in the address bar type

> localhost:3838
