# devindocker 0.0.1.9000

## Breaking 

* Parameter `project_path` is now named `path` as any directory path can be used.
Not only projects.

## Major

* Allow to add additional volumes using parameter `volumes`. See Vignette.
* Allow to change default url using parameter `url`
* Allow connection to a Docker network with parameter `network_name`. See Vignette.
* Allow to start container with root privileges with parameter `root = TRUE`. 

## Minor

* Allow to store library out of the project when using {renv}. See Vignette.

# devindocker 0.0.1

* Added a `NEWS.md` file to track changes to the package.
* First release
