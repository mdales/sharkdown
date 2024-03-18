---
Title: "The meaning of LIFE"
---
This is some documentation 

~~~shark-build:gdal-env
((from ghcr.io/osgeo/gdal:ubuntu-small-3.6.4))
~~~


~~~shark-run:gdal-env
$ echo %{project_ids} > %{path_prefix}%{project_ids}.txt
$ cat %{path_prefix}%{project_ids}.txt
~~~

