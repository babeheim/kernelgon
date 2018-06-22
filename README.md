kernelgon
============

The function `kernelgon` will calculate and plot polygons in base R graphics based on a collection of points stored as an $n$ by 2 matrix.

Requirements:
- R (v 3.3.2 or greater) https://cran.r-project.org/
- `ks` package (v 1.11.2 or greater)
- `concaveman` package (v 1.0.0 or greater)
- `igraph` package (v 1.2.1 or greater)

Installing on Ubuntu may require `libudunits2.so` initially by the command line command

```
sudo apt-get install libudunits2-dev
```

Also needed for `concaveman` is the `V8` package, which may also need a library file downloaded first, using

```
sudo apt-get install -y libv8-3.14-dev
```