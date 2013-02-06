# Conway's Game of Life

Console-based simulation of Conway's game of life.

## Rules of Life

The universe is a rectangular grid with edges that wrap around (like a torus). Each cell is either alive or dead.

* Any live cell with fewer than two live neighbours dies, as if caused by under-population.
* Any live cell with two or three live neighbours lives on to the next generation.
* Any live cell with more than three live neighbours dies, as if by overcrowding.
* Any dead cell with exactly three live neighbours becomes a live cell, as if by reproduction.

In the code these are expressed more tersely as:

* Any cell with exactly two live neighbours remains in its current state.
* Any cell with exactly three live neighbours comes alive (or remains alive).
* Any other cell dies (or remains dead).

## Compiling and running

    $ ghc -O2 -o life Main.hs
    [1 of 2] Compiling Life             ( Life.hs, Life.o )
    [2 of 2] Compiling Main             ( Main.hs, Main.o )
    Linking life.exe ...

    $ life-w 10 -h 10 -f 25 -n 10

     #########
    #  ### ##
        # ## #
       #  # ##
       # ##  #
    #     ##
     ### ##
    ### #  # #
       #### #
    ###   # ##



    ##
    #
    #  #     #
    #   ##   #
    ## #   #
       ###  ##
    #      ###
        # #




    ##

     #  #
      ###   #
     ###  #
     #### #
    #  #  ##
           ###




    ##
      # #
        ##
           #
    #   # #
    ## ####  #
          ####
            #



     #
     # ###
       ###
        # #
    ## ## ## #
     # ##
        # #
            ##



      # #
       # #
      #   #
    # #   ##
    ##    ##
     #    ##
       ###



       ##
      ####
     ### ###
    # #  #
    # #  #  #
    ### #  #
        ###
        #



      #  #
     #

    #    # # #
    # # ###
    # # #  # #
     #  # #
        #



    #
    ##  ##   #
        #  #
    # # #  # #
    ##  #
         #



    ##       #
    ##  ##   #
        # #
    #   ##  ##
    ## ###   #
