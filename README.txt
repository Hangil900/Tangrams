Tangrams Game (Coded in Ocaml)

The Region, Make, and Ui ml files were created by the proffesor.

To Run: The UI in this project depends on the library lablGL. The way to download this library differs for each machine. The tangrams game has already been made with the Make file, thus after downloading the lablGL library simply running the tangrams game (./tangrams) will display the UI.

In order to create the game of Tangrams, I implemented several precision number types. Using these number types, the convex polygon of a list of points were calculated. This method was repeatedly used to determine whether obstacles overlapped or not. In the numbers file, I implemented several precision number types. The most interesting number type Reals, is actually a function, which given an int returns the number to the nth decimal precision. Using this type definition for Reals, I was able to implement numebrs like pi and the natural number e. The geometry file implements the concepts of a Minkowski Difference/Sum and the Convex Hull, to give a convex polygon region given a set of points. The Game file, updates the current state of the game, depending on the users actions.