# Tamias
A game engine implemented in Common LISP, focusing currently on 2D support, but 3D is planned down the Road

# Building
Install an implementation of common lisp, SBCL is the only one I can guarantee Tamias will work on. Install quicklisp.
Then, start the implementation in Tamias' directory, type/copy in (load "compile.cl") . If any errors come up (unrelated to the implementation) then make sure you have the developer libs of sdl2, sdl2-image, and sdl2-mixer installed. If everything went smoothly, then type/copy in (load-demos) hit enter then (main) then hit enter. An sdl2 window should pop up. I'll eventually add in ways to test the other demos smoothly, i.e. pressing '1' or '2' to test others out. When you're finished checking it out, hit esc to exit. The exit key will be set to C-` [control-backquote] eventually.

The first 'official' release is TBD

# Updates
12/20/2018:
    Added some animation handling code. will need to be tested more thoroughly.
    Keyboard should have been fixed to use virtual keys (:1) instead of physical keys (:scancode-1)
    Audio updated to help facilitate better looping
    Hex color string parser implemented, needs fine tuning
    
    Possibility of crashing: medium, I still need to work on the API documentation
	    