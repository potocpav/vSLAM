# Haskell FastSLAM 2.0

This project is a part of my bachelor thesis. It is a (new) visual SLAM 
implementation in Haskell. Designed for a Ladybug 3 omnicamera mounted on top of
a tracked robot. The code is half Haskell, half C++. The C++ part deals
with ROS I/O, OpenCV feature detection, non-maxima supression and some coordinate
conversions. The haskell part can be used compeletely separately; the FFI is very
small.

## Performance

As of tag FastSLAM-2.0-v3, the error in a fairly nice scene is ~4% of
the distance traveled. With the use of gyros, it gets even better, like 2% :)
It operates in realtime on a normal desktop (4fps); optimizations are still well 
possible.

## Disclaimer

This project is not at the moment intended for usage by anyone other, than me.

I am not discouraging anyone to mess around. The code itself is quite fine, not 
too messy or anything. At least not the Haskell part. But I am quite sure, that 
to setup the build process correctly is gonna be quite a pain. Also, the input
data are quite specific and completely undocumented :/

I am gonna have it all cleaned up by the end of the May 2014 (that's the deadline).
So do not waste any time on this project yet. It IS gonna get better :-) I am 
planning to push it to Hackage, when it is cleaned up and ready for anyone to use.

Moreover, there are some issues, that are rather annoying, and that nobody else 
than me can probably sort out (maybe not even discover). And they're gonna prove 
a pain-in-the-ass for anybody stumbling upon them.

Seriously, you got better things to do :D

Have a nice day,
Pavel
