# strictmath

Pure common lisp implentation of Java's StrictMath class.  StrictMath is based on the pure software math library fdlibm, and as such, this solution should be equivalent and portable.

## Limitations

In the true "scratch an itch" category of software development, this library only does things as strictly needed by clnl in order to be consistent with regard to the JVM implementation of NetLogo.  Only the following things are implemented and tested:

* Double precision only
* Bounded to [0,360] or [0,2pi] only
* The following StrictMath methods
  * sin
  * cos
  * abs
  * toRadians

Anything else that works can be considered a pleasant surprise.

## License

This library lifts substantially from the evita-common-lisp implementation, as is noted in the LICENSE file.  Outside of that, functions are attempted ports of fdlibm functions, in as close as possible to a line for line translation.  As such, the files are similarly bound by the licenses included therein.

## Dependencies

The following are depended on by strictmath

* ieee-floats
