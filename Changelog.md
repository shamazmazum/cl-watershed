# Changelog

## Version 0.3

* Incompatible change: The result is now an array of fixnums. Watershed line is
  labeled with `-1`.
* Improvement: Boundary between objects is more smooth now.
* Improvement: Updated docstrings and the notebook.

## Version 0.2

* Incompatible change: All arrays (image, seeds, mask) now have type
  element-type `alexandria:non-negative-fixnum` (`(unsigned-byte 62)` in
  SBCL). Floating point numbers aren't used anymore.
* Improvement: Migration to `damn-fast-priority-queue`. Improvement in speed is
  about 30%.
