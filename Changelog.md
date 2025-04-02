# Changelog

## Version 0.2

* Incompatible change: All arrays (image, seeds, mask) now have type
  element-type `alexandria:non-negative-fixnum` (`(unsigned-byte 62)` in
  SBCL). Floating point numbers aren't used anymore.
* Improvement: Migration to `damn-fast-priority-queue`. Improvement in speed is
  about 30%.
