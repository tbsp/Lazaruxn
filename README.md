# Lazaruxn
Lazarus based uxn emulator

This is a very early implementation of the UXN Varvara virtual machine in FreePascal.

It supports basic file input (no output yet), and the basics of the screen device.

It is not ready for primetime, lacking support for vectors, controller, sound, file writing, datetime, etc.

# TODO

- Restructure the program to fire events which go to a queue and are processed in order to execute device vector code
- Properly handle errors (stack over/underflow, etc)
- Complete screen device (mono palette support)
- Add support for outstanding devices
- Sandbox file I/O
 
