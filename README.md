fsproj-mode
===========

An Emacs major-mode for creating, viewing and editing F# project files.

![alt tag](https://raw.github.com/simontcousins/fsproj-mode/master/screen-shot.png)

Where:

Column | Description
------ | -----------
S | The Status column (see Status table below).
No. | The position of the file within the project.
File Name | The name of the file.
Size | The size of the file in bytes.
Build Action | How the file is handled by the build.
Copy Action | How the file is copied by the build.


Status | Description
------ | -----------
- | The file is on the disk and not in the project.
+ | The file is on the disk and in the project.
! | The file is not on the disk and in the project.

Keys | Action
---- | ------
RET | Open the current file in this window.
e | Open the current file in this window.
f | Open the current file in this window.
g | Refresh the project buffer.
m | Move the position of the current file within the project.
o | Open the current file in another window.
q | Quit the project buffer.
? | Open Help in another window.
