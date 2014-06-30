# fsproj-mode

An Emacs major-mode for creating, viewing and editing F# project files.

![alt tag](https://raw.github.com/simontcousins/fsproj-mode/master/screen-shot.png)

### Columns

Column | Description
------ | -----------
S | The status of the file (see File Status).
No. | The position of the file within the project.
File Name | The name of the file.
Size | The size of the file in bytes.
Build Action | How the file is handled by the build.
Copy Action | How the file is copied by the build.


### File Status

Status | Description
------ | -----------
- | The file is on the disk and not in the project.
+ | The file is on the disk and in the project.
! | The file is not on the disk and in the project.

### Keys

Key | Action
--- | ------
RET | Open the file at cursor in this window.
d | Delete the file at cursor
e | Open the file at cursor in this window.
f | Open the file at cursor in this window.
g | Refresh the project buffer.
m | Move the position of the file at cursor within the project.
n | Add a new file to the project.
o | Open the file at cursor in another window.
q | Quit the project buffer.
+ | Add the file at cursor to the project.
- | Remove the file at cursor from the project.
? | Open Help in another window.
