
## Running a program

Program accepts one or two directories which must exist:

```
ExportApplePhotos dest-dir
ExportApplePhotos dest-dir dest-dir-deleted
```

- The first directory `dest-dir` is mandatory
  and is used to export all non-deleted assets.
- The second directory `dest-dir-deleted` is optional
  and if it is given it is used to export all deleted assets.

Note that the program needs permission to read phothos.
This permission can be set up in System Preferences, Security & Privacy, Privacy.

## File names

Names of exported files have following pattern:

```
[date time][letter][special].[extension]
```

- `date time` has format `yyyyMMdd_HHmmss`
  and represents a local date time when the asset was created.
- `letter` is either empty or contains a lower-case letter (`b`, `c`, etc)
  and its purpose is to distinguish assets creatded at the same moment.
- `special` is either empty or `-live` or `-burst`.
- `extension` is always lower case.

## Comparison to OneDrive app export

OneDrive app on iPhone can be configured to backup photos
and videos as well. The differences are:

- If a backed up asset is deleted from OneDrive it cannot
  be backed up again. OneDrive somewhere remembers which
  assets were backed up and it seems that this storage cannot
  be emptied. That's actually main reason why I started to write
  this tool.
- OneDrive app backs up only selected photos from bursts.
  On the other hand this tool exports all photos from bursts.
- File names follow pattern `[date time][number]_iOS.[extension]`.
  Where `date time` has the same format `yyyyMMdd_HHmmss`
  but it's UTC time. Which in my opinion is less useful for humans
  but on the other hand it prevents certain problems
  (eg. when switching summer time to winter time and same hour repeats twice).
  
  `number` has 3 digits and it's probably a number of milliseconds.
  I take time from EXIF metadata and they don't have sub-second precision
  so I guess OneDrive app takes them from `Photos.sqlite` database.
  And since seconds in EXIF are probably rounded my time
  may differ by 1 seconds from time from OneDrive app.

  `_iOS` part seems of no use to me. And `extension` doesn't have
  normalized case.


## About license

License of this project looks like the 3-clause BSD License
but it has additional conditions:

- This software must not be used for military purposes.
- Source code must not be used for training or validating AI models.
  For example AI models which generate source code.
