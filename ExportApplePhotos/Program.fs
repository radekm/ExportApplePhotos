open System
open System.IO

open Dapper
open MetadataExtractor
open Microsoft.Data.Sqlite

let toMap getKey items =
    items
    |> List.groupBy getKey
    |> List.map (function
        | key, [item] -> key, item
        | key, items -> failwith $"Multiple items with same key %A{key}: %A{items}")
    |> Map.ofList

type FileName = string
type Directory = string
type Metadata = { FileName : FileName
                  Directory : Directory
                  OrigFileName : string
                  Deleted : bool
                  // Sample data: `2024-06-18 00:16:25`.
                  DateCreatedUtc : string
                  // Sample data: `2024:06:17 18:16:25`.
                  ExifTimestampLocal : string option
                  // Adding `TimeZoneOffsetSeconds` to `DateCreatedUtc`
                  // should result in `ExifTimestampLocal`.
                  TimeZoneOffsetSeconds : int64
                  OriginalFileSize : int64
                }

module PhotoDatabase =
    type private MetadataRaw = { FileName : FileName
                                 Directory : Directory
                                 OrigFileName : string
                                 TrashedState : string
                                 Complete : int64
                                 DateCreated : string
                                 ExifTimestampString : string
                                 TimeZoneOffset : int64
                                 TimeZoneName : string  // Is it useful for anything?
                                 OriginalFileSize : int64
                               }

    /// Query tables `ZASSET` and `ZADDITIONALASSETATTRIBUTES` in `Phothos.sqlite` database.
    let queryMetadata dbFile : Map<Directory * FileName, Metadata> =
        let conStr = $"Data Source=%s{dbFile}"
        use con = new SqliteConnection(conStr)
        con.Open()
        let query = """
            select
                ZFILENAME as 'FileName',
                ZDIRECTORY as 'Directory',
                ZADDITIONALASSETATTRIBUTES.ZORIGINALFILENAME as 'OrigFileName',
                case ZTRASHEDSTATE
                    when 1 then 'deleted'
                    when 0 then 'not deleted'
                    else 'unknown ' || ZTRASHEDSTATE
                end as 'TrashedState',
                ZCOMPLETE as 'Complete',
                datetime('2001-01-01', ZDATECREATED || ' seconds') as 'DateCreated',
                ZADDITIONALASSETATTRIBUTES.ZEXIFTIMESTAMPSTRING as 'ExifTimestampString',
                ZADDITIONALASSETATTRIBUTES.ZTIMEZONEOFFSET as 'TimeZoneOffset',
                ZADDITIONALASSETATTRIBUTES.ZTIMEZONENAME as 'TimeZoneName',
                ZADDITIONALASSETATTRIBUTES.ZORIGINALFILESIZE as 'OriginalFileSize'
            from ZASSET
            join ZADDITIONALASSETATTRIBUTES on ZASSET.ZADDITIONALATTRIBUTES = ZADDITIONALASSETATTRIBUTES.Z_PK
        """
        con.Query<MetadataRaw>(query)
        |> List.ofSeq
        |> List.map (fun raw ->
            if raw.Complete <> 1 then
                failwithf "Asset not complete (it's probably not stored locally): %A" raw

            { FileName = raw.FileName
              Directory = raw.Directory
              OrigFileName = raw.OrigFileName
              Deleted =
                  match raw.TrashedState with
                  | "deleted" -> true
                  | "not deleted" -> false
                  | _ -> failwith $"Unknown trashed stated: %A{raw}"
              DateCreatedUtc = raw.DateCreated
              ExifTimestampLocal = raw.ExifTimestampString |> Option.ofObj
              TimeZoneOffsetSeconds = raw.TimeZoneOffset
              OriginalFileSize = raw.OriginalFileSize
            })
        |> toMap (fun metadata -> metadata.Directory, metadata.FileName)

type File = { Path : string
              TimeZone : string option
              CreatedDate : DateTime
              ContentId : string option
              BurstId : string option
            }

let private parseDateTime s format =
    DateTime.ParseExact(
        s,
        format,
        System.Globalization.CultureInfo.InvariantCulture.DateTimeFormat)

module DirectoryWithPhotos =
    let private listRec dir =
        let result = ResizeArray()
        let rec list dir =
            Directory.GetFiles dir
            |> result.AddRange
            Directory.GetDirectories dir
            |> Array.iter list
        list dir
        result |> Seq.toList

    let list dir =
        listRec dir
        |> List.filter (fun file ->
            // Retain only photos and videos
            (Path.GetExtension file).ToLowerInvariant() <> ".aae")
        |> List.choose (fun file ->
             let dirs = ImageMetadataReader.ReadMetadata file
             let readTag dir tag =
                 dirs
                 |> Seq.filter (fun d -> d.Name = dir)
                 |> Seq.collect (fun d -> d.Tags)
                 |> Seq.filter (fun t -> t.Name = tag)
                 |> Seq.map (fun t -> t.Description)
                 |> Seq.toList
                 |> function
                     | [] -> None
                     | [desc] -> Some desc
                     | descs -> failwith $"Tag %s{dir}/%s{tag} in file %s{file} has multiple values: %A{descs}"
             let createdDate =
                 match readTag "Exif SubIFD" "Date/Time Original", readTag "QuickTime Movie Header" "Created" with
                 | None, None ->
                     printfn $"Skipping file because it has no created date: %s{file}"
                     None
                 // For photos. Photos have local time.
                 | Some s, None -> Some (parseDateTime s "yyyy:MM:dd HH:mm:ss")
                 // For videos. Videos have UTC time.
                 | None, Some s -> Some (parseDateTime s "ddd MMM dd HH:mm:ss yyyy")
                 | Some _, Some _ -> failwith $"Multiple created dates for file %s{file}"
             createdDate
             |> Option.map (fun createdDate ->
                 { Path = file
                   TimeZone = readTag "Exif SubIFD" "Time Zone"
                   CreatedDate = createdDate
                   ContentId =
                       match
                           readTag "Apple Makernote" "Content Identifier",
                           readTag "QuickTime Metadata Header" "Content Identifier" with
                       | None, None -> None
                       | Some s, None -> Some s  // For photos.
                       | None, Some s -> Some s  // For videos.
                       | _ -> failwith $"Content id defined twice in file %s{file}"
                   BurstId = readTag "Apple Makernote" "Burst UUID"
                 }))

[<RequireQualifiedAccess>]
type Asset =
    // We keep live photo and corresponding live video together
    // because we have to ensure that they have the same file name.
    //
    // Note: Database `Phothos.sqlite` contains asset in table `ZASSET` only for `photo`.
    | LivePhoto of photo:File * video:File
    | Other of File

    member me.File =
        match me with
        | LivePhoto (photo, _) -> photo
        | Other file -> file

let filesToAssets (files : File list) =
    let live, other = files |> List.partition (fun file -> file.ContentId.IsSome)
    live
    |> List.groupBy (fun live -> live.ContentId.Value)
    |> List.map (fun (contentId, live) ->
        let video, photo = live |> List.partition (fun live -> Path.GetExtension live.Path = ".mov")
        match photo, video with
        | [photo], [video] -> Asset.LivePhoto (photo, video)
        | [photo], [] -> Asset.Other photo
        | [], [video] -> Asset.Other video
        | _ -> failwith $"Content id %s{contentId} has multiple photos %A{photo} or videos %A{video}")
    |> List.append (other |> List.map Asset.Other)

let copyFileAndPreserveTimestamps (source : string) (dest : string) =
    let sourceInfo = FileInfo source
    let destInfo = sourceInfo.CopyTo(dest, overwrite = false)

    destInfo.CreationTime <- sourceInfo.CreationTime
    destInfo.LastWriteTime <- sourceInfo.LastWriteTime
    destInfo.LastAccessTime <- sourceInfo.LastAccessTime

let almostSameDate (a : DateTime) (b : DateTime) =
    Math.Abs(a.Ticks - b.Ticks) <= TimeSpan.TicksPerSecond

[<EntryPoint>]
let main argv =
    let destDir, destDirDeleted =
        match argv with
        | [| dir |] -> dir, None
        | [| dir; dirDeleted |] -> dir, Some dirDeleted
        | _ -> failwith "Expecting destination directory and optional destination directory for deleted assets"
    if Directory.Exists destDir |> not then
        failwith $"Destination directory %s{destDir} doesn't exist"
    match destDirDeleted with
    | Some dirDeleted when Directory.Exists dirDeleted |> not ->
        failwith $"Destination directory for deleted assets %s{dirDeleted} doesn't exist"
    | _ -> ()

    let userDir = Environment.GetFolderPath Environment.SpecialFolder.UserProfile
    let photosLibraryDir = Path.Combine(userDir, "Pictures", "Photos Library.photoslibrary")
    let database = Path.Combine(photosLibraryDir, "database", "Photos.sqlite")
    let photosDir = Path.Combine(photosLibraryDir, "originals")

    if File.Exists database |> not then
        failwith $"Database %s{database} doesn't exist"
    if Directory.Exists photosLibraryDir |> not then
        failwith $"Directory with photos %s{photosLibraryDir} doesn't exist"

    let metadata = PhotoDatabase.queryMetadata database
    printfn "Found %d metadata in database %s" metadata.Count database
    //assetsInDb |> Map.iter (printfn "%s -> %A")

    let files = DirectoryWithPhotos.list photosDir
    printfn "Found %d photos and videos in directory %s" files.Length photosDir
    //files |> List.iter (printfn "%A")

    let getMetadataForFile (file : File) =
        let dirName = Path.GetFileName (Path.GetDirectoryName file.Path)
        let fileName = Path.GetFileName file.Path
        metadata
        |> Map.find (dirName, fileName)
    let getOrigFileName (file : File) = (getMetadataForFile file).OrigFileName
    let getCreatedDateLocal (file : File) =
        let metadata = getMetadataForFile file
        let createdUtc = parseDateTime metadata.DateCreatedUtc "yyyy-MM-dd HH:mm:ss"
        let createdLocal = createdUtc.AddSeconds(float metadata.TimeZoneOffsetSeconds)
        // Check result with `ExifTimestampLocal`.
        match metadata.ExifTimestampLocal with
        | None -> ()
        | Some exifLocal ->
            let exifLocal = parseDateTime exifLocal "yyyy:MM:dd HH:mm:ss"
            if exifLocal <> createdLocal then
                failwithf "File %A with metadata %A has created local %A which is different from exif local %A"
                    file metadata createdLocal exifLocal
        // Check result with `file.CreatedDate`
        // where videos have UTC time and other assets have local time.
        // I don't know why iPhone doesn't store timestamp for videos in the same timezone
        // as timestamp for photos.
        if file.Path.ToLowerInvariant().EndsWith ".mov" then
            // For some reason these timestamps differ by 1 second
            // so we can't compare them with `<>`.
            if not (almostSameDate createdUtc file.CreatedDate) then
                // Expect UTC time.
                failwithf "Video %A with metadata %A has created utc %A which is different from file time %A"
                    file metadata createdUtc file.CreatedDate
        else
            if createdLocal <> file.CreatedDate then
                // Expect local time.
                failwithf "Photo %A with metadata %A has created local %A which is different from file time %A"
                    file metadata createdLocal file.CreatedDate

        createdLocal

    // Assets in unique ordering by:
    // - created date,
    // - original file name (because different assets may have same created date).
    let orderedAssets =
        filesToAssets files
        |> List.groupBy (fun asset ->
            let file = asset.File
            getCreatedDateLocal file, getOrigFileName file)
        |> List.map (function
            | key, [asset] -> key, asset
            // Unique key guarantees unique ordering.
            | key, assets -> failwith $"Multiple assets with same key %A{key}: %A{assets}")
        |> List.sortBy fst
        |> List.map snd

    // Numbers will be used when renaming assets.
    // Assets with same created date will be differentiated by number.
    //
    // Note: It's important to number deleted and not deleted assets together
    // so we don't get name clashes when restoring deleted assets.
    let numberedAssets =
        let mutable lastDate = DateTime.MinValue
        let mutable i = 0
        orderedAssets
        |> List.map (fun asset ->
            let date = asset.File.CreatedDate
            i <- if date = lastDate then i + 1 else 0
            lastDate <- date
            asset, i)

    let deleted, notDeleted =
        numberedAssets
        |> List.partition (fun (asset, _) ->
            let file = asset.File
            (getMetadataForFile file).Deleted)

    // Copies `numberedAssets` to directory `destDir`.
    let copyAssetsTo (numberedAssets : list<Asset * int>) destDir =
        numberedAssets
        |> List.iter (fun (asset, i) ->
            let file = asset.File

            let fileSize = FileInfo(file.Path).Length
            let metadata = getMetadataForFile file
            if metadata.OriginalFileSize <> fileSize then
                failwithf "Current file size %d differs from original file size %d for file %A"
                    fileSize metadata.OriginalFileSize file

            let date = (getCreatedDateLocal file).ToString("yyyyMMdd_HHmmss")
            // Numbers ensure that `newFileName`s are unique.
            let number =
                // We use chars in file names because of:
                // (1) File sorting: Files sorts digits before dashes
                // so `20210630_1122551-burst.jpeg` appears
                // before `20210630_112255-burst.jpeg` which is wrong.
                // On the other hand lower case letters are sorted after dashes
                // so `20210704_113852b-burst.jpeg` appears
                // after `20210630_112255-burst.jpeg` which is correct.
                // (2) Lower case letters can be
                // more easily distinguished from seconds.
                match char (int 'a' + i) with
                | 'a' -> ""
                | c when c <= 'z' -> string c
                | _ -> failwith $"Too big number %d{i}"
            let special =
                if file.ContentId.IsSome then "-live"
                elif file.BurstId.IsSome then "-burst"
                else ""
            let extension = (Path.GetExtension file.Path).ToLowerInvariant()
            let newFileName = date + number + special + extension
            let newFilePath = Path.Combine(destDir, newFileName)

            match asset with
            | Asset.LivePhoto (photo, video) ->
                printfn "Copying live photo %s" newFilePath
                copyFileAndPreserveTimestamps photo.Path newFilePath
                let videoExtension = (Path.GetExtension video.Path).ToLowerInvariant()
                let newVideoPath = Path.ChangeExtension(newFilePath, videoExtension)
                printfn "Copying live video %s" newVideoPath
                copyFileAndPreserveTimestamps video.Path newVideoPath
            | Asset.Other _ ->
                printfn "Copying asset %s" newFilePath
                copyFileAndPreserveTimestamps file.Path newFilePath)

    printfn "Copying %d not deleted assets" notDeleted.Length
    copyAssetsTo notDeleted destDir

    // Copy deleted assets only if `destDirDeleted` was given.
    match destDirDeleted with
    | None -> printfn "Skipping %d deleted assets" deleted.Length
    | Some destDirDeleted ->
        printfn "Copying %d deleted assets" deleted.Length
        copyAssetsTo deleted destDirDeleted

    0
