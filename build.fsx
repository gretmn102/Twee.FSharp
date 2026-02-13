#!dotnet fsi
#r "nuget: Fake.Core.Target, 6.0"
#r "nuget: Fake.DotNet.Cli, 6.0"
#r "nuget: Fake.IO.FileSystem, 6.0"
#r "nuget: Fake.Core.ReleaseNotes, 6.0"

module Utils =
    open Fake.Core
    open Fake.DotNet
    open Fake.IO
    open Fake.IO.FileSystemOperators
    open Fake.IO.Globbing.Operators

    let npm args workingDir =
        let npmPath =
            match ProcessUtils.tryFindFileOnPath "npm" with
            | Some path -> path
            | None ->
                "npm was not found in path. Please install it and make sure it's available from your path. " +
                "See https://safe-stack.github.io/docs/quickstart/#install-pre-requisites for more info"
                |> failwith

        let arguments = args |> String.split ' ' |> Arguments.OfArgs

        Command.RawCommand (npmPath, arguments)
        |> CreateProcess.fromCommand
        |> CreateProcess.withWorkingDirectory workingDir
        |> CreateProcess.ensureExitCode
        |> Proc.run
        |> ignore

    let dotnet cmd workingDir =
        let result = DotNet.exec (DotNet.Options.withWorkingDirectory workingDir) cmd ""
        if result.ExitCode <> 0 then failwithf "'dotnet %s' failed in %s" cmd workingDir

    let removeBinAndObj path =
        try
            Shell.cleanDirs [
                path </> "bin"
                path </> "obj"
            ]
        with e ->
            printfn "%A" e.Message

    let initFakeRuntime () =
        System.Environment.GetCommandLineArgs()
        |> Array.skip 2 // skip fsi.exe; build.fsx
        |> Array.toList
        |> Context.FakeExecutionContext.Create false __SOURCE_FILE__
        |> Context.RuntimeContext.Fake
        |> Context.setExecutionContext

        Target.initEnvironment ()

    let findPackPath dir =
        let packPathPattern =
            dir </> "*.nupkg"

        !! packPathPattern
        |> Seq.truncate 2
        |> List.ofSeq
        |> function
            | [nupkgPath] -> nupkgPath
            | [] ->
                failwithf "'%s' not found" packPathPattern
            | nupkgPaths ->
                failwithf "More than one *.nupkg found: '%A'" nupkgPaths

module XmlText =
    let escape rawText =
        let doc = new System.Xml.XmlDocument()
        let node = doc.CreateElement("root")
        node.InnerText <- rawText
        node.InnerXml

Utils.initFakeRuntime ()

module CoreProject =
    open Fake.Core
    open Fake.Core.TargetOperators
    open Fake.IO
    open Fake.IO.FileSystemOperators

    open Utils

    let projectDirectory = Path.getFullName "./src"
    let deployDirectory = Path.getFullName "./deploy"
    let releasePath = projectDirectory </> "RELEASE_NOTES.md"

    let prefix = "Core"

    let commonBuildArgs = "-c Release"

    let dotnetBuildTarget = prefix + "DotnetBuild"
    Target.create dotnetBuildTarget (fun _ ->
        projectDirectory
        |> dotnet (sprintf "build %s" commonBuildArgs)
    )

    let dotnetCleanTarget = prefix + "DotnetClean"
    Target.create dotnetCleanTarget (fun _ ->
        removeBinAndObj projectDirectory
    )

    let deployCleanTarget = prefix + "DeployClean"
    Target.create deployCleanTarget (fun _ ->
        Shell.cleanDir deployDirectory
    )

    let deployTarget = prefix + "Deploy"
    Target.create deployTarget (fun _ ->
        projectDirectory
        |> dotnet (sprintf "build %s -o \"%s\"" commonBuildArgs deployDirectory)
    )

    let metaTarget = prefix + "Meta"
    Target.create metaTarget (fun _ ->
        let release = ReleaseNotes.load releasePath

        [
            "<Project xmlns=\"http://schemas.microsoft.com/developer/msbuild/2003\">"
            "<ItemGroup>"
            "    <PackageReference Include=\"Microsoft.SourceLink.GitHub\" Version=\"1.0.0\" PrivateAssets=\"All\"/>"
            "</ItemGroup>"
            "<PropertyGroup>"
            "    <EmbedUntrackedSources>true</EmbedUntrackedSources>"
            "    <PackageProjectUrl>https://github.com/gretmn102/Twee.FSharp/tree/main/src</PackageProjectUrl>"
            "    <PackageLicenseExpression>MIT</PackageLicenseExpression>"
            "    <RepositoryUrl>https://github.com/gretmn102/Twee.FSharp.git</RepositoryUrl>"
            sprintf "    <PackageReleaseNotes>%s</PackageReleaseNotes>"
                (String.concat "\n" release.Notes |> XmlText.escape)
            "    <PackageTags>fsharp;twine;twee</PackageTags>"
            "    <Authors>gretmn102</Authors>"
            sprintf "    <Version>%s</Version>" (string release.SemVer)
            "</PropertyGroup>"
            "</Project>"
        ]
        |> File.write false (
            projectDirectory </> "Directory.Build.props"
        )
    )

    let packTarget = prefix + "Pack"
    Target.create packTarget (fun _ ->
        projectDirectory
        |> dotnet (sprintf "pack %s -o \"%s\"" commonBuildArgs deployDirectory)
    )

    let publishToGitlab = prefix + "PublishToGitlab"
    Target.create publishToGitlab (fun _ ->
        let path = findPackPath deployDirectory
        let source = "https://gitlab.com/api/v4/projects/28574921/packages/nuget/index.json"
        let apiKey = Environment.environVarOrFail "GITLAB_DEPLOY_TOKEN"
        "."
        |> dotnet (
            String.concat " " [
                "nuget"
                "push"
                $"--source {source}"
                $"--api-key {apiKey}"
                "--skip-duplicate"
                $"{path}"
            ]
        )
    )

    let publishToGitlabLocal = prefix + "PublishToGitlabLocal"
    Target.create publishToGitlabLocal (fun _ ->
        let path = findPackPath deployDirectory
        let source = "gitlab"
        "."
        |> dotnet (
            String.concat " " [
                "nuget"
                "push"
                $"--source {source}"
                "--skip-duplicate"
                $"{path}"
            ]
        )
    )

    deployCleanTarget
        ==> metaTarget
        ==> deployTarget

    deployCleanTarget
        ==> metaTarget
        ==> packTarget

    packTarget ==> publishToGitlab
    packTarget ==> publishToGitlabLocal

module TestsProject =
    open Fake.Core
    open Fake.Core.TargetOperators
    open Fake.IO
    open Fake.IO.FileSystemOperators

    open Utils

    let projectDirectory = Path.getFullName "./tests"

    let prefix = "Tests"

    let commonBuildArgs = "-c Release"

    let dotnetBuildTarget = prefix + "DotnetBuild"
    Target.create dotnetBuildTarget (fun _ ->
        projectDirectory
        |> dotnet (sprintf "build %s" commonBuildArgs)
    )

    let dotnetRunTarget = prefix + "DotnetRun"
    Target.create dotnetRunTarget (fun _ ->
        projectDirectory
        |> dotnet (sprintf "run %s" commonBuildArgs)
    )

    let dotnetCleanTarget = prefix + "DotnetClean"
    Target.create dotnetCleanTarget (fun _ ->
        removeBinAndObj projectDirectory
    )

open Fake.Core
open Fake.Core.TargetOperators

let cleanTarget = "Clean"
Target.create cleanTarget (fun _ -> ())

CoreProject.deployCleanTarget ==> cleanTarget
CoreProject.dotnetCleanTarget ==> cleanTarget
TestsProject.dotnetCleanTarget ==> cleanTarget

try
    Target.runOrDefaultWithArguments CoreProject.deployTarget
with _ ->
    System.Environment.Exit(1)
