module TestTasks

open BlackFox.Fake
open Fake.DotNet

open ProjectInfo
open BasicTasks

let runTests = BuildTask.create "RunTests" [clean; build] {
    testProjects
    |> Seq.iter (fun testProject ->
        Fake.DotNet.DotNet.test(fun testParams ->
            {
                testParams with
                    Logger = Some "console;verbosity=detailed"
                    Configuration = DotNet.BuildConfiguration.fromString configuration
                    NoBuild = true
            }
        ) testProject
    )
}

let runTestsWithCodeCov = BuildTask.create "RunTestsWithCodeCov" [clean; build] {
    testProjects
    |> Seq.iter (fun testProject ->
        Fake.DotNet.DotNet.test(fun testParams ->
            {
                testParams with
                    Logger              = Some "console;verbosity=detailed"
                    Configuration       = DotNet.BuildConfiguration.fromString configuration
                    NoBuild             = true
                    Collect             = Some "XPlat Code Coverage"
                    ResultsDirectory    = Some "./TestResults"
            }
        ) testProject
    )
}