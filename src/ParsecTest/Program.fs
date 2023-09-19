module ParsecTest.Program

open Expecto

open Parsec.SourceFileReader
open Parsec.SourcePos

[<Tests>]
let sourcePosTests =
    testList
        "SourcePos"
        [ test "format position" {
              let pos = SourcePos.origin
              let formatted = $"%A{pos}"
              Expect.equal formatted "[Ln 1, Col 0]" "Line number: 1-based, Column number: 0-based"
          }

          test "format range" {
              let range =
                  { Start = { Line = 1; Column = 3 }
                    End = { Line = 1; Column = 7 } }

              let formatted = $"%A{range}"

              Expect.equal
                  formatted
                  "<[Ln 2, Col 3] - [Ln 2, Col 7]>"
                  "Both start and end positions should be formatted and combined"
          } ]

[<Tests>]
let sourceFileReaderTests =
    testList
        "SourceFileReader"
        [ test "Read" {
              let sourceFileReader = SourceFileReader "abc"
              Expect.equal (sourceFileReader.Read()) ({ Line = 0; Column = 1 }, Some 'a') "First char"
              Expect.equal (sourceFileReader.Read()) ({ Line = 0; Column = 2 }, Some 'b') "Second char"
              Expect.equal (sourceFileReader.Read()) ({ Line = 0; Column = 3 }, Some 'c') "Third char"
              Expect.equal (sourceFileReader.Read()) ({ Line = 0; Column = 3 }, None) "EOF"
          } ]

[<EntryPoint>]
let main args = runTestsInAssemblyWithCLIArgs [] args
