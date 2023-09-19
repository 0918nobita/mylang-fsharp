/// パーサに対する入力
module Parsec.SourceFileReader

open System.Collections.Generic

open SourcePos

type SourceFileReader(source: string) =
    let len = String.length source
    let posMemo = Dictionary<int, SourcePos>()

    do posMemo.Add(-1, SourcePos.origin)

    let mutable currentPos = SourcePos.origin

    member val CurrentIndex = -1 with get, set

    member this.CurrentPos = posMemo.[this.CurrentIndex]

    member this.Read() =
        this.CurrentIndex <- this.CurrentIndex + 1

        if this.CurrentIndex < len then
            if posMemo.ContainsKey this.CurrentIndex then
                let pos = posMemo.[this.CurrentIndex]
                let c = source.[this.CurrentIndex]
                (pos, Some c)
            else
                let c = source.[this.CurrentIndex]

                currentPos <-
                    if c = '\n' then
                        SourcePos.nextLine currentPos
                    else
                        SourcePos.nextColumn currentPos

                posMemo.[this.CurrentIndex] <- currentPos
                (currentPos, Some c)
        else
            (currentPos, None)

    member this.Reset() = this.CurrentIndex <- -1
