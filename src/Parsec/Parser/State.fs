module Parsec.Parser.State

open Type

/// <summary>パーサの現在の状態 <c>'State</c> を結果値として取り出すパーサの実体をつくる</summary>
let inline getState () : Parser<'State, 'State> =
    fun (state, _reader) ->
        Ok
            { NewState = state
              ParsedValue = state }

/// <summary><c>putState state</c> はパーサの状態を <c>state</c> に更新するパーサの実体をつくる</summary>
let inline putState (state: 'State) : Parser<'State, unit> =
    fun (_state, _reader) -> Ok { NewState = state; ParsedValue = () }

/// <summary><c>modifyState mapping</c> は、パーサの状態に対して関数 <c>mapping</c> を適用して更新するパーサの実体をつくる</summary>
let inline modifyState ([<InlineIfLambda>] mapping: 'State -> 'State) : Parser<'State, unit> =
    fun (state, _reader) ->
        Ok
            { NewState = mapping state
              ParsedValue = () }
