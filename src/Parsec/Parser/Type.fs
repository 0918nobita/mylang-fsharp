module Parsec.Parser.Type

open Parsec.SourcePos
open Parsec.SourceFileReader

/// <summary>パース成功</summary>
/// <remarks>遷移後の状態と結果値を内包する</remarks>
/// <typeparam name="State">パーサが持ち回る状態</typeparam>
/// <typeparam name="T">結果値</typeparam>
type Success<'State, 'T> = { NewState: 'State; ParsedValue: 'T }

/// <summary>パースの失敗の種別</summary>
/// <remarks><c>Soft</c> の場合バックトラック可能だが、<c>Hard</c> の場合バックトラックできない</remarks>
type FailureKind =
    | Soft
    | Hard

/// <summary>パース失敗</summary>
/// <remarks>パースが失敗した場合に得られる、「想定していた入力」「実際の入力」「発生位置」に関する情報</remarks>
type Failure =
    { Kind: FailureKind
      Actual: string
      Expected: string
      SourceRange: SourceRange }

/// <summary>パース結果</summary>
/// <remarks>失敗した際には、バックトラックを許す場合は <c>SoftFailure</c>、バックトラックを禁止する場合は <c>HardFailure</c> で表現する。</remarks>
/// <typeparam name="State">パーサが持ち回る状態</typeparam>
/// <typeparam name="T">パースに成功した場合の結果値</typeparam>
type ParseResult<'State, 'T> = Result<Success<'State, 'T>, Failure>

type Parser<'State, 'T> = 'State * SourceFileReader -> ParseResult<'State, 'T>
