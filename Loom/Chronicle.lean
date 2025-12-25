/-
  Loom.Chronicle - Chronicle logging integration for Loom

  Provides middleware for HTTP request logging using Chronicle.
-/

import Chronicle
import Loom.Controller
import Citadel

namespace Loom.Chronicle

/-- File logging middleware that writes HTTP requests to a log file -/
def fileLogging (logger : Chronicle.Logger) : Citadel.Middleware := fun handler req => do
  let start ← IO.monoNanosNow
  let resp ← handler req
  let elapsed := (← IO.monoNanosNow) - start
  let ms := elapsed.toFloat / 1000000.0

  let entry : Chronicle.LogEntry := {
    timestamp := start
    level := .info
    message := s!"[{req.method}] {req.path} -> {resp.status.code}"
    path := some req.path
    method := some (ToString.toString req.method)
    statusCode := some resp.status.code.toNat
    durationMs := some ms
  }
  Chronicle.Logger.logRequest logger entry
  pure resp

/-- Error-aware logging middleware that logs errors at ERROR level -/
def errorLogging (logger : Chronicle.Logger) : Citadel.Middleware := fun handler req => do
  let start ← IO.monoNanosNow
  let resp ← handler req
  let elapsed := (← IO.monoNanosNow) - start
  let ms := elapsed.toFloat / 1000000.0

  let level := if resp.status.code >= 500 then Chronicle.Level.error
               else if resp.status.code >= 400 then Chronicle.Level.warn
               else Chronicle.Level.info

  let entry : Chronicle.LogEntry := {
    timestamp := start
    level := level
    message := s!"[{req.method}] {req.path} -> {resp.status.code}"
    path := some req.path
    method := some (ToString.toString req.method)
    statusCode := some resp.status.code.toNat
    durationMs := some ms
  }
  Chronicle.Logger.logRequest logger entry
  pure resp

/-- File logging middleware for MultiLogger (writes to multiple files) -/
def fileLoggingMulti (logger : Chronicle.MultiLogger) : Citadel.Middleware := fun handler req => do
  let start ← IO.monoNanosNow
  let resp ← handler req
  let elapsed := (← IO.monoNanosNow) - start
  let ms := elapsed.toFloat / 1000000.0

  let entry : Chronicle.LogEntry := {
    timestamp := start
    level := .info
    message := s!"[{req.method}] {req.path} -> {resp.status.code}"
    path := some req.path
    method := some (ToString.toString req.method)
    statusCode := some resp.status.code.toNat
    durationMs := some ms
  }
  Chronicle.MultiLogger.logRequest logger entry
  pure resp

/-- Error-aware logging middleware for MultiLogger -/
def errorLoggingMulti (logger : Chronicle.MultiLogger) : Citadel.Middleware := fun handler req => do
  let start ← IO.monoNanosNow
  let resp ← handler req
  let elapsed := (← IO.monoNanosNow) - start
  let ms := elapsed.toFloat / 1000000.0

  let level := if resp.status.code >= 500 then Chronicle.Level.error
               else if resp.status.code >= 400 then Chronicle.Level.warn
               else Chronicle.Level.info

  let entry : Chronicle.LogEntry := {
    timestamp := start
    level := level
    message := s!"[{req.method}] {req.path} -> {resp.status.code}"
    path := some req.path
    method := some (ToString.toString req.method)
    statusCode := some resp.status.code.toNat
    durationMs := some ms
  }
  Chronicle.MultiLogger.logRequest logger entry
  pure resp

end Loom.Chronicle
