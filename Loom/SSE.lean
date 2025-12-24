/-
  Loom.SSE - Server-Sent Events integration

  Provides explicit publish API for broadcasting events to connected SSE clients.
  Actions can call these functions to push real-time updates to browsers.
-/
import Citadel
import Loom.Controller
import Loom.ActionM

namespace Loom.SSE

/-- SSE configuration -/
structure Config where
  /-- Default topic for events when none is specified -/
  defaultTopic : String := "default"
  /-- Heartbeat interval in seconds (sent as ping comments) -/
  heartbeatInterval : Nat := 15
  deriving Repr, Inhabited

/-- Global SSE publisher reference (set when app starts with SSE enabled) -/
initialize globalManagerRef : IO.Ref (Option Citadel.SSE.ConnectionManager) ← IO.mkRef none

/-- Initialize SSE with a connection manager (called by App.run when SSE is enabled) -/
def setup (manager : Citadel.SSE.ConnectionManager) : IO Unit :=
  globalManagerRef.set (some manager)

/-- Check if SSE is initialized -/
def isInitialized : IO Bool := do
  match ← globalManagerRef.get with
  | some _ => pure true
  | none => pure false

/-- Get the number of connected SSE clients -/
def clientCount : IO Nat := do
  match ← globalManagerRef.get with
  | some manager => manager.clientCount
  | none => pure 0

/-- Publish an event to a specific topic -/
def publish (topic : String) (event : Citadel.SSE.Event) : IO Unit := do
  match ← globalManagerRef.get with
  | none => IO.eprintln "Warning: SSE.publish called but SSE not initialized"
  | some manager => manager.broadcast topic event

/-- Publish to all connected clients regardless of topic -/
def publishAll (event : Citadel.SSE.Event) : IO Unit := do
  match ← globalManagerRef.get with
  | none => IO.eprintln "Warning: SSE.publishAll called but SSE not initialized"
  | some manager => manager.broadcastAll event

/-- Publish a simple message to a topic (event type = "message") -/
def publishMessage (topic : String) (data : String) : IO Unit :=
  publish topic (Citadel.SSE.Event.message data)

/-- Publish a named event with data to a topic -/
def publishEvent (topic : String) (eventType : String) (data : String) : IO Unit :=
  publish topic (Citadel.SSE.Event.named eventType data)

/-- Publish a simple message to all clients -/
def publishMessageAll (data : String) : IO Unit :=
  publishAll (Citadel.SSE.Event.message data)

/-- Publish a named event to all clients -/
def publishEventAll (eventType : String) (data : String) : IO Unit :=
  publishAll (Citadel.SSE.Event.named eventType data)

/-- Publish HTML content for HTMX SSE swapping -/
def publishHtml (topic : String) (eventType : String) (html : String) : IO Unit :=
  publishEvent topic eventType html

end Loom.SSE

/-! ## ActionM Extensions for SSE -/

namespace Loom.ActionM

/-- Publish an SSE event to a topic -/
def ssePublish (topic : String) (event : Citadel.SSE.Event) : ActionM Unit :=
  StateT.lift (SSE.publish topic event)

/-- Publish a message to a topic -/
def ssePublishMessage (topic : String) (data : String) : ActionM Unit :=
  StateT.lift (SSE.publishMessage topic data)

/-- Publish a named event to a topic -/
def ssePublishEvent (topic : String) (eventType : String) (data : String) : ActionM Unit :=
  StateT.lift (SSE.publishEvent topic eventType data)

/-- Publish HTML content for HTMX SSE swapping -/
def ssePublishHtml (topic : String) (eventType : String) (html : String) : ActionM Unit :=
  StateT.lift (SSE.publishHtml topic eventType html)

/-- Broadcast an event to all SSE clients -/
def sseBroadcast (event : Citadel.SSE.Event) : ActionM Unit :=
  StateT.lift (SSE.publishAll event)

/-- Broadcast a message to all SSE clients -/
def sseBroadcastMessage (data : String) : ActionM Unit :=
  StateT.lift (SSE.publishMessageAll data)

/-- Broadcast a named event to all clients -/
def sseBroadcastEvent (eventType : String) (data : String) : ActionM Unit :=
  StateT.lift (SSE.publishEventAll eventType data)

end Loom.ActionM
