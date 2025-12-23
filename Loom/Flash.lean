/-
  Loom.Flash - Flash messages (one-time messages between requests)
-/
import Loom.Session

namespace Loom

/-- Flash message storage -/
structure Flash where
  /-- Messages available for this request (read-only) -/
  current : List (String × String)
  /-- Messages to be set for the next request -/
  next : List (String × String)
  deriving Repr

namespace Flash

/-- Empty flash -/
def empty : Flash := { current := [], next := [] }

/-- Get a flash message (from current) -/
def get (f : Flash) (key : String) : Option String :=
  f.current.lookup key

/-- Set a flash message (for next request) -/
def set (f : Flash) (key value : String) : Flash :=
  let newNext := (key, value) :: f.next.filter (·.1 != key)
  { f with next := newNext }

/-- Convenience: set info message -/
def info (f : Flash) (msg : String) : Flash :=
  f.set "info" msg

/-- Convenience: set error message -/
def error (f : Flash) (msg : String) : Flash :=
  f.set "error" msg

/-- Convenience: set success message -/
def success (f : Flash) (msg : String) : Flash :=
  f.set "success" msg

/-- Convenience: set warning message -/
def warning (f : Flash) (msg : String) : Flash :=
  f.set "warning" msg

/-- Convenience: set notice message -/
def notice (f : Flash) (msg : String) : Flash :=
  f.set "notice" msg

/-- Check if there are any current messages -/
def hasMessages (f : Flash) : Bool :=
  !f.current.isEmpty

/-- Get all current messages -/
def all (f : Flash) : List (String × String) :=
  f.current

/-- Session key prefix for flash messages -/
private def flashPrefix : String := "_flash_"

/-- Load flash from session (moves stored -> current) -/
def fromSession (s : Session) : Flash × Session :=
  let flashKeys := s.data.filterMap fun (k, v) =>
    if k.startsWith flashPrefix then
      some (k.drop flashPrefix.length, v)
    else
      none
  -- Clear flash from session
  let newSession := flashKeys.foldl (init := s) fun sess (k, _) =>
    sess.delete (flashPrefix ++ k)
  ({ current := flashKeys, next := [] }, newSession)

/-- Save flash to session (moves next -> session) -/
def toSession (f : Flash) (s : Session) : Session :=
  f.next.foldl (init := s) fun sess (k, v) =>
    sess.set (flashPrefix ++ k) v

/-- Keep a flash message for another request -/
def keep (f : Flash) (key : String) : Flash :=
  match f.current.lookup key with
  | some v => f.set key v
  | none => f

/-- Keep all flash messages for another request -/
def keepAll (f : Flash) : Flash :=
  f.current.foldl (init := f) fun fl (k, v) =>
    { fl with next := (k, v) :: fl.next.filter (·.1 != k) }

end Flash

end Loom
