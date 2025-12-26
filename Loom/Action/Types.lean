/-
  Loom.Action.Types - HTMX primitive types for the Action/Interaction system

  This module defines the core types that represent HTMX concepts:
  - HxMethod: HTTP methods (GET, POST, PUT, DELETE, PATCH)
  - Swap: Content swap strategies
  - Target: Element targeting specifications
  - Trigger: Event trigger specifications
  - ResponseStrategy: How to respond after handler execution
-/

namespace Loom.Action

/-- HTTP methods for HTMX requests -/
inductive HxMethod where
  | get
  | post
  | put
  | delete
  | patch
  deriving Repr, BEq, Inhabited

namespace HxMethod

def toString : HxMethod → String
  | get => "get"
  | post => "post"
  | put => "put"
  | delete => "delete"
  | patch => "patch"

def toAttrName : HxMethod → String
  | get => "hx-get"
  | post => "hx-post"
  | put => "hx-put"
  | delete => "hx-delete"
  | patch => "hx-patch"

instance : ToString HxMethod := ⟨toString⟩

end HxMethod

/-- HTMX swap strategies for content replacement -/
inductive Swap where
  | innerHTML
  | outerHTML
  | beforebegin
  | afterbegin
  | beforeend
  | afterend
  | delete
  | none
  | custom (value : String)
  deriving Repr, BEq, Inhabited

namespace Swap

def toString : Swap → String
  | innerHTML => "innerHTML"
  | outerHTML => "outerHTML"
  | beforebegin => "beforebegin"
  | afterbegin => "afterbegin"
  | beforeend => "beforeend"
  | afterend => "afterend"
  | delete => "delete"
  | none => "none"
  | custom s => s

instance : ToString Swap := ⟨toString⟩

/-- Swap with timing modifier -/
def withTiming (s : Swap) (duration : String) : Swap :=
  .custom s!"{s.toString} swap:{duration}"

/-- Swap with settle timing -/
def withSettle (s : Swap) (duration : String) : Swap :=
  .custom s!"{s.toString} settle:{duration}"

/-- Swap with scroll behavior -/
def withScroll (s : Swap) (target : String := "top") : Swap :=
  .custom s!"{s.toString} scroll:{target}"

/-- Swap with show behavior -/
def withShow (s : Swap) (target : String := "top") : Swap :=
  .custom s!"{s.toString} show:{target}"

/-- Swap with focus-scroll disabled -/
def noFocusScroll (s : Swap) : Swap :=
  .custom s!"{s.toString} focus-scroll:false"

end Swap

/-- HTMX target specification for selecting elements -/
inductive Target where
  | this
  | closest (selector : String)
  | find (selector : String)
  | next (selector : Option String := none)
  | previous (selector : Option String := none)
  | css (selector : String)
  deriving Repr, BEq, Inhabited

namespace Target

def toString : Target → String
  | this => "this"
  | closest s => s!"closest {s}"
  | find s => s!"find {s}"
  | next none => "next"
  | next (some s) => s!"next {s}"
  | previous none => "previous"
  | previous (some s) => s!"previous {s}"
  | css s => s

instance : ToString Target := ⟨toString⟩

/-- Target by element ID -/
def id (elemId : String) : Target := .css s!"#{elemId}"

/-- Target the body element -/
def body : Target := .css "body"

end Target

/-- HTMX trigger event specification -/
inductive Trigger where
  | click
  | submit
  | change
  | keyup
  | keydown
  | load
  | revealed
  | intersect
  | every (seconds : Nat)
  | custom (event : String)
  deriving Repr, BEq, Inhabited

namespace Trigger

def toString : Trigger → String
  | click => "click"
  | submit => "submit"
  | change => "change"
  | keyup => "keyup"
  | keydown => "keydown"
  | load => "load"
  | revealed => "revealed"
  | intersect => "intersect"
  | every n => s!"every {n}s"
  | custom e => e

instance : ToString Trigger := ⟨toString⟩

end Trigger

/-- Trigger modifiers that can be applied to triggers -/
structure TriggerMod where
  base : Trigger
  delay : Option String := none
  throttle : Option String := none
  from_ : Option String := none
  target : Option String := none
  consume : Bool := false
  queue : Option String := none  -- first, last, all, none
  once : Bool := false
  changed : Bool := false
  filter : Option String := none
  deriving Repr, Inhabited

namespace TriggerMod

def fromTrigger (t : Trigger) : TriggerMod := { base := t }

def toString (tm : TriggerMod) : String :=
  let parts := [tm.base.toString]
  let parts := match tm.delay with
    | some d => parts ++ [s!"delay:{d}"]
    | none => parts
  let parts := match tm.throttle with
    | some t => parts ++ [s!"throttle:{t}"]
    | none => parts
  let parts := match tm.from_ with
    | some f => parts ++ [s!"from:{f}"]
    | none => parts
  let parts := match tm.target with
    | some t => parts ++ [s!"target:{t}"]
    | none => parts
  let parts := if tm.consume then parts ++ ["consume"] else parts
  let parts := match tm.queue with
    | some q => parts ++ [s!"queue:{q}"]
    | none => parts
  let parts := if tm.once then parts ++ ["once"] else parts
  let parts := if tm.changed then parts ++ ["changed"] else parts
  let parts := match tm.filter with
    | some f => parts ++ [s!"[{f}]"]
    | none => parts
  " ".intercalate parts

instance : ToString TriggerMod := ⟨toString⟩

def withDelay (tm : TriggerMod) (d : String) : TriggerMod := { tm with delay := some d }
def withThrottle (tm : TriggerMod) (t : String) : TriggerMod := { tm with throttle := some t }
def withFrom (tm : TriggerMod) (f : String) : TriggerMod := { tm with from_ := some f }
def withTarget (tm : TriggerMod) (t : String) : TriggerMod := { tm with target := some t }
def withConsume (tm : TriggerMod) : TriggerMod := { tm with consume := true }
def withQueue (tm : TriggerMod) (q : String) : TriggerMod := { tm with queue := some q }
def withOnce (tm : TriggerMod) : TriggerMod := { tm with once := true }
def withChanged (tm : TriggerMod) : TriggerMod := { tm with changed := true }
def withFilter (tm : TriggerMod) (f : String) : TriggerMod := { tm with filter := some f }

end TriggerMod

/-- Sync strategy for coordinating requests -/
inductive Sync where
  | drop
  | abort
  | replace
  | queue (strategy : String := "last")  -- first, last, all
  deriving Repr, BEq, Inhabited

namespace Sync

def toString : Sync → String
  | drop => "drop"
  | abort => "abort"
  | replace => "replace"
  | queue s => s!"queue {s}"

instance : ToString Sync := ⟨toString⟩

end Sync

/-- Params filtering strategy -/
inductive Params where
  | all
  | none
  | not (names : List String)
  | only (names : List String)
  deriving Repr, BEq, Inhabited

namespace Params

def toString : Params → String
  | all => "*"
  | none => "none"
  | not names => s!"not {",".intercalate names}"
  | only names => ",".intercalate names

instance : ToString Params := ⟨toString⟩

end Params

end Loom.Action
