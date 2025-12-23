/-
  Loom.Router - Named routes and URL generation
-/
import Citadel
import Loom.Controller

namespace Loom

/-- A named route with associated action -/
structure NamedRoute where
  name : String
  method : Herald.Core.Method
  pattern : String
  action : Action

/-- Collection of named routes -/
structure Routes where
  routes : List NamedRoute := []

namespace Routes

/-- Empty routes -/
def empty : Routes := { routes := [] }

/-- Add a named route -/
def add (r : Routes) (name : String) (method : Herald.Core.Method) (pattern : String) (action : Action) : Routes :=
  { routes := r.routes ++ [{ name, method, pattern, action }] }

/-- Add a GET route -/
def get (r : Routes) (pattern : String) (name : String) (action : Action) : Routes :=
  r.add name .GET pattern action

/-- Add a POST route -/
def post (r : Routes) (pattern : String) (name : String) (action : Action) : Routes :=
  r.add name .POST pattern action

/-- Add a PUT route -/
def put (r : Routes) (pattern : String) (name : String) (action : Action) : Routes :=
  r.add name .PUT pattern action

/-- Add a DELETE route -/
def delete (r : Routes) (pattern : String) (name : String) (action : Action) : Routes :=
  r.add name .DELETE pattern action

/-- Add a PATCH route -/
def patch (r : Routes) (pattern : String) (name : String) (action : Action) : Routes :=
  r.add name .PATCH pattern action

/-- Find a route by name -/
def findByName (r : Routes) (name : String) : Option NamedRoute :=
  r.routes.find? (·.name == name)

/-- Generate a URL path for a named route with parameters -/
def pathFor (r : Routes) (name : String) (params : List (String × String) := []) : Option String :=
  match r.findByName name with
  | none => none
  | some route =>
    -- Replace :param placeholders with values
    let path := params.foldl (init := route.pattern) fun acc (k, v) =>
      acc.replace s!":{k}" (Cookie.urlEncode v)
    -- Check if all params were replaced (no remaining :param)
    if (path.splitOn ":").length > 1 then none
    else some path

/-- Generate a URL path, panicking if route not found -/
def pathFor! (r : Routes) (name : String) (params : List (String × String) := []) : String :=
  match r.pathFor name params with
  | some path => path
  | none => panic! s!"Route not found: {name}"

/-- Get all route names -/
def names (r : Routes) : List String :=
  r.routes.map (·.name)

/-- Convert to Citadel router -/
def toCitadelRouter (r : Routes) : Citadel.Router :=
  r.routes.foldl (init := Citadel.Router.empty) fun router route =>
    router.add route.method route.pattern (fun req => do
      -- The action will be wrapped by the App to provide context
      -- For now, just return a placeholder
      pure Citadel.Response.notFound)

end Routes

/-- URL helper functions accessible from templates -/
structure UrlHelpers where
  routes : Routes
  baseUrl : String := ""

namespace UrlHelpers

/-- Create URL helpers from routes -/
def create (routes : Routes) (baseUrl : String := "") : UrlHelpers :=
  { routes, baseUrl }

/-- Get path for a named route -/
def pathFor (h : UrlHelpers) (name : String) (params : List (String × String) := []) : String :=
  h.routes.pathFor name params |>.getD "#"

/-- Get full URL for a named route -/
def urlFor (h : UrlHelpers) (name : String) (params : List (String × String) := []) : String :=
  let path := h.pathFor name params
  if h.baseUrl.isEmpty then path else s!"{h.baseUrl}{path}"

end UrlHelpers

end Loom
