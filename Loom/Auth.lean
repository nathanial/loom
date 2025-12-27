/-
  Loom.Auth - Common authentication middleware for route-level auth checks
-/
import Loom.Controller
import Loom.Middleware

namespace Loom.Auth

open Loom

/-- Check if a user is logged in (has user_id in session) -/
def isLoggedIn (ctx : Context) : Bool :=
  ctx.session.has "user_id"

/-- Get the current user ID from session -/
def currentUserId (ctx : Context) : Option String :=
  ctx.session.get "user_id"

/-- Middleware that requires authentication, redirects to /login if not logged in -/
def authRequired : RouteMiddleware :=
  RouteMiddleware.guard isLoggedIn "/login" "error" "Please log in to continue"

/-- Create a custom auth middleware with configurable redirect and message -/
def authRequiredTo (redirectTo : String) (flashMsg : String := "Please log in to continue") : RouteMiddleware :=
  RouteMiddleware.guard isLoggedIn redirectTo "error" flashMsg

/-- Check if user has a specific session value -/
def hasSessionValue (key : String) (value : String) (ctx : Context) : Bool :=
  ctx.session.get key == some value

/-- Middleware that requires a specific session value -/
def requireSessionValue (key : String) (value : String)
    (redirectTo : String) (flashMsg : String := "") : RouteMiddleware :=
  RouteMiddleware.guard (hasSessionValue key value) redirectTo "error" flashMsg

/-- Middleware that requires admin privileges (is_admin = "true" in session) -/
def adminRequired : RouteMiddleware :=
  RouteMiddleware.guard (hasSessionValue "is_admin" "true") "/"
    "error" "Access denied. Admin privileges required."

/-- Create admin middleware with custom redirect -/
def adminRequiredTo (redirectTo : String) (flashMsg : String := "Access denied. Admin privileges required.") : RouteMiddleware :=
  RouteMiddleware.guard (hasSessionValue "is_admin" "true") redirectTo "error" flashMsg

end Loom.Auth
